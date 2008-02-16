%%%-------------------------------------------------------------------
%%% File        : excel_util.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : parses the specific Excel components of a file
%%%
%%% Created     : 26 July 2007 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(excel_util).

-export([get_bound_sheet/3,
          put_log/2,parse_CRS_RK/2,
          parse_CRS_Uni16/2,parse_CRS_Uni16/3,
          get_len_CRS_Uni16/5,
          lookup_string/2,read_cell_range_add_list/2,
          read_cell_range_addies/3,
          write/3,read/3,append_sheet_name/2]).

%%% Debugging exports
-export([parse_CRS_RK_TESTING/2,shift_left2_TESTING/1]).

%%% Include file for eunit testing
-include_lib("eunit/include/eunit.hrl").

%%% Include files with macros encoding Microsoft File Format constants
-include("microsoftcompoundfileformat.hrl").
-include("microsoftbiff.hrl").
-include("excel_com_rec_subs.hrl").

get_bound_sheet(Bin,_Tables,FileOut)->
    <<SheetBOF:32/little-unsigned-integer,
     Visibility:8/little-unsigned-integer,
     SheetType:8/little-unsigned-integer,
     Name/binary>>=Bin,
    SheetName=excel_util:parse_CRS_Uni16(Name,FileOut),
    %% The Sheetnames are the names of all the worksheets in the workbook
    %% The order in which the Sheetnames appear in this function is determined
    %% by the zero-ordered Sheetindex. In other words the names appear here
    %% in the order of the sheets left to right in the tab bar of Excel
    {SheetBOF,Visibility,SheetType,Name,SheetName}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process the common record substructures that are used    %%%
%%% in the BIFF8/BIFF8X formats as described in Section 2.5 of the           %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_CRS_RK(RKBin,FileOut)->
    %% Section 2.5.5 of excelfileformats.pdf V1.40
    %% There is a bit mask applied but it is against the reassembled
    %% number and not the first digits of the little endian stream!
    <<Rem:6,Type:1,Shift:1,Rest:24>>=RKBin,
    %%<<Type:1,Shift:1,Rem:6,Rest:24>>=RKBin,
    put_log(FileOut,io_lib:fwrite("Shift is ~p~n"++
				  "(CRS_RK) Type is ~p~n"++
				  "RKBin is ~p",
				  [Shift,
				   Type,
				   RKBin])),
    %% Rebuild the number
    Num2 = case Type of
	       ?CRS_RK_FLOATING_POINT ->
		   FPVal= <<Rem:6,0:2,Rest:24>>,
		   <<Num:64/little-float>>= <<0:32,FPVal/binary>>,
		   Num;
	       ?CRS_RK_INTEGER ->
		   IntVal= shift_left2(RKBin),
		   <<Num:32/little-unsigned-integer>>= IntVal,
		   Num
	   end,
    case Shift of
	?CRS_RK_UNCHANGED         ->
	    Num2;
	?CRS_RK_SHIFT_DOWN_BY_100 ->
	    Num2/100
    end.

parse_CRS_Uni16(Bin,FileOut)->
    %% Section 2.5.3 of excelfileformats.pdf V1.40
    %% The plan is simple - proceed as if the initial index is a single byte
    %% long - but once you have enough info check if the string length is valid
    %% if it isn't then start again with the presumption that it is a 2 indexer
    try
	parse_CRS_Uni16_intermediate(Bin,FileOut)
    catch
	exit:_Reason ->
	    %% Try again with an index of 2
	    io:format("in parse_CRS_Uni16 "++
		      "trying an index length of 2~n"),
	    parse_CRS_Uni16(Bin,2,FileOut);
	  error:_Message ->
	    %% Try again with an index of 2
	    io:format("in parse_CRS_Uni16 "++
		      "trying an index length of 2~n"),
	    parse_CRS_Uni16(Bin,2,FileOut);
	  throw:Term ->
	    io:format("in parse_CRS_Uni16 "++
		      "Thrown with Term ~p~n",[Term]),
	    exit(Term)
    end.

%% the purpose of the intermediate function is for when you don't know
%% if the string is a 1 or 2 index length string and you need to have a guess...
%% in these cases you sometimes decide if the 1 index length has failed by testing
%% if the extracted string corresponds to the length of the binary...
parse_CRS_Uni16_intermediate(Bin,FileOut)->
    {Return,BinLen1,BinLen2}=parse_CRS_Uni16(Bin,1,FileOut),
    case BinLen1 of
	BinLen2 -> Return;
	_Other   -> io:format("bombing out of parse_CRS_Uni16 "++
			      "wrong index length~n"),
		    exit("Wrong Index Length")
    end.

parse_CRS_Uni16(Bin,IndexSize,FileOut)->
    %% put_log(FileOut,io_lib:fwrite("Bin is ~p~nIndexSize is ~p",
    %%        [Bin,IndexSize])),
    LenSize=IndexSize*8,
    %% io:format("in excel_util:parse_CRS_Uni16 Bin is ~p IndexSize is ~p LenSize is ~p~n",
    %%   [Bin,IndexSize,LenSize]),
    <<Len:LenSize/little-unsigned-integer,
     NFlags:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    %% io:format("in excel_util:parse_CRS_Uni16 NFlags is ~p~n",[NFlags]),
    {LenStr,Encoding,BinLen1,
      {RICH_TEXT,LenRichText,_LenRichTextIdx},
      {ASIAN,LenAsian,_LenAsianIdx},Rest3}=get_bits_CRS_Uni16(Len,IndexSize,Rest,NFlags,FileOut),
    BinLen2=erlang:size(Bin),
    put_log(FileOut,io_lib:fwrite("In excel_util:parse_CRS_Uni16~n  "++
				  "BinLen1 is ~p~n  BinLen2 is ~p~n",
				  [BinLen1,BinLen2])),
    %% io:format("In excel_util:parse_CRS_Uni16~n  "++
		%% 		  "-Bin is ~p~n-BinLen1 is ~p~n-BinLen2 is ~p~n",
		%% 		  [Bin,BinLen1,BinLen2]),
    %% Now we need to parse the rest of the binary
    %% io:format("In excel_util:parse_CRS_Uni16 Rest3 is ~p and LenStr is ~p~n",
    %%     [Rest3,LenStr]),
    <<String:LenStr/binary,Rest4/binary>>=Rest3,
    List1=[{Encoding,String}],
    case RICH_TEXT of
      match    -> 
		    <<RichText:LenRichText/binary,Rest5/binary>>=Rest4,
		    List2=[{richtext,RichText}|List1];
      no_match -> 
        Rest5=Rest4,
		    List2=List1
    end,
    case ASIAN of
      match    ->
		    <<Asian:LenAsian/binary>>=Rest5,
		    List3=[{asian,Asian}|List2];
      no_match -> List3=List2
    end,
    put_log(FileOut,io_lib:fwrite("in excel_util:parse_CRS_Uni16 List3 is ~p~n",[List3])),
    {List3,BinLen1,BinLen2}.

get_len_CRS_Uni16(Len,IndexSize,Bin,Flags,FileOut)->
    {_LenStr,_Encoding,BinLen,
      {_RICH_TEXT,_LenRichText,_LenRichTextIdx},
      {_ASIAN,_LenAsian,_LenAsianIdx},_Rest3}=get_bits_CRS_Uni16(Len,IndexSize,Bin,Flags,FileOut),
    BinLen.

get_bits_CRS_Uni16(Len,IndexSize,Bin,NFlags,FileOut)->
    {ok,UNCOMP}   =check_flags(NFlags,?CRS_UNI16_UNCOMPRESSED),
    {ok,ASIAN}    =check_flags(NFlags,?CRS_UNI16_ASIAN),
    {ok,RICH_TEXT}=check_flags(NFlags,?CRS_UNI16_RICH_TEXT),
    put_log(FileOut,io_lib:fwrite("Len is ~p~nNFlags is ~p~nUNCOMP is ~p~n"++
				  "ASIAN is ~p~nRICH_TEXT is ~p",
				  [Len,NFlags,UNCOMP,ASIAN,RICH_TEXT])),
    %% io:format("in get_len_CRS_Uni16 Len is ~p~nNFlags is ~p~nUNCOMP is ~p~n"++
		%% 		  "ASIAN is ~p~nRICH_TEXT is ~p~n",
		%% 		  [Len,NFlags,UNCOMP,ASIAN,RICH_TEXT]),
    case RICH_TEXT of
      match ->
        <<LenRichText:16/little-unsigned-integer,Rest2/binary>>=Bin,
        LenRichTextIdx=2;
      no_match ->
        Rest2=Bin,
        LenRichText=0,
        LenRichTextIdx=0
      end,
    %% io:format("in excel_util:get_len_CRS_Uni16 Bin is ~p~n-Rest2 is ~p~n",[Bin,Rest2]),
    case ASIAN of
      match ->
         <<LenAsian:16/little-unsigned-integer,Rest3/binary>>=Rest2,
         LenAsianIdx=4;
      no_match ->
        Rest3=Rest2,
        LenAsian=0,
        LenAsianIdx=0
    end,
    %% io:format("in excel_util:get_len_CRS_Uni16 Rest2 is ~p~n-Rest3 is ~p~n",[Rest2,Rest3]),
    %% We now have info to calculate the length of the binary
    {LenStr,Encoding} = case UNCOMP of
      match    -> {Len*2,'uni16-16'};
      no_match -> {Len,  'uni16-8'}
    end,
    %%put_log(FileOut,io_lib:fwrite("In excel_util:get_len_CRS_Uni16~n  "++
		%%		  "LenRichTextIdx is ~p~n  "++
		%%		  "LenAsianIdx is ~p~n  "++
		%%		  "LenStr is ~p~n  LenRichText is ~p~n  "++
		%%		  "LenAsian is ~p~n",
		%%		  [LenRichTextIdx,LenAsianIdx,LenStr,
		%%		   LenRichText,LenAsian])),
    %% io:format("In excel_util:get_len_CRS_Uni16~n "++
		%% 		  "LenRichTextIdx is ~p~n  "++
		%% 		  "LenAsianIdx is ~p~n  "++
		%% 		  "LenStr is ~p~n  LenRichText is ~p~n  "++
		%% 		  "LenAsian is ~p~n",
		%% 		  [LenRichTextIdx,LenAsianIdx,LenStr,
		%% 		   LenRichText,LenAsian]),
    BinLen=1+IndexSize+LenRichTextIdx+LenAsianIdx+LenStr+LenRichText*4+LenAsian,
    {LenStr,Encoding,BinLen,{RICH_TEXT,LenRichText,LenRichTextIdx},{ASIAN,LenAsian,LenAsianIdx},Rest3}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Utility functions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% write values to the tables
write(Tables,Name,[H|T])->
  {value,{Name,Tid}}=lists:keysearch(Name,1,Tables),
  ets:insert(Tid,{H,T}).

%% read values fromt the tables
read(Tables,Name,Key)->
  {value,{_TabName,Tid}}=lists:keysearch(Name,1,Tables),
  Return=ets:lookup(Tid,Key),
  case {Name,Return} of
    {arrayformula,[]} -> "fix me up in excel_util:read, ya wank - arrayformula record not read yet!";
    {sheetnames,[]}  -> [{{blah,blah},[{blah,"fix me up in excel_util:read, ya bam - sheetnames record not read yet!"}]}];
    _                       -> Return
    end.

%% append a sheet name to the sheetname table with a zero-based
%% index value
append_sheet_name(Tables,SheetName)->
    {value,{_Name,Tid}}=lists:keysearch(sheetnames,1,Tables),
    Size=ets:info(Tid,size),
    Record={{index,Size},[{name,SheetName}]},
    ets:insert(Tid,Record).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Eunit test functions                                                     %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_RK_test_() ->
    [?_assert(parse_CRS_RK(<<63,240,0,0>>,
			   "/dev/null") == 1.0),       % hex 3F F0 00 00
     ?_assert(parse_CRS_RK(<<239,240,0,1>>,
			   "/dev/null") == 0.01),      % hex EE F0 00 01
     ?_assert(parse_CRS_RK(<<0,75,86,70>>,
			   "/dev/null") == 1234321),   % hex 00 4B 56 46
     ?_assert(parse_CRS_RK(<<00,75,86,71>>,
			   "/dev/null") == 12343.21)]. % hex 00 4B 56 47

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These are some utility functions that ought to be in a library           %%%
%%%                                                                          %%%
%%% (These ones have been nicked and bodged from log_utilities in vixo...)   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_string(Tables,SSTIndex)->
    {value,{strings,Tid}}=lists:keysearch(strings,1,Tables),
    %%io:format("in excel_util:lookup_string Tid for String is ~p~n",[Tid]),
    %%io:format("in excel_util:lookup_string SSTIndex is ~p~n",[SSTIndex]),
    [{_,[{_,String}]}]=ets:lookup(Tid,{index,SSTIndex}),
    %%io:format("in excel_util:lookup_string String is ~p~n",[String]),
    String.

put_log(File,String) ->
    filelib:ensure_dir(File),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [String]),
            file:close(Id);
        _ ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%%  Internal Functions                                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shift_left2(Bin)->
    shift_left2(Bin,[]).

shift_left2(<<End:6,_:2>>,Residuum)->
    list_to_binary(lists:reverse([<<End:6,0:2>>|Residuum]));
shift_left2(<<Rem1:6,_:2,Rem2:6,Shift:2,Rest/binary>>,Residuum)->
    shift_left2(<<Rem2:6,Shift:2,Rest/binary>>,[<<Shift:2,Rem1:6>>|Residuum]);
shift_left2(<<Rem1:6,_:2,Rem2:6,Shift:2>>,Residuum)->
    shift_left2(<<Rem2:6,Shift:2>>,[<<Shift:2,Rem1:6>>|Residuum]).

check_flags(NFlags,Flag)->
    case NFlags band Flag of
	Flag -> {ok, match};
	_    -> {ok, no_match}
    end.

%% Read a Cell Range Address List
%% defined in Section 2.5.15 of excelfileformatV1-40.pdf 
read_cell_range_add_list(Bin,FileOut)->
  io:format("in excel_rev_comp:read_cell_range_add_list Bin is ~p~n",[Bin]),
  <<NoAddies:16/little-signed-integer,
    Rest/binary>>=Bin,
  io:format("in excel_rev_comp:read_cell_range_add_list NoAddies is ~p~n",
        [NoAddies]),
  read_cell_range_addies(NoAddies,Rest,FileOut).

%% This function pulls a set of cell_range_addies out of the list of them.
%% the format of each addie is described in Section 2.5.14 of the
%% excelfileformat.v1.40.pdf
read_cell_range_addies(N,Array,FileOut)->
  %%io:format("********Starting Cell Range List Parsing**********************************~n"),
  %%io:format("in excel_rev_comp:read_cell_range_addies"),
  read_cell_range_addies(N,Array,[],FileOut).

read_cell_range_addies(0,Bin,Residuum,_FileOut)->
  %%io:format("**********Ending Cell Range List Parsing**********************************~n"),
  {lists:reverse(Residuum),Bin};
read_cell_range_addies(N,Bin,Residuum,FileOut)->
  %%io:format("in excel_rev_comp:read_cell_range_addies for N of ~p~n",[N]),
  <<FirstRowIdx:16/little-signed-integer,
    LastRowIdx:16/little-signed-integer,
    FirstColIdx:16/little-signed-integer,
    LastColIdx:16/little-signed-integer,
    Rest/binary>>=Bin,
    NewResiduum=[{FirstRowIdx,LastRowIdx,FirstColIdx,LastColIdx}|Residuum],
    read_cell_range_addies(N-1,Rest,NewResiduum,FileOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These are the testing interfaces that should not be used in production   %%%
%%%                                                                          %%%
%%% (ie these wrapper functions make otherwise private functions exported)   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_CRS_RK_TESTING(Value,FileOut) -> parse_CRS_RK(Value,FileOut).
shift_left2_TESTING(Bin)            -> shift_left2(Bin).
