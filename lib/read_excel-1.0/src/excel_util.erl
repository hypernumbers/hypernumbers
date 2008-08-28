%%%-------------------------------------------------------------------
%%% File        : excel_util.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : parses the specific Excel components of a file
%%%
%%% Created     : 26 July 2007 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(excel_util).

-export([get_utf8/1,
         get_bound_sheet/2,
         parse_CRS_RK/1,
         parse_CRS_Uni16/1,
         parse_CRS_Uni16/2,
         get_len_CRS_Uni16/4,
         lookup_string/2,
         read_cell_range_add_list/2,
         read_cell_range_addies/3,
         write/3,
	 append/3,
         read/3,
         read_shared/2,
         get_length/2,
         append_sheetname/2]).

%%% Debugging exports
-export([parse_CRS_RK_TESTING/1,shift_left2_TESTING/1]).
-export([bodge_DEBUG/1]).

%%% Include file for eunit testing
-include_lib("eunit/include/eunit.hrl").

%%% Include files with macros encoding Microsoft File Format constants
-include("microsoftcompoundfileformat.hrl").
-include("microsoftbiff.hrl").
-include("excel_com_rec_subs.hrl").

get_utf8({[{'uni16-8',String}],_B,_C})->
    xmerl_ucs:to_utf8(binary_to_list(String));
get_utf8({[{'uni16-16',String}],_B,_C})->
    io:format("***********************************~n"),
    io:format("* in excel_util:get_utf8          *~n"),
    io:format("* This should not be being called *~n"),
    io:format("***********************************~n"),
    xmerl_ucs:to_utf8(xmerl_ucs:from_utf16le(binary_to_list(String))).

get_bound_sheet(Bin,_Tables)->
    <<SheetBOF:32/little-unsigned-integer,
     Visibility:8/little-unsigned-integer,
     SheetType:8/little-unsigned-integer,
     Name/binary>>=Bin,
    SheetName=excel_util:parse_CRS_Uni16(Name),
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
parse_CRS_RK(RKBin)->
    %% Section 2.5.5 of excelfileformats.pdf V1.40
    %% There is a bit mask applied but it is against the reassembled
    %% number and not the first digits of the little endian stream!
    <<Rem:6,Type:1,Shift:1,Rest:24>>=RKBin,
    %% io:format("in excel_util:parse_CRS_RK RKBin is ~p Rem is ~p Type is ~p "++
    %%	      "Shift is ~p and Rest is ~p~n",[RKBin,Rem,Type,Shift,Rest]),
    %% Rebuild the number
    Num2 = case Type of
	       ?CRS_RK_FLOATING_POINT ->
		   FPVal= <<Rem:6,0:2,Rest:24>>,
		   <<Num:64/little-float>>= <<0:32,FPVal/binary>>,
		   Num;
	       ?CRS_RK_INTEGER ->
		   %% io:format("in excel_util:parse_CRS_RK Integer~n"),
		   <<RKBin2:32/little-signed-integer>>=RKBin,
		   RKBin3= <<RKBin2:32>>,
		   %% io:format("in excel_util:parse_CRS_RK~n-RKBin is  ~p~n-"++
		   %%	     "RKBin3 is ~p~n",[RKBin,RKBin3]),
		   Num=get_integer(RKBin3),
		   %% io:format("in excel_util:parse_CRS_RK~n-Num is ~p~n",[Num]),
		   %% <<Num:64/little-unsigned-integer>>= <<0:32,IntVal/binary>>,
		   Num
	   end,
    %% io:format("in excel_util:parse_CRS_RK Num2 is ~p~n",[Num2]),
    case Shift of
	?CRS_RK_UNCHANGED         -> Num2;
	?CRS_RK_SHIFT_DOWN_BY_100 -> Num2/100
    end.

parse_CRS_Uni16(Bin)->
    %% Section 2.5.3 of excelfileformats.pdf V1.40
    %% The plan is simple - proceed as if the initial index is a single byte
    %% long - but once you have enough info check if the string length is valid
    %% if it isn't then start again with the presumption that it is a 2 indexer
    try
	parse_CRS_Uni16_intermediate(Bin)
    catch
	exit:_Reason ->
	    %% Try again with an index of 2
	    parse_CRS_Uni16(Bin,2);
	  error:_Message ->
	    %% Try again with an index of 2
	    io:format("in parse_CRS_Uni16 "++
		      "trying an index length of 2~n"),
	    parse_CRS_Uni16(Bin,2);
	  throw:Term ->
	    io:format("in parse_CRS_Uni16 "++
		      "Thrown with Term ~p~n",[Term]),
	    exit(Term)
    end.

%% the purpose of the intermediate function is for when you don't know
%% if the string is a 1 or 2 index length string and you need to have a guess...
%% in these cases you sometimes decide if the 1 index length has failed by testing
%% if the extracted string corresponds to the length of the binary...
parse_CRS_Uni16_intermediate(Bin)->
    {Return,BinLen1,BinLen2}=parse_CRS_Uni16(Bin,1),
    case BinLen1 of
	BinLen2  -> {Return,BinLen1,BinLen2};
	_Other   -> exit("Wrong Index Length")
    end.

parse_CRS_Uni16(Bin,IndexSize)->
    LenSize=IndexSize*8,
    <<Len:LenSize/little-unsigned-integer,
     NFlags:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    {LenStr,Encoding,BinLen1,
     {RICH_TEXT,LenRichText,_LenRichTextIdx},
     {ASIAN,LenAsian,_LenAsianIdx},Rest3}=get_bits_CRS_Uni16(Len,IndexSize,Rest,NFlags),
    BinLen2=erlang:size(Bin),
    %% Now we need to parse the rest of the binary
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
    {List3,BinLen1,BinLen2}.

get_len_CRS_Uni16(Len,IndexSize,Bin,Flags)->
    {_LenStr,_Encoding,BinLen,
     {_RICH_TEXT,_LenRichText,_LenRichTextIdx},
     {_ASIAN,_LenAs,_LenAIdx},_Rest3}=get_bits_CRS_Uni16(Len,IndexSize,Bin,Flags),
    BinLen.

get_bits_CRS_Uni16(Len,IndexSize,Bin,NFlags)->
    {ok,UNCOMP}   =check_flags(NFlags,?CRS_UNI16_UNCOMPRESSED),
    {ok,ASIAN}    =check_flags(NFlags,?CRS_UNI16_ASIAN),
    {ok,RICH_TEXT}=check_flags(NFlags,?CRS_UNI16_RICH_TEXT),
    case RICH_TEXT of
        match ->
            <<LenRichText:16/little-unsigned-integer,Rest2/binary>>=Bin,
            LenRichTextIdx=2;
        no_match ->
            Rest2=Bin,
            LenRichText=0,
            LenRichTextIdx=0
    end,
    case ASIAN of
        match ->
            <<LenAsian:16/little-unsigned-integer,Rest3/binary>>=Rest2,
            LenAsianIdx=4;
        no_match ->
            Rest3=Rest2,
            LenAsian=0,
            LenAsianIdx=0
    end,
    %% We now have info to calculate the length of the binary
    {LenStr,Encoding} = case UNCOMP of
                            match    -> {Len*2,'uni16-16'};
                            no_match -> {Len,  'uni16-8'}
                        end,
    BinLen=1+IndexSize+LenRichTextIdx+LenAsianIdx+LenStr+LenRichText*4+LenAsian,
    {LenStr,Encoding,BinLen,{RICH_TEXT,LenRichText,LenRichTextIdx},
     {ASIAN,LenAsian,LenAsianIdx},Rest3}.

%% Read a Cell Range Address List
%% defined in Section 2.5.15 of excelfileformatV1-40.pdf
%%
%% Size can be either '8bit' or '16bit' depending on how the column index 
%% is stored
%%
read_cell_range_add_list(Bin,Size)->
    %%io:format("in excel_util:read_cell_range_add_list Bin is ~p~n",[Bin]),
    <<NoAddies:16/little-signed-integer,
     Rest/binary>>=Bin,
    %%io:format("in excel_rev_comp:read_cell_range_add_list NoAddies is ~p~n",
    %%      [NoAddies]),
    read_cell_range_addies(NoAddies,Size,Rest).

%% This function pulls a set of cell_range_addies out of the list of them.
%% the format of each addie is described in Section 2.5.14 of the
%% excelfileformat.v1.40.pdf
read_cell_range_addies(N,Size,Array)->
    read_cell_range_addies(N,Size,Array,[]).

read_cell_range_addies(0,_Size,Bin,Acc)->
    {lists:reverse(Acc),Bin};
read_cell_range_addies(N,'16bit',Bin,Acc)->
    <<FirstRowIdx:16/little-signed-integer,
     LastRowIdx:16/little-signed-integer,
     FirstColIdx:16/little-signed-integer,
     LastColIdx:16/little-signed-integer,
     Rest/binary>>=Bin,
    NewAcc=[{FirstRowIdx,LastRowIdx,FirstColIdx,LastColIdx}|Acc],
    read_cell_range_addies(N-1,'16bit',Rest,NewAcc);
read_cell_range_addies(N,'8bit',Bin,Acc)->
    <<FirstRowIdx:16/little-signed-integer,
     LastRowIdx:16/little-signed-integer,
     FirstColIdx:8/little-signed-integer,
     LastColIdx:8/little-signed-integer,
     Rest/binary>>=Bin,
    NewAcc=[{FirstRowIdx,LastRowIdx,FirstColIdx,LastColIdx}|Acc],
    read_cell_range_addies(N-1,'8bit',Rest,NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Utility functions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% get the current length of the table
get_length(Tables,Name)->
    {value,{Name,Tid}}=lists:keysearch(Name,1,Tables),
    Info=ets:info(Tid),
    {_,{_,Length}}=lists:keysearch(size,1,Info),
    Length.

%% append a value to a table that needs a sequential index
append(Tables,Name,Record) ->
    Index={index,get_length(Tables,Name)},
    {value,{Name,Tid}}=lists:keysearch(Name,1,Tables),
    ets:insert(Tid,{Index,Record}).

%% write values to the tables
write(Tables,Name,[H|T])->
    {value,{Name,Tid}}=lists:keysearch(Name,1,Tables),
    ets:insert(Tid,{H,T}).

%% read shared formula from the shared/array formula table
%% this is a bit messy
%%
%% Normally the tExp token will contain the top left hand marker of the 
%% appropriate record but this is not always the case
%%
%% See Section 3.10.1 of excelfileformatV1-41.pdf
%%
%% There can be an overlap of shared formulae and array formulae as well 
%% - in which case the array supercedes the shared (I think)
%%
%% So first read for an array - if that returns blank then 
%% search for an exact shared match
%% if that turns up blank then onto a range interception
%% on a shared (ie don't check the top left but that the passed in row/col
%% intersects the range
read_shared(Tables,{{sheet,Name},{row_index,Row},{col_index,Col}})->
    TableName=sh_arr_formula,
    {value,{_,Tid}}=lists:keysearch(TableName,1,Tables),
    %% this fun reads an array formula
    ArrayFn=fun(X,Acc)->
                    case X of
                        {{{sheet,Name},{row_index,Row},
                          {col_index,Col}},_Rest} -> [X|Acc];
                        _Other                    -> Acc
                    end
            end,
    %% this fun reads an shared formula which matches at the top left
    ExShrdFn = fun(X,Acc)->
                       case X of
                           {{{sheet,Name},{firstrow,Row},
                             {firstcol,Col},{lastrow,_LastRow},
                             {lastcol,_LastCol}},_Rest} -> [X|Acc];
                           _Other                      -> Acc
                       end
               end,
    %% this fun reads a shared formula which intersects with the range
    IntShrdFn=fun(X,Acc)->
                       case X of
                           {{{sheet,Name},{firstrow,FirstRow},
                             {firstcol,FirstCol},{lastrow,LastRow},
                             {lastcol,LastCol}},_Rest} 
                           when Row >= FirstRow, Row =< LastRow,
                           Col >= FirstCol, Col =< LastCol       -> [X|Acc];
                           _Other                                -> Acc
                       end
               end,
    FirstReturn = ets:foldl(ArrayFn,[],Tid),
    %% io:format("in excel_util:read_shared FirstReturn is ~p~n",[FirstReturn]),
    SecondReturn = case FirstReturn of
                       []      -> ets:foldl(ExShrdFn,[],Tid);
                       _Other2 -> FirstReturn
                   end,
    %% io:format("in excel_util:read_shared SecondReturn is ~p~n",[SecondReturn]),
    ThirdReturn = case SecondReturn of
		      []      -> ets:foldl(IntShrdFn,[],Tid);
		      _Other3 -> SecondReturn
		  end,
    %% io:format("in excel_util:read_shared ThirdReturn is ~p~n",[ThirdReturn]),
    %% sometimes Excel will store two or more shared formulae that overlap
    %% or more acurately it stores the same token set with two different ranges
    %% this tends to cause things to wig...
    %% so we need to check that when there is more than one formula returned
    %% the tokens are the same
    FourthReturn=check_formulae(ThirdReturn),
    %% io:format("in excel_util:read_shared FourthReturn is ~p~n",[FourthReturn]),
    FourthReturn.

check_formulae([H|T]) -> case check_formulae([H|T],[]) of
			     true  -> [H|[]];
			     false -> exit("overlapping incompatible shared formulae")
			 end.

check_formulae([],Acc)    -> check2(Acc);
check_formulae([H|T],Acc) ->
    {{{sheet,Name},{firstrow,_A},{firstcol,_B},{lastrow,_C},{lastcol,_T}},Tokens} = H, 
    check_formulae(T,[{Name,Tokens}|Acc]).

check2(List) -> check2(List, []).

check2([],_X)        -> true;
check2([H|T],[])     -> check2(T,H);
check2([X|T],X)      -> check2(T,X);
check2([_H|_T],_Acc) -> false.

%% read values from the tables
read(Tables,Name,Key)->
    {value,{_TableName,Tid}}=lists:keysearch(Name,1,Tables),
    %% filefilters:dump([{Name,Tid}]),
    ets:lookup(Tid,{index,Key}).

%% append a sheet name to the sheetname table with a zero-based
%% index value
append_sheetname(Tables,SheetName)->
    {value,{_TableName,Tid}}=lists:keysearch(sheetnames,1,Tables),
    Size=ets:info(Tid,size),
    Record={{index,Size},[{name,SheetName}]},
    ets:insert(Tid,Record).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These are some utility functions that ought to be in a library           %%%
%%%                                                                          %%%
%%% (These ones have been nicked and bodged from log_utilities in vixo...)   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_string(Tables,SSTIndex)->
    {value,{strings,Tid}}=lists:keysearch(strings,1,Tables),
    [{_,[{_,String}]}]=ets:lookup(Tid,{index,SSTIndex}),
    String.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%%  Internal Functions                                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% negative number
get_integer(Bin) ->
    <<Type:1,_Rest:31>>=Bin,
    case Type of
	1 -> %% Rest is a 30 bit binary in ones complement
	    -1*ones_complement(shift_left2(Bin))+1;
	0 -> shift_left2(Bin)
    end.

ones_complement(Bin) ->
    Mask=1073741823, % 30 ones in binary 111111111111111111111111111111
    %% Return=Int bxor Mask,
    Bin bxor Mask.

%% ones_complement(<<>>,Acc)                -> list_to_binary(lists:reverse(Acc));
%% ones_complement(<<1:1,Rest/binary>>,Acc) -> ones_complement(<<Rest/binary>>,[0|Acc]);
%% ones_complement(<<0:1,Rest/binary>>,Acc) -> ones_complement(<<Rest/binary>>,[1,Acc]).

shift_left2(Bin)->
    <<Thirty:30,_:2>>=Bin,
    Bin2= <<0:2,Thirty:30>>,
    io:format("in excel_util:shift_left2/1~n-Bin is ~p~n-Bin2 is ~p~n",
	      [Bin,Bin2]),
    <<Return:32>>=Bin2,
    Return.

%%shift_left2(<<End:6,_:2>>,Acc)->
%%    list_to_binary(lists:reverse([<<End:6,0:2>>|Acc]));
%%shift_left2(<<Rem1:6,_:2,Rem2:6,Shift:2,Rest/binary>>,Acc)->
%%    shift_left2(<<Rem2:6,Shift:2,Rest/binary>>,[<<Shift:2,Rem1:6>>|Acc]);
%%shift_left2(<<Rem1:6,_:2,Rem2:6,Shift:2>>,Acc)->
%%    shift_left2(<<Rem2:6,Shift:2>>,[<<Shift:2,Rem1:6>>|Acc]).

check_flags(NFlags,Flag)->
    case NFlags band Flag of
	Flag -> {ok, match};
	_    -> {ok, no_match}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These are the testing interfaces that should not be used in production   %%%
%%%                                                                          %%%
%%% (ie these wrapper functions make otherwise private functions exported)   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_CRS_RK_TESTING(Value) -> parse_CRS_RK(Value).
shift_left2_TESTING(Bin)    -> shift_left2(Bin).

bodge_DEBUG(Bin)->
    Len=length(binary_to_list(Bin)),
    bodge(Bin,Len,[]).

bodge(<<>>,0,Acc)->
    lists:reverse(Acc);
bodge(<<Char:8/little-unsigned-integer,Rest/binary>>,Len,Acc) 
  when Char < 32; Char > 126 ->
    bodge(Rest,Len-1,[32|Acc]);
bodge(<<Char:8/little-signed-integer,Rest/binary>>,Len,Acc)->
    bodge(Rest,Len-1,[Char|Acc]).

