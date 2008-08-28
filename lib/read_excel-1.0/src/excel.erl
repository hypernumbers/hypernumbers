%%%-------------------------------------------------------------------
%%% File        : excel.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : parses the specific Excel components of a file
%%%
%%% Created     :  6 Apr 2007 by Gordon Guthrie <gordonguthrie@gg-laptop>
%%%-------------------------------------------------------------------
-module(excel).

-export([read_excel/9,get_file_structure/8]).

%% Prolly should be in excel_util
-export([get_named_SID/2]).

-include("microsoftcompoundfileformat.hrl").
-include("microsoftbiff.hrl").
-include("excel_base_tokens.hrl").
-include("excel_classified_tokens.hrl").
-include("excel_records.hrl").
-include("excel_structure.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Although this appears to be a distinct module that relates to Excel %%%
%%% it is actually an implementation of the Microsoft Compound File     %%%
%%% Format for the specific case of an Excel file.                      %%%
%%%                                                                     %%%
%%% This format is described in the compounddocfileformat.pdf which can %%%
%%% be found at http://sc.openoffice.org/compdocfileformat.pdf          %%%
%%%                                                                     %%%
%%% This version used in the spec of this function is Version 1.3       %%%
%%%                                                                     %%%
%%% This module performs two distinct functions - firstly it reads the  %%%
%%% normal storage stream - the second one reads the short storage      %%%
%%% - essentially little files are stuck in short storage and big ones  %%%
%%% are in normal storage streams.                                      %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to read the whole file                                    %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_excel(Directory,SAT,SSAT,SectorSize,ShortSectorSize,
	   _MinStreamSize,{{SubLoc,SubSID},SubStreams},FileIn,Tables)->
    %% Bear in mind that if the location is short stream the 'SID' returned
    %% is actually an SSID!
    %% io:format("~n~nNow going to parse the Excel Workbook only!~n"),
    {Location,SID}=get_workbook_SID(Directory),
    Bin=case Location of
	    normal_stream -> get_normal_stream(SID,Directory,SAT,SectorSize,
                                               FileIn);
	    short_stream  -> get_short_stream(SID,Directory,SAT,SSAT,SectorSize,
					      ShortSectorSize,FileIn)
	end,
    %% Now parsing the Excel components of the file
    %%
    %% To understand what is going on here you need to read Section 4.1.2 of
    %% excelfileformatV1-40.pdf Basically we first read the 'Workbook Globals 
    %% SubStream' and the all the 'Sheet SubStreams'
    %%
    %% The 'Workbook Globals Substream' contains things that are 'file wide':
    %% * string
    %% * tables
    %% * formats
    %% * fonts
    %% * styles
    %% * etc, etc
    %% as described in Section 4.2.5 of excelfileformatV1-40.pdf
    %%
    %% The 'Sheet Substreams' constist of things on a particular Excel tab as 
    %% described in Section 4.2.5 of excelfileformatV1-40.pdf
    %%

    %% First parse the 'Workbook Globals SubStream' 
    CurrentFormula="There is no current Formula yet!",
    parse_bin(Bin,{'utf-8',list_to_binary("Workbook Globals SubStream")},
              CurrentFormula,Tables),
    %% Now parse all the 'Sheet Substeams'
    [parse_substream(SubSID,SubLoc,X,Directory,SAT,SSAT,SectorSize,
		     ShortSectorSize,FileIn,Tables) || X <- SubStreams],
    %% Now that the complete file is read reverse_compile the token stream
    %%    
    post_process_tables(Tables).

parse_substream(SubSID,Location,SubStream,Directory,SAT,SSAT,SectorSize,
		ShortSectorSize,FileIn,Tables)->
    {{[{Type,NameBin}],_,_},Offset}=SubStream,
    %% Name=binary_to_list(NameBin),
    %% io:format("Now parsing stream ~p~n",[Name]),
    Bin=case Location of 
	    normal_stream -> get_normal_stream(SubSID,Directory,SAT,SectorSize,
					       FileIn);
	    short_stream  -> get_short_stream(SubSID,Directory,SAT,SSAT,
                                              SectorSize,ShortSectorSize,FileIn)
	end,
    %% Because we are reading a substream we want to chop off the 
    %% binary up to the offset
    <<_Discard:Offset/binary,Rest/binary>>=Bin,
    %% pass in the actual name of the substream
    CurrentFormula="There is no current Formula yet!",
    parse_bin(Rest,{Type,NameBin},CurrentFormula,Tables).

parse_bin(Bin,SubStreamName,CurrentFormula,Tables)->
    <<Identifier:16/little-unsigned-integer,
     RecordSize:16/little-unsigned-integer,
     Rest/binary>>=Bin,
    {_,NameBin}=SubStreamName,
    Name=binary_to_list(NameBin),
    case Identifier of
 	?EOF ->	    
	    %% io:format("workstream ~p read!~n",[Name]),
 	    ok;
	?SST ->
	    %% io:format("In excel:parse_bin Name is ~p~n",[Name]),
	    {ok,BinList,Rest2}=get_single_SST(Bin),
	    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,BinList,
                                                           Name,CurrentFormula,
                                                           Tables),
 	    parse_bin(Rest2,SubStreamName,NewCurrentFormula,Tables);
	_Other ->
 	    <<Record:RecordSize/binary,Rest3/binary>>=Rest,
	    %% Bodge=excel_records:bodge(Record),
	    %% bits:log("Record is "++integer_to_list(Other)),
	    %% bits:log("Record is "++Bodge),
	    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,Record,
                                                           Name,CurrentFormula,
                                                           Tables),
 	    parse_bin(Rest3,SubStreamName,NewCurrentFormula,Tables)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to get a concatenated SST record                          %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_single_SST(Bin)->
    get_single_SST(Bin,[]).

get_single_SST(Bin,Residuum)->    
    <<Identifier:16/little-unsigned-integer,Rest/binary>>=Bin,
    case Identifier of
	?SST      -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     get_single_SST(Rest3,[Record|Residuum]);
	?CONTINUE -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     get_single_SST(Rest3,[Record|Residuum]);
	%% the EXTSST record is simply an index for fast lookup
	%% on the SST record so we just chuck it...
	?EXTSST   -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<_Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     {ok,lists:reverse(Residuum),Rest3}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to read the structure of the file                         %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_file_structure(ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
		   _MinStreamSize,Tables,FileIn)->
    %% Remember that if the stream is a short stream the SID in the directory
    %% is actually an SSID!
    {Location,SID}=get_workbook_SID(ParsedDirectory),
    Bin=case Location of
	    normal_stream ->
		get_normal_stream(SID,ParsedDirectory,SAT,SectorSize,FileIn);
	    short_stream ->
		%% First thing we are going to do is get the
		%% normal stream that contains the short stream
		{_,SID2}=excel:get_named_SID(ParsedDirectory,?ROOT_ENTRY),
		Bin2=get_normal_stream(SID2,ParsedDirectory,SAT,
                                       SectorSize,FileIn),
		SSIDs=filefilters:get_SIDs(SSAT,SID),
		get_short_bin(Bin2,SSIDs,ShortSectorSize)
	end,
    {{Location,SID},get_bound_list(Bin,Tables)}.

get_workbook_SID(ParsedDirectory)->
    %% In Excel this SID is called 'Workbook' in the files written by 
    %% whatever wrote the test files uses 'Book' so we need to look for
    %% both of them
    Response=excel:get_named_SID(ParsedDirectory,?EXCEL_WKBK1),
    case Response of
	{error,_Err} -> {ok,Ret}=excel:get_named_SID(ParsedDirectory,?EXCEL_WKBK2),
		       Ret;
	{ok,Ret2}   -> Ret2
    end.

get_named_SID(Directory,Name)->
    SIDList=[{lists:keysearch(name,1,X),lists:keysearch(sid,1,X),
	      lists:keysearch(location,1,X)} 
	     || {_Y,X} <- Directory],
    Details=lists:keysearch({value,{name,Name}},1,SIDList),
    case Details of
	false -> {error, "Named SID not found"};
	_     -> {_,{_,{value,{sid,SID}},{value,{location,Location}}}}=Details,
		 {ok,{Location,SID}}
    end.

get_normal_stream(SID,_ParsedDirectory,SAT,SectorSize,FileIn)->
    SIDs=filefilters:get_SIDs(SAT,SID),
    get_stream(SAT,SIDs,SectorSize,FileIn).

get_short_stream(SSID,ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
		 FileIn)->
    %% First thing we are going to do is get the
    %% normal stream that contains the short stream
    %% might have a problem here too
    {_,RootSID}=excel:get_named_SID(ParsedDirectory,?ROOT_ENTRY),
    Bin=get_normal_stream(RootSID,ParsedDirectory,SAT,SectorSize,FileIn),
    SSIDs=filefilters:get_SIDs(SSAT,SSID),
    get_short_bin(Bin,SSIDs,ShortSectorSize).

get_stream(_SAT,SIDs,SectorSize,FileIn)->
    {ok,FileHandle}=file:open(FileIn,[raw,binary]),
    Position=0,
    Bin=get_storage(FileHandle,Position,SIDs,SectorSize),
    file:close(FileHandle),
    Bin.

get_bound_list(Bin,Tables)->
    get_bound_list(Bin,[],Tables).

get_bound_list(Bin,Residuum,Tables)->
    <<Identifier:16/little-unsigned-integer,
     RecordSize:16/little-unsigned-integer,Rest/binary>>=Bin,
    case Identifier of
 	?EOF ->	    
	    %%io:format("workstream read!~n"),
 	    Residuum;
	?BOUNDSHEET ->
 	    <<Record:RecordSize/binary,Rest2/binary>>=Rest,
 	    Return=excel_util:get_bound_sheet(<<Record:RecordSize/binary>>,
                                              Tables),
	    {SheetBOF,_Visibility,_SheetType,_Name,SheetName}=Return,
	    NewResiduum=[{SheetName,SheetBOF}|Residuum],
 	    get_bound_list(Rest2,NewResiduum,Tables);
	_Other ->
 	    <<_Record:RecordSize/binary,Rest2/binary>>=Rest,
 	    get_bound_list(Rest2,Residuum,Tables)
    end.

get_storage(FileHandle,Position,SIDs,SectorSize)->
    get_storage(FileHandle,Position,SIDs,SectorSize,
		[]).

get_storage(_FileHandle,_Position,[],_SectorSize,Residuum)->
    list_to_binary(lists:reverse(Residuum));
get_storage(FileHandle,Position,SIDs,SectorSize,Residuum)->
    %% Check if we are have read the whole stream
    Bin=read_storage_stream(FileHandle,SIDs,SectorSize,Position,SectorSize),
    [_H|T]=SIDs,
    NewResiduum=[Bin|Residuum],
    get_storage(FileHandle,Position,T,SectorSize,NewResiduum).

get_short_bin(Bin,SSIDs,SectorSize)->
    get_short_bin(Bin,SSIDs,SectorSize,[]).

get_short_bin(_Bin,[],_SectorSize,Residuum)->
    list_to_binary(lists:reverse(Residuum));
get_short_bin(Bin,[0|T],SectorSize,Residuum) ->
    Size=SectorSize*8,
    <<Seg:Size/integer,_Rest/binary>>=Bin,
    %% remember - you are not cutting down the binary!
    %% which is why we dont pass 'Rest' on but send 'Bin' on again
    get_short_bin(Bin,T,SectorSize,[<<Seg:Size>>|Residuum]);
get_short_bin(Bin,[H|T],SectorSize,Residuum) ->
    Position=H*SectorSize*8,
    Size=SectorSize*8,
    <<_Skip:Position,Seg:Size/integer,_Rest/binary>>=Bin,
    %% remember - you are not cutting down the binary!
    %% which is why we dont pass 'Rest' on but send 'Bin' on again
    get_short_bin(Bin,T,SectorSize,[<<Seg:Size>>|Residuum]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% The excel file is processed on record at a time into a set of ets   %%%
%%% tables. Some of the entries in these records cannot be properly     %%%
%%% handled at read time because they depend on as yet unread records.  %%%
%%% These tables are then post-processed by these functions             %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
post_process_tables(Tables)->
    %% Excel has a number of built in formats
    {ok,ok}=add_built_in_formats(Tables),
    %%filefilters:dump(Tables),
    type_formats(Tables),
    fix_up_externalrefs(Tables),
    fix_up_cells(Tables),
    convert_dates(Tables),
    %%filefilters:dump(Tables),
    ok.

%% this function adds the data type to format information
%% eg is this format for text, numbers or dates or general
type_formats(Tables) ->
    {value,{formats,Tid}}=lists:keysearch(formats,1,Tables),
    Fun = fun(X,_Residuum) ->
		  {Index,[{type,_Type1},Category,{format,Format}]}=X,
		  Return=format:get_src(Format),
		  case Return of
		      {erlang,{Type2,_Output}} -> 
			  ok;
		      {error,error_in_format} -> 
			  io:format("in excel:type_formats bug in "++
				    "format parser for format: ~p~n",
				    [Format]),
			  Type2='needed to make the case safe',
			  exit("number format parser bug!")
		  end,
		  NewFormat={format,Format},
		  ets:insert(Tid,[{Index,[{type,Type2},Category,{NewFormat}]}])
	  end,
    Return=ets:foldl(Fun,[],Tid),
    Return.

%% reverse compile the basic cells
%% this fun reverse compiles all the tokens in the table 'cell_tokens' and 
%% then injects them into the table 'cells' which is prepopulated with
%% all the non-formulae cell values
fix_up_cells(Tables)->
    {value,{cell_tokens,Cell_TokensId}}=lists:keysearch(cell_tokens,1,Tables),
    {value,{cell,CellId}}              =lists:keysearch(cell,1,Tables),
    Fun=fun(X,_Residuum)->
		{Index,[XF,{tokens,Tokens},{tokenarrays,TokenArray}]}=X,
		Formula=excel_rev_comp:reverse_compile(Index,Tokens,TokenArray,
						       Tables),
		ets:insert(CellId,[{Index,[XF,{formula,Formula}]}])
	end,
    ets:foldl(Fun,[],Cell_TokensId).

%% This function merges the contents of the ets table 'sheetnames' into 
%% 'externalrefs'
fix_up_externalrefs(Tables)->
    {value,{externalrefs,ExternalRefs}}=lists:keysearch(externalrefs,1,Tables),
    Fun=fun(X,Y) -> 
                case X of
                    {Index,[{this_file,placeholder},[]]} -> [Index|Y];
                    _                                     -> Y
                end
        end,
    case ets:info(ExternalRefs,size) of
        0 -> ok;
        _ -> [Index]=ets:foldl(Fun,[],ExternalRefs),
             SheetNames=lists:reverse(get_sheetnames(Tables)),
             excel_util:write(Tables,externalrefs,[Index,{this_file,expanded},
                                                   SheetNames])
    end.

convert_dates(Tables)->
    {value,{cell,Tid1}}   =lists:keysearch(cell,1,Tables),
    {value,{xf,Tid2}}     =lists:keysearch(xf,1,Tables),
    {value,{formats,Tid3}}=lists:keysearch(formats,1,Tables),
    %%Fun looks up the values from the cells table and the format table 
    %% (via XF) if the format is of type 'date' it switches the type 
    %% of the number from 'number' to 'date'
    Fun=fun(X,_Acc) -> 
		{Index,[{xf_index,XF},Body]}=X,
		XFVal=ets:lookup(Tid2,{index,XF}),
		[{_,[{format_index,Idx},_,_]}]=XFVal,
		Format=ets:lookup(Tid3,{format_index,Idx}),
		[{_,[{type,Type},_,_]}]=Format,
		Body2=case Type of
			  date   -> convert_dates2(Body,Tables);
			  _      -> Body
		      end,
		%{Index,Body2},
		ets:insert(Tid1,[{Index,[{xf_format,XF},Body2]}])
	end,
    ets:foldl(Fun,[],Tid1).

convert_dates2({value,number,Number},Tables) ->
    [{_,[{value,DateMode}]}]=excel_util:read(Tables,misc,datemode),
    Date=case DateMode of
	  "Windows"   -> muin_date:excel_win_to_gregorian(Number);
	  "Macintosh" -> muin_date:excel_mac_to_gregorian(Number)
      end,
    {value,date,Date};
convert_dates2(Body,_Tables) -> Body.

get_sheetnames(Tables)->
    Fun = fun({_Index,[{name,SheetName}]},Y) ->
                  [SheetName|Y]
          end,
    {value,{sheetnames,SheetNames}}=lists:keysearch(sheetnames,1,Tables),
    ets:foldl(Fun,[],SheetNames).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Utility functions                                                   %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_storage_stream(_,_,_,_,0)-> <<>>;
read_storage_stream(FileHandle,SIDList,SectorSize,Position,Size)->
    StartSIDNumber=Position div SectorSize+1,
    StartSID=lists:nth(StartSIDNumber,SIDList),
    BytesOffset=Position rem SectorSize,
    BytesLeft=SectorSize-BytesOffset,
    if
	BytesLeft < Size ->
	    %% need to do a double read
	    FirstTruePosition=StartSID*SectorSize+?HEADER_SIZE+BytesOffset,
	    file:position(FileHandle,FirstTruePosition),
	    {ok,Bin1}=file:read(FileHandle,BytesLeft),
	    %% now read the rest from the next Sector
	    SecondSID=lists:nth(StartSIDNumber+1,SIDList),
	    SecondTruePosition=SecondSID*SectorSize+?HEADER_SIZE,
	    file:position(FileHandle,SecondTruePosition),
	    {ok,Bin2}=file:read(FileHandle,Size-BytesLeft),
	    Bin=list_to_binary([Bin1,Bin2]);
	true->
	    %% can do a single read
	    FirstTruePosition=StartSID*SectorSize+?HEADER_SIZE+BytesOffset,
	    file:position(FileHandle,FirstTruePosition),
	    {ok,Bin}=file:read(FileHandle,Size)
    end,
    Bin.

%% Excel has a number of built in number formats
%% These are described in Section 5.49 of excelfileformatV1-41.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% BUG     - this routine just sticks in a Dollar sign - but it should be the %%
%%           appropriate currency symbol of the country that Excel is using!  %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_built_in_formats(Tables) ->
    L=[[{format_index,0},{type,general},{category,general},
	{format,"General"}],
       [{format_index,1},{type,number},{category,decimal},
	{format,"0"}],
       [{format_index,2},{type,number},{category,decimal},
	{format,"0.00"}],
       [{format_index,3},{type,number},{category,decimal},
	{format,"#,##0"}],
       [{format_index,4},{type,number},{category,decimal},
	{format,"#,##0.00"}],
       [{format_index,5},{type,number},{category,currency},
	{format,"\"$\"##0_);"++
	 "(\"$\"#,##0)"}],
       [{format_index,6},{type,number},{category,currency},
	{format,"\"$\"##0_);"++
	 "[Red](\"$\"#,##0)"}],
       [{format_index,7},{type,number},{category,currency},
	{format,"\"$\"##0.00_);"++
	 "(\"$\"#,##0.00)"}],
       [{format_index,8},{type,number},{category,currency},
	{format,"\"$\"##0.00_);"++
	 "[Red](\"$\"#,##0.00)"}],
       [{format_index,9},{type,number},{category,percent},
	{format,"0%"}],
       [{format_index,10},{type,number},{category,percent},
	{format,"0.00%"}],
       [{format_index,11},{type,number},{category,scientific},
	{format,"0.00e+00"}],
       [{format_index,12},{type,number},{category,fraction},
	{format,"#?/?"}],
       [{format_index,13},{type,number},{category,fraction},
	{format,"#??/??"}],
       [{format_index,14},{type,date},{category,date},
	{format,"M/D/YY"}],
       [{format_index,15},{type,date},{category,date},
	{format,"D-MMM-YY"}],
       [{format_index,16},{type,date},{category,date},
	{format,"D-MMM"}],
       [{format_index,17},{type,date},{category,date},
	{format,"MMM-YY"}],
       [{format_index,18},{type,date},{category,time},
	{format,"h:mm AM/PM"}],
       [{format_index,19},{type,date},{category,time},
	{format,"h:mm:ss AM/PM"}],
       [{format_index,20},{type,date},{category,time},
	{format,"h:mm"}],
       [{format_index,21},{type,date},{category,time},
	{format,"h:mm:ss"}],
       [{format_index,22},{type,date},{category,datetime},
	{format,"M/D/YY h:mm"}],
       [{format_index,37},{type,number},{category,account},
	{format,"_(#,##0_);(#,###0)"}],
       [{format_index,38},{type,number},{category,account},
	{format,"_(#,##0_);"++
	 "[Red](#,###0)"}],
       [{format_index,39},{type,number},{category,account},
	{format,"_(#,##0.00_);"++
	 "(#,###0.00)"}],
       [{format_index,40},{type,number},{category,account},
	{format,"_(#,##0.00_);"++
	 "[Red](#,###0.00)"}],
       %% these formats are different in the Microsoft Book
       %% Microsoft Excel97 Developers Kit p427!
       %% index 41 and 42 are transposed as are index 43 and 44!
       [{format_index,41},{type,number},{category,currency},
	{format,"_(\"$\"*#,##0_);"++
	 "_(\"$\"*(#,##0);"++
	 "_(\$\"*\"-\"_);"++
	 "_(@_)"}],
       [{format_index,42},{type,number},{category,currency},
	{format,"_(*#,##0_);"++
	 "_(*(#,##0);"++
	 "_(*\"-\"_);"++
	 "_(@_)"}],
       [{format_index,43},{type,number},{category,currency},
	{format,"_(\"$\"*#,##0.00_);"++
	 "_(\"$\"*(#,##0.00);"++
	 "_(\$\"*\"-\"??_);"++
	 "_(@_)"}],
       [{format_index,44},{type,number},{category,currency},
	{format,"_(*#,##0.00_);"++
	 "_(*(#,##0.00);"++
	 "_(*\"-\"??_);"++
	 "_(@_)"}],
       [{format_index,45},{type,date},{category,time},
	{format,"mm:ss"}],
       [{format_index,46},{type,date},{category,time},
	{format,"[h]:mm:ss"}],
       [{format_index,47},{type,date},{category,time},
	{format,"mm:ss.0"}],
       [{format_index,48},{type,number},{category,scientific},
	{format,"##0.0E+0"}],
       [{format_index,49},{type,string},{category,text},
	{format,"@"}]],
    Fun = fun(Record,[]) -> excel_util:write(Tables,formats,Record), [] end,
    []=lists:foldl(Fun,[],L),
    io:format("in excel:add_built_in_formats All built in formats added!~n"),
    {ok,ok}.
