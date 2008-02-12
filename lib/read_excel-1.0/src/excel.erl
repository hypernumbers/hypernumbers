%%%-------------------------------------------------------------------
%%% File        : excel.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : parses the specific Excel components of a file
%%%
%%% Created     :  6 Apr 2007 by Gordon Guthrie <gordonguthrie@gg-laptop>
%%%-------------------------------------------------------------------
-module(excel).

-export([read_excel/10,get_file_structure/9]).

%%% Suspect this should be in a util file of some sort...
-export([get_named_SID/3]).

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
	   _MinStreamSize,{{SubLoc,SubSID},SubStreams},FileIn,Tables,FileOut)->
    %% Bear in mind that if the location is short stream the 'SID' returned
    %% is actually an SSID!
    io:format("~n~nNow going to parse the Excel Workbook only!~n"),
    {Location,SID}=get_named_SID(Directory,?EXCEL_WORKBOOK,FileOut),
    Bin=case Location of
	    normal_stream -> get_normal_stream(SID,Directory,SAT,SectorSize,
					       FileIn,FileOut);
	    short_stream  -> get_short_stream(SID,Directory,SAT,SSAT,SectorSize,
					      ShortSectorSize,FileIn,FileOut)
	end,
    %% Now parsing the Excel components of the file
    %%
    %% To understand what is going on here you need to read Section 4.1.2 of excelfileformatV1-40.pdf
    %% Basically we first read the 'Workbook Globals SubStream' and the all the 'Sheet SubStreams'
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
    %% The 'Sheet Substreams' constist of things on a particular Excel tab as describe in 
    %% Section 4.2.5 of excelfileformatV1-40.pdf
    %%

    %% First parse the 'Workbook Globals SubStream' 
    parse_bin(Bin,{'utf-8',list_to_binary("Workbook Globals SubStream")},Tables,FileOut),
    %% Now parse all the 'Sheet Substeams'
    [parse_substream(SubSID,SubLoc,X,Directory,SAT,SSAT,SectorSize,
		     ShortSectorSize,FileIn,Tables,FileOut) || X <- SubStreams],
    %% Now that the complete file is read reverse_compile the token stream
    %%    
    make_formulae(Tables,FileOut).

parse_substream(SubSID,Location,SubStream,Directory,SAT,SSAT,SectorSize,
		ShortSectorSize,FileIn,Tables,FileOut)->
    excel_util:put_log(FileOut,io_lib:fwrite("Now parsing substream ~p",
					     [SubStream])),
    io:format("Now parsing substream ~p~n",[SubStream]),
    {_,Offset}=SubStream,
    {[{_,NameBin}],_}=SubStream,
    Name=binary_to_list(NameBin),
    Bin=case Location of 
	    normal_stream -> get_normal_stream(SubSID,Directory,SAT,SectorSize,
					       FileIn,FileOut);
	    short_stream  -> get_short_stream(SubSID,Directory,SAT,SSAT,SectorSize,
					      ShortSectorSize,FileIn,FileOut)
	end,
    %% Because we are reading a substream we want to chop off the 
    %% binary up to the offset
    <<_Discard:Offset/binary,Rest/binary>>=Bin,
    %% pass in the actual name of the substream
    parse_bin(Rest,Name,Tables,FileOut).

%% parse_bin(Bin,Tables,FileOut)->
%%     parse_bin(Bin,[],Tables,FileOut).

parse_bin(Bin,Name,Tables,FileOut)->
    <<Identifier:16/little-unsigned-integer,
     RecordSize:16/little-unsigned-integer,Rest/binary>>=Bin,
    case Identifier of
 	?EOF ->	    
 	    excel_util:put_log(FileOut,io_lib:fwrite("In excel:parse_bin - Workstream ~p read!",
                              [Name])),
 	    io:format("workstream ~p read!~n",[Name]),
 	    ok;
	?SST ->
	    %%io:format("In excel:parse_bin Identifier is SST~n"),
	    {ok,BinList,Rest2}=get_single_SST(Bin,FileOut),
	    %% io:format("in excel:parse_bin SST_bin is ~p~n",[SST_bin]),
	    {ok,ok}=excel_records:parse_rec(Identifier,BinList,Name,Tables,FileOut),
 	    parse_bin(Rest2,Name,Tables,FileOut);
	_Other ->
 	    <<Record:RecordSize/binary,Rest3/binary>>=Rest,
	    {ok,ok}=excel_records:parse_rec(Identifier,Record,Name,Tables,FileOut),
 	    parse_bin(Rest3,Name,Tables,FileOut)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to get a concatenated SST record                          %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_single_SST(Bin,FileOut)->
    get_single_SST(Bin,FileOut,[]).

get_single_SST(Bin,FileOut,Residuum)->    
    <<Identifier:16/little-unsigned-integer,Rest/binary>>=Bin,
    case Identifier of
	?SST      -> %%io:format("in excel_get_single_SST for SST~n"),
		     <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     get_single_SST(Rest3,FileOut,[Record|Residuum]);
	?CONTINUE -> %%io:format("in excel_get_single_SST for CONTINUE~n"),
		     <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     get_single_SST(Rest3,FileOut,[Record|Residuum]);
	?EXTSST   -> %%io:format("in excel_get_single_SST for EXTSST~n"),
		     %% the EXTSST record is simply an index for fast lookup
		     %% on the SST record so we just chuck it...
		     <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
		     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
		     {ok,lists:reverse(Residuum),Rest3}
		     %% Other -> io:format("in excel_get_single_SST for Other of ~p~n",[Other]),
		     %% 	 exit("oh, shit!")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to read the structure of the file                         %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_file_structure(ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
		   _MinStreamSize,Tables,FileIn,FileOut)->
    %% Remember that if the stream is a short stream the SID in the directory
    %% is actually an SSID!
    {Location,SID}=excel:get_named_SID(ParsedDirectory,?EXCEL_WORKBOOK,FileOut),
    Bin=case Location of
	    normal_stream ->
		get_normal_stream(SID,ParsedDirectory,SAT,SectorSize,
				  FileIn,FileOut);
	    short_stream ->
		%% First thing we are going to do is get the
		%% normal stream that contains the short stream
		{_,SID2}=excel:get_named_SID(ParsedDirectory,
					     ?ROOT_ENTRY,FileOut),
		Bin2=get_normal_stream(SID2,ParsedDirectory,SAT,SectorSize,
				       FileIn,FileOut),
		SSIDs=filefilters:get_SIDs(SSAT,SID,FileOut),
		get_short_bin(Bin2,SSIDs,ShortSectorSize)
	end,
  {{Location,SID},get_bound_list(Bin,Tables,FileOut)}.

get_named_SID(Directory,Name,FileOut)->
    SIDList=[{lists:keysearch(name,1,X),lists:keysearch(sid,1,X),
	      lists:keysearch(location,1,X)} 
	     || {_Y,X} <- Directory],
    excel_util:put_log(FileOut,io_lib:fwrite("in excel:get_named_SID "++
					     "Directory is ~p~nSIDList is ~p~n",
					     [Directory,SIDList])),
    Details=lists:keysearch({value,{name,Name}},1,SIDList),
    {_,{_,{value,{sid,SID}},{value,{location,Location}}}}=Details,
    excel_util:put_log(FileOut,io_lib:fwrite("in excel:get_named_SID the SID "++
					     "of the ~p is ~p and it is "++
					     "stored in ~p~n",
					     [Name,SID,Location])),
    {Location,SID}.

get_normal_stream(SID,_ParsedDirectory,SAT,SectorSize,FileIn,FileOut)->
    SIDs=filefilters:get_SIDs(SAT,SID,FileOut),
    get_stream(SAT,SIDs,SectorSize,FileIn,FileOut).

get_short_stream(SSID,ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
		 FileIn,FileOut)->
    %% First thing we are going to do is get the
    %% normal stream that contains the short stream
    {_,RootSID}=excel:get_named_SID(ParsedDirectory,
				 ?ROOT_ENTRY,FileOut),
    
    Bin=get_normal_stream(RootSID,ParsedDirectory,SAT,SectorSize,
			   FileIn,FileOut),
    SSIDs=filefilters:get_SIDs(SSAT,SSID,FileOut),
    get_short_bin(Bin,SSIDs,ShortSectorSize).

get_stream(_SAT,SIDs,SectorSize,FileIn,FileOut)->
    {ok,FileHandle}=file:open(FileIn,[raw,binary]),
    Position=0,
    Bin=get_storage(FileHandle,Position,SIDs,SectorSize,FileOut),
    file:close(FileHandle),
    excel_util:put_log(FileOut,io_lib:fwrite("in excel:get_stream "++
					     "Bin has size ~p",
					     [erlang:size(Bin)])),
    Bin.

get_bound_list(Bin,Tables,FileOut)->
    get_bound_list(Bin,[],Tables,FileOut).

get_bound_list(Bin,Residuum,Tables,FileOut)->
    <<Identifier:16/little-unsigned-integer,
     RecordSize:16/little-unsigned-integer,Rest/binary>>=Bin,
    case Identifier of
 	?EOF ->	    
 	    excel_util:put_log(FileOut,"workstream read!"),
 	    io:format("workstream read!~n"),
 	    Residuum;
	?BOUNDSHEET ->
 	    <<Record:RecordSize/binary,Rest2/binary>>=Rest,
 	    Return=excel_util:get_bound_sheet(<<Record:RecordSize/binary>>,Tables,FileOut),
	    {SheetBOF,Visibility,SheetType,Name,SheetName}=Return,
	    NewResiduum=[{SheetName,SheetBOF}|Residuum],
 	    get_bound_list(Rest2,NewResiduum,Tables,FileOut);
	_Other ->
 	    <<_Record:RecordSize/binary,Rest2/binary>>=Rest,
 	    get_bound_list(Rest2,Residuum,Tables,FileOut)
    end.

get_storage(FileHandle,Position,SIDs,SectorSize,FileOut)->
    excel_util:put_log(FileOut,io_lib:fwrite("SIDs are ~p~n",[SIDs])),
    get_storage(FileHandle,Position,SIDs,SectorSize,FileOut,
		[]).

get_storage(_FileHandle,_Position,[],_SectorSize,_FileOut,Residuum)->
    list_to_binary(lists:reverse(Residuum));
get_storage(FileHandle,Position,SIDs,SectorSize,FileOut,Residuum)->
    %% Check if we are have read the whole stream
    Bin=read_storage_stream(FileHandle,SIDs,SectorSize,Position,SectorSize),
    [_H|T]=SIDs,
    NewResiduum=[Bin|Residuum],
    get_storage(FileHandle,Position,T,SectorSize,FileOut,NewResiduum).

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
    
make_formulae(Tables,FileOut)->
  io:format("Tables are ~p~n",[Tables]),
  {value,{cell_tokens,Cell_TokensId}}=lists:keysearch(cell_tokens,1,Tables),
  {value,{cell,CellId}}              =lists:keysearch(cell,1,Tables),
  Fun=fun(X,Residuum)->
    {Index,[XF,{tokens,Tokens},{tokenarrays,TokenArray}]}=X,
    Formula=excel_rev_comp:reverse_compile(Tokens,TokenArray,Tables,FileOut),
    ets:insert(CellId,[{Index,[XF,{formula,Formula}]}])
  end,
  CellList=ets:foldl(Fun,[],Cell_TokensId),
  io:format("Tokens to cells transformation gives:~n~p~n",[CellList]).
  