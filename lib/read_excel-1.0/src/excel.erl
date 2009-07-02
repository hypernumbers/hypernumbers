%%%-------------------------------------------------------------------
%%% File    excel.erl
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc    This module parses the specific Excel components of
%%%         a file. The structure of this process is shown 
%%%         schematically below:
%%%         
%%%         <img src="./diagram2.png" />
%%%
%%% @end
%%% Created     :  6 Apr 2007 by Gordon Guthrie <gordonguthrie@gg-laptop>
%%%-------------------------------------------------------------------
-module(excel).

-export([read_excel/9,get_file_structure/8]).

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
%% @doc this is the interface for reading and Excel 97-2003 file
read_excel(Directory,SAT,SSAT,SectorSize,ShortSectorSize,
           _MinStreamSize,{{SubLoc,SubSID},SubStreams},FileIn,Tables)->
    % Bear in mind that if the location is short stream the 'SID' returned
    % is actually an SSID!
    % io:format("~n~nNow going to parse the Excel Workbook only!~n"),
    {Location,SID}=get_workbook_SID(Directory),
    Bin=case Location of
            normal_stream -> get_normal_stream(SID,Directory,SAT,SectorSize,
                                               FileIn);
            short_stream  -> get_short_stream(SID,Directory,SAT,SSAT,SectorSize,
                                              ShortSectorSize,FileIn)
        end,
    % Now parsing the Excel components of the file
    %
    % To understand what is going on here you need to read Section 4.1.2 of
    % excelfileformatV1-40.pdf Basically we first read the 'Workbook Globals
    % SubStream' and the all the 'Sheet SubStreams'
    %
    % The 'Workbook Globals Substream' contains things that are 'file wide':
    % * string
    % * tables
    % * formats
    % * fonts
    % * styles
    %* etc, etc
    % as described in Section 4.2.5 of excelfileformatV1-40.pdf
    %
    % The 'Sheet Substreams' constist of things on a particular Excel tab as
    % described in Section 4.2.5 of excelfileformatV1-40.pdf
    %

    % First parse the 'Workbook Globals SubStream'
    CurrentFormula="There is no current Formula yet!",
    parse_bin(Bin,{'utf-8',list_to_binary("Workbook Globals SubStream")},
              CurrentFormula,Tables),
    % Now parse all the 'Sheet Substeams'
    [parse_substream(SubSID,SubLoc,X,Directory,SAT,SSAT,SectorSize,
                     ShortSectorSize,FileIn,Tables) || X <- SubStreams],
    % Now that the complete file is read reverse_compile the token stream
    %
    excel_post_process:post_process_tables(Tables).

parse_substream(SubSID,Location,SubStream,Directory,SAT,SSAT,SectorSize,
                ShortSectorSize,FileIn,Tables)->
    {{[{Type,NameBin}],_,_},Offset}=SubStream,
    % Name=binary_to_list(NameBin),
    % io:format("Now parsing stream ~p~n",[Name]),
    Bin=case Location of
            normal_stream -> get_normal_stream(SubSID,Directory,SAT,SectorSize,
                                               FileIn);
            short_stream  -> get_short_stream(SubSID,Directory,SAT,SSAT,
                                              SectorSize,ShortSectorSize,FileIn)
        end,
    % Because we are reading a substream we want to chop off the
    % binary up to the offset
    <<_Discard:Offset/binary,Rest/binary>>=Bin,
    % pass in the actual name of the substream
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
            % io:format("workstream ~p read!~n",[Name]),
            ok;
        _Other ->
            <<Record:RecordSize/binary,Rest2/binary>>=Rest,
            % now get the next identifier
            % because if the next identifier is a ?CONTINUE then we want to wire
            % the records up together
            <<NextIdentifier:16/little-unsigned-integer,_Rest3/binary>>=Rest2,
            case {Identifier, NextIdentifier} of
                % SST CONTINUES have some funny stuff going on with the compression
                % of Unicode.
                % See Section 5.21 of excelfileformatV1-42.pdf
                {?SST, ?CONTINUE} -> 
                    {ok,BinList,Rest4}=get_single_SST(Bin),
                    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,BinList,
                                                           Name,CurrentFormula,
                                                           Tables),
                    parse_bin(Rest4,SubStreamName,NewCurrentFormula,Tables);
                {?SST, _} ->
                    %io:format("In no SST continuation...~n"),
                    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,[Record],
                                                           Name,CurrentFormula,
                                                           Tables),
                    parse_bin(Rest2,SubStreamName,NewCurrentFormula,Tables);
                {_, ?CONTINUE} ->
                    {ok, NewBin,Rest4}=get_single_record(Identifier, Bin),
                    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,NewBin,
                                                           Name,CurrentFormula,
                                                           Tables),
                    parse_bin(Rest4,SubStreamName,NewCurrentFormula,Tables);
                _ ->
                    {ok,NewCurrentFormula}=excel_records:parse_rec(Identifier,Record,
                                                                   Name,CurrentFormula,
                                                                   Tables),
                    parse_bin(Rest2,SubStreamName,NewCurrentFormula,Tables)
            end
    end.

get_single_record(Identifier,Bin)->
    get_single_record(Identifier,Bin,[]).

get_single_record(Identifier,Bin,Residuum)->
    io:format("In get_single_record Identifier is ~p~n", [Identifier]),
    <<Identifier2:16/little-unsigned-integer,Rest/binary>>=Bin,
    io:format("In get_single_record Identifier2 is ~p~n", [Identifier2]),
    case Identifier2 of
        ?CONTINUE -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
                     <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
                     get_single_record(Identifier,Rest3,[Record|Residuum]);
        Identifier -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
                      <<Record:RecordSize/binary,Rest3/binary>>=Rest2,
                      io:format("in get_single_record RecordSize is ~p~n-Record is ~p~n",
                                [RecordSize, Record]),
                      get_single_record(Identifier, Rest3, [Record|Residuum]);
        _          -> {ok,list_to_binary(lists:reverse(Residuum)), Bin}
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
        _         -> {ok,lists:reverse(Residuum),Rest}
    end.
%        % the EXTSST record is simply an index for fast lookup
%        % on the SST record so we just chuck it...
%        ?EXTSST   -> <<RecordSize:16/little-unsigned-integer,Rest2/binary>>=Rest,
%                     <<_Record:RecordSize/binary,Rest3/binary>>=Rest2,
%                     {ok,lists:reverse(Residuum),Rest3}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to read the structure of the file                         %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc this should really be in excel_util
get_file_structure(ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
                   _MinStreamSize,Tables,FileIn)->
    % Remember that if the stream is a short stream the SID in the directory
    % is actually an SSID!
    {Location,SID}=get_workbook_SID(ParsedDirectory),
    Bin=case Location of
            normal_stream ->
                get_normal_stream(SID,ParsedDirectory,SAT,SectorSize,FileIn);
            short_stream ->
                % First thing we are going to do is get the
                % normal stream that contains the short stream
                {_,SID2}=get_named_SID(ParsedDirectory,?ROOT_ENTRY),
                Bin2=get_normal_stream(SID2,ParsedDirectory,SAT,
                                       SectorSize,FileIn),
                SSIDs=excel_util:get_SIDs(SSAT,SID),
                get_short_bin(Bin2,SSIDs,ShortSectorSize)
        end,
    {{Location,SID},get_bound_list(Bin,Tables)}.

get_workbook_SID(ParsedDirectory)->
    % In Excel this SID is called 'Workbook' in the files written by
    % whatever wrote the test files uses 'Book' so we need to look for
    % both of them
    Response=get_named_SID(ParsedDirectory,?EXCEL_WKBK1),
    case Response of
        {error,_Err} -> {ok,Ret}=get_named_SID(ParsedDirectory,?EXCEL_WKBK2),
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
    SIDs=excel_util:get_SIDs(SAT,SID),
    get_stream(SAT,SIDs,SectorSize,FileIn).

get_short_stream(SSID,ParsedDirectory,SAT,SSAT,SectorSize,ShortSectorSize,
                 FileIn)->
    % First thing we are going to do is get the
    % normal stream that contains the short stream
    % might have a problem here too
    {_,RootSID}=get_named_SID(ParsedDirectory,?ROOT_ENTRY),
    Bin=get_normal_stream(RootSID,ParsedDirectory,SAT,SectorSize,FileIn),
    SSIDs=excel_util:get_SIDs(SSAT,SSID),
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
            % io:format("workstream read!~n"),
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
    % Check if we are have read the whole stream
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
    % remember - you are not cutting down the binary!
    % which is why we dont pass 'Rest' on but send 'Bin' on again
    get_short_bin(Bin,T,SectorSize,[<<Seg:Size>>|Residuum]);
get_short_bin(Bin,[H|T],SectorSize,Residuum) ->
    Position=H*SectorSize*8,
    Size=SectorSize*8,
    <<_Skip:Position,Seg:Size/integer,_Rest/binary>>=Bin,
    % remember - you are not cutting down the binary!
    % which is why we dont pass 'Rest' on but send 'Bin' on again
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
            % need to do a double read
            FirstTruePosition=StartSID*SectorSize+?HEADER_SIZE+BytesOffset,
            file:position(FileHandle,FirstTruePosition),
            {ok,Bin1}=file:read(FileHandle,BytesLeft),
            % now read the rest from the next Sector
            SecondSID=lists:nth(StartSIDNumber+1,SIDList),
            SecondTruePosition=SecondSID*SectorSize+?HEADER_SIZE,
            file:position(FileHandle,SecondTruePosition),
            {ok,Bin2}=file:read(FileHandle,Size-BytesLeft),
            Bin=list_to_binary([Bin1,Bin2]);
        true->
            % can do a single read
            FirstTruePosition=StartSID*SectorSize+?HEADER_SIZE+BytesOffset,
            file:position(FileHandle,FirstTruePosition),
            {ok,Bin}=file:read(FileHandle,Size)
    end,
    Bin.

