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
read_excel(Directory, SAT, SSAT, SectorSize, ShortSectorSize,
           _MinStreamSize,{{SubLoc, SubSID}, SubStreams}, FileIn, Tables)->
    
    % Bear in mind that if the location is short stream the 'SID' returned
    % is actually an SSID!
    {Location, SID} = get_workbook_SID(Directory),
    Bin = case Location of
            normal_stream ->
                  get_normal_stream(SID, SAT, SectorSize, FileIn);
            short_stream  ->
                  get_short_stream(SID, Directory, SAT, SSAT, SectorSize,
                                   ShortSectorSize, FileIn)
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
    WorkBook = {'utf-8',<<"Workbook Globals SubStream">>},
    parse_bin(Bin, WorkBook, no_formula, Tables),
    
    % Now parse all the 'Sheet Substeams'
    [ parse_substream(SubSID, SubLoc, X, Directory, SAT, SSAT, SectorSize,
                      ShortSectorSize, FileIn, Tables) || X <- SubStreams],
    
    % Now that the complete file is read reverse_compile the token stream
    excel_post_process:post_process_tables(Tables).

parse_substream(SubSID, Location, SubStream, Directory, SAT, SSAT, SectorSize,
                ShortSectorSize, FileIn, Tables) ->
    
    {{[{Type, NameBin}], _, _}, Offset} = SubStream,

    Bin = case Location of
              normal_stream ->
                  get_normal_stream(SubSID, SAT, SectorSize, FileIn);
              short_stream  ->
                  get_short_stream(SubSID, Directory, SAT, SSAT,
                                   SectorSize, ShortSectorSize, FileIn)
        end,
    
    % Because we are reading a substream we want to chop off the
    % binary up to the offset
    <<_Discard:Offset/binary, Rest/binary>> = Bin,
    
    % pass in the actual name of the substream
    parse_bin(Rest, {Type, NameBin}, no_formula, Tables).

parse_bin(Bin, {_, NameBin}=SubStreamName, Formula, Tables)->
    
    <<Id:16/little-unsigned-integer, Size:16/little-unsigned-integer,
     Rest/binary>> = Bin,
    
    case Id of
        ?EOF   -> ok;
        _Other ->
            <<Record:Size/binary, Rest2/binary>> = Rest,
            % now get the next identifier
            % because if the next identifier is a ?CONTINUE 
            % then we want to wire the records up together
            <<NextId:16/little-unsigned-integer, _Rest3/binary>> = Rest2,
            
            {Rec, TheRest} = 
                case {Id, NextId} of
                    % SST CONTINUES have some funny stuff 
                    % going on with the compression of Unicode.
                    % See Section 5.21 of excelfileformatV1-42.pdf
                    {?SST, ?CONTINUE} ->
                        {ok, BinList, Rest4} = get_single_SST(Bin),
                        {BinList, Rest4};
                    {?SST, _} ->
                        {[Record], Rest2};
                    {_, ?CONTINUE}    ->
                        {ok, NBin, R} = get_single_record(Id, Bin),
                        {NBin, R};
                    _ ->
                        {Record, Rest2}
                end,

            Name = binary_to_list(NameBin),

            Fla = case excel_records:parse_rec(Id, Rec, Name, Tables) of
                      {write, Table, Data, NewFormula} ->
                          excel_util:write(Tables, Table, Data),
                          NewFormula;
                      {write, Table, Data} ->
                          excel_util:write(Tables, Table, Data),
                          Formula;
                      {append, Table, Data} ->
                          excel_util:append(Tables, Table, Data),
                          Formula;
                      ok ->
                          Formula
                  end,
            parse_bin(TheRest, SubStreamName, Fla, Tables)
    end.

get_single_record(Id, Bin)->
    get_single_record(Id, Bin, []).

get_single_record(Id, <<Id2:16/little-unsigned-integer, Rest/binary>>, Acc)
  when Id2 =:= ?CONTINUE orelse Id2 =:= Id ->
    <<Size:16/little-unsigned-integer, Rest2/binary>>=Rest,
    <<Record:Size/binary, Rest3/binary>> = Rest2,
    get_single_record(Id, Rest3, [Record | Acc]);

get_single_record(_Id, Bin, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc)), Bin}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to get a concatenated SST record                          %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_single_SST(Bin)->
    get_single_SST(Bin,[]).

get_single_SST(<<Rec:16/little-unsigned-integer, Rest/binary>>, Acc)
  when Rec =:= ?SST orelse Rec =:= ?CONTINUE ->
    <<Size:16/little-unsigned-integer, Rest2/binary>> = Rest,
    <<Record:Size/binary, Rest3/binary>> = Rest2,
    get_single_SST(Rest3, [Record | Acc]);

get_single_SST(<<_Id:16/little-unsigned-integer,Rest/binary>>, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Functions to read the structure of the file                         %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc this should really be in excel_util
get_file_structure(ParsedDirectory, SAT, SSAT, SectorSize, ShortSectorSize,
                   _MinStreamSize, Tables, FileIn)->
    % Remember that if the stream is a short stream the SID in the directory
    % is actually an SSID!
    {Location, SID} = get_workbook_SID(ParsedDirectory),
    Bin = case Location of
              normal_stream ->
                  get_normal_stream(SID, SAT,SectorSize,FileIn);
              short_stream ->
                  % First thing we are going to do is get the
                  % normal stream that contains the short stream
                  {_, SID2} = get_named_SID(ParsedDirectory, ?ROOT_ENTRY),
                  Bin2 = get_normal_stream(SID2, SAT, SectorSize, FileIn),
                  SSIDs = excel_util:get_SIDs(SSAT, SID),
                  get_short_bin(Bin2, SSIDs, ShortSectorSize)
          end,
    {{Location, SID}, get_bound_list(Bin, Tables)}.

get_workbook_SID(ParsedDirectory)->
    % In Excel this SID is called 'Workbook' in the files written by
    % whatever wrote the test files uses 'Book' so we need to look for
    % both of them
    case get_named_SID(ParsedDirectory,?EXCEL_WKBK1) of
        {error, _Err} ->
            {ok, Ret} = get_named_SID(ParsedDirectory,?EXCEL_WKBK2),
            Ret;
        {ok, Ret2} ->
            Ret2
    end.

get_named_SID(Directory, Name)->
    SIDList=[{lists:keysearch(name, 1, X), lists:keysearch(sid, 1, X),
              lists:keysearch(location, 1, X)}
             || {_Y, X} <- Directory],
    Details = lists:keysearch({value,{name,Name}},1,SIDList),
    case Details of
        false -> {error, "Named SID not found"};
        _     -> {_,{_,{value,{sid,SID}},{value,{location,Location}}}}=Details,
                 {ok,{Location,SID}}
    end.

get_normal_stream(SID, SAT, SectorSize, FileIn) ->
    SIDs = excel_util:get_SIDs(SAT, SID),
    get_stream(SAT, SIDs, SectorSize, FileIn).

get_short_stream(SSID, ParsedDirectory, SAT, SSAT, SectorSize,
                 ShortSectorSize, FileIn) ->
    % First thing we are going to do is get the
    % normal stream that contains the short stream
    % might have a problem here too
    {_,RootSID}=get_named_SID(ParsedDirectory,?ROOT_ENTRY),
    Bin=get_normal_stream(RootSID, SAT,SectorSize,FileIn),
    SSIDs=excel_util:get_SIDs(SSAT,SSID),
    get_short_bin(Bin,SSIDs,ShortSectorSize).

get_stream(_SAT,SIDs,SectorSize,FileIn)->
    {ok, Handle} = file:open(FileIn, [raw, binary]),
    Bin = get_storage(Handle, 0, SIDs, SectorSize),
    file:close(Handle),
    Bin.

get_bound_list(Bin, Tables)->
    get_bound_list(Bin, [], Tables).

get_bound_list(Bin, Acc, Tables)->
    <<Id:16/little-unsigned-integer,
     Size:16/little-unsigned-integer, Rest/binary>> = Bin,
    case Id of
        ?EOF ->
            Acc;
        ?BOUNDSHEET ->
            <<Record:Size/binary, Rest2/binary>> = Rest,
            {SheetBOF, _Visibility, _SheetType, _Name,SheetName} =
                excel_util:get_bound_sheet(<<Record:Size/binary>>, Tables),
            get_bound_list(Rest2, [{SheetName, SheetBOF} | Acc], Tables);
        _Other ->
            <<_Rec:Size/binary, Rest2/binary>> = Rest,
            get_bound_list(Rest2, Acc, Tables)
    end.

get_storage(File, Position, SIDs, SectorSize) ->
    get_storage(File, Position, SIDs, SectorSize, []).

get_storage(_File, _Position, [], _SectorSize, Acc) ->
    list_to_binary(lists:reverse(Acc));
get_storage(File, Position, [_H|T]=SIDs, SectorSize, Acc) ->
    % Check if we are have read the whole stream
    Bin = read_storage_stream(File, SIDs, SectorSize, Position, SectorSize),
    get_storage(File, Position, T, SectorSize, [Bin|Acc]).

get_short_bin(Bin, SSIDs, SectorSize) ->
    get_short_bin(Bin, SSIDs, SectorSize, []).

get_short_bin(_Bin, [], _SectorSize, Acc) ->
    list_to_binary(lists:reverse(Acc));
get_short_bin(Bin, [0|T], SectorSize, Acc) ->
    Size=SectorSize*8,
    <<Seg:Size/integer, _Rest/binary>> = Bin,
    get_short_bin(Bin, T, SectorSize, [<<Seg:Size>>|Acc]);
get_short_bin(Bin, [H|T], SectorSize, Acc) ->
    Position=H*SectorSize*8,
    Size=SectorSize*8,
    <<_Skip:Position,Seg:Size/integer,_Rest/binary>>=Bin,
    get_short_bin(Bin,T,SectorSize,[<<Seg:Size>>|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% Utility functions                                                   %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_storage_stream(_,_,_,_,0)->
    <<>>;
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

