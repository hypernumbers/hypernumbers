%%%-------------------------------------------------------------------
%%% File        : filefilters.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : runs Microsoft Office filters
%%%
%%% Created     :  4 Apr 2007 by Gordon Guthrie <gordonguthrie@gg-laptop>
%%%-------------------------------------------------------------------
-module(filefilters).

%%% Exports
-export([read/3,read/2,filter_file/1,get_SIDs/2]).

%%% Debugging exports - not for proper use
-export([test_DEBUG/0,
         create_ets_DEBUG/0,
         delete_ets_DEBUG/1,
         dump_DEBUG/1]).

-export([dump/1]).

-include("spriki.hrl").
-include("microsoftcompoundfileformat.hrl").
-include("microsoftbiff.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Functions for filtering the files                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(excel,FileIn,Fun)->
    read_excel(excel,FileIn,Fun).

read(excel,FileIn)->
    Fun= fun(X) -> io:format("About to dump tables~n"),
                   dump(X)
         end,
    read_excel(excel,FileIn,Fun).

read_excel(excel,FileIn,Fun)->
    Tables=create_ets(),
    {ok,Response}=filter_file(FileIn),
    {ParsedDirectory,ParsedSAT,ParsedSSAT,_SSAT_StartSID,
     SectorSize,ShortSectorSize,MinStreamSize}=Response,
    SubStreams=excel:get_file_structure(ParsedDirectory,ParsedSAT,ParsedSSAT,
                                        SectorSize,ShortSectorSize,
                                        MinStreamSize,Tables,FileIn),
    print_structure(FileIn,ParsedDirectory,SubStreams),
    excel:read_excel(ParsedDirectory,ParsedSAT,ParsedSSAT,
                     SectorSize,ShortSectorSize,
                     MinStreamSize,SubStreams,FileIn,Tables),
    Fun(Tables);

read_excel(_Other,_,_) ->
    {error, file_type_not_supported}.

filter_file(FileIn)->
    {ok,FileHandle}=file:open(FileIn,[raw,binary]),
    {ok,Bin}=file:read(FileHandle,?HEADER_SIZE),
    file:close(FileHandle),
    read_compound_file_header(Bin,FileIn).

%% This section is based on Section 8 of the Open Office description of the
%% Microsoft compound file format.
%%
%% It can be found at http://sc.openoffice.org/compdocfileformat.pdf
read_compound_file_header(<<?BIFF8_MAGIC_NUMBER:64/little-signed-integer,
                           _UID:16/binary,
                           _Version:4/binary,
                           _ByteOrder:2/binary,
                           RawSectorSize:16/little-signed-integer,
                           RawShortSectorSize:16/little-signed-integer,
                           _Blank:10/binary,
                           _NoOfSectorsInSAT:32/little-signed-integer,
                           DirectoryFirstSID:32/little-signed-integer,
                           _Blank2:4/binary,
                           MinStreamSize:32/little-signed-integer,
                           SSAT_StartSID:32/little-signed-integer,
                           _NoOfSSATSectors:32/little-signed-integer,
                           FirstSectorMSAT_SID:32/little-signed-integer,
                           NoOfMSATSectors:32/little-signed-integer,
                           BulkMSAT:436/binary>>,
                          FileIn)->
%% SectorSize is calculated via power of 2 ssz as per Section 4.1 of
%% http://sc.openoffice.org/compdocfileformat.pdf
    SectorSize=trunc(math:pow(2,RawSectorSize)),

%% As per SectorSize
    ShortSectorSize=trunc(math:pow(2,RawShortSectorSize)),

%% Now lets process the header
%% First up read the master Sector Allocation Table
%% Described in Section 5.1 of
%% http://sc.openoffice.org/compdocfileformat.pdf
    MSAT=read_MSAT(NoOfMSATSectors,FirstSectorMSAT_SID,BulkMSAT),

%% Now lets process the Sector Allocation Table
%% as per Section 5.2 of
%% http://sc.openoffice.org/compdocfileformat.pdf
    RawSAT=read_sectors(MSAT,SectorSize,FileIn),
    ParsedSAT=parse_SAT(RawSAT),

%% Now lets process the Short-Sector Allocation Table
%% as per Section 6.2 of
%% http://sc.openoffice.org/compdocfileformat.pdf
    if
        SSAT_StartSID > 0 ->
            RawSSAT=read_sectors([SSAT_StartSID],SectorSize,FileIn),
            ParsedSSAT=parse_SAT(RawSSAT);
        true ->
            _RawSSAT=[],
            ParsedSSAT=[]
    end,

%% Moving onto the Directory as per Section 7.1 of
%% http://sc.openoffice.org/compdocfileformat.pdf
    DirectorySIDs=get_SIDs(ParsedSAT,DirectoryFirstSID),
    RawDirectory=read_sectors(DirectorySIDs,SectorSize,FileIn),
    ParsedDirectory=parse_Directory(list_to_binary(RawDirectory),
                                    MinStreamSize),
    {ok,{ParsedDirectory,ParsedSAT,ParsedSSAT,SSAT_StartSID,
         SectorSize,ShortSectorSize,MinStreamSize}};
read_compound_file_header(_Other,_FileIn) ->
    io:format("This is not a valid Biff8 Microsoft Compound Document~n"++
              "In other words it is not written by Microsoft Excel 97, "++
              "2000 or 2003~n"),
    {error,"Not valid Biff8 Microsoft Compound Document"}.

%% This function translates the header file into the storage and stream
%% structure described in Section 2 of of the Open Office description of the
%% Microsoft compound file format.
%%
%% It can be found at http://sc.openoffice.org/compdocfileformat.pdf

%% This patterns matches a non-extended MSAT as per Section 4.1
read_MSAT(0,?END_OF_CHAIN_SID,BulkMSAT)->
    get_sectors(BulkMSAT);
read_MSAT(_FirstSectorMSAT_SID,_NoOfMSATSectors,_BulkMSAT)->
    ok.

get_SIDs(ParsedSAT,SID)->
    get_SIDs(ParsedSAT,SID,[]).

get_SIDs(_ParsedSAT,?END_OF_CHAIN_SID,Residuum)->
    lists:reverse(Residuum);
get_SIDs(ParsedSAT,SID,Residuum)->
    {value,{SID,Value}} = lists:keysearch(SID,1,ParsedSAT),
    NewResiduum=[SID|Residuum],
    get_SIDs(ParsedSAT,Value,NewResiduum).

get_sectors(BulkMSAT)->
    get_sectors(BulkMSAT,[]).

get_sectors(<<?FREE_SID:32/little-signed-integer,_Rest/binary>>,Residuum)->
    lists:reverse(Residuum);
get_sectors(<<SID:32/little-signed-integer,Rest/binary>>,Residuum)->
    NewResiduum=[SID|Residuum],
    get_sectors(Rest,NewResiduum).

read_sectors(SAT,SectorSize,FileIn)->
    {ok,FileHandle}=file:open(FileIn,[raw,binary]),
    read_sectors(SAT,FileHandle,SectorSize,[]).

read_sectors([],FileHandle,_SectorSize,Residuum)->
    file:close(FileHandle),
    lists:reverse(Residuum);
read_sectors([H|T],FileHandle,SectorSize,Residuum)->
    {ok,Bin}=file:pread(FileHandle,H*SectorSize+?HEADER_SIZE,SectorSize),
    read_sectors(T,FileHandle,SectorSize,[Bin|Residuum]).

%% This function parses both the Sector Allocation Table AND
%% the Short Sector Allocation Table
parse_SAT(SATList)->
    parse_SAT(SATList,[]).

parse_SAT([],Residuum)->
    make_index_hash(lists:flatten(lists:reverse(Residuum)));
parse_SAT([H|T],Residuum)->
    NewResiduum=[parse_SAT_Bin(H)|Residuum],
    parse_SAT(T,NewResiduum).

parse_SAT_Bin(Bin)->
    parse_SAT_Bin(Bin,[]).

parse_SAT_Bin(<<>>,Residuum)->
    lists:reverse(Residuum);
parse_SAT_Bin(<<SID:32/little-signed-integer,Rest/binary>>,Residuum)->
    parse_SAT_Bin(Rest,[SID|Residuum]).

%% The SAT Hash is described in Section 5.2.2 of
%% http://sc.openoffice.org/compdocfileformat.pdf
%% Basically the list we have has an implied key of the zero-based index
%% of the list (ie the first member 'is' the zero-th sector and the
%% number in it is the index of the next sector
%% This function will step through that list and stick an explit index in
make_index_hash(List)->
    make_index_hash(List,0,[]).

make_index_hash([],_,Residuum)->
    lists:reverse(Residuum);
make_index_hash([H|T],Index,Residuum)->
    make_index_hash(T,Index+1,[{Index,H}|Residuum]).

parse_Directory(RawDirectory,MinStreamSize)->
    parse_Directory(RawDirectory,MinStreamSize,[]).

parse_Directory(<<>>,_MinStreamSize,Residuum)->
    make_index_hash(lists:reverse(Residuum));
parse_Directory(<<Name:64/binary,
                 NameSize:16/little-signed-integer,
                 Type:8/little-signed-integer,
                 Colour:8/little-signed-integer,
                 LeftDID:32/little-signed-integer,
                 RightDID:32/little-signed-integer,
                 RootDID:32/little-signed-integer,
                 UID:16/binary,
                 Flags:4/binary,
                 TimeCreated:8/binary,
                 TimeLastModified:8/binary,
                 SID:32/little-signed-integer,
                 StreamSize:32/little-signed-integer,
                 _Blank:4/binary,
                 Rest/binary>>,MinStreamSize,Residuum) ->
%% If the Sector Size is bigger than the stream size then the stream
%% will be in the short sector table - otherwise it will be in a normal one
    Location=if (StreamSize >= MinStreamSize) -> normal_stream;
                true                          -> short_stream
             end,
    DirectoryEntry=[{name,{'utf16-16',Name}},
                    {bodge_name,bodge_string(Name,NameSize)},
                    {location,Location},
                    {namesize,NameSize},
                    {type,Type},
                    {colour,Colour},
                    {leftDID,LeftDID},
                    {rightDID,RightDID},
                    {rootDID,RootDID},
                    {uid,UID},
                    {flags,Flags},
                    {time_created,TimeCreated},
                    {time_last_modified,TimeLastModified},
                    {sid,SID},
                    {stream_size,StreamSize}],
    parse_Directory(Rest,MinStreamSize,
                    [DirectoryEntry|Residuum]);
parse_Directory(Other,_MinStreamSize,Other2)->
    io:format(" in filefilters:parse_Directory Other "++
              "is ~p Other2 is ~p "++
              "Oh, shit!~n",[Other,Other2]).

print_structure(_FileIn,_Directory,{SubLocation,_SubStreams})->
%% This function will bork as soon as it is handed a substream name
%% that has a Asian or Rich Text Name - but as it is fed by
%% output that is internal to Excel I think it will still continue
%% to come in unicode16-8 format (suck'n'see fella)
    {_SubLoc,_SID}=SubLocation,
%%    io:format("~nThe structure of the Microsoft Coumpound Document Object ~p "++
%%              "is:~n",[FileIn]),
%%    List=[{lists:keysearch('bodge_name',1,X),lists:keysearch(location,1,X)}
%%    || {_,X} <- Directory],
%%    [io:format("* stream ~p is in ~p~n",
%%         [BodgeName,Location]) || {{_,{_,BodgeName}},
%%                   {_,{_,Location}}} <- List],
%%    io:format("the substreams in ~p are:~n",[SubLoc]),
%%    [io:format("* substream: ~p~n",[X]) || {[{_,X}],_} <- SubStreams],
    ok.

delete_ets([])    -> ok;
delete_ets([{_TableName,Tid}|T]) -> ets:delete(Tid),
                                    delete_ets(T).

create_ets()->
    [{cell,           ets:new(cell,           [ordered_set,private])},
     {array_formulae, ets:new(array_formulae, [ordered_set,private])},
     {cell_tokens,    ets:new(cell_tokens,    [ordered_set,private])},
     {strings,        ets:new(strings,        [ordered_set,private])},
     {names,          ets:new(names,          [ordered_set,private])},
     {fixedupnames,   ets:new(fixedupnames,   [ordered_set,private])},
     {sh_arr_formula, ets:new(sh_arr_formula, [ordered_set,private])},
     {externsheets,   ets:new(externsheets,   [ordered_set,private])},
     {sheetnames,     ets:new(sheetnames,     [ordered_set,private])},
     {externalrefs,   ets:new(externalrefs,   [ordered_set,private])},
     {formats,        ets:new(formats,        [ordered_set,private])},
     {xf,             ets:new(xf,             [ordered_set,private])},
     {extra_fns,      ets:new(extra_fns,      [ordered_set,private])},
     {lacunae,        ets:new(lacunae,        [ordered_set,private])},
     {misc,           ets:new(misc,           [ordered_set,private])},
     {warnings,       ets:new(warnings,       [ordered_set,private])}].

%% Bodge string attempts to take a list in some class of unicode encoding and
%% make it 'readable' - it is for logging/debugging only and not to be used
%% to prepare output for reuse (YMMV son!)...
%%
%% For non-native speakers bodge means something similar to lash-up, kludge
%% and generally 'do the wrong thing' just for convenience - in this case
%% we assume that the language is some class of Western European/Latin script
%% and to hell with the rest of the world/accented letters etc...
bodge_string(_Bin,0)->
    [];
bodge_string(Bin,NameSize)->
    bodge_string(Bin,NameSize-2,[]). % Strip off the trailing zero (2 bytes)

bodge_string(_Other,0,Residuum)->
    lists:reverse(Residuum);
bodge_string(<<Char:16/little-signed-integer,Rest/binary>>,
             NameSize,Residuum) when Char < 32->
    bodge_string(Rest,NameSize-2,[32|Residuum]);
bodge_string(<<Char:16/little-signed-integer,Rest/binary>>,
             NameSize,Residuum)->
    bodge_string(Rest,NameSize-2,[Char|Residuum]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Debugging functions                                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_ets_DEBUG()        -> create_ets().
delete_ets_DEBUG(Tables)  -> delete_ets(Tables).
dump_DEBUG(Tables)        -> dump(Tables).

test_DEBUG()->
    % File="minitest.xls",
    % File="e_gnumeric_operators_add.xls",
    % File="b_array_formulae.xls",
    % File="c_basic_functions_tests_a_e.xls",
    % File="b_abs_and_rel_addressing.xls",
    % File="d_gnumeric_date_and_time.xls",
    % File="b_ping.xls",
    % File="b_basic_unicode_strings.xls",
    % File="b_three_dee_ref.xls",
    % File="b_floats.xls",
    % File="b_simple_arrays_and_ranges.xls",
    % File="d_gnumeric_address.xls",
    % File="junk_external_ref.xls",
    % File="z_junk.xls",
    % File="a_should_be_10_tests.xls",
    % File="rich_text_junk.xls",
    % File="f_loan_amortization_schedule.xls",
    % File="f_12_month_cash_flow_statement.xls",
    % File="f_payroll_calculator.xls",
    % File="f_services_invoice_with_tax_calculation.xls",
    File="b_array_formulae.xls",
    FileRoot="C:/opt/code/trunk/tests/"++
        "excel_files/Win_Excel07_As_97",
    io:format("in filefilters:test_DEBUG FileRoot is ~p and File is ~p~n",
              [FileRoot,File]),
    read(excel,FileRoot++"/"++File),
    io:format("~s processed~n",[File]).

dump([])-> io:format("All tables dumped~n");
dump([{Table,Tid}|T])->
    case Table of
        externalrefs   -> dump2({Table,Tid});
        externsheets   -> dump2({Table,Tid});
        % names          -> dump2({Table,Tid});
        fixedupnames   -> dump2({Table,Tid});
        % lacunae        -> dump2({Table,Tid});
        % extra_fns      -> dump2({Table,Tid});
        % cell           -> dump2({Table,Tid});
        % sheetnames     -> dump2({Table,Tid});
        % xf             -> dump2({Table,Tid});
        % formats        -> dump2({Table,Tid});
        % sh_arr_formula -> dump2({Table,Tid});
        warnings       -> dump2({Table,Tid});
        _              -> io:format("skipping Table ~p in filefilters:dump~n",[Table])
    end,
    dump(T).

dump2({Table,Tid}) ->
    io:format("~nDumping table: ~p~n",[Table]),
    Fun = fun(X,_Y) -> io:format("~p: ~p~n",[Table,X]) end,
    ets:foldl(Fun,[],Tid).

