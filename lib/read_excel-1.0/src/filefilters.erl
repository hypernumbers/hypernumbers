%%%-------------------------------------------------------------------
%%% File        : filefilters.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : runs Microsoft Office filters
%%%
%%% Created     :  4 Apr 2007 by Gordon Guthrie <gordonguthrie@gg-laptop>
%%%-------------------------------------------------------------------
-module(filefilters).

%%% Exports
-export([read/4,read/3,filter_file/2,get_SIDs/3]).

%%% Debugging exports - not for proper use
-export([test_DEBUG/0]).

-include("spriki.hrl").
-include("microsoftcompoundfileformat.hrl").
-include("microsoftbiff.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Functions for filtering the files                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(excel,FileIn,FileOut,Fun)->
    io:format("in filefilters:read/4 FileIn is ~p~nFileOut is ~p~n",
	      [FileIn,FileOut]),
    
read_excel(excel,FileIn,FileOut,Fun).

read(excel,FileIn,FileOut)->
    Fun= fun(X) ->     
		 io:format("About to dump tables~n"),
		 dump(X)
	 end,
    read_excel(excel,FileIn,FileOut,Fun).
    
read_excel(excel,FileIn,FileOut,Fun)->
    Tables=create_ets(),
    io:format("Tables created: ~p~n",[Tables]),
    {ok,Response}=filter_file(FileIn,FileOut),
    {ParsedDirectory,ParsedSAT,ParsedSSAT,_SSAT_StartSID,
     SectorSize,ShortSectorSize,MinStreamSize}=Response,
    %%io:format("in filefilters:read ParsedDirectory is ~p~n",[ParsedDirectory]),
    SubStreams=excel:get_file_structure(ParsedDirectory,ParsedSAT,ParsedSSAT,
					SectorSize,ShortSectorSize,
					MinStreamSize,FileIn,FileOut),
    print_structure(FileIn,ParsedDirectory,SubStreams),
    excel:read_excel(ParsedDirectory,ParsedSAT,ParsedSSAT,
		     SectorSize,ShortSectorSize,
		     MinStreamSize,SubStreams,FileIn,Tables,FileOut),
    Fun(Tables);
read_excel(Other,_,_,_) ->
    io:format("in filefilters:read File Type ~p is not supported~n",[Other]),
    {error, file_type_not_supported}.

filter_file(FileIn,FileOut)->
    {ok,FileHandle}=file:open(FileIn,[raw,binary]),
    {ok,Bin}=file:read(FileHandle,?HEADER_SIZE),
    file:close(FileHandle),
    %%excel_util:put_log(FileOut,io_lib:fwrite("First 512 bytes (Compound "++
		%%			     "File Header) of File ~p read~n",
		%%			     [FileIn])),
    read_compound_file_header(Bin,FileIn,FileOut).

%% This section is based on Section 8 of the Open Office description of the
%% Microsoft compound file format.
%%
%% It can be found at http://sc.openoffice.org/compdocfileformat.pdf
read_compound_file_header(<<?BIFF8_MAGIC_NUMBER:64/little-signed-integer,
			   _UID:16/binary,
			   _Version:4/binary,
			   ByteOrder:2/binary,
			   RawSectorSize:16/little-signed-integer,
			   RawShortSectorSize:16/little-signed-integer,
			   _Blank:10/binary,
			   NoOfSectorsInSAT:32/little-signed-integer,
			   DirectoryFirstSID:32/little-signed-integer,
			   _Blank2:4/binary,
			   MinStreamSize:32/little-signed-integer,
			   SSAT_StartSID:32/little-signed-integer,
			   NoOfSSATSectors:32/little-signed-integer,
			   FirstSectorMSAT_SID:32/little-signed-integer,
			   NoOfMSATSectors:32/little-signed-integer,
			   BulkMSAT:436/binary>>,
			  FileIn,FileOut)->
    %%excel_util:put_log(FileOut,io_lib:fwrite("VALID BIFF8 MICROSOFT "++
		%%			     "COMPOUND FILE FORMAT~n~n",[])),
    %% util2:print_as_hex("UID",UID),
    %% util2:print_as_hex("Version",Version),
    %% util2:print_as_hex("ByteOrder",ByteOrder),
    %%excel_util:put_log(FileOut,io_lib:fwrite("ByteOrder is ~p~n",[ByteOrder])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("RawSectorSize: ~p~n",
		%%			     [RawSectorSize])),

    %% SectorSize is calculated via power of 2 ssz as per Section 4.1 of
    %% http://sc.openoffice.org/compdocfileformat.pdf
    SectorSize=trunc(math:pow(2,RawSectorSize)),
    %%excel_util:put_log(FileOut,io_lib:fwrite("SectorSize (bytes): ~p~n",
		%%			     [SectorSize])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("RawShortSectorSize: ~p~n",
		%%			     [RawShortSectorSize])),

    %% As per SectorSize
    ShortSectorSize=trunc(math:pow(2,RawShortSectorSize)),
    %%excel_util:put_log(FileOut,io_lib:fwrite("ShortSectorSize (bytes): ~p~n",
		%%     [ShortSectorSize])),
    %%util2:print_as_hex("_Blank",_Blank),
    %%excel_util:put_log(FileOut,io_lib:fwrite("NoOfSectors: ~p~n",
		%%			     [NoOfSectorsInSAT])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("DirectoryFirstSID: ~p~n",
		%%			     [DirectoryFirstSID])),
    %%util2:print_as_hex("_Blank2",_Blank2),
    %%excel_util:put_log(FileOut,io_lib:fwrite("MinStreamSize: ~p~n",
		%%			     [MinStreamSize])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("SSAT_StartSID: ~p~n",
		%%			     [SSAT_StartSID])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("NoOfSSATSectors: ~p~n",
		%%			     [NoOfSSATSectors])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("FirstSectorMSAT_SID: ~p~n",
		%%			     [FirstSectorMSAT_SID])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("NoOfMSATSectors: ~p~n",
		%%			     [NoOfMSATSectors])),
    %%util2:print_as_hex("BulkMSAT",BulkMSAT),

    %% Now lets process the header
    %% First up read the master Sector Allocation Table
    %% Described in Section 5.1 of
    %% http://sc.openoffice.org/compdocfileformat.pdf
    MSAT=read_MSAT(NoOfMSATSectors,FirstSectorMSAT_SID,BulkMSAT,FileOut),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Master Sector Allocation "++
		%%			     "Table is:~n~p~n",[MSAT])),

    %% Now lets process the Sector Allocation Table
    %% as per Section 5.2 of
    %% http://sc.openoffice.org/compdocfileformat.pdf
    RawSAT=read_sectors(MSAT,SectorSize,FileIn),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Raw SAT is:~n~p~n",[RawSAT])),
    ParsedSAT=parse_SAT(RawSAT),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Sector Allocation Table "++
		%%			     "is:~n~p~n",[ParsedSAT])),

    %% Now lets process the Short-Sector Allocation Table
    %% as per Section 6.2 of
    %% http://sc.openoffice.org/compdocfileformat.pdf
    if 
	SSAT_StartSID > 0 ->
	    %%excel_util:put_log(FileOut,io_lib:fwrite("There is a short sector "++
			%%			     "allocation table~n",[])),
	    RawSSAT=read_sectors([SSAT_StartSID],SectorSize,FileIn),
	    %%excel_util:put_log(FileOut,io_lib:fwrite("RawSSAT is:~n~p~n",
			%%			     [RawSSAT])),
	    ParsedSSAT=parse_SAT(RawSSAT);
	    %%excel_util:put_log(FileOut,io_lib:fwrite("ParsedSSAT is:~n~p~n",
			%%			     [ParsedSSAT]));
	true ->
	    %%excel_util:put_log(FileOut,io_lib:fwrite("NO a short sector "++
			%%			     "allocation table~n",[])),
	    _RawSSAT=[],
	    ParsedSSAT=[]
    end,

    %% Moving onto the Directory as per Section 7.1 of
    %% http://sc.openoffice.org/compdocfileformat.pdf
    DirectorySIDs=get_SIDs(ParsedSAT,DirectoryFirstSID,FileOut),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The SIDs for the Directory "++
		%%			     "are:~n~p~n",[DirectorySIDs])),
    RawDirectory=read_sectors(DirectorySIDs,SectorSize,FileIn),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The raw Directory is:~n~p~n",
		%%		  	     [RawDirectory])),
    ParsedDirectory=parse_Directory(list_to_binary(RawDirectory),
				    MinStreamSize,FileOut),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Directory is:~n~p~n",
		%%			     [ParsedDirectory])),
    {ok,{ParsedDirectory,ParsedSAT,ParsedSSAT,SSAT_StartSID,
	 SectorSize,ShortSectorSize,MinStreamSize}};
read_compound_file_header(_Other,_FileIn,_FileOut) ->
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
read_MSAT(0,?END_OF_CHAIN_SID,BulkMSAT,FileOut)->
    %%excel_util:put_log(FileOut,io_lib:fwrite("~nMASTER SECTOR ALLOCATION "++
		%%			     "TABLE DESCRIPTION~n~n",[])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Master Sector Allocation "++
		%%			     "Table is fully specified "++
		%%			     "in the header. No futher "++
		%%			     "sectors used for the "++
		%%			     "Master Sector Allocation Table~n",
		%%			     [])),
    get_sectors(BulkMSAT);
read_MSAT(_FirstSectorMSAT_SID,_NoOfMSATSectors,_BulkMSAT,FileOut)->
    %%excel_util:put_log(FileOut,io_lib:fwrite("~nMASTER SECTOR ALLOCATION "++
		%%			     "TABLE DESCRIPTION~n~n",[])),
    %%excel_util:put_log(FileOut,io_lib:fwrite("The Master Sector Allocation "++
		%%			     "Table is NOT fully specified "++
		%%			     "in the header. Further "++
		%%			     "sectors are used, but I don't"++
		%%			     "know how to parse 'em~n"++
		%%			     "It's gonna be a sector read "++
		%%			     "from the SID somehow...",[])),
    ok.

get_SIDs(ParsedSAT,SID,FileOut)->
    %%excel_util:put_log(FileOut,io_lib:fwrite("in filefilters:get_SIDs "++
		%%			     "ParsedSAT is ~p and SID is ~p~n",
		%%			     [ParsedSAT,SID])),
    get_SIDs(ParsedSAT,SID,[],FileOut).

get_SIDs(_ParsedSAT,?END_OF_CHAIN_SID,Residuum,_FileOut)->
    lists:reverse(Residuum);
get_SIDs(ParsedSAT,SID,Residuum,FileOut)->
    {value,{SID,Value}} = lists:keysearch(SID,1,ParsedSAT),
    NewResiduum=[SID|Residuum],
    get_SIDs(ParsedSAT,Value,NewResiduum,FileOut).

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

parse_Directory(RawDirectory,MinStreamSize,FileOut)->
    parse_Directory(RawDirectory,MinStreamSize,[],FileOut).

parse_Directory(<<>>,_MinStreamSize,Residuum,_FileOut)->
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
		 Rest/binary>>,MinStreamSize,Residuum,FileOut) ->
    excel_util:put_log(FileOut,io_lib:fwrite("~nPARSING DIRECTORY ENTRY~n~n",[])),
    excel_util:put_log(FileOut,io_lib:fwrite("NameSize: ~p~n",[NameSize])),
    excel_util:put_log(FileOut,io_lib:fwrite("Guessing Name: ~p~n",
					     [bodge_string(Name,NameSize)])),
    excel_util:put_log(FileOut,io_lib:fwrite("Type: ~p~n",[Type])),
    excel_util:put_log(FileOut,io_lib:fwrite("Colour: ~p~n",[Colour])),
    excel_util:put_log(FileOut,io_lib:fwrite("LeftDID: ~p~n",[LeftDID])),
    excel_util:put_log(FileOut,io_lib:fwrite("RightDID: ~p~n",[RightDID])),
    excel_util:put_log(FileOut,io_lib:fwrite("RootDID: ~p~n",[RootDID])),
    excel_util:put_log(FileOut,io_lib:fwrite("SID: ~p~n",[SID])),
    excel_util:put_log(FileOut,io_lib:fwrite("StreamSize: ~p~n",[StreamSize])),
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
		    [DirectoryEntry|Residuum],FileOut);
parse_Directory(Other,_MinStreamSize,Other2,_FileOut)->
    io:format(" in filefilters:parse_Directory Other "++
	      "is ~p Other2 is ~p "++
	      "Oh, shit!~n",[Other,Other2]).

print_structure(FileIn,Directory,{SubLocation,SubStreams})->
    %% This function will bork as soon as it is handed a substream name
    %% that has a Asian or Rich Text Name - but as it is fed by
    %% output that is internal to Excel I think it will still continue
    %% to come in unicode16-8 format (suck'n'see fella)
    {SubLoc,_SID}=SubLocation,
    io:format("~nThe structure of ~p is:~n",[FileIn]),
    List=[{lists:keysearch('bodge_name',1,X),lists:keysearch(location,1,X)} 
	  || {_,X} <- Directory],
    [io:format("* stream ~p is in ~p~n",
	       [BodgeName,Location]) || {{_,{_,BodgeName}},
					 {_,{_,Location}}} <- List],
    io:format("the substreams in ~p are:~n",[SubLoc]),
    [io:format("* substream: ~p~n",[X]) || {[{_,X}],_} <- SubStreams],
    ok.

create_ets()->
    [{cell,ets:new(cell,[ordered_set,private])},
     {strings,ets:new(strings,[ordered_set,private])},
     {names,ets:new(names,[ordered_set,private])},
     {arrayformula,ets:new(names,[ordered_set,private])}].

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
    bodge_string(Bin,NameSize-2,[]). % Strip of the trailing zero (2 bytes)

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

test_DEBUG()-> 
    File = "Minitest.xls",
    %%File = "booleans_and_errors.xls",
    %%File="basic_functions_tests_ii.xls",
    %%File="arithmetic_and_precedence.xls",
    %%File="simple_arrays_and_ranges.xls",
    %%File="sum_with_one_arg.xls",
    %%File="block_of_numbers.xls",
    %%File="gnumeric_db.xls",
    FileRoot="c:/Users/Gordon Guthrie/Documents/SVN/trunk/testroot/"++
     	"excel_import_test/files/Win Excel 2007 (as 97)",
    io:format("in filefilters:test_DEBUG FileRoot is ~p and File is ~p~n",
              [FileRoot,File]),
    read(excel,FileRoot++"/"++File,
    	 FileRoot++"/scratch/string_test.log"),
    io:format("~s processed~n",[File]).
%%     read(excel,FileRoot++"/mediumfile4.xls",
%%     	 FileRoot++"/scratch/mediumfile4.log"),
%%     io:format("mediumfile4.xls processed~n").
%%   read(excel,FileRoot++"/cellformat_import_biff8.xls",FileRoot++
%%    	 "/scratch/cell_format_import_biff8.log"),
%%   io:format("cellformat_import_biff8.xls processed~n"),
%%   read(excel,FileRoot++"/cells_import_biff8.xls",FileRoot++
%%    	 "/scratch/cells_import_biff8.log"),
%%   io:format("cells_import_biff8.xls processed~n"),
%%   read(excel,FileRoot++"/test1.xls",FileRoot++"/scratch/test1.log"),
%%   io:format("test1.xls processed~n"),
%%   read(excel,FileRoot++"/10eii1.xls",FileRoot++"/scratch/10eii1.log"),
%%   io:format("10eii1.xls processed~n").

dump([])-> io:format("All tables dumped~n");
dump([{Name,Tid}|T])->
    io:format("Dumping table: ~p~n",[Name]),
    Fun = fun(X,_Y) -> io:format("~p~n",[X]) end,
    ets:foldl(Fun,[],Tid),
    dump(T).
