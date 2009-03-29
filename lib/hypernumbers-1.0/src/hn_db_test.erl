%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc        This is the test functions for hn_db_api.ler
%%%
%%% @end
%%% Created : 22 Feb 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
%%% @private
-module(hn_db_test).

-include("spriki.hrl").

%%% Debugging interface
-export([dirty/0,
         copy_DEBUG/0,
         delete_cell_contents_DEBUG/1,
         clear_cells_DEBUG/1,
         testing/0,
         delete_DEBUG/2,
         insert_DEBUG/2,
         clear_TEST/0,
         quickie/0]).

-export([read_styles_DEBUG/0,
         hn_DEBUG/0]). % Debugging

-import(hn_db_api, [
                    write_attributes/2,
                    write_last/1,
                    % write_permission/2,
                    % write_style/2,
                    read_attributes/2,
                    read/1,
                    read_styles/1,
                    % read_permissions/1,
                    % update_style/2,
                    recalculate/1,
                    reformat/1,
                    drag_n_drop/2,
                    copy_n_paste/2,
                    cut_n_paste/2,
                    % copy_page/2,
                    insert/1,
                    insert/2,
                    delete/1,
                    delete/2,
                    clear/1,
                    clear/2,
                    % delete_permission/1,
                    % delete_style/1,
                    % notify/5,
                    % notify_back/3,
                    notify_back_create/2,
                    read_incoming_hn/2,
                    write_remote_link/3,
                    notify_from_web/7,
                    notify_back_from_web/4,
                    handle_dirty_cell/1,
                    handle_dirty_notify_in/1,
                    handle_dirty_notify_out/5,
                    handle_dirty_notify_back_in/4,
                    handle_dirty_notify_back_out/3,
                    register_hn_from_web/4,
                    create_db/0
                   ]).

%% @hidden
dirty() ->

    Path = ["dirty1"],

    insert_delete("insert", Path, {cell, {5, 5}}, vertical),
    insert_delete("insert", Path, {cell, {7, 7}}, vertical),

    io:format("in hn_db_test:dirty - going into Dirty 1~n"),
    % write a line of cells that link to each other
    dirty1(),

    io:format("in hn_db_test:dirty - going into Dirty 2~n"),
    % now write a line of hypernumbers pointing to the first cells
    dirty2(),

    test_util:wait(25),

    io:format("in hn_db_test:dirty - triggering rewrite~n"),
    % now rewrite the first cell triggering all the cells and their
    % hypernumbers to recalculcate
    write_value(Path, "123", {1, 1}, []),

    test_util:wait(25),

    io:format("in hn_db_test:dirty - testing dependency tree propagation~n"),
    % now write new value in the middle of the first list and check
    % that the hypernumbers dependency trees update properly
    write_value(Path, "Starts in Row 15", {1, 15}, []),

    test_util:wait(25),

    io:format("in hn_db_test:dirty - going into Dirty 3~n"),
    % now clear some hypernumbers children and check that the parent side remote
    % links have been cleared..
    dirty3().
    
%    io:format("in hn_db_test:dirty - going into Dirty 4~n"),
%    % now do some row and column inserts on the parent page
%    dirty4().
    
%    io:format("in hn_db_test:dirty - going into Dirty 5~n"),
%    % now do some row and column inserts on the child page
%    dirty5().

%% @hidden
dirty5() ->

    FunName2 = "insert",

    % first up a cell on the parent page
    Path2 = ["dirty2"],
    Site2 = "http://il_ballo.dev:9000",

    insert_delete(FunName2, Path2, {cell, {1, 2}}, vertical).

%% @hidden
dirty4() ->
    
    FunName = "insert",
   
    % first up a cell on the parent page
    Path = ["dirty1"],
    Site = "http://127.0.0.1:9000",

    insert_delete(FunName, Path, {cell, {1, 2}}, vertical).
                 
%% @hidden
dirty3()->
    FunName = "dirty2",

    Path2 = [FunName],
    Site2 = "http://il_ballo.dev:9000",
    
    clear_cells(Site2, Path2, {1, 1}),
    clear_cells(Site2, Path2, {1, 2}).

%% @hidden
dirty2() ->
    FunName = "dirty2",

    Path = [FunName],
    Path2 = [FunName, "data"],
    Site = "http://127.0.0.1:9000",
    Site2 = "http://il_ballo.dev:9000",
    
    io:format("Clearing the cells...~n"),
    
    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A1?hypernumber\")", {1, 1}, []),
    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A2?hypernumber\")", {1, 2}, []),
    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A3?hypernumber\")", {1, 3}, []),
    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A4?hypernumber\")", {1, 4}, []).
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A5?hypernumber\")", {1, 5}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A6?hypernumber\")", {1, 6}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A7?hypernumber\")", {1, 7}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A8?hypernumber\")", {1, 8}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A9?hypernumber\")", {1, 9}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A10?hypernumber\")", {1, 10}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A11?hypernumber\")", {1, 11}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A12?hypernumber\")", {1, 12}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A13?hypernumber\")", {1, 13}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A14?hypernumber\")", {1, 14}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A15?hypernumber\")", {1, 15}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A16?hypernumber\")", {1, 16}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A17?hypernumber\")", {1, 17}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A18?hypernumber\")", {1, 18}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A19?hypernumber\")", {1, 19}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A20?hypernumber\")", {1, 20}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A21?hypernumber\")", {1, 21}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A22?hypernumber\")", {1, 22}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A23?hypernumber\")", {1, 23}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A24?hypernumber\")", {1, 24}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A25?hypernumber\")", {1, 25}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A26?hypernumber\")", {1, 26}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A27?hypernumber\")", {1, 27}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A28?hypernumber\")", {1, 28}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A29?hypernumber\")", {1, 29}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A30?hypernumber\")", {1, 30}, []),
%    write_value(Site2, Path, "=hn(\"http://127.0.0.1:9000/dirty1/A31?hypernumber\")", {1, 31}, []).

%% @hidden
dirty1() ->

    FunName = "dirty1",

    Path = [FunName],
    Path2 = [FunName, "data"],
    Site = "http://127.0.0.1:9000",
    Site2 = "http://il_ballo.dev:9000",
    
    io:format("Clearing the cells...~n"),

    write_value(Path, "1", {1, 1}, []),
    write_value(Path, "=a1+1", {1, 2}, []),
    write_value(Path, "=a2+1", {1, 3}, []),
    write_value(Path, "=a3+1", {1, 4}, []),
    write_value(Path, "=a4+1", {1, 5}, []).
%    write_value(Path, "=a5+1", {1, 6}, []),
%    write_value(Path, "=a6+1", {1, 7}, []),
%    write_value(Path, "=a7+1", {1, 8}, []),
%    write_value(Path, "=a8+1", {1, 9}, []),
%    write_value(Path, "=a9+1", {1, 10}, []),
%    write_value(Path, "=a10+1", {1, 11}, []),
%    write_value(Path, "=a11+1", {1, 12}, []),
%    write_value(Path, "=a12+1", {1, 13}, []),
%    write_value(Path, "=a13+1", {1, 14}, []),
%    write_value(Path, "=a14+1", {1, 15}, []),
%    write_value(Path, "=a15+1", {1, 16}, []),
%    write_value(Path, "=a16+1", {1, 17}, []),
%    write_value(Path, "=a17+1", {1, 18}, []),
%    write_value(Path, "=a18+1", {1, 19}, []),
%    write_value(Path, "=a19+1", {1, 20}, []),
%    write_value(Path, "=a20+1", {1, 21}, []),
%    write_value(Path, "=a21+1", {1, 22}, []),
%    write_value(Path, "=a22+1", {1, 23}, []),
%    write_value(Path, "=a23+1", {1, 24}, []),
%    write_value(Path, "=a24+1", {1, 25}, []),
%    write_value(Path, "=a25+1", {1, 26}, []),
%    write_value(Path, "=a26+1", {1, 27}, []),
%    write_value(Path, "=a27+1", {1, 28}, []),
%    write_value(Path, "=a28+1", {1, 29}, []),
%    write_value(Path, "=a29+1", {1, 30}, []),
%    write_value(Path, "=a30+1", {1, 31}, []).

%% @hidden
quickie() ->
    RefX = #refX{site = "http://il_ballo.dev:9000", path = ["clear", "data"],
                 obj = {cell, {1, 7}}, auth = '_'},
    Return = hn_db_api:read_attributes(RefX, ["value", "dependency-tree"]),
    io:format("in quickie Return is ~p~n", [Return]).

%% @hidden
testing() ->
    clear_DEBUG2().
% insert_DEBUG2("insert"),
% delete_DEBUG2()

clear_DEBUG2() ->

    FunName = "clear",

    Path = [FunName],
    Path2 = [FunName, "data"],
    Site = "http://127.0.0.1:9000",
    Site2 = "http://il_ballo.dev:9000",
    
    io:format("Clearing the cells...~n"),

    test_util:wait(20),

    % now write out the data and perform the tests
    write_value(Path, FunName, {1, 1}, [bold, underline, center]),

    write_value(Path, "before: "++FunName, {3, 1}, [bold, underline, center]),
    colour(Path, {3, 1}, "grey"),

    % cell stuff first
    write_value(Path, "Cell "++FunName, {1, 3},
                [bold, underline, center]),

    write_value(Path, "Links to Column A", {3, 3},
                [bold, underline, center]),
    
    write_value(Path, "123", {1, 4}, []),
    write_value(Path, "<-- plain value", {2, 4}, []),
   
    write_value(Path, "=c5", {1, 5}, []),
    write_value(Path, "<-- local child", {2, 5}, []),
    write_value(Path, "456", {3, 5}, []),

    write_value(Path, "987", {1, 6}, []),
    write_value(Path, "<-- local parent", {2, 6}, []),
    write_value(Path, "=a6", {3, 6}, []),
    
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/clear/data/A7?hypernumber\")",
                {1, 7}, []),
    write_value(Path, "<-- remote parent", {2, 7}, []),
    write_value(Site2, Path2, "xxx", {1, 7}, []),

    write_value(Path, "=hn(\"http://il_ballo.dev:9000/clear/data/A7?hypernumber\")",
                    {1, 8}, []),
    write_value(Path, "<-- same remote parent as above ^^", {2, 8}, []),

    write_value(Path, "yyy", {1, 9}, []),
    write_value(Path, "<-- 2 remote children", {2, 9}, []),
    write_value(Site2, Path2, "=hn(\"http://127.0.0.1:9000/clear/A9?hypernumber\")",
                {1, 9}, []),
    write_value(Site2, Path2, "=hn(\"http://127.0.0.1:9000/clear/A9?hypernumber\")",
                {2, 9}, []),
     
    colour(Path, {1, 4}, "orange"),
    colour(Path, {1, 5}, "orange"),
    colour(Path, {1, 6}, "orange"),
    colour(Path, {1, 7}, "orange"),
    colour(Path, {1, 8}, "orange"),
    colour(Path, {1, 9}, "orange"),

    clear_cells(Site, Path, {1, 7}),
    
    make_thick(Path, 1), 
    make_thick(Path, 2),
    make_thick(Path, 3).

delete_DEBUG2() ->

    FunName = "delete",

    Path = [FunName],

    io:format("Clearing the cells...~n"),

    clear_cells_DEBUG(Path),
    clear_cells_DEBUG([FunName | "data"]),
    clear_cells_DEBUG("http://il_ballo.dev:9000", [FunName, "data"]),

    test_util:wait(20),

    % now write out the data and perform the tests
    write_value(Path, FunName, {2, 1}, [bold, underline, center]),

    write_value(Path, "before: "++FunName, {3, 1}, [bold, underline, center]),
    colour(Path, {3, 1}, "grey"),

    % cell stuff first
    write_value(Path, "Cell "++FunName++" (down)", {2, 3},
                [bold, underline, center]),

    write_value(Path, "Links to Column A", {3, 3},
                [bold, underline, center]),

    write_value(Path, "=./data/A1", {1, 4}, []),

    write_value(Path, FunName++" a cell here", {1, 5}, []),
    write_value(Path, "987", {1, 6}, []),
    write_value(Path, "=./data/B1", {1, 7}, []),
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/insert/data/C1?hypernumber\")",
                {1, 8}, []),
    colour(Path, {1, 4}, "orange"),
    colour(Path, {1, 5}, "orange"),
    colour(Path, {1, 6}, "orange"),
    colour(Path, {1, 7}, "orange"),
    write_value(Path, "=A6", {3, 5}, []),
    colour(Path, {3, 5}, "yellow").

%% @hidden
insert_DEBUG2(FunName) ->

    Path = [FunName],

    io:format("Clearing the cells...~n"),

    clear_cells_DEBUG(Path),
    clear_cells_DEBUG([FunName | "data"]),
    clear_cells_DEBUG("http://il_ballo.dev:9000", [FunName, "data"]),

    test_util:wait(20),

    % now write out the data and perform the tests
    write_value(Path, FunName, {1, 1}, [bold, underline, center]),

    write_value(Path, "before: "++FunName, {3, 1}, [bold, underline, center]),
    colour(Path, {3, 1}, "grey"),

    % cell stuff first
    write_value(Path, "Cell "++FunName++" (down)", {1, 3},
                [bold, underline, center]),

    write_value(Path, "Links to Column A", {3, 3},
                [bold, underline, center]),

    write_value(Path, "=./data/A1", {1, 4}, []),

    %    write_value(Path, FunName++" a cell here", {1, 5}, []),
    write_value(Path, "987", {1, 6}, []),
    write_value(Path, "=./data/B1", {1, 7}, []),
    %    write_value(Path, "=hn(\"http://il_ballo.dev:9000/insert/data/C1?hypernumber\")",
    %                {1, 8}, []),
    %    colour(Path, {1, 4}, "orange"),
    %    colour(Path, {1, 5}, "orange"),
    colour(Path, {1, 6}, "orange"),
    colour(Path, {1, 7}, "orange"),
    %    colour(Path, {1, 8}, "orange"),
    %    write_value(Path, "<- was =./data/B1 now blank", {2, 7}, []),
    %    write_value(Path, "<- was hypernumber now =./data/B1", {2, 8}, []),
    %    write_value(Path, "<- also check http://il_ballo.dev:9000/a/path/A1 which pointed here", {3, 7}, []),
    %    write_value(Path, "<- was blank now hypernumber", {2, 9}, []),

    write_value(Path, "=A6", {3, 5}, []),
    colour(Path, {3, 5}, "yellow"),

    %    write_value("http://il_ballo.dev:9000",["a","path"],
    %                "=hn(\"http://127.0.0.1:9000/"++FunName++"/A6?hypernumber\")",
    %                {1, 1}, []),

    %    io:format("Insert a cell...~n"),

    % ensure that the dirty tables are loaded to make this test work...
    gen_server:cast(dirty_cell,             {setstate, passive}),
    gen_server:cast(dirty_notify_in,      {setstate, passive}),
    gen_server:cast(dirty_notify_out,      {setstate, passive}),
    gen_server:cast(dirty_notify_back_in,  {setstate, passive}),
    io:format("~n********Stopping subscriptions to mnesia************~n"),

    % Now set up the various bits of data that is used to mark the 
    % insert tests
    io:format("writing out data...~n"),
    write_data([FunName,"data"]),
    %    write_data("http://il_ballo.dev:9000",[FunName,"data"]),

    insert_delete(FunName, Path, {cell, {1, 5}}, vertical),

    io:format("~n********Restarting subscriptions to mnesia**********~n"),
    _Return1=gen_server:cast(dirty_cell,            {setstate, active}),
    _Return2=gen_server:call(dirty_cell,            flush, infinity),
    _Return3=gen_server:cast(dirty_notify_in,       {setstate, active}),
    _Return4=gen_server:call(dirty_notify_in,       flush, infinity),
    _Return5=gen_server:cast(dirty_notify_out,      {setstate, active}),
    _Return6=gen_server:call(dirty_notify_out,      flush, infinity),
    _Return7=gen_server:cast(dirty_notify_back_in,  {setstate, active}),
    _Return8=gen_server:call(dirty_notify_incoming, flush, infinity),

    make_thick(Path, 1),
    make_thick(Path, 2),
    make_thick(Path, 3),

    make_high(Path, 6),
    make_high(Path, 7),
    make_high(Path, 8),

    %    write_value(Path, "Row "++FunName++" (right)", {1, 12}, 
    %    [bold, underline, center]),
    %    write_value(Path, FunName++" a row here", {1, 13}, []),
    %    write_value(Path, "=./data/A1", {1, 14}, []),
    %    colour(Path, {1, 13}, "orange"),
    %    colour(Path, {1, 14}, "orange"),

    %    make_thin(Path, 2),

    %    write_value(Path, FunName++" Column Here", {3, 3}, 
    %    [bold, underline, center]),

    %    make_thick(Path, 3),

    %    make_thin(Path, 4),

    %    make_thick(Path, 5),

    %    write_value(Path, "Range "++FunName++" (down)", {5, 14}, 
    %    [bold, underline, center]),
    %    write_value(Path, "=./data/A1", {5, 15}, []),
    %    write_value(Path, "=./data/B1", {5, 16}, []),
    %    write_value(Path, "=./data/C1", {5, 17}, []),
    %    write_value(Path, "=./data/D1", {5, 18}, []),
    %    colour(Path, {5, 15}, "orange"),
    %    colour(Path, {5, 16}, "orange"),
    %    colour(Path, {5, 17}, "orange"),
    %    colour(Path, {5, 18}, "orange"),

    %    write_value(Path, "=./data/E1", {6, 15}, []),
    %    write_value(Path, "=./data/F1", {6, 16}, []),
    %    write_value(Path, "=./data/G1", {6, 17}, []),
    %    write_value(Path, "=./data/H1", {6, 18}, []),
    %    colour(Path, {6, 15}, "orange"),
    %    colour(Path, {6, 16}, "yellow"),
    %    colour(Path, {6, 17}, "yellow"),
    %    colour(Path, {6, 18}, "orange"),

    %    write_value(Path, "=./data/I1", {7, 15}, []),
    %    write_value(Path, "=./data/J1", {7, 16}, []),
    %    write_value(Path, "=./data/K1", {7, 17}, []),
    %    write_value(Path, "=./data/L1", {7, 18}, []),
    %    colour(Path, {7, 15}, "orange"),
    %    colour(Path, {7, 16}, "yellow"),
    %    colour(Path, {7, 17}, "yellow"),
    %    colour(Path, {7, 18}, "orange"),

    %    write_value(Path, "=./data/M1", {8, 15}, []),
    %    write_value(Path, "=./data/N1", {8, 16}, []),
    %    write_value(Path, "=./data/O1", {8, 17}, []),
    %    write_value(Path, "=./data/P1", {8, 18}, []),
    %    colour(Path, {8, 15}, "orange"),
    %    colour(Path, {8, 16}, "orange"),
    %    colour(Path, {8, 17}, "orange"),
    %    colour(Path, {8, 18}, "orange"),

    %    make_thin(Path, 9),

    %    write_value(Path, "Range "++FunName++" (right)", {10, 14}, 
    %    [bold, underline, center]),
    %    write_value(Path, "A", {10, 15}, []),
    %    write_value(Path, "B", {10, 16}, []),
    %    write_value(Path, "C", {10, 17}, []),
    %    write_value(Path, "D", {10, 18}, []),
    %    colour(Path, {10, 15}, "orange"),
    %    colour(Path, {10, 16}, "orange"),
    %    colour(Path, {10, 17}, "orange"),
    %    colour(Path, {10, 18}, "orange"),

    %    write_value(Path, "E", {11, 15}, []),
    %    write_value(Path, "F", {11, 16}, []),
    %    write_value(Path, "G", {11, 17}, []),
    %    write_value(Path, "H", {11, 18}, []),
    %    colour(Path, {11, 15}, "orange"),
    %    colour(Path, {11, 16}, "yellow"),
    %    colour(Path, {11, 17}, "yellow"),
    %    colour(Path, {11, 18}, "orange"),

    %    write_value(Path, "I", {12, 15}, []),
    %    write_value(Path, "J", {12, 16}, []),
    %    write_value(Path, "K", {12, 17}, []),
    %    write_value(Path, "L", {12, 18}, []),
    %    colour(Path, {12, 15}, "orange"),
    %    colour(Path, {12, 16}, "yellow"),
    %    colour(Path, {12, 17}, "yellow"),
    %    colour(Path, {12, 18}, "orange"),

    %    write_value(Path, "M", {13, 15}, []),
    %    write_value(Path, "N", {13, 16}, []),
    %    write_value(Path, "O", {13, 17}, []),
    %    write_value(Path, "P", {13, 18}, []),
    %    colour(Path, {13, 15}, "orange"),
    %    colour(Path, {13, 16}, "orange"),
    %    colour(Path, {13, 17}, "orange"),
    %    colour(Path, {13, 18}, "orange"),

    %    make_thick(Path, 10),

    % Now do the inserts and deletes

    %    io:format("'bout to wait...~n"),
    %    test_util:wait(25),
    %    io:format("'done waitin...~n"),
    %    write_value(Path, "after: "++FunName, {3, 1}, [bold, underline, center]),
    %    colour(Path, {3, 1}, "red"),

    %    io:format("At the end...~n"),

    %    insert_delete(FunName, Path, {cell, {1, 5}}, vertical),
    % insert_delete(FunName, Path, {cell, {1, 5}}, horizontal),
    % insert_delete(FunName, Path, {row, {12, 12}}),
    % insert_delete(FunName, Path, {column, {3, 3}}),
    % insert_delete(FunName, Path, {range, {6, 16, 7, 18}}, vertical),
    % insert_delete(FunName, Path, {range, {11, 16, 13, 18}}, vertical),

    ok.

%% @hidden
clear_TEST() ->
    % Site = "http://127.0.0.1:9000",
    Path = ["test"],

    write_data("http://il_ballo.dev:9000",["data"]),
    write_data(["data2"]),

    write_value(Path, "integer below", {1, 2}, [bold]),
    write_value(Path, "1", {1, 3}, [{colour, "yellow"}]),

    write_value(Path, "local ref below", {1, 5}, [bold]),
    write_value(Path, "=../data2/b1", {1, 6}, [{colour, "yellow"}]),

    write_value(Path, "remote ref below", {1, 8}, [bold]),
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/data/E1?hypernumber\")",
                {1, 9}, [{colour, "yellow"}]),

    make_thick(Path, 1),

    test_util:wait(25),

    % rewrite the same formula
    write_value(Path, "=hn(\"http://il_ballo.dev:9000/data/E1?hypernumber\")",
                {1, 9}, [{colour, "yellow"}]),

    % clear the remote hyperlink
    % clear_cells(Site, Path, {1, 9}),

    % clear_cells_DEBUG(Site, Path),
    ok.

%% @hidden
delete_DEBUG(Ref, Type) -> delete(Ref, Type).

%% @hidden
insert_DEBUG(Ref, Type) -> insert(Ref, Type).

%% @hidden
copy_DEBUG() ->
    io:format("in copy_DEBUG going into drag'n'drop (2)~n"),
    copy_DEBUG2("drag_n_drop"),
    io:format("in copy_DEBUG going into copy'n'paste (2)~n"),
    copy_DEBUG2("copy_n_paste"),
    io:format("in copy_DEBUG going into cut'n'paste (2)~n"),
    copy_DEBUG2("cut_n_paste"),
    io:format("in copy_DEBUG going into copy'n'paste (3)~n"),
    copy_DEBUG3("copy_n_paste"),
    io:format("in copy_DEBUG going into cut'n'paste (3)~n"),
    copy_DEBUG3("cut_n_paste").

%% @hidden
delete_cell_contents_DEBUG(Site, Path) ->
    Target = #refX{site = Site, path = Path, obj = {range, {1, 1, 30, 30}}},
    clear(Target, contents).

%% @hidden
delete_cell_contents_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    io:format("In delete_cell_contents_DEBUG Path is ~p~n", [Path]),
    delete_cell_contents_DEBUG(Site, Path).

%% @hidden
clear_cells_DEBUG(Site, Path) ->
    Target = #refX{site = Site, path = Path, obj = {range, {1, 1, 30, 30}}},
    clear(Target, all).

%% @hidden
clear_cells_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    clear_cells_DEBUG(Site, Path).

%% @hidden
copy_DEBUG3(FunName) ->

    Path = [FunName, "for_ranges"],

    clear_cells_DEBUG(Path),

    test_util:wait(25),

    write_value(Path, FunName++" - ranges", {1, 1}, [bold, underline]),

    make_high(Path, 1),

    write_value(Path, "From Range", {1, 2}, [bold]),
    write_value(Path, "a", {1, 3}, [{colour, "yellow"}]),
    write_value(Path, "b", {1, 4}, [{colour, "yellow"}]),
    write_value(Path, "c", {1, 5}, [{colour, "yellow"}]),
    write_value(Path, "d", {2, 3}, [{colour, "yellow"}]),
    write_value(Path, "e", {2, 4}, [{colour, "yellow"}]),
    write_value(Path, "f", {2, 5}, [{colour, "yellow"}]),
    write_value(Path, "g", {3, 3}, [{colour, "yellow"}]),
    write_value(Path, "h", {3, 4}, [{colour, "yellow"}]),
    write_value(Path, "i", {3, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Range (the same size)", {1, 7}, [bold]),
    make_high(Path, 7),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {1, 3, 3, 5}},
                                     {range, {1, 8, 3, 10}}),

    write_value(Path, "From", {5, 2}, [bold]),

    write_value(Path, "aa", {5, 3}, [{colour, "yellow"}]),
    write_value(Path, "bb", {5, 4}, [{colour, "yellow"}]),
    write_value(Path, "cc", {5, 5}, [{colour, "yellow"}]),
    write_value(Path, "dd", {6, 3}, [{colour, "yellow"}]),
    write_value(Path, "ee", {6, 4}, [{colour, "yellow"}]),
    write_value(Path, "ff", {6, 5}, [{colour, "yellow"}]),
    write_value(Path, "gg", {7, 3}, [{colour, "yellow"}]),
    write_value(Path, "hh", {7, 4}, [{colour, "yellow"}]),
    write_value(Path, "ii", {7, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Range (smaller)", {5, 7}, [bold]),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {5, 3, 7, 5}},
                                     {range, {5, 8, 6, 9}}),

    write_value(Path, "From", {9, 2}, [bold]),

    write_value(Path, "aaa", {9, 3}, [{colour, "yellow"}]),
    write_value(Path, "bbb", {9, 4}, [{colour, "yellow"}]),
    write_value(Path, "ccc", {9, 5}, [{colour, "yellow"}]),
    write_value(Path, "ddd", {10, 3}, [{colour, "yellow"}]),
    write_value(Path, "eee", {10, 4}, [{colour, "yellow"}]),
    write_value(Path, "fff", {10, 5}, [{colour, "yellow"}]),
    write_value(Path, "ggg", {11, 3}, [{colour, "yellow"}]),
    write_value(Path, "hhh", {11, 4}, [{colour, "yellow"}]),
    write_value(Path, "iii", {11, 5}, [{colour, "yellow"}]),

    write_value(Path, "To Cell", {9, 7}, [bold]),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {9, 3, 12, 5}},
                                     {cell, {9, 8}}),

    write_value(Path, "From Range", {1, 12}, [bold]),
    write_value(Path, "aaaa", {1, 13}, [{colour, "yellow"}]),
    write_value(Path, "bbbb", {1, 14}, [{colour, "yellow"}]),
    write_value(Path, "cccc", {2, 13}, [{colour, "yellow"}]),
    write_value(Path, "dddd", {2, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Vertical Tiles", {1, 16}, [bold]),
    make_high(Path, 16),

    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {1,13, 2, 14}},
                                     {range, {1, 17, 2, 22}}),

    write_value(Path, "From Range", {4, 12}, [bold]),
    write_value(Path, "A", {4, 13}, [{colour, "yellow"}]),
    write_value(Path, "B", {4, 14}, [{colour, "yellow"}]),
    write_value(Path, "C", {5, 13}, [{colour, "yellow"}]),
    write_value(Path, "D", {5, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Horizontal Tiles", {4, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {4, 13, 5, 14}},
                                     {range, {4, 17, 7, 18}}),

    write_value(Path, "From Range", {9, 12}, [bold]),
    write_value(Path, "AA", {9, 13}, [{colour, "yellow"}]),
    write_value(Path, "BB", {9, 14}, [{colour, "yellow"}]),
    write_value(Path, "CC", {10, 13}, [{colour, "yellow"}]),
    write_value(Path, "DD", {10, 14}, [{colour, "yellow"}]),

    write_value(Path, "To Tiles", {9, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {9, 13, 10, 14}},
                                     {range, {9, 17, 12, 20}}),

    write_value(Path, "From Range", {14, 12}, [bold]),
    write_value(Path, "AAA", {14, 13}, [{colour, "yellow"}]),
    write_value(Path, "BBB", {14, 14}, [{colour, "yellow"}]),
    write_value(Path, "CCC", {15, 13}, [{colour, "yellow"}]),
    write_value(Path, "DDD", {15, 14}, [{colour, "yellow"}]),

    write_value(Path, "Non-Tiling Range", {14, 16}, [bold]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {range, {14, 13, 15, 14}},
                                     {range, {14, 17, 16, 16}}).

%% @hidden
copy_DEBUG2(FunName) ->

    Path = [FunName],

    clear_cells_DEBUG(Path),

    test_util:wait(25),

    write_value(Path, FunName++" - cell to cell", {1, 1}, [bold, underline]),

    % cell to cell drop down
    write_value(Path, "integer below", {1, 2}, [bold]),
    write_value(Path, "1", {1, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 3}},
                                     {cell, {1, 4}}),
    colour(Path, {1, 3}, "cyan"),

    write_value(Path, "float below", {1, 5}, [bold]),
    write_value(Path, "1.1", {1, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 6}},
                                     {cell, {1, 7}}),
    colour(Path, {1, 6}, "cyan"),

    write_value(Path, "string below", {1, 8}, [bold]),
    write_value(Path, "hey!", {1, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 9}},
                                     {cell, {1, 10}}),
    colour(Path, {1, 9}, "cyan"),

    write_value(Path, "date below", {1, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {1, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 12}},
                                     {cell, {1, 13}}),
    colour(Path, {1, 12}, "cyan"),

    write_value(Path, "boolean below", {1, 14}, [bold]),
    write_value(Path, "true", {1, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {1, 15}},
                                     {cell, {1, 16}}),
    colour(Path, {1, 15}, "cyan"),

    make_thick(Path, 1),
    make_thick(Path, 2),
    make_high(Path, 1),

    % cell to cell across
    write_value(Path, "integer beside", {2, 2}, [bold]),
    write_value(Path, "1", {2, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 3}},
                                     {cell, {3, 3}}),
    colour(Path, {2, 3}, "cyan"),

    write_value(Path, "float beside", {2, 5}, [bold]),
    write_value(Path, "1.1", {2, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 6}},
                                     {cell, {3, 6}}),
    colour(Path, {2, 6}, "cyan"),

    write_value(Path, "string beside", {2, 8}, [bold]),
    write_value(Path, "hey!", {2, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 9}},
                                     {cell, {3, 9}}),
    colour(Path, {2, 9}, "cyan"),

    write_value(Path, "date beside", {2, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {2, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 12}},
                                     {cell, {3, 12}}),
    colour(Path, {2, 12}, "cyan"),

    write_value(Path, "boolean beside", {2, 14}, [bold]),
    write_value(Path, "true", {2, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {2, 15}},
                                     {cell, {3, 15}}),
    colour(Path, {2, 15}, "cyan"),

    make_thin(Path, 4),

    write_value(Path, FunName++" - cell to down 'thin' range", {5, 1},
                [bold, underline]),

    % cell to range down
    write_value(Path, "integer below", {5, 2}, [bold]),
    write_value(Path, "1", {5, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 3}},
                                     {range, {5, 4, 5, 5}}),
    colour(Path, {5, 3}, "cyan"),

    write_value(Path, "float below", {6, 5}, [bold]),
    write_value(Path, "1.1", {6, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {6, 6}},
                                     {range, {6, 7, 6, 8}}),
    colour(Path, {6, 6}, "cyan"),

    write_value(Path, "string below", {5, 8}, [bold]),
    write_value(Path, "hey!", {5, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 9}},
                                     {range, {5, 10, 5, 11}}),
    colour(Path, {5, 9}, "cyan"),

    write_value(Path, "date below", {6, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {6, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {6, 12}},
                                     {range, {6, 13, 6, 14}}),
    colour(Path, {6, 12}, "cyan"),

    write_value(Path, "boolean below", {5, 14}, [bold]),
    write_value(Path, "true", {5, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {5, 15}},
                                     {range, {5, 16, 5, 17}}),
    colour(Path, {5, 15}, "cyan"),

    write_value(Path, FunName++" - cell to across 'thin' range", {7, 1},
                [bold, underline]),

    make_thick(Path, 5),
    make_thick(Path, 6),

    % cell to range down
    write_value(Path, "integer beside", {7, 2}, [bold]),
    write_value(Path, "1", {7, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 3}},
                                     {range, {7, 4, 8, 4}}),
    colour(Path, {7, 3}, "cyan"),

    write_value(Path, "float beside", {7, 5}, [bold]),
    write_value(Path, "1.1", {7, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 6}},
                                     {range, {7, 7, 8, 7}}),
    colour(Path, {7, 6}, "cyan"),

    write_value(Path, "string beside", {7, 8}, [bold]),
    write_value(Path, "hey!", {7, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 9}},
                                     {range, {7, 10, 8, 10}}),
    colour(Path, {7, 9}, "cyan"),

    write_value(Path, "date beside", {7, 11}, [bold]),
    write_value(Path, "1/2/3 4:5:6", {7, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 12}},
                                     {range, {7, 13, 8, 13}}),
    colour(Path, {7, 12}, "cyan"),

    write_value(Path, "boolean beside", {7, 14}, [bold]),
    write_value(Path, "true", {7, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {7, 15}},
                                     {range, {7, 16,  8, 16}}),
    colour(Path, {7, 15}, "cyan"),

    make_thick(Path, 7),
    make_thick(Path, 8),

    make_thin(Path, 9),

    % cell to 'thick' ranges don't increment even if they are drag'n'drop
    write_value(Path, FunName++" - cell to 'thick' range", {10,1},
                [bold, underline]),

    write_value(Path, "integer", {10,2}, [bold]),
    write_value(Path, "1", {10,3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {10,3}},
                                     {range, {10,4, 11, 10}}),
    colour(Path, {10,3}, "cyan"),

    % same as above but arsey backwards range
    write_value(Path, "testing inverted range", {10,11}, [bold]),
    write_value(Path, "1", {10,12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {10,12}},
                                     {range, {10, 20, 11, 13}}),
    colour(Path, {10,3}, "cyan"),

    make_thick(Path, 10),

    make_thin(Path, 12),

    % set up formula data
    write_value(Path, "data for formula", {13,2}, [bold]),
    colour(Path, {13, 2}, "yellow"),
    write_value(Path, "1", {13, 3}, []),
    write_value(Path, "22", {13, 4}, []),
    write_value(Path, "333", {13, 5}, []),
    write_value(Path, "4444", {13, 6}, []),
    write_value(Path, "5555", {13, 7}, []),
    write_value(Path, "11111", {14, 3}, []),
    write_value(Path, "222222", {14, 4}, []),
    write_value(Path, "333333", {14, 5}, []),
    write_value(Path, "4444444", {14, 6}, []),
    write_value(Path, "55555555", {14, 7}, []),
    colour(Path, {13, 3}, "orange"),
    colour(Path, {13, 4}, "orange"),
    colour(Path, {13, 5}, "orange"),
    colour(Path, {13, 6}, "orange"),
    colour(Path, {13, 7}, "orange"),
    colour(Path, {14, 3}, "orange"),
    colour(Path, {14, 4}, "orange"),
    colour(Path, {14, 5}, "orange"),
    colour(Path, {14, 6}, "orange"),
    colour(Path, {14, 7}, "orange"),

    make_thick(Path, 13),

    make_thin(Path, 15),

    % some formula stuff
    write_value(Path, "formula below", {16, 2}, [bold]),
    write_value(Path, "=m3+n3", {16, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 3}},
                                     {range, {16, 4, 17, 6}}),
    colour(Path, {16, 3}, "cyan"),

    write_value(Path, "fix col formula below", {16, 7}, [bold]),
    write_value(Path, "=$m3+n3", {16, 8}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 8}},
                                     {range, {16, 9, 17, 11}}),
    colour(Path, {16, 8}, "cyan"),

    write_value(Path, "fix row formula below", {16, 12}, [bold]),
    write_value(Path, "=m$3+n3", {16, 13}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 13}},
                                     {range, {16, 14, 17, 16}}),
    colour(Path, {16, 13}, "cyan"),

    write_value(Path, "fix row and col formula below", {16,17}, [bold]),
    write_value(Path, "=$m$3+n3", {16, 18}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, Path, {cell, {16, 18}},
                                     {range, {16, 19, 17, 21}}),
    colour(Path, {16, 18}, "cyan"),

    make_thick(Path, 16).

%% @hidden
hn_DEBUG() ->
    Path = ["hypernumbers"],
    Site1 = "http://il_ballo.dev:9000",
    Path1 = ["data"],
    clear_cells_DEBUG(Path),
    clear_cells_DEBUG(Site1, Path1),
    write_data(Site1, Path1),

    write_value(Path,"=hn(\"http://il_ballo.dev:9000/data/A1?hypernumber\")",
                {1, 1}, []).

%% % @hidden
%get_tiles_DEBUG() ->
%    F1 = #refX{obj = {range, {1, 1, 2, 2}}},
%    F2 = #refX{obj = {range, {1, 1, 3, 3}}},
%    T1 = #refX{obj = {range, {1, 1, 3, 3}}},
%    T2 = #refX{obj = {range, {1, 1, 6, 6}}},
%    T3 = #refX{obj = {range, {1, 1, 3, 6}}},
%    T4 = #refX{obj = {range, {1, 1, 6, 3}}},

%    T5 = #refX{obj = {range, {1, 1, 1, 3}}},
%    T6 = #refX{obj = {range, {1, 3, 3, 3}}},
%    T7 = #refX{obj = {range, {2, 3, 4, 7}}},
%    T8 = #refX{obj = {range, {1, 1, 1, 1}}},

%    get_tiles(F1, T8),
%    io:format("**********************************~n"),
%    get_tiles(F2, T8),
%    io:format("**********************************~n"),
%    get_tiles(F2, T1),
%    io:format("**********************************~n"),   
%    get_tiles(F2, T2),
%    io:format("**********************************~n"),   
%    get_tiles(F2, T3),
%    io:format("**********************************~n"),   
%    get_tiles(F2, T4),  
%    io:format("**********************************~n"),   
%    get_tiles(F2, T7).    

%% @hidden
read_styles_DEBUG() ->
    Site = "http://blah.com",
    Path = ["some", "path"],
    write_value(Site, Path, "1", {1, 1}, []),
    write_value(Site, Path, "bob", {1, 2}, []),
    write_value(Site, Path, "true", {2, 1}, []),
    write_value(Site, Path, "1/1/09", {2, 2}, []),

    RefX1 = #refX{site = Site, path = Path, obj = {cell, {1, 1}}},
    RefX2 = #refX{site = Site, path = Path, obj = {cell, {1, 2}}},
    RefX3 = #refX{site = Site, path = Path, obj = {cell, {2, 1}}},
    RefX4 = #refX{site = Site, path = Path, obj = {cell, {2, 2}}},

    RefX5 = #refX{site = Site, path = Path, obj = {column, {2, 2}}},
    RefX6 = #refX{site = Site, path = Path, obj = {row, {2, 2}}},
    RefX7 = #refX{site = Site, path = Path, obj = {range,{1, 1, 2, 2}}},
    RefX8 = #refX{site = Site, path = Path, obj = {page, "/"}},

    List = [RefX1, RefX2, RefX3, RefX4, RefX5, RefX6, RefX7, RefX8],
    [io:format("read_styles returns ~p~n", [read_styles_DEBUG2(X)]) || X <- List].

%% @hidden
read_styles_DEBUG2(X) ->
    io:format("reading styles for ~p~n", [X]),
    Fun = fun() ->
                  hn_db_wu:read_styles(X)
          end,
    mnesia:activity(transaction, Fun).

insert_delete(Fun, Path, Target) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj = Target},
    erlang:apply(hn_db_api, list_to_atom(Fun), [Ref]).

insert_delete(Fun, Path, Target, Type) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj= Target},
    erlang:apply(hn_db_api, list_to_atom(Fun), [Ref, Type]).

cut_n_drag_n_copy_n_drop_n_paste(Fun, Path, From, To) ->
    Site = "http://127.0.0.1:9000",
    From1 = #refX{site =  Site, path = Path, obj = From},
    To1   = #refX{site =  Site, path = Path, obj = To},
    erlang:apply(hn_db_api, list_to_atom(Fun), [From1, To1]).

%% choose the site to write to
write_value(Site, Path, Value, {X, Y}, Attributes) ->
    RefX = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    write_attributes(RefX, [{"formula", Value}]),
    write_attr_DEBUG(RefX, Attributes).

%% just write to the default
write_value(Path, Value, {X, Y}, Attributes) ->
    Site = "http://127.0.0.1:9000",
    write_value(Site, Path, Value, {X, Y}, Attributes).

write_attr_DEBUG(_RefX, []) -> ok;
write_attr_DEBUG(RefX, [Attr | T]) ->
    Attr2 = case Attr of
                bold              -> {"font-weight", "bold"};
                underline         -> {"text-decoration", "underline"};
                center            -> {"font-align", "center"};
                {colour, Colour}  -> {"background-color", Colour};
                thin              -> {"width", 30}; 
                thick             -> {"width", 200}
            end,
    write_attributes(RefX, [Attr2]),
    write_attr_DEBUG(RefX, T).

colour(Path, {X, Y}, Colour) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    Val = {"background-color", Colour},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_high(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {row, {X, X}}},
    Val = {height, 30},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_thin(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {column, {X, X}}},
    Val = {"width", 30},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

make_thick(Path, X) ->
    Site = "http://127.0.0.1:9000",
    RefX = #refX{site = Site, path = Path, obj = {column, {X, X}}},
    Val = {"width", 200},
    Fun = fun() ->
                  hn_db_wu:write_attr(RefX, Val)
          end,
    mnesia:activity(transaction, Fun).

write_data(Path) ->
    Site = "http://127.0.0.1:9000",
    write_data(Site, Path).

write_data(Site, Path) ->
    clear_cells_DEBUG(Site, Path),
    % writes out sample data for hyperlinks
    write_value(Site, Path, "1", {1, 1}, []),
    write_value(Site, Path, "2", {2, 1}, []),
    write_value(Site, Path, "3", {3, 1}, []).
%    write_value(Site, Path, "4", {4, 1}, []),
%    write_value(Site, Path, "5", {5, 1}, []),
%    write_value(Site, Path, "6", {6, 1}, []),
%    write_value(Site, Path, "7", {7, 1}, []),
%    write_value(Site, Path, "8", {8, 1}, []),
%    write_value(Site, Path, "9", {9, 1}, []).

clear_cells(Site, Path, {X, Y}) ->
    Target = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    clear(Target, all).    
