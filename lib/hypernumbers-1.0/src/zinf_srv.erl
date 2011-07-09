%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers.com
%%% @doc       The zinf server handles z-order and infinite
%%%            dependencies
%%% @end
%%% Created :  18 Jan 2011 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(zinf_srv).

-behaviour(gen_server).

-include("spriki.hrl").
-include("keyvalues.hrl").
-include("errvals.hrl").
-include("syslib.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(maxqueuesize, 250).
-define(E, error_logger:error_msg).
-define(sq_bra, 91).
-define(sq_ket, 93).

-compile(export_all).

%% API
-export([start_link/1]).

%% Programatic API
-export([
         process_zinfs/1,
         check_zinfs/1,
         dump/1
        ]).

%% API for fns used in new_db_api
-export([
         add/2,
         del/2,
         expand_zrefs/1
        ]).

-export([
         test_dump/0
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          site,
          zinf_tree
         }).

-record(selector,
        {
          objdict = orddict:new()
         }).

%%%===================================================================
%%% API
%%%===================================================================

process_zinfs(Site) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:cast({global, Id}, process_zinfs).

check_zinfs(Site) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:cast({global, Id}, check_zinfs).

dump(Site) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:cast({global, Id}, {dump, Site}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:start_link({local, Id}, ?MODULE, [Site], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Site]) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    global:re_register_name(Id, self()),
    [{kvstore, ?zinf_tree, Zinf_tree}] = new_db_api:read_kv(Site, ?zinf_tree),
    % the zinf tree is intermittently written to the db
    % there SHOULD BE no processed records in table dirty_zinf
    % if there are any then it is beause the zinf server terminated abnormally
    % SOOO, go to that table and mark all processed records as unprocessed
    % and then tell this server to process zinfs...
    Tbl = new_db_wu:trans(Site, dirty_zinf),
    Size = mnesia:table_info(Tbl, size),
    if
        Size == 0 -> ok;
        Size >  0 -> io:format("zinf_srv resetting ~p records in dirty_zinf~n",
                               [Size]),
                     ok = new_db_api:reset_dirty_zinfs(Site),
                     ok = process_zinfs(Site)
    end,
    {ok, #state{site = Site, zinf_tree = Zinf_tree}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    #state{site = S, zinf_tree = Tree} = State,
    {Act, NewT} =
        case Msg of
            process_zinfs -> {write, process_zs(S, Tree)};
            check_zinfs   -> {nothing, check_zs(S, Tree)};
            {dump, Site}  -> dump(Tree, Site),
                             {nothing, Tree}
        end,
    case Act of
        write -> new_db_api:maybe_write_zinftree(S, NewT, ?maxqueuesize);
        _     -> ok
    end,
    {noreply, State#state{zinf_tree = NewT}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_zs(Site, Tree) ->
    new_db_api:process_dirty_zinfs(Site, Tree, fun add/2, fun del/2).

check_zs(Site, Tree) ->
    ok = new_db_api:process_dirties_for_zinf(Site, Tree, fun check/2),
    Tree.

add({XRefX, Idx}, Tree) ->
    #xrefX{path = P, obj = Obj} = XRefX,
    alter_tree(Tree, hn_util:parse_zpath(P), add_selector(Idx, Obj)).

del({XRefX, Idx}, Tree) ->
    #xrefX{path = P, obj = Obj} = XRefX,
    alter_tree(Tree, hn_util:parse_zpath(P), delete_selector(Idx, Obj)).

check(Tree, #xrefX{site = S, path = P} = XRefX) ->
    match_tree(Tree, S, P, match(XRefX), []).

dump(Tree, Site) ->
    io:format("Dumping Zinf tree for ~p~n", [Site]),
    ok = dump_tree(Tree, []).

%%%===================================================================
%%% Funs to be applied to the tree
%%%===================================================================
delete_selector(Idx, Obj) ->
    fun(#selector{objdict = OD}) ->
            V = orddict:fetch(Obj, OD),
            Ret = case lists:delete(Idx, V) of
                      [] -> orddict:erase(Obj, OD);
                      N  -> orddict:store(Obj, N, OD)
                  end,
            Status = case Ret of
                         [] -> {deleted, selector};
                         _  -> removed
                     end,
            {Status, #selector{objdict = Ret}}
    end.

add_selector(Idx, Obj) ->
    fun(#selector{objdict = OD}) ->
            NewOD = case orddict:is_key(Obj, OD) of
                        false -> orddict:append(Obj, Idx, OD);
                        true  -> V = orddict:fetch(Obj, OD),
                                 NewV = case lists:member(Idx, V) of
                                            true  -> V;
                                            false -> [Idx | V]
                                        end,
                                 orddict:store(Obj, NewV, OD)
                    end,
            {added, #selector{objdict = NewOD}}
    end.

match(#xrefX{obj = {cell, {X, Y}}}) ->
    fun({selector, List}) ->
            match_1(List, X, Y, [])
    end.

% old and new rows and cols
match_1([], _X, _Y, Acc) -> Acc;
match_1([{{row, {Y1, Y2}}, Idxs} | T], X, Y, Acc)
  when Y >= Y1 andalso Y =< Y2 ->
    match_1(T, X, Y, [Idxs | Acc]);
match_1([{{column, {X1, X2}}, Idxs} | T], X, Y, Acc)
  when X >= X1 andalso X =< X2 ->
    match_1(T, X, Y, [Idxs | Acc]);
match_1([{{range, {X1, X2, Y1, Y2}}, Idxs} | T], X, Y, Acc)
  when X >= X1 andalso X =< X2 andalso Y >= Y1 andalso Y =< Y2 ->
    match_1(T, X, Y, [Idxs | Acc]);
match_1([{{cell, {X, Y}}, Idxs} | T], X, Y, Acc) ->
    match_1(T, X, Y, [Idxs | Acc]);
match_1([_H | T], X, Y, Acc) ->
    match_1(T, X, Y, Acc).

%%%===================================================================
%%% Tree manipulation functions
%%%===================================================================
-spec alter_tree(gb_tree(),
                 [string()],
                 fun((#selector{}) -> #selector{})) -> gb_tree().
alter_tree(Tree, List, Fun) ->
    case alter_tree1(Tree, List, Fun) of
        {{deleted, Seg}, NewTree} -> NewTree,
                                     gb_trees:delete(Seg, NewTree);
        {_, NewTree}              -> NewTree
    end.

alter_tree1(Tree, [], Fun) ->
    {St, Ret} = case gb_trees:lookup(selector, Tree) of
                    none       -> {Status, NewSels} = Fun(#selector{}),
                                  {Status, gb_trees:insert(selector, NewSels, Tree)};
                    {value, V} -> {St2, NewVs} = Fun(V),
                                  {St2, gb_trees:enter(selector, NewVs, Tree)}
          end,
    {St, Ret};
alter_tree1(Tree, [H | T], Fun) ->
    {St, Ret} = case gb_trees:lookup(H, Tree) of
                    none       -> Empty = gb_trees:empty(),
                                  {Status, NewVal} = alter_tree1(Empty, T, Fun),
                                  {Status, gb_trees:insert(H, NewVal, Tree)};
                    {value, V} -> {Status, NewVal} = alter_tree1(V, T, Fun),
                                  {Status, gb_trees:enter(H, NewVal, Tree)}
                end,
    {St2, Ret2} = case St of
                      {deleted, K} -> {St3, NewTree} = trim(K, H, Ret),
                                      {St3, NewTree};
                      _Other       -> {ok, Ret}
    end,
    {St2, Ret2}.

trim(K, Seg, Tree) ->
    {value, SubTree} = gb_trees:lookup(Seg, Tree),
    NewSubTree = gb_trees:delete(K, SubTree),
    Status = case gb_trees:size(NewSubTree) of
                 0  -> {deleted, Seg};
                 _N -> ok
             end,
    {Status, gb_trees:enter(Seg, NewSubTree, Tree)}.

dump_tree(Tree, Acc) -> Iter = gb_trees:iterator(Tree),
                        dump_t2(Iter, Acc).

dump_t2([], _Htap)  -> ok;
dump_t2(Iter, Htap) ->
    case gb_trees:next(Iter) of
        none                -> ok;
        {selector, Sel, I2} -> {selector, List} = Sel,
                               dump_p(List, lists:reverse(Htap)),
                               dump_t2(I2, Htap);
        {{_, K}, V, I2}     -> dump_tree(V, [K | Htap]),
                               dump_t2(I2, Htap)
    end.

dump_p([], _Path)     -> ok;
dump_p([H | T], Path) -> {_, List} = H,
                         io:format("at ~p Idx's are ~p~n", [Path, List]),
                         dump_p(T, Path).

match_tree(Tree, S, List, Fun, Htap) ->
    iterate(gb_trees:iterator(Tree), S, List, Fun, Htap, []).

iterate(Iter, S, [], Fun, Htap, Acc) ->
    case gb_trees:next(Iter) of
        none                -> [];
        {selector, Sel, []} -> Fun(Sel);
        {_K, _V, I2}        -> iterate(I2, S, [], Fun, Htap, Acc)
    end;
iterate(Iter, S, [H | T] = List, Fun, Htap, Acc) ->
    case gb_trees:next(Iter) of
         none       -> lists:flatten(Acc);
         {K, V, I2} ->
            NewAcc = case match_seg(K, H, S, Htap) of
                         match    -> match_tree(V, S, T, Fun, [H | Htap]);
                         nomatch  -> [];
                         error    -> []
                     end,
            iterate(I2, S, List, Fun, Htap, [NewAcc | Acc])
     end.

match_seg({seg, S},     S,  _Site, _Htap) -> match;
match_seg({seg, _S1}, _S2,  _Site, _Htap) -> nomatch;
match_seg(selector,    _S,  _Site, _Htap) -> nomatch;
match_seg({zseg, S1},   S,   Site,  Htap) ->
    Path = lists:reverse([S | Htap]),
    case run_zeval(Site, Path, S1) of
        match          -> match;
        nomatch        -> nomatch;
        {errval, _Err} -> error;
        {error, _}     -> error   % Old style errors from fns (shouldn't exist!)
    end.

run_zeval(Site, Path, Z) ->
    Z2 = string:strip(string:strip(Z, right, ?sq_ket), left, ?sq_bra),
    % this expression is not 'real' so we run it in a non-existent cell
    % {cell, {0, 0}} is 1 up and 1 left of the cell 'A1'
    {ok, Toks} = xfl_lexer:lex(Z2, {0, 0}),
    % need to set up the process dictionary
    Fun = fun() -> try
                       muin:zeval_from_zinf(Site, Path, Toks)
                   catch
                       error:
                       Err  -> ?E("Zseg ~p on ~p and ~p failed: ~p~n",
                                  [Toks, Site, Path, Err]),
                               ?ERRVAL_VAL;
                       exit:
                       Exit -> ?E("Zseg ~p on ~p and ~p failed: ~p~n",
                                  [Toks, Site, Path, Exit]),
                               ?ERRVAL_VAL
                   end
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

expand_zrefs(#xrefX{path = P, site = S, obj = O}) ->
    ZSegs = hn_util:parse_zpath(P),
    % we need to create #xrefX{}s for each expanded zref in the path
    % so lets pass in a #refX{} to make it cleaner down the line
    RefX = #refX{site = S, path = P, obj = O},
    expandp(ZSegs, RefX, [], []).

expandp([], _, _, Acc) ->
    hslists:uniq(lists:merge(Acc));
expandp([{seg, S} | T], X, Htap, Acc) ->
    expandp(T, X, [S | Htap], Acc);
expandp([{zseg, Z} | T], X, Htap, Acc) ->
    {ok, Toks} = xfl_lexer:lex(Z, {0, 0}),
    NewAcc = walk(Toks, X, ["[true]" | Htap], []),
    expandp(T, X, [Z | Htap], [NewAcc | Acc]).

walk([], _, _, Acc) -> Acc;
walk([{cellref, _, {cellref, _, _, Path, Ref}} | T], RefX, Htap, Acc) ->
    NewPath = muin_util:walk_path(lists:reverse(Htap), Path),
    Ref2 = hd(lists:reverse(string:tokens(Ref, "/"))),
    Obj = hn_util:parse_ref(Ref2),
    RefX2 = RefX#refX{path = NewPath, obj = Obj},
    [XRefX] = new_db_wu:refXs_to_xrefXs_create([RefX2]),
    walk(T, RefX, Htap, [XRefX | Acc]);
walk([{rangeref, _, {rangeref, _, Path, _, _, _, _, Ref}} | T],
     RefX, Htap, Acc) ->
    NewPath = muin_util:walk_path(lists:reverse(Htap), Path),
    Ref2 = hd(lists:reverse(string:tokens(Ref, "/"))),
    Obj = hn_util:parse_ref(Ref2),
    RefX2 = RefX#refX{path = NewPath, obj = Obj},
    [XRefX] = new_db_wu:refXs_to_xrefXs_create([RefX2]),
    walk(T, RefX, Htap, [XRefX | Acc]);
walk([_ | T], RefX, Htap, Acc) ->
    walk(T, RefX, Htap, Acc).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

% create an entry and delete it - check the tree returned is empty
% on the root path
testA1([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P2 = [],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~nSTART DELETING FROM TREE of ~p...~n", [NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% create an entry and delete it - check the tree returned is empty
% one segment in
testA1a([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P2 = ["one"],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~nSTART DELETING FROM TREE of ~p...~n", [NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% create one and delete one deep and dirty on down
testA1b([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P2 = ["one", "two", "lots", "more", "ya", "bas"],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~nStart deleting from tree of ~p...~n", [NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add the same page multiple times then delete it
testA2([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    %P1 = ["one", "two"],
    P2 = ["one", "two", "three"],
    %P3 = ["one", "[or(true, false)]", "three"],
    %Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    %Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    %Idx1 = 1,
    Idx2 = 2,
    %Idx3 = 3,
    Actions1 = [
               %{Idx1, #xrefX{site = S, path = P2, obj = Obj2}},
               %{Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               %{Idx3, #xrefX{site = S, path = P3, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               %{Idx1, #xrefX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               %{Idx3, #xrefX{site = S, path = P3, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~n~nSTART DELETING FROM ~p~n", [NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all in the same order
testA3([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    %P1 = ["one", "two"],
    P2 = ["one", "two", "three"],
    %P3 = ["one", "[or(true, false)]", "three"],
    P4 = ["a"],
    P5 = ["b"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
               {Idx1, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               {Idx3, #xrefX{site = S, path = P4, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P5, obj = Obj2}}
              ],
    Actions2 = lists:reverse(hslists:dedup([Actions1, []])),
    io:format("Actions1 is ~p~nActions2 is ~p~n", [Actions1, Actions2]),
    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~n~nSTART DELETING FROM ~p~n", [NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all in reverse order
testA4([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    %P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "two", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
               {Idx1, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
               {Idx2, #xrefX{site = S, path = P3, obj = Obj1}},
               {Idx3, #xrefX{site = S, path = P3, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% same as previous but with branches that don't overlap
testA4a([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["banjo", "blammo"],
    P2 = ["one", "two", "houseboat"],
    P3 = ["one", "foxtrot", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
               {Idx1, #xrefX{site = S, path = P1, obj = Obj2}},
               {Idx2, #xrefX{site = S, path = P1, obj = Obj1}},
               {Idx3, #xrefX{site = S, path = P1, obj = Obj1}},
               {Idx1, #xrefX{site = S, path = P2, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx3, #xrefX{site = S, path = P2, obj = Obj2}},
               {Idx1, #xrefX{site = S, path = P3, obj = Obj3}},
               {Idx2, #xrefX{site = S, path = P3, obj = Obj2}},
               {Idx3, #xrefX{site = S, path = P3, obj = Obj2}}
              ],
    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all
testA5([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "two", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj3}},

                {Idx2, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj3}},

                {Idx3, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj3}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions1),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all
testA5a([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "ambience", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx1, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx1, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx1, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx1, #xrefX{site = S, path = P3, obj = Obj3}},

                {Idx2, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx2, #xrefX{site = S, path = P3, obj = Obj3}},

                {Idx3, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj3}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj3}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun del/2, Tr, L)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    NewTree2 = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

testB0([]) ->
    S = "http://example.com",
    P1 = ["one", "two"],
    %P2 = ["one"],
    %P3 = ["one", "[or(true, false)]", "three"],
    %Obj1 = {column, {1, 1}},
    %Obj2 = {column, {1, 2}},
    %Obj3 = {row, {2, 3}},
    %Idx1 = 1,
    %Idx2 = 2,
    %Idx3 = 3,
    Cell1 = {cell, {1, 1}},
    RefX = #xrefX{site = S, path = P1, obj = Cell1},
    List = check(gb_trees:empty(), RefX),
    io:format("is ~p~n", [Cell1]),
    ?assertEqual([], List).

testB1([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    %P2 = ["one"],
    %P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {1, 1}},
    %Obj2 = {column, {1, 2}},
    %Obj3 = {row, {2, 3}},
    Idx1 = 1,
    %Idx2 = 2,
    %Idx3 = 3,
    Cell1 = {cell, {1, 1}},
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    RefX = #xrefX{site = S, path = P1, obj = Cell1},
    [N] = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(N, Idx1).

testB2([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "bingo", "three"],
    Obj1 = {column, {1, 1}},
    %Obj2 = {column, {1, 2}},
    %Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Cell1 = {cell, {1, 1}},
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj1}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    RefX = #xrefX{site = S, path = P1, obj = Cell1},
    [N] = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(N, 1).

testB3([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    %P2 = ["one"],
    %P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {3, 4}},
    Obj2 = {column, {3, 3}},
    Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Cell1 = {cell, {3, 4}},
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj3}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
           end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    RefX = #xrefX{site = S, path = P1, obj = Cell1},
    List = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(lists:sort([Idx1, Idx2]), lists:sort(List)).

testB4([]) ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["one", "two"],
    %P2 = ["one"],
    %P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {3, 4}},
    Obj2 = {column, {3, 3}},
    Obj3 = {row, {1, 8}},
    Obj4 = {row, {7, 7}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Idx4 = 4,
    Cell1 = {cell, {4, 8}},
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P1, obj = Obj2}},
                {Idx3, #xrefX{site = S, path = P1, obj = Obj3}},
                {Idx4, #xrefX{site = S, path = P1, obj = Obj4}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   XRefXs = lists:merge([R], expand_zrefs(R)),
                   L = [{X, I} || X <- XRefXs],
                   lists:foldl(fun add/2, Tr, L)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    RefX = #xrefX{site = S, path = P1, obj = Cell1},
    List = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(lists:sort([Idx1, Idx3]), lists:sort(List)).

% cant run these tests now that we make XRefXs for zinfs and don't spoof 'em
% simple match of 2 zsegs
%% testC1([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["one", "[true]", "a", "1"],
%%     P2 = ["one", "two", "[true]", "1"],
%%     P3 = ["one", "[true]", "[true]", "1"],
%%     P4 = ["[true]", "[true]", "a", "[true]"],
%%     P5 = ["one", "two", "a", "1"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],
%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%           end,
%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx1, Idx2, Idx3, Idx4]), lists:sort(List)).

%% % longer match of 2 zsegs with a seg
%% testC2([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["d", "[true]", "[true]"],
%%     P2 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr ) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P2, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx1]), lists:sort(List)).

%% % set of different z matches
%% testC3([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["d", "[true]", "[true]"],
%%     P2 = ["[true]", "[true]", "[true]"],
%%     P3 = ["d", "[true]", "7"],
%%     P4 = ["d", "4", "[true]"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1  = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx1, Idx2, Idx3, Idx4]), lists:sort(List)).

%% % force a fail on z seg
%% testC4([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["d", "[true]", "[true]"],
%%     P2 = ["[true]", "[true]", "[true]"],
%%     P3 = ["d", "[true]", "7"],
%%     P4 = ["d", "4", "banjo"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx1, Idx2, Idx3]), lists:sort(List)).

%% % force more than 1 fail of z seg
%% testC5([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["d", "[true]", "loopy"],
%%     P2 = ["[true]", "[true]", "[true]"],
%%     P3 = ["d", "[true]", "7"],
%%     P4 = ["d", "[true]", "banjo"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx2, Idx3]), lists:sort(List)).

%% % add some shorter z paths that don't match
%% testC6([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["[true]", "loopy"],
%%     P2 = ["[true]", "[true]", "[true]"],
%%     P3 = ["d", "[true]", "7"],
%%     P4 = ["[true]", "banjo"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx2, Idx3]), lists:sort(List)).

%% % add some longer z paths that don't match
%% testC7([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["[true]", "loopy", "randy", "mandy", "shandy"],
%%     P2 = ["[true]", "[true]", "[true]"],
%%     P3 = ["d", "[true]", "7", "[true]"],
%%     P4 = ["[true]", "banjo"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
%%               ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx2]), lists:sort(List)).

%% % force more than 1 fail of z seg
%% testC8([]) ->
%%     Tree = gb_trees:empty(),
%%     S = "http://example.com",
%%     P1 = ["[if(true, true, false}"],
%%     P2 = ["[true]", "4", "[if(true, true, true)]"],
%%     P3 = ["[true]", "[true]", "7"],
%%     P4 = ["[4 > 3]", "[or(true, true)]", "[true]"],
%%     P5 = ["d", "4", "7"],
%%     Obj1 = {cell, {3, 4}},
%%     Idx1 = 1,
%%     Idx2 = 2,
%%     Idx3 = 3,
%%     Idx4 = 4,
%%     Actions1 = [
%%                 {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P1, obj = Obj1}},
%%                 {Idx1, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
%%                 {Idx1, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
%%                 {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}              ],

%%     Fun1 = fun({I, R}, Tr) ->
%%                    XRefXs = lists:merge([R], expand_zrefs(R)),
%%                    L = [{X, I} || X <- XRefXs],
%%                    lists:foldl(fun add/2, Tr, L)
%%            end,

%%     NewTree1 = lists:foldl(Fun1, Tree, Actions1),
%%     RefX = #xrefX{site = S, path = P5, obj = Obj1},
%%     List = check(NewTree1, RefX),
%%     io:format("is ~p in ~p~n", [Obj1, NewTree1]),
%%     ?assertEqual(lists:sort([Idx1, Idx2, Idx3, Idx4]),
%%                  lists:sort(hslists:uniq(List))).

test_dump() ->
    Tree = gb_trees:empty(),
    S = "http://example.com",
    P1 = ["[or(true, false]", "loopy", "randy", "mandy", "shandy"],
    P2 = ["[true]", "[biscuit()]", "[true]"],
    P3 = ["d", "[oddjob(1,2,3)]", "7", "[true]"],
    P4 = ["[true]", "banjo"],
    %P5 = ["d", "4", "7"],
    Obj1 = {cell, {3, 4}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Idx4 = 4,
    Actions1 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx4, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
              ],

    Fun1 = fun({I, R}, Tr) ->
                   add({R, I}, Tr)
           end,

    Actions2 = [
                {Idx1, #xrefX{site = S, path = P1, obj = Obj1}},
                {Idx2, #xrefX{site = S, path = P2, obj = Obj1}},
                {Idx3, #xrefX{site = S, path = P3, obj = Obj1}},
                {Idx4, #xrefX{site = S, path = P4, obj = Obj1}}
              ],

    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del({R, I}, Tr)
           end,

    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    NewTree2 = lists:foldl(Fun2, NewTree1, Actions2),
    dump(NewTree2, S).

unit_test_() ->

    Setup = fun() -> ok end,

    SeriesA = [
               fun testA1/1,
               fun testA1a/1,
               fun testA1b/1,
               fun testA2/1,
               fun testA3/1,
               fun testA4/1,
               fun testA4a/1,
               fun testA5/1,
               fun testA5a/1,

               fun testB0/1,
               fun testB1/1,
               fun testB2/1,
               fun testB3/1,
               fun testB4/1

               % fun testC1/1,
               % fun testC2/1,
               % fun testC3/1,
               % fun testC4/1,
               % fun testC5/1,
               % fun testC6/1,
               % fun testC7/1,
               % fun testC8/1
              ],

    %{setup, Setup, Cleanup,
     {setup, Setup, [{with, [], SeriesA}]}.

