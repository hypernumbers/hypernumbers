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
-include_lib("eunit/include/eunit.hrl").
-include("keyvalues.hrl").
-include("errvals.hrl").

-define(sq_bra, 91).
-define(sq_ket, 93).

%% API
-export([start_link/1]).

%% Programatic API
-export([
         add_zinf/3,
         del_zinf/3,
         check_ref/2
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

-record(state,
        {
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

add_zinf(Site, CellIdx, RefX) when is_record(RefX, refX) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:call(Id, {add_zinf, {CellIdx, RefX}}).

del_zinf(Site, CellIdx, RefX) when  is_record(RefX, refX) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:call(Id, {del_zinf, {CellIdx, RefX}}).
    
check_ref(Site, RefX) when is_record(RefX, refX) ->
    Id = hn_util:site_to_atom(Site, "_zinf"),
    gen_server:call(Id, {check_ref, RefX}).

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
    [{kvstore, ?zinf_tree, Zinf_tree}] = hn_db_api:read_kv(Site, ?zinf_tree),
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
handle_call(Request, _From, #state{site = S, zinf_tree = Tree} = State) ->
    {Act, {Rep, NewT}} =
        case Request of
            {add_zinf, {Idx, RefX}} -> {write, add(Tree, Idx, RefX)};
            {del_zinf, {Idx, RefX}} -> {write, del(Tree, Idx, RefX)};
            {check_ref, RefX}       -> {nothing, {check(Tree, RefX), Tree}}
        end,
    case Act of
        write   -> hn_db_api:write_kv(S, ?zinf_tree, NewT);
        nothing -> ok
    end,
    {reply, Rep, State#state{zinf_tree = NewT}}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
add(Tree, Idx, RefX) ->
    #refX{path = P, obj = Obj} = RefX,
    NewTree = alter_tree(Tree, hn_util:parse_zpath(P), add_selector(Idx, Obj)),
    {ok, NewTree}.

del(Tree, Idx, RefX) ->
    #refX{path = P, obj = Obj} = RefX,
    NewTree = alter_tree(Tree, hn_util:parse_zpath(P), delete_selector(Idx, Obj)),
    {ok, NewTree}.

check(Tree, #refX{site = S, path = P} = RefX) ->
    Dirty = match_tree(Tree, S, P, match(RefX), []),
    {ok, Dirty}.

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

match(#refX{obj = {cell, {X, Y}}}) ->
    fun({selector, List}) ->
            match_1(List, X, Y, [])
    end.

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

match_tree(Tree, S, List, Fun, Htap) ->
    iterate(gb_trees:iterator(Tree), S, List, Fun, Htap, []).

iterate(Iter, S, [], Fun, Htap, Acc) ->
    case gb_trees:next(Iter) of
        none                 -> [];
        {selector, Sel, _I2} -> Fun(Sel);
        {_K, _V, I2}         -> iterate(I2, S, [], Fun, Htap, Acc)
    end;
iterate(Iter, S, [H | T] = List, Fun, Htap, Acc) ->
     case gb_trees:next(Iter) of
         none       -> lists:flatten(Acc);
         {K, V, I2} -> NewAcc = case match_seg(K, H, S, Htap) of
                                    match   -> match_tree(V, S, T, Fun, [H | Htap]);
                                    nomatch -> [];
                                    ?ERRVAL_VAL -> [?ERRVAL_VAL]
                                end,
                       iterate(I2, S, List, Fun, Htap, [NewAcc | Acc])
     end.

match_seg({seg, S},     S,  _Site, _Htap) -> match;
match_seg({seg, _S1}, _S2,  _Site, _Htap) -> nomatch;
match_seg({zseg, S1},   S,   Site,  Htap) -> Path = lists:reverse([S | Htap]),
                                             run_zeval(Site, Path, S1);
match_seg(selector,    _S,  _Site, _Htap) -> nomatch.

run_zeval(Site, Path, Z) ->
    Z2 = string:strip(string:strip(Z, right, ?sq_ket), left, ?sq_bra),
    {ok, Toks} = xfl_lexer:lex(Z2, {1, 1}),
    try
        muin:zeval(Site, Path, Toks)
    catch
        error:
        _Err  -> ?ERRVAL_VAL;
        exit:
        _Exit -> ?ERRVAL_VAL
    end.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

% create an entry and delete it - check the tree returned is empty
% on the root path
testA1([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P2 = [],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~nSTART DELETING FROM TREE of ~p...~n", [NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% create an entry and delete it - check the tree returned is empty
% one segment in
testA1a([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P2 = ["one"],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~nSTART DELETING FROM TREE of ~p...~n", [NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% create one and delete one deep and dirty on down
testA1b([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P2 = ["one", "two", "lots", "more", "ya", "bas"],
    Obj2 = {column, {1, 2}},
    Idx2 = 2,
    Actions1 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~nStart deleting from tree of ~p...~n", [NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add the same page multiple times then delete it
testA2([]) -> 
    Tree = {ok, gb_trees:empty()},
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
               %{Idx1, #refX{site = S, path = P2, obj = Obj2}},
               %{Idx2, #refX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #refX{site = S, path = P2, obj = Obj1}},
               %{Idx3, #refX{site = S, path = P3, obj = Obj3}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    Actions2 = [
               %{Idx1, #refX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #refX{site = S, path = P2, obj = Obj1}},
               %{Idx2, #refX{site = S, path = P2, obj = Obj1}},
               %{Idx3, #refX{site = S, path = P3, obj = Obj3}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~n~nSTART DELETING FROM ~p~n", [NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all in the same order
testA3([]) -> 
    Tree = {ok, gb_trees:empty()},
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
               {Idx1, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj1}},
               {Idx2, #refX{site = S, path = P2, obj = Obj1}},
               {Idx3, #refX{site = S, path = P4, obj = Obj3}},
               {Idx2, #refX{site = S, path = P5, obj = Obj2}}
              ],
    Actions2 = lists:reverse(hslists:dedup([Actions1, []])),
    io:format("Actions1 is ~p~nActions2 is ~p~n", [Actions1, Actions2]),
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~n~nSTART DELETING FROM ~p~n", [NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions2),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all in reverse order
testA4([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    %P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
               {Idx1, #refX{site = S, path = P2, obj = Obj2}},
               {Idx2, #refX{site = S, path = P2, obj = Obj1}},
               {Idx2, #refX{site = S, path = P3, obj = Obj1}},
               {Idx3, #refX{site = S, path = P3, obj = Obj3}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% same as previous but with branches that don't overlap
testA4a([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P1 = ["banjo", "blammo"],
    P2 = ["one", "two", "[or(true, false)]"],
    P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    %Cell1 = {cell, {1, 1}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
               {Idx1, #refX{site = S, path = P1, obj = Obj2}},
               {Idx2, #refX{site = S, path = P1, obj = Obj1}},
               {Idx3, #refX{site = S, path = P1, obj = Obj1}},
               {Idx1, #refX{site = S, path = P2, obj = Obj3}},
               {Idx2, #refX{site = S, path = P2, obj = Obj2}},
               {Idx3, #refX{site = S, path = P2, obj = Obj2}},
               {Idx1, #refX{site = S, path = P3, obj = Obj3}},
               {Idx2, #refX{site = S, path = P3, obj = Obj2}},
               {Idx3, #refX{site = S, path = P3, obj = Obj2}}
              ],
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
    io:format("NewTree2 is ~p~n", [NewTree2]),    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all 
testA5([]) -> 
    Tree = {ok, gb_trees:empty()},
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
                {Idx1, #refX{site = S, path = P1, obj = Obj1}},
                {Idx1, #refX{site = S, path = P2, obj = Obj1}},
                {Idx1, #refX{site = S, path = P3, obj = Obj1}},
                {Idx1, #refX{site = S, path = P1, obj = Obj2}},
                {Idx1, #refX{site = S, path = P2, obj = Obj2}},
                {Idx1, #refX{site = S, path = P3, obj = Obj2}},
                {Idx1, #refX{site = S, path = P1, obj = Obj3}},
                {Idx1, #refX{site = S, path = P2, obj = Obj3}},
                {Idx1, #refX{site = S, path = P3, obj = Obj3}},

                {Idx2, #refX{site = S, path = P1, obj = Obj1}},
                {Idx2, #refX{site = S, path = P2, obj = Obj1}},
                {Idx2, #refX{site = S, path = P3, obj = Obj1}},
                {Idx2, #refX{site = S, path = P1, obj = Obj2}},
                {Idx2, #refX{site = S, path = P2, obj = Obj2}},
                {Idx2, #refX{site = S, path = P3, obj = Obj2}},
                {Idx2, #refX{site = S, path = P1, obj = Obj3}},
                {Idx2, #refX{site = S, path = P2, obj = Obj3}},
                {Idx2, #refX{site = S, path = P3, obj = Obj3}},

                {Idx3, #refX{site = S, path = P1, obj = Obj1}},
                {Idx3, #refX{site = S, path = P2, obj = Obj1}},
                {Idx3, #refX{site = S, path = P3, obj = Obj1}},
                {Idx3, #refX{site = S, path = P1, obj = Obj2}},
                {Idx3, #refX{site = S, path = P2, obj = Obj2}},
                {Idx3, #refX{site = S, path = P3, obj = Obj2}},
                {Idx3, #refX{site = S, path = P1, obj = Obj3}},
                {Idx3, #refX{site = S, path = P2, obj = Obj3}},
                {Idx3, #refX{site = S, path = P3, obj = Obj3}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, Actions1),
    io:format("NewTree2 is ~p~n", [NewTree2]),
    ?assertEqual(gb_trees:empty(), NewTree2).

% add many pages in and delete them all 
testA5a([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {1, 1}},
    Obj2 = {column, {1, 2}},
    Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Actions1 = [
                {Idx1, #refX{site = S, path = P1, obj = Obj1}},
                {Idx1, #refX{site = S, path = P2, obj = Obj1}},
                {Idx1, #refX{site = S, path = P3, obj = Obj1}},
                {Idx1, #refX{site = S, path = P1, obj = Obj2}},
                {Idx1, #refX{site = S, path = P2, obj = Obj2}},
                {Idx1, #refX{site = S, path = P3, obj = Obj2}},
                {Idx1, #refX{site = S, path = P1, obj = Obj3}},
                {Idx1, #refX{site = S, path = P2, obj = Obj3}},
                {Idx1, #refX{site = S, path = P3, obj = Obj3}},

                {Idx2, #refX{site = S, path = P1, obj = Obj1}},
                {Idx2, #refX{site = S, path = P2, obj = Obj1}},
                {Idx2, #refX{site = S, path = P3, obj = Obj1}},
                {Idx2, #refX{site = S, path = P1, obj = Obj2}},
                {Idx2, #refX{site = S, path = P2, obj = Obj2}},
                {Idx2, #refX{site = S, path = P3, obj = Obj2}},
                {Idx2, #refX{site = S, path = P1, obj = Obj3}},
                {Idx2, #refX{site = S, path = P2, obj = Obj3}},
                {Idx2, #refX{site = S, path = P3, obj = Obj3}},

                {Idx3, #refX{site = S, path = P1, obj = Obj1}},
                {Idx3, #refX{site = S, path = P2, obj = Obj1}},
                {Idx3, #refX{site = S, path = P3, obj = Obj1}},
                {Idx3, #refX{site = S, path = P1, obj = Obj2}},
                {Idx3, #refX{site = S, path = P2, obj = Obj2}},
                {Idx3, #refX{site = S, path = P3, obj = Obj2}},
                {Idx3, #refX{site = S, path = P1, obj = Obj3}},
                {Idx3, #refX{site = S, path = P2, obj = Obj3}},
                {Idx3, #refX{site = S, path = P3, obj = Obj3}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    NewTree1 = lists:foldl(Fun1, Tree, Actions1),
    Fun2 = fun({I, R}, {ok, Tr}) ->
                   del(Tr, I, R)
           end,
    io:format("~n~n~nSTART DELETING FROM TREE ~p~n",[NewTree1]),
    {ok, NewTree2} = lists:foldl(Fun2, NewTree1, lists:reverse(Actions1)),
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
    RefX = #refX{site = S, path = P1, obj = Cell1},
    {ok, List} = check(gb_trees:empty(), RefX),
    io:format("is ~p~n", [Cell1]),
    ?assertEqual([], List).

testB1([]) -> 
    Tree = {ok, gb_trees:empty()},
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
                {Idx1, #refX{site = S, path = P1, obj = Obj1}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    {ok, NewTree1} = lists:foldl(Fun1, Tree, Actions1),
    RefX = #refX{site = S, path = P1, obj = Cell1},
    {ok, [N]} = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(N, Idx1).

testB2([]) -> 
    Tree = {ok, gb_trees:empty()},
    S = "http://example.com",
    P1 = ["one", "two"],
    P2 = ["one"],
    P3 = ["one", "[or(true, false)]", "three"],
    Obj1 = {column, {1, 1}},
    %Obj2 = {column, {1, 2}},
    %Obj3 = {row, {2, 3}},
    Idx1 = 1,
    Idx2 = 2,
    Idx3 = 3,
    Cell1 = {cell, {1, 1}},
    Actions1 = [
                {Idx1, #refX{site = S, path = P1, obj = Obj1}},
                {Idx2, #refX{site = S, path = P2, obj = Obj1}},
                {Idx3, #refX{site = S, path = P3, obj = Obj1}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    {ok, NewTree1} = lists:foldl(Fun1, Tree, Actions1),
    RefX = #refX{site = S, path = P1, obj = Cell1},
    {ok, [N]} = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(N, 1).

testB3([]) -> 
    Tree = {ok, gb_trees:empty()},
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
                {Idx1, #refX{site = S, path = P1, obj = Obj1}},
                {Idx2, #refX{site = S, path = P1, obj = Obj2}},
                {Idx3, #refX{site = S, path = P1, obj = Obj3}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    {ok, NewTree1} = lists:foldl(Fun1, Tree, Actions1),
    RefX = #refX{site = S, path = P1, obj = Cell1},
    {ok, List} = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(lists:sort([Idx1, Idx2]), lists:sort(List)).

testB4([]) -> 
    Tree = {ok, gb_trees:empty()},
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
                {Idx1, #refX{site = S, path = P1, obj = Obj1}},
                {Idx2, #refX{site = S, path = P1, obj = Obj2}},
                {Idx3, #refX{site = S, path = P1, obj = Obj3}},
                {Idx4, #refX{site = S, path = P1, obj = Obj4}}
              ],
    
    Fun1 = fun({I, R}, {ok, Tr}) ->
                  add(Tr, I, R)
          end,
    {ok, NewTree1} = lists:foldl(Fun1, Tree, Actions1),
    RefX = #refX{site = S, path = P1, obj = Cell1},
    {ok, List} = check(NewTree1, RefX),
    io:format("is ~p in ~p~n", [Cell1, NewTree1]),
    ?assertEqual(lists:sort([Idx1, Idx3]), lists:sort(List)).

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
              ],
    
    %{setup, Setup, Cleanup, 
     {setup, Setup, [{with, [], SeriesA}]}.
