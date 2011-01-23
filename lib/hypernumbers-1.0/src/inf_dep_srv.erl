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

-record(state, {
          zinf_tree
         }).
-record(selector, {
          objdict = orddict:new(),
          sub = gb_trees:empty()
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
    gen_server:call(Id, {check_ref, {RefX}}).

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
    {ok, #state{zinf_tree = Zinf_tree}}.

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
handle_call(Request, _From, #state{zinf_tree = Tree} = State) ->
    {Reply, NewTree}
        = case Request of
              {add_zinf, {Idx, RefX}} -> add(Tree, Idx, RefX);
              {del_zinf, {Idx, RefX}} -> del(Tree, Idx, RefX);
              {check_ref, RefX}             -> {check(Tree, RefX), Tree}
        end,
    {reply, Reply, State#state{zinf_tree = NewTree}}.

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
    io:format("Should add here for ~p~n", [RefX]),
    NewTree = alter_tree(Tree, hn_util:parse_zpath(P), add_selector(Idx, Obj)),
    io:format("NewTree is ~p~n", [NewTree]),
    {ok, Tree}.

del(Tree, _Idx, _RefX) ->
    io:format("Should delete here~n"),
    {ok,Tree}.

check(_Tree, _RefX) ->
    io:format("should check here~n"),
    ok.

%%%===================================================================
%%% Funs to be applied to the tree
%%%===================================================================
add_selector(Idx, Obj) ->
    fun(#selector{objdict = OD} = S) ->
            io:format("Idx is ~p~n", [Idx]),
            NewOD = case orddict:is_key(Obj, OD) of
                        false -> orddict:append(Obj, Idx, OD);
                        true  -> IL = orddict:fetch(Obj, OD),
                                 io:format("IL is ~p~n", [IL]),
                                 NewIL = [Idx | IL],
                                 ordict:append(Obj, NewIL, OD)
                    end,
            io:format("OD is ~p~nNewOD is ~p~n", [OD, NewOD]),
            S#selector{objdict = NewOD}
    end.

%%%===================================================================
%%% Tree manipulation functions
%%%===================================================================
-spec alter_tree(gb_tree(), 
                 [string()],
                 fun((#selector{}) -> #selector{})) -> gb_tree().
alter_tree(Tree, [], Fun) ->
    case gb_trees:lookup(selector, Tree) of
        none       -> NewCtrls = Fun(#selector{}),
                      gb_trees:insert(selector, NewCtrls, Tree);
        {value, V} -> NewVs = Fun(V),
                      gb_trees:enter(selector, NewVs, Tree)
    end;
alter_tree(Tree, [H | T], Fun) ->
    io:format("H is ~p~n", [H]),
    case gb_trees:lookup(H, Tree) of
        none       -> Empty = gb_trees:empty(),
                      NewVal = alter_tree(Empty, T, Fun),
                      gb_trees:insert(H, NewVal, Tree);
        {value, V} -> NewVal = alter_tree(V, T, Fun),
                      gb_trees:enter(H, NewVal, Tree)
    end.
