%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO write a proper module description
%%% also this is totally not resilient
%%% the restart behaviour is broken. If this module wigs then it is restarted WITHOUT
%%% being resubscribed to mnesia for changes which is a mess...
%%% On restart it should process the appropriate dirty table and also resubscribe
%%% to the mnesia events...
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").
-record(state, {type = [], state = active}).

-export([start_link/1,
         init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(api, hn_db_api).

%% @spec start_link(Arg) -> StartLink
%% @doc  start a link between this and supervisor
start_link(Arg) ->
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%% @spec init(Arg) -> {ok,State}
%% @doc  Start server
init([Type]) ->
    % we need a little time for the config file to start itself properly
    % io:format("about to wait in dirty_srv init...~n"),
    % test_util:wait(75),
    ok = gen_server:cast(Type, {subscribe, Type}),
    {ok, #state{type = Type}}.

%% @spec handle_info(Event,State) -> {noreply, State}
%% @doc  handle events from subscription to mnesia
handle_info({mnesia_table_event, {write, _Table, Rec, _OldRecs, _ActId}},
            State) ->
    case State#state.state of
        passive -> ok;
        active  -> proc_dirty(Rec, State#state.type)
    end,
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply, State}
%% @doc  ignore delete events from mnesia
handle_info({mnesia_table_event, {delete, _, _, _, _}}, State) ->
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply, State}
%% @doc  catch / flush unhandled events
handle_info(_Info, State) ->
    ?INFO("Unmatched Event ~p", [_Info]),
    {noreply, State}.

%% @spec handle_call(flush, From, State) -> {reply, ok, State}
%% @doc  first shrink dirty_cell, then flush the dirty table, 
%%       read all the current dirty cells and trigger recalculation
%% @todo this function is no longer used and should probably be deleted...
handle_call(flush, _From, State = #state{type = dirty_cell}) ->

    % This fun shrinks the dirty cell table
    % (but not dirty_notify_in)
    ?INFO("In dirty_src:handle_call for flush (start) "++
          "for dirty_cell Size is ~p~n",
          [mnesia:table_info(dirty_cell, size)]),
    Fun = fun() ->
                  % see how big the dirty cell table is
                  % get all the dirty_cell indices
                  Match1 = ms_util:make_ms(dirty_cell, [{index, '$1'}]),
                  List = mnesia:match_object(Match1),
                  Fn2 = fun(X, Acc) ->
                                {_, Index, _} = X,
                                Links = hn_db:read_links(Index, parent),
                                case Links of
                                    [] -> Acc;
                                    L  -> [{X, L} | Acc]
                                end
                        end,
                  LinksList = lists:foldl(Fn2, [], List),
                  % now iterate over this list and look for parents that are
                  % on the dirty list
                  % If a dirty_cell has a dirty parent then we will shrink it out
                  DeleteList = shrink(LinksList, List),
                  Fn3 = fun({_, X, _}) -> mnesia:dirty_delete({dirty_cell, X}) end,
                  lists:foreach(Fn3, DeleteList),
                  Match2 = ms_util:make_ms(dirty_cell, []),
                  mnesia:match_object(Match2)
          end,
    {atomic,List2} = mnesia:transaction(Fun),
    ?INFO("In dirty_src:handle_call for flush (end) "++
          "Dirty Cell Table Size is ~p~n",
          [mnesia:table_info(dirty_cell, size)]),
    lists:foreach(fun(X) -> proc_dirty(X, dirty_cell) end, List2),

    {reply, ok, State};
%% for other tables, just flush 'em...
handle_call(flush, _From, State = #state{type = Type}) ->
    Fun = fun() ->
                  Match = ms_util:make_ms(Type, []),
                  mnesia:match_object(Match)
          end,
    List = mnesia:activity(transaction, Fun),
    lists:foreach(fun(X) -> proc_dirty(X, Type) end, List),
    {reply, ok, State}.

%% @spec handle_cast({setstate,NState}, State) -> {noreply, State}
%% @doc  active server will recalc on write, passive will ignore
handle_cast({setstate,active}, State) -> 
    {noreply, State#state{state = active}};
handle_cast({setstate,passive}, State) -> 
    {noreply, State#state{state = passive}};

%% @spec handle_cast(subscribe, State) -> {noreply, State}
%% @doc  subscribe to table events from mnesia
handle_cast({subscribe, Table}, State) ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    [ok = sub_unsubscribe(Table, X, subscribe) || X <- Sites],
    {noreply, State};
%% @spec handle_cast(subscribe, State) -> {noreply,State}
%% @doc  unsubscribe from table events from mnesia
handle_cast({unsubscribe, Table}, State) ->
    HostsInfo = hn_config:get(hosts),
    Sites = hn_util:get_hosts(HostsInfo),
    [ok = sub_unsubscribe(Table, X, unsubscribe) || X <- Sites],
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc  exit the gen_server
terminate(_Reason, _State) ->           
    ok.    
%% @spec code_change(Version, State, Extra) -> {ok, State}
%% @doc  handle code_change
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.

%% @spec proc_dirty(Rec, Type) -> ok
%% @doc  processes the dirty record
proc_dirty(Rec, _Table) ->
    {Site, NewRecType, Rec2} = hn_db_wu:split_trans(Rec),
    case NewRecType of
        dirty_cell            -> #dirty_cell{timestamp = T} = Rec2,
                                          % dirty_cell records are rewritten in 
                                          % insert/delete operations so we need 
                                          % to re-read it here
                                          ?api:handle_dirty_cell(Site, T);
        dirty_inc_hn_create   -> ?api:notify_back_create(Site, Rec2);
        dirty_notify_in       -> ?api:handle_dirty(Site, Rec2);
        dirty_notify_out      -> #dirty_notify_out{delay = D} = Rec2,
                                 ok = timer:sleep(D),
                                 ?api:handle_dirty(Site, Rec2);
        dirty_notify_back_in  -> ?api:handle_dirty(Site, Rec2);
        dirty_notify_back_out -> ?api:handle_dirty(Site, Rec2)
    end.

%%%
%%% Utility Functions
%%% 
shrink(ParentsList, List) -> shrink(ParentsList, List, []).

shrink([], _List, Acc)         -> Acc;
shrink([Dirty | T], List, Acc) -> DirtyParents = has_dirty_parent(List, Dirty),
                                  NewAcc = case DirtyParents of
                                               false  -> Acc;
                                               Dirty2 -> [Dirty2 | Acc]
                                           end,
                                  shrink(T, List, NewAcc).

%% One true is good enough!
has_dirty_parent([], _Dirty)       -> false;
has_dirty_parent([H | T], Parent)  -> {dirty_cell, Index,_} = H,
                                      {_Cell, Links} = Parent,
                                      case lists:keymember(Index, 3, Links) of
                                          true  -> H;
                                          false -> has_dirty_parent(T, Parent)
                                      end.

%% subscribe/unsubscribe to the mnesia tables
sub_unsubscribe(Table, Site, Action) ->
    NewTable = hn_db_wu:trans(Site, Table),
    {ok, _} = case Action of
                  subscribe   ->
                      mnesia:wait_for_tables([NewTable], 1000),
                      mnesia:subscribe({table, NewTable, detailed});
                  unsubscribe ->
                      mnesia:unsubscribe({table, NewTable, detailed})
              end,
    ok.
