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

%% @spec start_link(Arg) -> StartLink
%% @doc  start a link between this and supervisor
start_link(Arg) -> 
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%% @spec init(Arg) -> {ok,State}
%% @doc  Start server
init([Type]) ->
    {ok, #state{type = Type}}.
 
%% @spec handle_info(Event,State) -> {noreply, State}
%% @doc  handle events from subscription to mnesia
handle_info({mnesia_table_event, {write, _, Rec, _, _}}, State) ->
    case State#state.state of
        passive -> ok;
        active  -> process_dirty(Rec, State#state.type)
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
    % (but not dirty_incoming_hn)
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
    lists:foreach(fun(X) -> process_dirty(X, dirty_cell) end, List2),
    
    {reply, ok, State};
%% for other tables, just flush 'em...
handle_call(flush, _From, State = #state{type = Type}) ->
    Fun = fun() ->
                  Match = ms_util:make_ms(Type, []),
                  mnesia:match_object(Match)
          end,
    List = mnesia:activity(transaction, Fun),
    lists:foreach(fun(X) -> process_dirty(X, Type) end, List),
    {reply, ok, State}.

%% @spec handle_cast({setstate,NState}, State) -> {noreply, State}
%% @doc  active server will recalc on write, passive will ignore
handle_cast({setstate,active}, State) -> 
    {noreply, State#state{state = active}};
handle_cast({setstate,passive}, State) -> 
    {noreply, State#state{state = passive}};

%% @spec handle_cast(subscribe, State) -> {noreply, State}
%% @doc  subscribe to table events from mnesia
handle_cast(subscribe, State = #state{type = Type}) ->
    mnesia:subscribe({table, Type, detailed}),
    {noreply, State};
%% @spec handle_cast(subscribe, State) -> {noreply,State}
%% @doc  unsubscribe from table events from mnesia
handle_cast(unsubscribe,State = #state{type=Type}) ->
    mnesia:unsubscribe({table, Type, detailed}),
    {noreply, State}.
    
%% @spec terminate(Reason, State) -> ok
%% @doc  exit the gen_server
terminate(_Reason, _State) ->           
    ok.    
%% @spec code_change(Version, State, Extra) -> {ok, State}
%% @doc  handle code_change
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.

%% @spec process_dirty(Record, Type) -> ok
%% @doc  processes the dirty record
process_dirty(Rec, dirty_notify_incoming) ->
    #dirty_notify_incoming{child = Child, parent = Parent, change = Change} = Rec,
    ChildIdx = hn_util:refX_from_index(Child),
    ParentIdx = hn_util:refX_from_index(Parent),
    {ok, ok} = hn_db_api:notify_incoming(ParentIdx, ChildIdx, Change),
    ok;
process_dirty(Rec, dirty_cell) ->
    io:format("in dirty_srv:process_dirty Rec is ~p~n", [Rec]),
    Index = Rec#dirty_cell.index,
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Logging code                                                      %
    % #index{path = Path, row = Row, column = Col} = Index,             %
    % Str=string:join(Path,"/")++" Row "++integer_to_list(Row)++" Col " %
    %     ++integer_to_list(Col),                                       %
    % bits:log(Str),                                                    %
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok = mnesia:dirty_delete({dirty_cell, Index}),
    ok = hn_db:cell_changed(Index);
process_dirty(Rec, dirty_incoming_create) ->
    io:format("in dirty_srv:process_dirty for dirty_incoming_create - "++
              "write me!~n"),
    ok;
process_dirty(Rec, dirty_incoming_hn) ->
    exit("broken - fix me! an incoming hn has changed - all its parents need "++
         "to be marked dirty (not it)!"),
    Index = Rec#dirty_incoming_hn.index,
    ok = mnesia:dirty_delete({dirty_incoming_hn, Index}),
    ok = hn_db:hn_changed(Index);
process_dirty(Rec, dirty_outgoing_hn) ->
    #dirty_outgoing_hn{index = Index, outgoing = O,
                       value = V, timestamp = T} = Rec,
    RefX = hn_util:refX_from_index(Index),
    {ok, ok} = hn_db_api:notify_hypernumber(RefX, O, V, T),
    ok;
process_dirty(Rec, dirty_outgoing_update) ->
    io:format("in dirty_srv:process_dirty for dirty_outgoing_update - "++
              "write me!~n"),
    ok.
    
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
