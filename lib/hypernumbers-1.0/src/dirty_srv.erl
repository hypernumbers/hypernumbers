%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(dirty_srv).
-behaviour(gen_server).

-include("handy_macros.hrl").
-include("hypernumbers.hrl").
-include("spriki.hrl").
-record(state, {type=[],state = active}).

-export([start_link/1, init/1, handle_call/3, 
    handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @spec start_link(Arg) -> StartLink.
%% @doc  start a link between this and supervisor
start_link(Arg) -> 
    gen_server:start_link({local, Arg}, ?MODULE, [Arg], []).

%% @spec init(Arg) -> {ok,State}.
%% @doc  Start server
init([Type]) ->
    {ok, #state{type=Type}}.
 
%% @spec handle_info(Event,State) -> {noreply,State}.
%% @doc  handle events from subscription to mnesia
handle_info({mnesia_table_event,{write,_,Rec,_,_}},State) ->
    case State#state.state of
        passive -> ok;
        active  -> trigger_recalc(Rec,State#state.type)
    end,
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply,State}.
%% @doc  ignore delete events from mnesia
handle_info({mnesia_table_event,{delete,_,_,_,_}},State) ->
    {noreply, State};

%% @spec handle_info(Else,State) -> {noreply,State}.
%% @doc  catch / flush unhandled events
handle_info(_Info,State) ->
    ?INFO("UnMatched Event ~p",[_Info]),
    {noreply, State}.

%% @spec handle_call(flush,From,State) -> {reply,ok,State}.
%% @doc  first shrink dirty_cell, then flush the dirty table, 
%%       read all the current dirty cells and trigger recalculation
handle_call(flush, _From, State = #state{type=Type}) ->

    % This fun shrinks the dirty cell table
    % (but not dirty_hypernumber)
    ?INFO("In dirty_src:handle_call for flush (start) "++
              "Dirty Cell Table Size is ~p~n",
              [mnesia:table_info(dirty_cell,size)]),
    Fun = fun() ->
                  % see how big the dirty cell table is
                  % get all the dirty_cell indices
                  Match=ms_util:make_ms(dirty_cell,[{index,'$1'}]),
                  List=mnesia:match_object(Match),
                  Fn2 = fun(X,Acc) ->
                                {_,Index,_}=X,
                                Links=hn_db:read_links(Index,parent),
                                case Links of
                                    [] -> Acc;
                                    L  -> [{X,L}|Acc]
                                end
                        end,
                  LinksList=lists:foldl(Fn2,[],List),
                  % now iterate over this list and look for parents that are
                  % on the dirty list
                  % If a dirty_cell has a dirty parent then we will shrink it out
                  DeleteList=shrink(LinksList,List),
                  Fn3 = fun({_,X,_}) -> mnesia:dirty_delete({dirty_cell,X}) end,
                  lists:foreach(Fn3,DeleteList),
                  mnesia:match_object({Type,'_','_'})
          end,
    {atomic,List2} = mnesia:transaction(Fun),
    ?INFO("In dirty_src:handle_call for flush (end) "++
              "Dirty Cell Table Size is ~p~n",
              [mnesia:table_info(dirty_cell,size)]),
    lists:foreach(fun(X) -> trigger_recalc(X,Type) end,List2),
    
    {reply, ok, State}.

%% @spec handle_cast({setstate,NState}, State) -> {noreply,State}.
%% @doc  active server will recalc on write, passive will ignore
handle_cast({setstate,active}, State) -> 
    {noreply, State#state{state=active}};
handle_cast({setstate,passive}, State) -> 
    {noreply, State#state{state=passive}};

%% @spec handle_cast(subscribe, State) -> {noreply,State}.
%% @doc  subscribe to table events from mnesia
handle_cast(subscribe, State = #state{type=Type}) -> 
    mnesia:subscribe({table,Type,detailed}),
    {noreply,State};
%% @spec handle_cast(subscribe, State) -> {noreply,State}.
%% @doc  unsubscribe from table events from mnesia
handle_cast(unsubscribe,State = #state{type=Type}) ->
    mnesia:unsubscribe({table,Type,detailed}),
    {noreply, State}.
    
%% @spec terminate(Reason, State) -> ok.
%% @doc  exit the gen_server
terminate(_Reason, _State) ->           
    ok.    
%% @spec code_change(Version, State, Extra) -> {ok,State}.
%% @doc  handle code_change
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.

%% @spec trigger_recalc(Record, Type) -> ok.
%% @doc  trigger recalculation for cell Rec.index
trigger_recalc(Rec,Type) ->
    
    Index = ?COND(Type == dirty_cell,
                  Rec#dirty_cell.index,
                  Rec#dirty_hypernumber.index),
    % #index{path=Path,row=Row,column=Col}=Index,
    % Str=string:join(Path,"/")++" Row "++integer_to_list(Row)++" Col "
    %    ++integer_to_list(Col),
    % bits:log(Str),
    ok = mnesia:dirty_delete({Type, Index}),
    ok = case Type of
             dirty_cell        -> hn_db:cell_changed(Index);
             dirty_hypernumber -> hn_db:hn_changed(Index)
         end,
    ok.

shrink(ParentsList,List) -> % io:format("in shrink~n-ParentsList is ~p~n-List is ~p~n",
                            %          [ParentsList,List]),
                            shrink(ParentsList,List,[]).

shrink([],_List,Acc) -> Acc;
shrink([Dirty|T],List,Acc) ->
    DirtyParents = has_dirty_parent(List,Dirty), 
    NewAcc = case DirtyParents of
                 false  -> Acc;
                 Dirty2 -> [Dirty2|Acc]
             end,
    % io:format("in dirty_srv:shrink NewAcc is ~p~n",[NewAcc]),
    shrink(T,List,NewAcc).

%% One true is good enough!
has_dirty_parent([],_Dirty) -> false;
has_dirty_parent([H|T],Parent)  ->
    {dirty_cell,Index,_}=H,
    {Cell,Links}=Parent,
    case lists:keymember(Index,3,Links) of
        true  -> % io:format("true~n"),
                 H;
        false -> % io:format("false~n"),
                 has_dirty_parent(T,Parent)
    end.
