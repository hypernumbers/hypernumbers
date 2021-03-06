%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009-2014, Hypernumbers Ltd
%%% @doc       the status server keeps a log of what people are up to
%%%            so 'gordon edited /some/page/ 5 minutes ago
%%%
%%%            It also loads the site in and out of memory based
%%%            if the site_cache_mode environment variable is set
%%%
%%% @end
%%% Created :  7 May 2009 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(status_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         get_status/1,
         update_status/3,
         tick/1
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

-define(diff, calendar:time_difference).

-include("spriki.hrl").

-define(IDLE_TIME, 600000). % 10 mins in microseconds

-record(state,
        {
          site,
          status = [],
          site_cache_mode,
          in_mem,
          time_last_updated
         }).

-record(status_site,
        {
          site,
          time_purged = calendar:now_to_universal_time(now()),
          list
         }).

-record(status_user,
        {
          name,
          details
         }).

-record(status_details,
        {
          path,
          change,
          timestamp = calendar:now_to_universal_time(now())
         }).

%%%===================================================================
%%% API
%%%===================================================================
tick(Site) ->
    Id = hn_util:site_to_atom(Site, "_status"),
    gen_server:cast({global, Id}, tick).

get_status(Site) ->
    Id = hn_util:site_to_atom(Site, "_status"),
    gen_server:call({global, Id}, {get_status, Site}).

update_status(User, RefX, Change) ->
    #refX{site = Site, path = Path} = RefX,
    Id = hn_util:site_to_atom(Site, "_status"),
    User2 = case User of
                undefined -> "anonymous2";
                _         -> {ok, U2} = passport:uid_to_email(User),
                             U2
            end,
    gen_server:cast({global, Id}, {update, User2, Site, Path, Change}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting status_srv for ~p~n", [Site]);
       _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_status"),
    gen_server:start_link({global, Id}, ?MODULE, [Site], []).

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
    {ok, CacheMode} = application:get_env(hypernumbers, site_cache_mode),
    InMem = case CacheMode of
                true  -> Ret = disc_only(Site),
                         % start ticking
                         ok = make_tick(?IDLE_TIME, Site),
                         Ret;
                false -> true
    end,
    {ok, #state{site = Site, site_cache_mode = CacheMode, in_mem = InMem,
                time_last_updated = util2:get_timestamp()}}.

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
handle_call({get_status, Site}, _From, State) ->
    #state{status = Status} = State,
    {NewState, Struct} = case lists:keysearch(Site, 2, Status) of
                             false      -> {State, []};
                             {value, R} -> #status_site{list = L} = R,
                                           {N, St} = transform_status(L),
                                           R2 = R#status_site{list = N},
                                           L2 = lists:keyreplace(Site, 2, Status, R2),
                                           {State#state{status= L2}, St}
                         end,
    Reply = {struct, Struct},
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({update, U, S, P, Ch}, #state{status = []} = State) ->
    Status = [make_first_record(U, S, P, Ch)],
    NewState = handle_caching(State),
    {noreply, NewState#state{status = Status}};
handle_cast({update, U, S, P, Ch}, State) ->
    #state{status = Status} = State,
    NewStatus = case lists:keysearch(S, 2, Status) of
                    false      -> [make_first_record(U, S, P, Ch) | Status];
                    {value, L} -> NewSite = add_to_site(L, U, S, P, Ch, State),
                                  lists:keyreplace(S, 2, Status, NewSite)
               end,
    NewState = handle_caching(State),
    {noreply, NewState#state{status = NewStatus}};
handle_cast(tick, State) ->
    #state{site = Site, in_mem = InMem, time_last_updated = Then} = State,

    Now = util2:get_timestamp(),
    % Idle time is microseconds so adjust timestamps down
    Diff = trunc((Now - Then)/1000),

    % if it is in memory check if an IDLE_TIME has elapased since last updated
    % * if an idle time has elapsed then (mebbies) write the site to disk
    %   - if the dbsrv is recalcing it wont write down
    % * if an IDLE TIME hasn't elapse then tick for an IDLE_TIME
    %   from the time of the last update
    % * if its on disk already do nothing
    {NewMem, Tick} = case InMem of
                         true ->
                             if
                                 Diff >= ?IDLE_TIME ->
                                     {disc_only(Site), ?IDLE_TIME};
                                 Diff < ?IDLE_TIME ->
                                     {true, ?IDLE_TIME - Diff}
                             end;
                         false ->
                             {InMem, ?IDLE_TIME}
                     end,
    %tick again
    ok = make_tick(Tick, Site),
    {noreply, State#state{in_mem = NewMem}};
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
transform_status(List) -> transform_status(List, [], []).

transform_status([], Acc1, Acc2)      -> {Acc1, Acc2};
transform_status([H | T], Acc1, Acc2) ->
    {NewU, NewH} = purge(H),
    NewAcc1 = case NewU of
                  [] -> Acc1;
                  _  -> [NewU | Acc1]
              end,
    NewAcc2 = case NewH of
                  [] -> [];
                  _  -> lists:flatten([NewH | Acc2])
              end,
    transform_status(T, NewAcc1, NewAcc2).

make_first_record(U, S, P, Ch) ->
    D = #status_details{path = P, change = Ch},
    User = #status_user{name = U, details = [D]},
    #status_site{site = S, list = [User]}.

add_to_site(SiteRec, U, S, P, Ch, _St) ->
    #status_site{list = List} = SiteRec,
    D = #status_details{path = P, change = Ch},
    List2 = case lists:keysearch(U, 2, List) of
              false      -> [#status_user{name = U, details = [D]} | List];
              {value, R} -> U2 = add_to_user(R, P, D),
                            lists:keyreplace(U, 2, List, U2)
          end,
    #status_site{site = S, list = List2}.

add_to_user(R, P, D) ->
    #status_user{details = OldD} = R,
    case lists:keysearch(P, 2, OldD) of
             false        -> R#status_user{details = [D | OldD]};
             {value, _D2} -> NewD = lists:keyreplace(P, 2, OldD, D),
                             R#status_user{details = NewD}
         end.

purge(#status_user{name = N, details = D}) ->
    NewD = purge_details(D),
    NewU = #status_user{name = N, details = NewD},
    case NewD of
        [] -> {NewU, {struct, []}};
        _  -> {NewU, [{(N), {struct, extract(NewD)}}]}
    end.

purge_details(D) -> purge_d1(D, []).

% purge_details1 uses the fact that details are placed on the status
% list in time order to truncate the update list
purge_d1([], Acc)      -> A2 = lists:reverse(Acc),
                          if
                              length(A2) =< 5 -> A2;
                              length(A2) >  5 -> [A, B, C, D, E | _F] = A2,
                                                 [A, B, C, D , E]
                          end;
purge_d1([H | T], Acc) -> #status_details{timestamp = Ts} = H,
                          Now = calendar:now_to_universal_time(now()),
                          {D, _} = ?diff(Ts, Now),
                          if
                              D > 7  -> purge_d1(T, Acc);
                              D =< 7 -> purge_d1(T, [H | Acc])
                          end.

extract(List) -> extract1(List, []).

extract1([], Acc) -> lists:reverse(Acc);
extract1([#status_details{path = P, change = Ch, timestamp = Ts} | T], Acc) ->
    P2 = hn_util:list_to_path(P),
    Msg = get_msg(Ts),
    Acc2 = {P2, {struct, [{Ch, Msg}]}},
    extract1(T, [Acc2 | Acc]).

get_msg(Ts) ->
    Now = calendar:now_to_universal_time(now()),
    Diff = ?diff(Ts, Now),
    case Diff of
        {0, {0, 0, S}} -> integer_to_list(S) ++ " seconds ago";
        {0, {0, M, _}} -> integer_to_list(M) ++ " minutes ago";
        {0, {H, _, _}} -> integer_to_list(H) ++ " hours ago";
        {D, {_, _, _}} -> integer_to_list(D) ++ " days ago"
    end.

handle_caching(#state{site_cache_mode = false} = State) ->
    State;
handle_caching(#state{site_cache_mode = true, in_mem = true} = State) ->
    State;
handle_caching(#state{site_cache_mode = true, in_mem = false} = State) ->
    ok = hn_db_admin:disc_and_mem(State#state.site),
    State#state{in_mem = true, time_last_updated = util2:get_timestamp()}.

make_tick(Delay, Site) ->
    {ok, _TRef} = timer:apply_after(Delay, erlang, spawn,
                                    [status_srv, tick, [Site]]),
    ok.

disc_only(Site) ->
    case dbsrv:is_busy(Site) of
        true  -> hn_db_admin:disc_and_mem(Site),
                 true;
        false -> hn_db_admin:disc_only(Site),
                 false
    end.

