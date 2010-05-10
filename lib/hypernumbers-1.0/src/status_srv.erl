%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  7 May 2009 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(status_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, get_status/1, update_status/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(diff, calendar:time_difference).

-include("spriki.hrl").

-record(site, {site, time_purged = calendar:now_to_universal_time(now()), list}).
-record(user, {name, details}).
-record(details, {path, change, timestamp = calendar:now_to_universal_time(now())}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, []}.

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
    {NewState, Struct} = case lists:keysearch(Site, 2, State) of
                             false      -> {State, []};
                             {value, R} -> #site{list = L} = R,
                                           {N, St} = transform_status(L),
                                           R2 = R#site{list = N},
                                           L2 = lists:keyreplace(Site, 2, State, R2),
                                           {L2, St}
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
handle_cast({update, U, S, P, Ch}, []) ->
    State = [make_first_record(U, S, P, Ch)],
    {noreply, State};
handle_cast({update, U, S, P, Ch}, State) ->
    NewState = case lists:keysearch(S, 2, State) of
                   false      -> [make_first_record(U, S, P, Ch) | State];
                   {value, L} -> NewSite = add_to_site(L, U, S, P, Ch, State),
                                 lists:keyreplace(S, 2, State, NewSite)
               end,
    {noreply, NewState};
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

get_status(Site) ->
    gen_server:call(status_srv, {get_status, Site}).

update_status(User, Site, Path, Change) ->
    gen_server:cast(status_srv, {update, User, Site, Path, Change}).

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
    D = #details{path = P, change = Ch},
    User = #user{name = U, details = [D]},
    #site{site = S, list = [User]}.

add_to_site(SiteRec, U, S, P, Ch, _St) ->
    #site{list = List} = SiteRec,
    D = #details{path = P, change = Ch},
    List2 = case lists:keysearch(U, 2, List) of
              false      -> [#user{name = U, details = [D]} | List];
              {value, R} -> U2 = add_to_user(R, P, D),
                            lists:keyreplace(U, 2, List, U2)
          end,
    #site{site = S, list = List2}.

add_to_user(R, P, D) ->
    #user{details = OldD} = R,
    case lists:keysearch(P, 2, OldD) of
             false        -> R#user{details = [D | OldD]};
             {value, _D2} -> NewD = lists:keyreplace(P, 2, OldD, D),
                             R#user{details = NewD}
         end.

purge(#user{name = N, details = D}) ->
    NewD = purge_details(D),
    NewU = #user{name = N, details = NewD},
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
purge_d1([H | T], Acc) -> #details{timestamp = Ts} = H,
                          Now = calendar:now_to_universal_time(now()),
                          {D, _} = ?diff(Ts, Now),
                          if
                              D > 7  -> purge_d1(T, Acc);
                              D =< 7 -> purge_d1(T, [H | Acc])
                          end.
                                         
extract(List) -> extract1(List, []).

extract1([], Acc) -> lists:reverse(Acc);
extract1([#details{path = P, change = Ch, timestamp = Ts} | T], Acc) ->
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
             
