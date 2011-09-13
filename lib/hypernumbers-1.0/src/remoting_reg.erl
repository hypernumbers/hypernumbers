%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(HEAP_SIZE, 250000).

-include("spriki.hrl").

%% gen_server callbacks
-export([
         start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         notify_site/1,
         notify_pages/1,
         notify_change/4,
         notify_delete/3,
         notify_delete_attrs/4,
         notify_style/3,
         notify_error/5,
         request_update/4,
         notify_refresh/2,
         timestamp/0
        ]).

-record(state, {updates = [], waiting = []}).

%%
%% Gen Server API
%%

start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:start_link({global, Id}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%%
%% Handle messages
%%

handle_call(_Req, _From, State) -> {reply, invalid_message, State}.

%% @doc  Handle incoming update message
handle_cast({msg, Site, Path, Msg}, State) ->
    #state{updates = Updates} = State,
    Packet   = {msg, Site, Path, Msg, timestamp()},
    {heap_size, HSZ} = process_info(self(), heap_size),
    true = if
               HSZ >  ?HEAP_SIZE -> garbage_collect(self());
               HSZ =< ?HEAP_SIZE -> true
           end,
        NUpdates = [Packet | Updates],
    {noreply, send_to_waiting(State#state{updates = NUpdates})};
%% @doc  Handle incoming request for updates
handle_cast({fetch, Site, Path, Time, Pid}, State) ->
    #state{waiting = Waiting} = State,
    NWaiting = [{Site, Path, Time, Pid} | Waiting],
    {noreply, send_queued(State#state{waiting = NWaiting})};
handle_cast(_Msg, State) ->
    error_logger:error_msg("Invalid Cast in remoting_reg ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State)    -> {noreply, State}.
terminate(_Reason, _State)   -> ok.
code_change(_Old, State, _E) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% API Calls
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_update(Site, Path, Time, Pid) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {fetch, Site, Path, Time, Pid}).

%% @doc Notify server of site details refresh
notify_site(Site) ->
    Msg = {struct, [{"type", "site_refresh"}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(["/"]),
                                   term_to_binary(Msg)}).

%% @doc Notify server that the pages have changed
notify_pages(Site) ->
    Msg = {struct, [{"type", "pages_refresh"}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    PID = global:whereis_name(Id),
    gen_server:cast(PID, {msg, Site, term_to_binary(["/"]),
                          term_to_binary(Msg)}).

%% @doc  Notify server of full page refresh
notify_refresh(Site, Path) ->
    Msg = {struct, [{"type", "refresh"},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of change to a cell
notify_change(Site, Path, {RefType, _} = R, Attrs) ->
    Fun = fun({"__"++_Hidden, _}) -> false; (_X) -> true end,
    FilteredAttrs = lists:filter(Fun, Attrs),
    Attrs2 = hn_util:jsonify_attrs(FilteredAttrs),
    Msg = {struct, [{"type", "change"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)},
                    {"attrs", {struct, Attrs2}}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

notify_delete_attrs(Site, Path, {RefType, _} = R, Attrs) ->
    Msg = {struct, [{"type", "delete_attrs"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)},
                    {"attrs", {struct, [{X, ""} || X <- Attrs]}}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

notify_delete(Site, Path, {RefType, _} = R) ->
    Msg = {struct, [{"type", "delete"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of a new style
notify_style(Site, Path, Style) ->
    {Key, CSS} = hn_mochi:style_to_css(Style),
    Msg = {struct, [{"path", hn_util:list_to_path(Path)},
                    {"type", "style"},
                    {"index", Key},
                    {"css", CSS}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of an error to a cell
notify_error(Site, Path, Ref, error_in_formula, Value) ->
    Msg = {struct, [{"type", "error"},
                    {"reftype", "cell"},
                    {"ref", hn_util:obj_to_change_msg(Ref)},
                    {"original", Value},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Internal Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc  Send an update to the comet server to forward to client
send_to_server(Server, Time, Msgs) ->
    Server ! {msg, {struct, [{"time", Time},
                             {"msgs", {array, lists:reverse(Msgs)}}]}}.

%% @doc  When a client requests data, send any message that are older
%%       than the time the client reports (and on the same site)
send_queued(State) ->
    #state{updates = Updates, waiting = Waiting} = State,
    [{_Site, Path, Time, Pid} | OldWaiting ] = Waiting,

    F = fun({msg, _Site1, Path1, Msg, Time1}) ->
                Time < Time1
                    andalso (is_site(Msg) orelse has_path(Path1, Path))
        end,

    {Match, _Else} = lists:partition(F, Updates),

    Wait = case Match of
               []   -> Waiting;
               List -> send_to_server(Pid, timestamp(),
                                      [ Msg || {msg, _, _, Msg, _} <- List ]),
                   OldWaiting
           end,
    NewUpdates = expire_updates(Updates),
    State#state{updates = NewUpdates, waiting = Wait}.

%% @doc  When an update is received, automatically send the update to
%%       any clients waiting on the same page
send_to_waiting(State) ->
    #state{updates = Updates, waiting = Waiting} = State,
    [{msg, _MsgSite, MsgPath, Msg, Time}  | _Rest ] = Updates,

    F = case is_site(Msg) of
            true  -> fun(_) -> true end;
            false -> fun({_Site, SrvPath, _Time, _Pid}) ->
                             has_path(MsgPath, SrvPath)
                     end
        end,

    {Match, Rest} = lists:partition(F, Waiting),
    [ send_to_server(Pid, Time, [Msg]) || {_S, _P, _T, Pid} <- Match ],
    State#state{updates = expire_updates(Updates), waiting = Rest}.

has_path(MsgPath, ClientPath) ->
    lists:member(binary_to_term(MsgPath), ClientPath).

is_site(Binary) ->
    {struct, List} = binary_to_term(Binary),
    case lists:keysearch("type", 1, List) of
        {value, {"type", "style"}}         -> true;
        {value, {"type", "site_refresh"}}  -> true;
        {value, {"type", "pages_refresh"}} -> true;
        _                                  -> false
    end.

timestamp() ->
    microsecs(erlang:now()).

microsecs({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

%% Expires any messages older than one minute
expire_updates(Old) ->
    FifteenSecsAgo = timestamp() - 15000000,
    [ Msg || Msg = {msg, _Site, _Path, _Msg, Time} <- Old,
             Time > FifteenSecsAgo].

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-define(SITE, "http://example.com:1234").

unit_test_() ->
    %% linked pid will die when test ends
    Setup = fun() -> ?MODULE:start_link(?SITE) end,
    Cleanup = fun(_) -> ok end,
    {setup, Setup, Cleanup,
     [fun test_basic_update/0,
      fun test_multiple_update/0,
      fun test_basic_waiting/0,
      fun test_multiple_waiting/0]}.

run_changes() ->
    notify_change(?SITE, [], {cell, {1,1}}, [{"value", 99}]),
    notify_change(?SITE, ["test"], {cell, {1,1}}, [{"value", 88}]),
    notify_change(?SITE, [], {cell, {1,1}}, [{"value", 77}]).

get_changes(Path, Time) ->
    request_update(?SITE, Path, Time, self()),
    receive
        {msg, {struct, [{"time", TStamp}, {"msgs", {array, Msgs}}]}} ->
            {TStamp, Msgs}
    after
        1000 -> {null, []}
    end.

test_basic_update() ->
    Time = timestamp(),
    run_changes(),
    {_, Msgs} = get_changes([[]], Time),
    ?assertEqual(2, length(Msgs)).

test_multiple_update() ->
    Time = timestamp(),
    run_changes(),
    {_, Msgs} = get_changes([[], ["test"]], Time),
    ?assertEqual(3, length(Msgs)).

test_basic_waiting() ->
    spawn( fun() -> timer:sleep(100), run_changes() end ),
    {Time, Changes1} = get_changes([[]], timestamp()),
    {_, Changes2} = get_changes([[]], Time),
    ?assertEqual(2, length(Changes1) + length(Changes2)).

test_multiple_waiting() ->
    spawn( fun() -> timer:sleep(100), run_changes() end ),
    {Time, Changes1} = get_changes([[], ["test"]], timestamp()),
    {_, Changes2} = get_changes([[], ["test"]], Time),
    ?assertEqual(3, length(Changes1) + length(Changes2)).
