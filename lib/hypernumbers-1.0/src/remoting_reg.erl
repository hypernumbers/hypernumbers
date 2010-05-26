%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-export([ notify_change/4, notify_delete/3, notify_style/3, notify_error/5, 
          request_update/4, notify_refresh/2, timestamp/0 ]).

%%
%% Gen Server API
%%

start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:start_link({local, Id}, ?MODULE, [], []).

init([]) ->
    {ok, {[], []}}.

%%
%% Handle messages
%%

handle_call(_Req, _From, State) -> {reply,invalid_message, State}.

%% @doc  Handle incoming update mesage
handle_cast({msg, Site, Path, Msg}, {Updates, Waiting}) ->
    Packet   = {msg, Site, Path, Msg, timestamp()},
    NUpdates = [Packet | Updates], 
    {noreply, send_to_waiting(NUpdates, Waiting)};
%% @doc  Handle incoming request for updates
handle_cast({fetch, Site, Path, Time, Pid}, {Updates, Waiting}) ->
    NWaiting = [{Site, Path, Time, Pid} | Waiting], 
    {noreply, send_queued(Updates, NWaiting)};
handle_cast(_Msg, State) ->
    ?ERROR("Invalid Cast in remoting_reg ~p ",[_Msg]),
    {noreply, State}.

handle_info(_Info, State)       -> {noreply, State}.

terminate(_Reason, _State)      -> ok.

code_change(_Old, State, _E)    -> {ok, State}.

%%
%% API Calls
%%

request_update(Site, Path, Time, Pid) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {fetch, Site, Path, Time, Pid}).

%% @doc  Notify server of full page refresh
notify_refresh(Site, Path) ->
    Msg = {struct, [{"type", "refresh"},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {msg, Site, Path, Msg}). 

%% @doc  Notify server of change to a cell
notify_change(Site, Path, {RefType, _} = R, Attrs) ->
    Attrs2 = hn_util:jsonify_attrs(Attrs),
    Msg = {struct, [{"type", "change"}, {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_str(R)}, 
                    {"attrs", {struct, Attrs2}}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {msg, Site, Path, Msg}). 

notify_delete(Site, Path, {RefType, _} = R) ->
    Msg = {struct, [{"type", "delete"}, {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_str(R)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {msg, Site, Path, Msg}). 

%% @doc  Notify server of a new style
notify_style(Site, Path, Style) ->
    {Key, CSS} = hn_mochi:style_to_css(Style),
    Msg = {struct, [{"path", hn_util:list_to_path(Path)},
                    {"type", "style"}, {"index", Key}, {"css", CSS}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {msg, Site, Path, Msg}). 

%% @doc  Notify server of an error to a cell
notify_error(Site, Path, Ref, error_in_formula, Value) ->
    Msg = {struct, [{"type", "error"}, {"reftype", "cell"},
                    {"ref", hn_util:obj_to_str(Ref)}, 
                    {"original", Value},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast(Id, {msg, Site, Path, Msg}). 

%%
%% Internal Functions
%%

%% @doc  Send an update to the comet server to forward to client
send_to_server(Server, Time, Msgs) ->
    Server ! {msg, {struct, [{"time", Time}, 
                             {"msgs", {array, lists:reverse(Msgs)}}]}}.

%% @doc  When a client requests data, send any message that are older
%%       than the time the client reports (and on the same site)
send_queued(Updates, Waiting) ->

    [{Site, Path, Time, Pid} | OldWaiting ] = Waiting, 
    
    F = fun({msg, Site1, Path1, _Msg, Time1}) -> 
                Site1 == Site andalso lists:member(Path1, Path)
                    andalso Time < Time1
        end,

    {Match, _Else} = lists:partition(F, Updates), 
    
    Wait = case Match of 
               []   -> Waiting;
               List -> 
                   Msgs = lists:map(fun({msg, _, _, X, _}) -> X end,List),
                   send_to_server(Pid, timestamp(), Msgs),
                   OldWaiting
           end,
    {expire_updates(Updates), Wait}.

%% @doc  When an update is recieved, automatically send the update to 
%%       any clients waiting on the same page
send_to_waiting(Updates, Waiting) ->
    [{msg, Site, Path, Msg, Time}  | _Rest ] = Updates,     
    F = fun(Server) -> is_site(Site, Path, Server) end,
    Send = fun({_S, _P, _T, Pid}) -> send_to_server(Pid, Time, [Msg]) end,
    {Match, Rest} = lists:partition(F, Waiting), 
    lists:map(Send, Match),    
    {expire_updates(Updates), Rest}.

is_site(Site, Path, {Site, Path1, _Time, _Pid}) -> lists:member(Path, Path1);
is_site(_Site, _Path, _Server)                  -> false.


timestamp() ->
    microsecs(erlang:now()).

microsecs({MegaSecs,Secs,MicroSecs}) ->
        (MegaSecs*1000000 + Secs)*1000000 + MicroSecs. 

%% Expires any messages older than one minute
expire_updates(Old) ->
    Now = timestamp(),
    F   = fun({msg, _Site, _Path, _Msg, Time}) -> 
                  Time > (Now-60000000)
          end,
    lists:filter(F, Old).


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

