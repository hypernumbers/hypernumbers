%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-export([ notify_change/6, notify_style/4, notify_error/5, 
          request_update/4, notify_refresh/2, timestamp/0 ]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, {[], []}}.

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

request_update(Site, Path, Time, Pid) ->
    gen_server:cast(remoting_reg, {fetch, Site, Path, Time, Pid}).

%% @doc  Notify server of full page refresh
notify_refresh(Site, Path) ->
    Msg = {struct, [{"type", "refresh"}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

%% @doc  Notify server of change to a cell
notify_change(Site, Path, Type, {RefType, _} = R, Name, Value) ->
    {Name2, Val2} = hn_util:jsonify_val({Name, Value}), 
    Msg = {struct, [{"type", Type}, {"reftype", RefType},
                    {"ref", hn_util:obj_to_str(R)}, 
                    {"name", Name2}, {"value", Val2}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

%% @doc  Notify server of a new style
notify_style(Site, Path, Index, Style) ->
    {Key, CSS} = hn_mochi:style_to_css(Index, Style),
    Msg = {struct, [{"type", "style"}, {"index", Key}, {"css", CSS}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

%% @doc  Notify server of an error to a cell
notify_error(Site, Path, Ref, error_in_formula, Value) ->
    Msg = {struct, [{"type", "error"}, {"reftype", "cell"},
                    {"ref", hn_util:obj_to_str(Ref)}, 
                    {"original", Value}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

%% @doc  Send an update to the comet server to forward to client
send_to_server(Server, Time, Msgs) ->
    Server ! {msg, {struct, [{"time", Time}, 
                             {"msgs", {array, lists:reverse(Msgs)}}]}}.

%% @doc  When a client requests data, send any message that are older
%%       than the time the client reports (and on the same site)
send_queued(Updates, Waiting) ->

    [{Site, Path, Time, Pid} | OldWaiting ] = Waiting, 
    
    F = fun({msg, Site1, Path1, _Msg, Time1}) -> 
                Site1 == Site andalso Path1 == Path andalso Time < Time1
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

is_site(Site, Path, {Site, Path, _Time, _Pid}) -> true;
is_site(_Site, _Path, _Server)                 -> false.


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

handle_call(_Req, _From, State) -> {reply,invalid_message, State}.
handle_info(_Info, State)       -> {noreply, State}.
terminate(_Reason, _State)      -> ok.
code_change(_Old, State, _E)    -> {ok, State}.
