%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Phonecall supervisor for handling incomging calls
%%%
%%% @end
%%% Created :  6 Mar 2012 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(phonecall_sup).

-behaviour(supervisor).

-include("twilio.hrl").
-include("twilio_web.hrl").

%% API
-export([
         start_link/1,
         init_call/5,
         call_complete/2,
         recording_notification/2,
         gather_response/2,
         goto_state/3
         %call_in_progress/2
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec init_call(list(), list(), #twilio{}, list(), list()) -> pid() | string().
init_call(Site, Type, Params, TwiML_EXT, Callbacks) ->
    ChildSpec = gen_child_spec(Type, Params, TwiML_EXT),
    Id = hn_util:site_to_atom(Site, "_phonecall"),
    case supervisor:start_child({global, Id}, ChildSpec) of
        {ok, Pid} ->
            gen_server:call(Pid, {start_call, Type, Params, Callbacks});
        Other ->
            {error, Other}
    end.

-spec call_complete(string(), #twilio{}) -> pid() | string().
call_complete(Site, Params) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Site, Call),
    gen_server:call(Pid, {call_complete, Params}).

-spec recording_notification(string(), #twilio{}) -> pid() | string().
recording_notification(Site, Params) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Site, Call),
    gen_server:call(Pid, {recording_notification, Params}).

-spec gather_response(string(),#twilio{}) -> pid() | string().
gather_response(Site, Params) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Site, Call),
    gen_server:call(Pid, {gather_response, Params}).

-spec goto_state(string(), #twilio{}, list()) -> pid() | string().
goto_state(Site, Params, State) ->
    Call = Params#twilio.call_sid,
    Pid = get_pid(Site, Call),
    gen_server:call(Pid, {goto_state, Params, State}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(link()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    case application:get_env(hypernumbers, startup_debug) of
        {ok, true} -> io:format("...starting phonecall for ~p~n", [Site]);
        _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_phonecall"),
    supervisor:start_link({global, Id}, ?MODULE, [Site]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([_Site]) -> {ok,{{one_for_one,1,30}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_child_spec(Type, Params, TwiML_EXT) ->
    #twilio{call_sid = CallSID} = Params,
    {CallSID, {phonecall_srv, start_link, [Type, Params, TwiML_EXT]},
     transient, brutal_kill, worker, [phonecall_srv]}.

get_pid(Site, Call) ->
    Id = hn_util:site_to_atom(Site, "_phonecall"),
    Servers = supervisor:which_children({global, Id}),
    case lists:keyfind(Call, 1, Servers) of
        false             -> exit("server doesn't exist");
        {Call, Pid, _, _} -> case Pid of
                                 undefined -> exit("server has been closed");
                                 Pid       -> Pid
                             end
    end.
