%%%----------------------------------------------------------------------
%%% File    : yaws_sup.erl
%%% Author  : Claes Wikstrom <klacke@bluetail.com>
%%% Purpose : 
%%% Created : 16 Jan 2002 by Claes Wikstrom <klacke@bluetail.com>
%%%----------------------------------------------------------------------

-module(yaws_sup).
-author('klacke@bluetail.com').
-include("../include/yaws.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).
-export([get_app_args/0]).

-import(lists, [member/2]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->

    Sess = {yaws_session_server, {yaws_session_server, start_link, []},
            permanent, 5000, worker, [yaws_session_server]},

    YawsLog = {yaws_log, {yaws_log, start_link, []},
               permanent, 5000, worker, [yaws_log]},

    YawsServArgs = [get_app_args()],
    YawsServ = {yaws_server, {yaws_server, start_link, YawsServArgs},
                permanent, 5000, worker, [yaws_server]},

    YawsRSS = {yaws_rss, {yaws_rss, start_link, []},
               permanent, 5000, worker, [yaws_rss]},

    YawsEventManager = {yaws_event_manager, 
                        {gen_event, start_link,[{local,yaws_event_manager}]},
                        permanent, 5000, worker, [gen_event]},

    {ok,{{one_for_all,10,30}, [YawsLog, YawsRSS, YawsServ, Sess, 
                               YawsEventManager]}}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
get_app_args() ->
    AS=init:get_arguments(),
    Debug = case application:get_env(yaws, debug) of
                undefined ->
                    member({yaws, ["debug"]}, AS);
                {ok, Val}  ->
                    Val
            end,
    Trace = case application:get_env(yaws, trace) of
                undefined ->
                    case {member({yaws, ["trace", "http"]}, AS),
                          member({yaws, ["trace", "traffic"]}, AS)} of
                        {true, _} ->
                            {true, http};
                        {_, true} ->
                            {true, traffic};
                        _ ->
                            false
                    end;
                {ok, http} ->
                    {true, http};
                {ok, traffic} ->
                    {true, traffic};
                _ ->
                    false
            end,
    TraceOutput = case application:get_env(yaws, traceoutput) of
                      undefined ->
                          member({yaws, ["traceoutput"]}, AS);
                      {ok, Val3}  ->
                          Val3
                  end,
    Conf = case application:get_env(yaws, conf) of
               undefined ->
                   find_c(AS);
               {ok, File} ->
                   {file, File}
           end,
    RunMod = case application:get_env(yaws, runmod) of
                 undefined ->
                     find_runmod(AS);
                 {ok,Mod} ->
                     {ok,Mod}
             end,
    Embedded = case application:get_env(yaws, embedded) of
                   undefined ->
                       false;
                   {ok, Emb} ->
                       Emb
               end,
    Id = case application:get_env(yaws, id) of
             undefined ->
                 "default";
             {ok, Id0} ->
                 Id0
         end,

    #env{debug = Debug, trace = Trace, 
         traceoutput = TraceOutput, conf = Conf, 
         runmod = RunMod, embedded = Embedded, id = Id}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
find_c([{conf, [File]} |_]) ->
    {file, File};
find_c([_|T]) ->
    find_c(T);
find_c([]) ->
    false.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
find_runmod([{runmod, [Mod]} |_]) ->
    {ok,l2a(Mod)};
find_runmod([_|T]) ->
    find_runmod(T);
find_runmod([]) ->
    false.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.
