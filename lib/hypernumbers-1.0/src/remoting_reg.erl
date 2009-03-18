%%%-----------------------------------------------------------------------------
%%% File        remoting_reg.erl
%%% @author     Gordon Guthrie 
%%% @doc        remoting_reg handles registration from Flex remoting
%%% @copyright  Hypernumbers Ltd
%%%
%%% Created     : 6 Feb 2007 by gordonguthrie@backawinner.gg
%%% @private
%%%-----------------------------------------------------------------------------
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-export([ notify/6, notify/4, timestamp/0 ]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, {[], []}}.

handle_cast({msg, Site, Path, Msg}, {Updates, Waiting}) ->
    NUpdates = [{msg, Site, Path, Msg, timestamp()} | Updates], 
    {noreply, send_to_waiting(NUpdates, Waiting)};

handle_cast({fetch, Site, Path, Time, Pid}, {Updates, Waiting}) ->
    NWaiting = [{Site, Path, Time, Pid} | Waiting], 
    {noreply, send_queued(Updates, NWaiting)};

handle_cast(_Msg, State) ->
    ?INFO("Invalid Cast in remoting_reg ~p ",[_Msg]),
    {noreply, State}.

%% Invalid Message
handle_call(_Request,_From,State) ->
    ?INFO("Invalid Call in remoting_reg ~p ~p",[_Request,_From]),
    {reply,invalid_message, State}.
    
handle_info(_Info, State) ->    {noreply, State}.
terminate(_Reason, _State) ->   ?INFO("terminating",[]),ok.
code_change(_Old, State, _E) -> {ok, State}.

notify(Site, Path, Type, {RefType, _} = R, Name, Value) ->
    {Name2, Val2} = hn_util:jsonify_val({Name, Value}), 
    Msg = {struct, [{"type", Type}, {"reftype", RefType},
                    {"ref", hn_util:ref_to_str(R)}, 
                    {"name", Name2}, {"value", Val2}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

notify(Site, Path, Index, Style) ->
    {Key, CSS} = hn_mochi:style_to_css(Index, Style),
    Msg = {struct, [{"type", "style"}, {"index", Key}, {"css", CSS}]},
    gen_server:cast(remoting_reg, {msg, Site, Path, Msg}). 

send_to_server(Server, Time, Msgs) ->
    Server ! {msg, {struct, [{"time", Time}, 
                             {"msgs", {array, Msgs}}]}}.

send_queued(Updates, Waiting) ->

    [{Site, Path, Time, Pid} | OldWaiting ] = Waiting, 
    
    F = fun({msg, Site1, Path1, _Msg, Time1}) -> 
                Site1 == Site andalso Path1 == Path andalso Time =< Time1
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

send_to_waiting(Updates, Waiting) ->
    
    [{msg, Site, Path, Msg, Time}  | _Rest ] = Updates, 
    
    F = fun({Site1, Path1, _Time1, _Pid}) -> 
                Site1 == Site andalso Path1 == Path
        end,
    
    Send = fun({_Site, _Path, _Time2, Pid}) ->
                   send_to_server(Pid, Time, [Msg])
           end,
    
    {Match, Rest} = lists:partition(F, Waiting), 
    lists:map(Send, Match),    
    {expire_updates(Updates), Rest}.

timestamp() ->
    {Mega, Sec, Milli}  = erlang:now(), 
    list_to_integer(lists:concat([Mega, Sec, pad(Milli)])).

pad(X) ->
    lists:flatten(io_lib:format("~-7.10.0B",[X])).

expire_updates(Old) ->
    Now = timestamp(),
    F   = fun({msg, _Site, _Path, _Msg, Time}) -> 
                  Time > (Now-10000000)
          end,
    lists:filter(F, Old).

