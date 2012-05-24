%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc        manages phone redirects
%%%
%%% @end
%%% Created : 24 May 2012 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(phoneredir_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API
-export([
         add_redir/2,
         get_redir/1,
         last_call/1
        ]).

-define(SERVER, ?MODULE).

-record(state, {calls = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

add_redir(Sid, Redir) ->
    gen_server:call(?MODULE, {add_redir, Sid, Redir}).
get_redir(Sid) ->
    gen_server:call(?MODULE, {get_redir, Sid}).
last_call(Sid) ->
    gen_server:call(?MODULE, {last_call, Sid}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting phoneredir_srv~n");
       _Other     -> ok
    end,
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
    {ok, #state{}}.

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
handle_call(Request, _From, State) ->
    {NewR, NewS} = case Request of
                       {add_redir, Sid, Redir} ->
                           NewC = dict:store(Sid, Redir, State#state.calls),
                           {ok, State#state{calls = NewC}};
                       {get_redir, Sid} ->
                           {dict:fetch(Sid, State#state.calls), State};
                       {last_call, Sid} ->
                           R = dict:fetch(Sid, State#state.calls),
                           NewC = dict:erase(Sid, State#state.calls),
                           {R, State#state{calls = NewC}}
                   end,
    {reply, NewR, NewS}.

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
