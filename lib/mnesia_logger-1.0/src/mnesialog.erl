%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       a logger/debugger for mnesia
%%%
%%% @end
%%% Created :  5 May 2009 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(mnesialog).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         start/2,
         start/1,
         start/0,
         stop/0,
         clear/0,
         start_logging/1,
         start_logging/2,
         stop_logging/1,
         set_mnesia_debug_level/1
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

-define(SERVER, ?MODULE).
-define(NAME, "mnesia.log").
-define(DEFAULTLOGLEVEL, simple).

-record(state, {path, name, tables = []}).

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
handle_call({start, Path, Name}, _From, _State) ->
    % Dont check return, may be repaired
    disk_log:open([{name, Name}, {file, Path ++ Name}]),
    Reply = ok,
    {reply, Reply, #state{path = Path, name = Name}};  
handle_call(stop, _From, #state{name = Name}) ->
    ok = mnesia:set_debug_level(none),
    disk_log:close(Name),
    Reply = ok,
    {reply, Reply, #state{}};
handle_call(clear, _From, #state{name = Name}) ->
    disk_log:close(Name),
    file:delete(Name),
    Reply = ok,
    {reply, Reply, #state{}};
handle_call({start_logging, _T, _L}, _F,
            #state{path = undefined, name = undefined} = State) ->
    Reply = {error, log_not_started},
    {reply, Reply, State};
handle_call({start_logging, Table, LogLevel}, _From,
            #state{tables = Tables} = State) ->
    ok = mnesia:wait_for_tables([Table], 1000),
    {ok, _} = mnesia:subscribe({table, Table, LogLevel}),
    Reply = ok,
    {reply, Reply, State#state{tables = [Table | Tables]}};
handle_call({stop_logging, _T}, _F,
            #state{path = undefined, name = undefined} = State) ->
    Reply = {error, log_not_started},
    {reply, Reply, State};
handle_call({stop_logging, Table}, _From, #state{tables = Tables} = State) ->
    ok = mnesia:unsubscribe({table, Table}),
    Reply = ok,
    NewTables = lists:delete(Table, Tables),
    {reply, Reply, State#state{tables = NewTables}};
handle_call({set_mnesia_debug_level, Level}, _From, State) ->
    ok = mnesia:set_debug_level(Level),
    Reply = ok,
    {reply, Reply, State};
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
handle_info(Info, #state{name = Name} = State) ->
    disk_log:alog(Name, Info),
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
%%% Api
%%%===================================================================

%% @spec start(Path, Logname) -> ok
%% @doc This starts the log
start(Path, LogName) ->
    gen_server:call(?MODULE, {start, Path, LogName}).

%% @spec start(Path) -> ok
%% @doc This starts the log with the default name on the path
start(Path) ->
    gen_server:call(?MODULE, {start, Path, ?NAME}).

%% @spec start() -> ok
%% @doc This starts the log with the default name in 
%% the current working directory
start() ->
    gen_server:call(?MODULE, {start, "./", ?NAME}).

%% @spec stop() -> ok
%% @doc Closes log
stop() ->
    gen_server:call(?MODULE, stop).

%% @spec clear() -> ok
%% @doc Deletes current log
clear() ->
    gen_server:call(?MODULE, clear).

%% @spec start_log(Table, LogLevel) -> ok
%% @doc stars logging the table with the log level
start_logging(Table, LogLevel)
  when (LogLevel == simple orelse LogLevel == detailed) ->
    gen_server:call(?MODULE, {start_log, Table, LogLevel}).

%% @spec start_log(Table) -> ok
%% @doc starts logging the table with the default log level
start_logging(Table) ->
    gen_server:call(?MODULE, {start_logging, Table, ?DEFAULTLOGLEVEL}).

%% @spec stop_logging(Table, LogLevel) -> ok
%% @doc stops logging the table
stop_logging(Table) ->
    gen_server:call(?MODULE, {stop_logging, Table}).

set_mnesia_debug_level(Level) when (Level == none
                                    orelse Level == verbose
                                    orelse Level == debug
                                    orelse Level == trace
                                    orelse Level == false
                                    orelse Level == true) ->
    gen_server:call(?MODULE, {set_mnesia_debug_leve, Level}).

