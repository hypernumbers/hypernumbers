%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie <gordonguthrie@poliwag>
%%% @copyright (C) 2009, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2009 by Gordon Guthrie <gordonguthrie@poliwag>
%%%-------------------------------------------------------------------
-module(tiny_srv).

-behaviour(gen_server).

-include("spriki.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% export for internal spawn only
-export([tick/0, tock/0]).

-define(SERVER, ?MODULE). 
-define(CLOCKTICK, 2000).
-define(PATH, ["request_site"]).

-record(state, {site, port}).

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

    Site = tiny_util:get_tiny_site(),
    [_, "//" ++ Site2, Port] = string:tokens(Site, ":"),
    NewState = #state{site = Site2, port = Port},

    _Pid = spawn_link(?MODULE, tick, []),
    
    {ok, NewState}.

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
handle_cast(tock, State) ->
    SiteName = "http://" ++ State#state.site ++ ":" ++ State#state.port,
    RefX1 = #refX{site = SiteName,
                  path = ?PATH,
                  obj = {column, {1, 1}}},
    RefX2 = #refX{site = SiteName,
                  path = ?PATH,
                  obj = {column, {3, 3}}},
    Requested = hn_db_api:read_last(RefX1),
    Fulfilled = hn_db_api:read_last(RefX2),
    #refX{obj = {cell, {_, MaxR}}} = Requested,
    #refX{obj = {cell, {_, MaxF}}} = Fulfilled,
    ok = case MaxR of
             MaxF   -> ok;
             _Other -> provision(State#state.site, State#state.port, MaxF + 1)
         end,
    _Pid = spawn_link(?MODULE, tick, []),
    {noreply, State};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tock() ->
    gen_server:cast(?MODULE, tock).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick() -> 
    timer:sleep(?CLOCKTICK),
    tiny_srv:tock().

provision(Site, Port, Row) ->
    
    SiteName = "http://" ++ Site ++ ":" ++ Port,
    RefX = #refX{site = SiteName, path = ?PATH,
                 obj = {range, {1, Row, 2, Row}}},
    List = hn_db_api:read_attributes(RefX, ["formula"]),
    
    {Email, Type} = parse(List),

    Type2 = normalise(Type),
    Password = tiny_util:get_password(),
    Sub = tiny_util:get_unallocated_sub(),
    IsValidEmail = tiny_util:is_valid_email(Email),
    [User | _T] = string:tokens(Email, "@"),
    Expiry = "=now()+31",

    RefX1 = RefX#refX{obj = {cell, {3, Row}}},
    RefX2 = RefX#refX{obj = {cell, {4, Row}}},
    RefX3 = RefX#refX{obj = {cell, {5, Row}}},
    RefX4 = RefX#refX{obj = {cell, {6, Row}}},

    case IsValidEmail of
       false -> Sub2 = "Not Allocated";
       _     -> Sub2 = Sub,
                {ok, Host} = application:get_env(tiny, host),
                {ok, Port2} = application:get_env(tiny, port),
                SiteName2 = "http://" ++ Sub2 ++ "."  ++ Host  ++ ":"
                    ++ integer_to_list(Port2),
                hn_setup:site(SiteName2, list_to_atom(Type2),
                              [{user, User},
                               {email, Email},
                               {site, SiteName},
                               {password, Password},
                               {subdomain, Sub2}
                              ])
                %% hn_util:send_email("team@tiny.hn", Email, "hey!")
    end,
    ok = hn_db_api:write_attributes([
                      {RefX1, [{"formula", Sub2}]},
                      {RefX2, [{"formula", User}]},
                      {RefX3, [{"formula", Password}]},
                      {RefX4, [{"formula", Expiry}]}
                     ]),
    ok.

parse(List) -> p1(List, [], []).

p1([], Email, Type)                                          -> {Email, Type};
p1([{#refX{obj = {cell, {1, _}}}, {_, Type}}  | T], A1, _A2) -> p1(T, A1, Type);
p1([{#refX{obj = {cell, {2, _}}}, {_, Email}} | T], _A1, A2) -> p1(T, Email, A2).

normalise(String) ->
    String2 = re:replace(string:to_lower(String), " ", "_", [global, {return, list}]),
    re:replace(String2, "-", "_", [global, {return, list}]).
    
