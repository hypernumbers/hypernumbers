%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie gordon@hypernumbers
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 6th January 2010
%%%-------------------------------------------------------------------
-module(factory_srv).

-behaviour(gen_server).

-include("spriki.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% export for internal spawn only
-export([tick/1, tock/1]).

-define(SERVER, ?MODULE). 
-define(CLOCKTICK, 2000).
-define(PATH, ["request_site"]).

-record(state, {site, port, args}).

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
start_link(Site, Args) ->
    SrvName = list_to_atom(fmt("~s_~p", [hn_util:parse_site(Site), ?MODULE])),
    gen_server:start_link({local, SrvName}, ?MODULE, [Args], []).

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
init(Args) ->
    NewState = parse_args(Args),
    _Pid     = spawn_link(?MODULE, tick, [self()]),
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
    ok   = handle_tock(State),
    _Pid = spawn_link(?MODULE, tick, [self()]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_tock([])      -> ok;
handle_tock([H | T]) ->

    SiteName = "http://" ++ H#state.site ++ ":" ++ H#state.port,
    RefX1 = #refX{site = SiteName,
                  path = ?PATH,
                  obj = {column, {1, 1}}},
    RefX2 = #refX{site = SiteName,
                  path = ?PATH,
                  obj = {column, {4, 4}}},
    Requested = hn_db_api:read_last(RefX1),
    Fulfilled = hn_db_api:read_last(RefX2),
    #refX{obj = {cell, {_, MaxR}}} = Requested,
    #refX{obj = {cell, {_, MaxF}}} = Fulfilled,
    ok = case MaxR of
             MaxF   -> ok;
             _Other ->
                 case catch provision(H#state.site, H#state.port,
                                      MaxF + 1, H) of
                     ok ->
                         ok;
                     Else ->
                         Msg = lists:flatten(io_lib:format("~p",[Else])),
                         Ref = RefX1#refX{obj = {cell, {3, MaxF+1}}},
                         ok = hn_db_api:write_attributes(
                                [{Ref, [{"formula", Msg}]}])
                 end

         end,
    handle_tock(T).

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
tock(Server) ->
    gen_server:cast(Server, tock).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(Server) -> 
    timer:sleep(?CLOCKTICK),
    factory_srv:tock(Server).

parse_args(Args) ->
    parse_a1(Args, []).

parse_a1([], Acc)      -> Acc;
parse_a1([H | T], Acc) ->
    Site = proplists:get_value(self, H),
    [_, "//" ++ Site2, Port] = string:tokens(Site, ":"),
    parse_a1(T, [#state{site=Site2, port=Port, args=H} | Acc]).

provision(CurrentSite, Port, Row, State) ->

    CurrentSiteName = "http://" ++ CurrentSite ++ ":" ++ Port,
    RefX = #refX{site = CurrentSiteName, path = ?PATH,
                 obj = {range, {1, Row, 3, Row}}},
    List = hn_db_api:read_attributes(RefX, ["formula"]),

    {Type, Email, SubDomain} = parse(List),
    io:format("Type is ~p Email is ~p SubDomain is ~p~n",
              [Type, Email, SubDomain]),
    SubDomain2 = ustring:pr(ustring:to_lower(ustring:new(SubDomain))),
    Type2 = normalise(Type),
    Password = tiny_util:get_password(),
    [User | _T] = string:tokens(Email, "@"),
    RefX1 = RefX#refX{obj = {cell, {4, Row}}},
    RefX2 = RefX#refX{obj = {cell, {5, Row}}},

    Template = code:priv_dir(sitemods) ++ "/site_types/" ++ Type2,

    DoesSiteTemplateExist = filelib:is_dir(Template),
    io:format("Template is ~p DoesSiteTemplateExist is ~p~n",
              [Template, DoesSiteTemplateExist]),

    case DoesSiteTemplateExist of
        false ->
            Sub = "Not Allocated - site type doesn't exist " ++ Email;
        _ ->
            Sub = SubDomain2,
            Host  = proplists:get_value(host, State#state.args),
            Port2 = proplists:get_value(port, State#state.args),
            NewSite  = "http://" ++ Sub  ++ "."  ++ Host  ++ ":"
                ++ integer_to_list(Port2),
            EmailNewSite =
                case Port2 of
                    80  -> "http://" ++ Sub  ++ "."  ++ Host;
                    % 443 -> "https://" ++ Sub  ++ "."  ++ Host;
                    _   -> "http://" ++ Sub  ++ "."  ++ Host  ++ ":"
                               ++ integer_to_list(Port2)
                end,
            
            ok = hn_setup:site(NewSite, list_to_atom(Type2),
                               [{user, User},
                                {email, Email},
                                {site, EmailNewSite},
                                {password, Password},
                                {subdomain, Sub }
                               ]),

            S = "Hi ~s~n~nWelcome to hypernumbers, we have set up your site "
                "at:~n~n ~s~n~nTo make changes to the site follow the "
                "instructions on the main page"
                "~n~nYour Username:     ~s     Your Password:"
                "     ~s~n~nThanks for signing up, "
                "hope you enjoy your new site!~n~n"
                "The hypernumbers team",

            Msg = fmt(S, [User, EmailNewSite, User, Password]),

            case application:get_env(hypernumbers, environment) of
                {ok, development} ->
                    io:format("Email is ~p~n", [Email]),
                    io:format("~p",[Msg]);
                {ok, production}  ->
                    hn_util:email(Email,
                                  "\"Hypernumbers Team\""
                                  ++ "<noreply@hypernumbers.com>",
                                  "Your new Hypernumbers site is live!", Msg)
            end
    end,

    ok = hn_db_api:write_attributes([
                                     {RefX1, [{"formula", Sub}]},
                                     {RefX2, [{"formula", Password}]}
                                    ]),
    ok.

parse(List) ->
    p1(List, [], [], []).

p1([], Email, Type, SubDomain) ->
    {Email, Type, SubDomain};
p1([{#refX{obj = {cell, {1, _}}}, {_, Type}}  | T], _A1, A2, A3) ->
    p1(T, Type, A2, A3);
p1([{#refX{obj = {cell, {2, _}}}, {_, Email}} | T], A1, _A2, A3) ->
    p1(T, A1, Email, A3);
p1([{#refX{obj = {cell, {3, _}}}, {_, SubDomain}} | T], A1, A2, _A3) ->
    p1(T, A1, A2, SubDomain).

normalise(String) ->
    String2 = re:replace(string:to_lower(String), " ", "_",
                         [global, {return, list}]),
    re:replace(String2, "-", "_", [global, {return, list}]).

fmt(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).


