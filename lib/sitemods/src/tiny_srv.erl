%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009 Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(tiny_srv).

-behaviour(gen_server).

-include("spriki.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% export for internal spawn only
-export([tick/0, tock/0]).

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
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
    
    %% Create sub domains if not created
    case catch mnesia:table_info(tiny_sub, all) of
        {'EXIT',{aborted,_Abort}} ->
            tiny_util:make_subs();
        _Else ->
            ok
    end,

    NewState = parse_args(Args),

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
    ok = handle_tock(State),
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

handle_tock([])      -> ok;
handle_tock([H | T]) ->
    SiteName = "http://" ++ H#state.site ++ ":" ++ H#state.port,
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

parse_args(Args) -> parse_a1(Args, []).

parse_a1([], Acc) -> Acc;
parse_a1([H | T], Acc) ->
    Site = proplists:get_value(self, H),
    [_, "//" ++ Site2, Port] = string:tokens(Site, ":"),
    parse_a1(T, [#state{site=Site2, port=Port, args=H} | Acc]).

provision(CurrentSite, Port, Row, State) ->
    
    CurrentSiteName = "http://" ++ CurrentSite ++ ":" ++ Port,
    RefX = #refX{site = CurrentSiteName, path = ?PATH,
                 obj = {range, {1, Row, 2, Row}}},
    List = hn_db_api:read_attributes(RefX, ["formula"]),
    
    {Email, Type} = parse(List),
 
    Type2 = normalise(Type),
    Password = tiny_util:get_password(),
    IsValidEmail = tiny_util:is_valid_email(Email),
    [User | _T] = string:tokens(Email, "@"),
    Expiry = "=now()+31",

    RefX1 = RefX#refX{obj = {cell, {3, Row}}},
    RefX2 = RefX#refX{obj = {cell, {4, Row}}},
    RefX3 = RefX#refX{obj = {cell, {5, Row}}},
    RefX4 = RefX#refX{obj = {cell, {6, Row}}},
    
    Template = code:priv_dir(sitemods) ++
        "/site_types/" ++ Type2,

    DoesSiteTemplateExist = filelib:is_dir(Template),

    case {IsValidEmail, DoesSiteTemplateExist} of
        {not_email, false} ->
            Sub = "Not Allocated - invalid e-mail " ++ Email
                ++ "and non-existant template " ++ Type2;
        {not_email, _} ->
            Sub = "Not Allocated - invalid e-mail " ++ Email;
        {_, false} ->
            Sub = "Not Allocated non-existant template " ++ Type2;
        _ ->
            Sub = tiny_util:get_unallocated_sub(),

            Host  = proplists:get_value(host, State#state.args),
            Port2 = proplists:get_value(port, State#state.args),

            NewSite  = "http://" ++ Sub  ++ "."  ++ Host  ++ ":"
                ++ integer_to_list(Port2),
            EmailNewSite = case Port2 of
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
            
            S = "Hi ~s~n~nWelcome to tiny.hn, we have set up your site "
                "at:~n~n ~s~n~nTo make changes to the site follow the "
                "instructions on the main page"
                "~n~nYour Username:     ~s     Your Password:"
                "     ~s~n~nThanks for signing up, "
                "hope you enjoy your tiny site!~n~n"
                "When you've finished customising your site don't forget "
                "to email or twitter your friends!~n~n"
                "The tiny.hn team",
            
            Msg = fmt(S, [User, EmailNewSite, User, Password]),
            
            case application:get_env(hypernumbers, environment) of
                {ok, development} ->
                    io:format("~p",[Msg]);
                {ok, production}  ->
                    hn_util:email(Email, "\"tiny.hn Team\" <noreply@tiny.hn>",
                                  "Your new tiny.hn site is live!", Msg)
            end
    end,

    ok = hn_db_api:write_attributes([
                                     {RefX1, [{"formula", Sub}]},
                                     {RefX2, [{"formula", User}]},
                                     {RefX3, [{"formula", Password}]},
                                     {RefX4, [{"formula", Expiry}]}
                                    ]),

    ok.

parse(List) ->
    p1(List, [], []).

p1([], Email, Type) ->
    {Email, Type};
p1([{#refX{obj = {cell, {1, _}}}, {_, Type}}  | T], A1, _A2) ->
    p1(T, A1, Type);
p1([{#refX{obj = {cell, {2, _}}}, {_, Email}} | T], _A1, A2) ->
    p1(T, Email, A2).

normalise(String) ->
    String2 = re:replace(string:to_lower(String), " ", "_",
                         [global, {return, list}]),
    re:replace(String2, "-", "_", [global, {return, list}]).

fmt(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).
    
    
