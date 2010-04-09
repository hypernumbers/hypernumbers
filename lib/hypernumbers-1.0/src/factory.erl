%%% Factory is a hypernumbers service.
%%%
-module(factory).

-behaviour(gen_server).

%% API
-export([start_link/0,
         provision_site/3, provision_site/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

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
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

-spec provision_site(string(), string(), atom()) -> any(). 
provision_site(Zone, Email0, SiteType) ->
    Email = string:to_lower(Email0),
    case valid_email(Email) of
        true -> 
            Call = {provision, Zone, Email, SiteType},
            case gen_server:call({global, ?MODULE}, Call) of
                {Site, Uid, Name, HT} ->
                    EmailBody = new_site_email(Name, Site, Email),
                    case application:get_env(hypernumbers, environment) of
                        {ok, development} ->
                            io:format("Email Body:~n~s~n--END EMAIL~n",[EmailBody]);
                        {ok, production}  ->
                            hn_net_util:email(Email, "\"tiny.hn Team\" <noreply@tiny.hn>",
                                              "Your new tiny.hn site is live!", EmailBody)
                    end,
                    {Site, Uid, Name, HT};
                _Else  ->
                    {error, bad_provision}
            end;
        false ->
            {error, invalid_email}
    end.

-spec provision_site(string(), string(), atom(), string()) -> no_return().
provision_site(_Zone, _Email0, _SiteType, _CustomHost) ->
    throw(undefined),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
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
handle_call({provision, Zone, Email, Type}, _From, State) ->
    {ok, {Host, {_Ip, Port, Node}}} = hns:link_resource(Zone),
    Uid = passport:get_or_create_user(Email),
    Name = extract_name_from_email(Email),
    Site = lists:flatten(io_lib:format("http://~s:~b", [Host,Port])),
    ok = rpc:call(Node, hn_setup, site, 
                  [Site, Type, [{creator, Uid},
                                {email, Email},
                                {name, Name}]]),
    HT = passport:create_hypertag(Site, ["_invite", Name, "some", "page"], 
                                  Uid, [], never),
    {reply, {Site, Uid, Name, HT}, State};

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

extract_name_from_email(Email) ->
    LocalPart = lists:takewhile(fun(X) -> X /= $@ end, Email),
    [Name | _Rest] =  string:tokens(LocalPart, "."),
    capitalize_name(Name).
    
capitalize_name([X|Rest]) -> [string:to_upper(X)|Rest].

new_site_email(Name, Site, Email) ->
    S = "Hi ~s~n~nWelcome to tiny.hn, we have set up your site "
        "at:~n~n ~s~n~nTo make changes to the site follow the "
        "instructions on the main page"
        "~n~nYour Username:     ~s     ~nYour Password:"
        "     ~s~n~nThanks for signing up, "
        "hope you enjoy your tiny site!~n~n"
        "~n~n The tiny.hn team",
    lists:flatten(io_lib:format(S, [Name, Site, Email, "you don't have one"])).

-spec valid_email(string()) -> boolean(). 
valid_email(Email) ->
    EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
        ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
        ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
        ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
        ++ "|biz|info|mobi|name|aero|jobs|museum)", %" for syntax highighting
    case re:run(Email, EMail_regex) of
        nomatch    -> false;
        {match, _} -> true
    end.
