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

-include("auth.hrl").

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

provision_site(Zone, Email, SiteType) ->
    Uid = passport:create_uid(),
    provision_site_(Zone, Email, SiteType, Uid).
provision_site(Zone, Email, SiteType, [$_|Uid]) ->
    provision_site_(Zone, Email, SiteType, Uid);
provision_site(Zone, Email, SiteType, Uid) ->
    provision_site_(Zone, Email, SiteType, Uid).

-spec provision_site(string(), string(), atom(), uid()) 
                    -> {ok, new | existing, string(), uid(), string()} | 
                       {error, bad_provision | invalid_email}.
provision_site_(Zone, Email, SiteType, SuggestedUid) ->
    case valid_email(Email) of
        true -> 
            Call = {provision, Zone, Email, SiteType, SuggestedUid},
            case gen_server:call({global, ?MODULE}, Call) of
                {ok, New_Existing, Site, Uid, Name} ->
                    post_provision(New_Existing, Site, Uid, Email, Name),
                    {ok, New_Existing, Site, Uid, Name};
                _Else ->
                    {error, bad_provision}
            end;
        false ->
            {error, invalid_email}
    end.

%% %% This will be needed for 'non-generated' zone deployments: ie. 'uses.hn'.
%%     -spec provision_site(string(), string(), atom(), string()) -> no_return().
%% provision_site(_Zone, _Email0, _SiteType, _CustomHost) ->
%%     throw(undefined),
%%     ok.

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
handle_call({provision, Zone, Email, Type, SuggestedUid}, _From, State) ->
    {ok, {Host, {_Ip, Port, Node}}} = hns:link_resource(Zone),
    {ok, NE, Uid} = passport:get_or_create_user(Email, SuggestedUid),
    Name = extract_name_from_email(Email),
    Site = lists:flatten(io_lib:format("http://~s:~b", [Host,Port])),
    ok = rpc:call(Node, hn_setup, site, 
                  [Site, Type, [{creator, Uid},
                                {email, Email},
                                {name, Name}]]),
    {reply, {ok, NE, Site, Uid, Name}, State};

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

-spec post_provision(new | existing, string(), uid(), string(), string())
                    -> ok.

%% User does not have any existing sites, log them into their new site
%% directly.
post_provision(new, Site, Uid, Email, Name) ->
    EmailBody = first_site_email(Site, Uid, Email, Name),
    send_email(Email, EmailBody);

%% User already exists, so redirect them to their new site and let
%% them login normally.
post_provision(existing, Site, _Uid, Email, Name) ->
    EmailBody = additional_site_email(Site, Name),
    send_email(Email, EmailBody).

send_email(To, EmailBody) ->
    case application:get_env(hypernumbers, environment) of
        {ok, development} ->
            io:format("Email Body:~n~s~n--END EMAIL--~n",[EmailBody]);
        {ok, production}  ->
            spawn(hn_net_util, email, 
                  [To, 
                   "\"Hypernumbers Team\" <noreply@hypernumbers.com>",
                   "Your new site is live!", 
                   EmailBody]),
            ok
    end.    

extract_name_from_email(Email) ->
    LocalPart = lists:takewhile(fun(X) -> X /= $@ end, Email),
    [Name | _Rest] =  string:tokens(LocalPart, "."),
    capitalize_name(Name).
    
capitalize_name([X|Rest]) -> [string:to_upper(X)|Rest].

first_site_email(Site, Uid, Email, Name) ->
    Path = ["_validate", Name],
    Data = [{emailed, true}],
    HT = passport:create_hypertag(Site, Path, Uid, Email, Data, "never"),
    lists:flatten(
      ["Hi ", Name, ",\n\n",
       "We hope you're having a fun time "
       "building your new site:\n\n ", 
       hn_util:strip80(Site), "\n\n"
       "We just need one more thing. Please click the following "
       "link to validate your site, it only takes a moment and allows "
       "you to set your password.\n\n",
       "Click or paste the following into your browser:\n\n",
       HT,"\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).

additional_site_email(Site, Name) ->
    lists:flatten(
      ["Hi ", Name, ",\n\n", 
       "Cool, we're glad you want another site! "
       "We've built it for you, and it's located here:\n\n ",
       hn_util:strip80(Site), "\n\n",
       "Just use your existing account to login.\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).

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
