%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       populates the CRM system in HighRise with new site
%%%            commissioned, and also MailChimp mailing lists
%%% @end
%%% Created : 18th March 2013
%%%-------------------------------------------------------------------
-module(marketing_integration_srv).

-behaviour(gen_server).

-include("spriki.hrl").
-include("xmerl.hrl").

-define(WLOG, new_db_api:write_commission_logD).
-define(HIGHRISE, "https://vixo1.highrisehq.com").
-define(HIGHRISEAUTH, "33d3579291e10ced535aaaa91631b018").
-define(HIGHRISENONE, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<nil-classes type=\"array\"/>\n").
-define(MAILCHIMP, "https://us6.api.mailchimp.com/1.3/").
-define(MAILCHIMPAUTH, "9b13129a90c88b299b80992cbf2836f2-us6").
-define(MAILCHIMPLIST, "4b750e1d42").

%% API
-export([
         start_link/0
        ]).

%% Debuggin
-export([
         push_to_integration/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API
-export([
         site_commissioned/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {calls = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================
site_commissioned() ->
    gen_server:call(?SERVER, site_commissioned).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    case application:get_env(hypernumbers, startup_debug) of
        {ok, true} -> io:format("...starting marketing_integration_srv~n");
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
    ok = push_to_integration(),
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
handle_call(site_commissioned, _From, State) ->
    io:format("Site commissioned...~n"),
    ok = push_to_integration(),
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
push_to_integration() ->
    Comms = new_db_api:get_unprocessed_commissionsD(),
    io:format("Commissions is ~p~n", [Comms]),
    [ok = write_to_highrise(X)  || X <- Comms],
    [ok = ?WLOG(#refX{site = CS, path = CP, obj = CO}, NS, STy, U, E, Z, true)
     || #commission{uid = U, email = E, site = NS, sitetype = STy, zone = Z,
                    commissioning_site = CS, commissioning_path = CP,
                    commissioning_cell = CO} <- Comms],
    ok.

write_to_highrise(Commission) ->
    #commission{uid = U, email = EM, sitetype = STy} = Commission,
    % search for the user
    io:format("Searching for user with UID of ~p~n", [U]),
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people/search.xml?criteria[uid]=" ++ U,
    Ret = httpc:request(get, {URL, Headers}, [], []),
    {ok, {{_, Code, _}, _, Body}} = Ret,
    case {Code, Body} of
        {200, ?HIGHRISENONE} ->
            io:format("Need to create user...~n"),
            XML = create_highrise_user_xml(Commission),
            ok = create_user_in_highrise(XML);
        {200, _} ->
            io:format("Person exists...~n") end,
    Details = xmerl_scan:string(Body),
    Id = get_highrise_id(Details),
    Note = create_highrise_note(Commission),
    %ok = add_highrise_note(Id, Note),
    Tag = create_highrise_tag("commissioned"),
    %ok = add_highrise_tag(Id, Tag),
    ok = add_to_mailchimp(EM, STy),
    ok.

add_to_mailchimp(EMail, SiteType) ->
    Name = hn_util:extract_name_from_email(EMail),
    ST = atom_to_list(SiteType),
    Groupings = [{struct, [
                           {name,    "commissioned"},
                           {groups, {array, [ST]}}
                          ]
                 }],
    Batch = [{struct, [
                       {"EMAIL",      EMail},
                       {"EMAIL_TYPE", html},
                       {"FNAME",      "{-}"},
                       {"LNAME",      Name},
                       {"GROUPINGS",  {array, Groupings}}
                      ]}],
    Json = {struct, [{apikey,            ?MAILCHIMPAUTH},
                     {id,                ?MAILCHIMPLIST},
                     {double_optin,      false},
                     {update_existing,   true},
                     {replace_interests, false},
                     {batch,             {array, Batch}}
                    ]},
    Json2 = lists:flatten(mochijson:encode(Json)),
    io:format("Json2 is ~p~n", [Json2]),
    URL = ?MAILCHIMP ++ "?method=listBatchSubscribe",
    Ret = httpc:request(post, {URL, [], ["application/json"], Json2}, [], []),
    {ok, {{_, Code, _}, _, Body}} = Ret,
    io:format("Code is ~p~nBody is ~p~n", [Code, Body]),
    ok.

get_highrise_id(XML) ->
    #xmlElement{content = [_C1, C2 | _R]} = element(1, XML),
    #xmlElement{content = List} = C2,
    get(List, id).

get([], _)                                            -> "";
get([#xmlElement{name = Id, content = [C]} | _T], Id) -> get_val(C);
get([_H | T], Id)                                     -> get(T, Id).

get_val(#xmlText{value = V}) -> V.

create_user_in_highrise(XML) ->
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people.xml",
    Ret = httpc:request(post, {URL, Headers, ["application/xml"], XML}, [], []),
    {ok, {{_, 201, _}, _, _Body}} = Ret,
    ok.

get_highrise_headers() ->
    Encoded = base64:encode_to_string(?HIGHRISEAUTH ++ ":loverboy, ooh, ooh"),
    _Headers = [{"Authorization","Basic " ++ Encoded}].

create_highrise_tag(Tag) -> "<name>" ++ Tag ++ "</name>".

add_highrise_note(Id, XML) ->
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people/" ++ Id ++ "/notes.xml",
    Ret = httpc:request(post, {URL, Headers, ["application/xml"], XML}, [], []),
    {ok, {{_, 201, _}, _, _Body}} = Ret,
    ok.

add_highrise_tag(Id, XML) ->
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people/" ++ Id ++ "/tags.xml",
    Ret = httpc:request(post, {URL, Headers, ["application/xml"], XML}, [], []),
    {ok, {{_, 201, _}, _, _Body}} = Ret,
    ok.

create_highrise_note(Commission) ->
    #commission{uid = _U, email = Em, site = S, sitetype = St,
                commissioning_site = CS, commissioning_path = CP,
                commissioning_cell = CC} = Commission,
    URL = CS ++ hn_util:list_to_path(CP) ++ hn_util:obj_to_ref(CC),
    Message = "A site of type " ++ atom_to_list(St) ++ " was created at "
        ++ S ++ " on " ++ dh_date:format("Y/m/d H:i:s") ++ " by " ++ Em ++ ".\n"
        ++ "The factory was at " ++ URL ++ ".\n",
    _XML = "<note><body>" ++ Message ++ "</body></note>".

create_highrise_user_xml( #commission{uid = Uid, email = EMail}) ->
    Name = hn_util:extract_name_from_email(EMail),
    _XML = "<person>"
        ++ "<first-name>{-}</first-name>"
        ++ "<last-name>" ++ Name ++ "</last-name>"
        ++ "<title></title>"
        ++ "<company-name></company-name>"
        ++ "<background></background>"
        ++ "<linkedin_url></linkedin_url>"
        ++ "<contact-data>"
        ++ "  <email-addresses>"
        ++ "    <email-address>"
        ++ "      <address>" ++ EMail ++ "</address>"
        ++ "      <location></location>"
        ++ "    </email-address>"
        ++ "  </email-addresses>"
        ++ "  <phone-numbers>"
        ++ "    <phone-number>"
        ++ "      <number></number>"
        ++ "      <location></location>"
        ++ "    </phone-number>"
        ++ "    <phone-number>"
        ++ "      <number></number>"
        ++ "      <location></location>"
        ++ "    </phone-number>"
        ++ "  </phone-numbers>"
        ++ "</contact-data>"
        ++ "<!-- custom fields -->"
        ++ "<subject_datas type=\"array\">"
        ++ "<subject_data>"
        ++ "  <subject_field_id type=\"integer\">741406</subject_field_id>"
        ++ "  <subject_field_label>UID</subject_field_label>"
        ++ "  <value>" ++ Uid ++ "</value>"
        ++ "</subject_data>"
        ++ "</subject_datas>"
        ++ "</person>".
