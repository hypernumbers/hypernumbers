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
         site_commissioned/0,
         nudge/2
        ]).

% Debuggin
-export([
         test/1,
         test/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {calls = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================
site_commissioned() ->
    gen_server:call(?SERVER, site_commissioned).

test(Name) ->
    test(Name, blank).

test(Name, SiteType) ->
    Email = Name ++ "@hypernumbers.com",
    S = "http://hypernumbers.dev:9000",
    P = ["some", "page"],
    O = {cell, {1, 1}},
    NewSite = "http://newsite.vixo.com",
    {ok, _, UID} = passport:get_or_create_user(Email),
    Zone = "dev",
    LoginURL = "http://some.logging.com/blah/blah/",
    Synched = false,
    Commission = #commission{uid = UID, email = Email, site = NewSite,
                             sitetype = SiteType, zone = Zone,
                             link = LoginURL, comm_site = S,
                             comm_path = P, comm_cell = O,
                             synched = Synched},
    new_db_api:write_commission_logD(Commission),
    ok = push_to_integration().

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
    [ok = write_to_highrise(X)  || X <- Comms],
    [ok = new_db_api:write_commission_logD(X#commission{synched = true})
     || X <- Comms],
    ok.

nudge(Email, Nudge) when is_list(Nudge) ->
    % search for the user
    {ok, existing, UID} = passport:get_or_create_user(Email),
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people/search.xml?criteria[uid]=" ++ UID,
    Ret = httpc:request(get, {URL, Headers}, [], []),
    {ok, {{_, 200, _}, _, Body}} = Ret,
    Details = xmerl_scan:string(Body),
    Id = get_highrise_id(Details, existinguser),
    Tag = create_highrise_tag("Nudged: " ++ Nudge),
    ok = add_highrise_tag(Id, Tag),
    Opts = [
            {"NUDGE",     Nudge},
            {"NUDGEDATE", dh_date:format("Y-m-d H:h:s")}
            ],
    ok = add_to_mailchimp(Email, Opts).

write_to_highrise(Commission) ->
    #commission{uid = U, email = EM, sitetype = STy, site = S, link = L} = Commission,
    % search for the user
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people/search.xml?criteria[uid]=" ++ U,
    Ret = httpc:request(get, {URL, Headers}, [], []),
    {ok, {{_, Code, _}, _, Body}} = Ret,
    Id = case {Code, Body} of
             {200, ?HIGHRISENONE} ->
                 XML = create_highrise_user_xml(Commission),
                 _Id = create_user_in_highrise(XML);
             {200, _} ->
                 Details = xmerl_scan:string(Body),
                 _Id = get_highrise_id(Details, existinguser)
    end,
    Note = create_highrise_note(Commission),
    ok = add_highrise_note(Id, Note),
    Tag = create_highrise_tag("Commissioned"),
    Tag2 = create_highrise_tag("Site Type Commissioned: " ++ atom_to_list(STy)),
    [ok = add_highrise_tag(Id, X) || X <- [Tag, Tag2]],
    Opts = [
            {"SITETYPE", atom_to_list(STy)},
            {"DATECOM",  dh_date:format("Y-m-d H:h:s")},
            {"SIGNIN",   L},
            {"SITE",     S}
           ],
    ok = add_to_mailchimp(EM, Opts).

add_to_mailchimp(EMail, Opts) ->
    Name = hn_util:extract_name_from_email(EMail),
    Batch = [
             {"EMAIL",      EMail},
             {"EMAIL_TYPE", html},
             {"FNAME",      "{-}"},
             {"LNAME",      Name}
            ],
    Batch2 = [{struct, lists:merge(Batch, Opts)}],
    Json = {struct, [{apikey,            ?MAILCHIMPAUTH},
                     {id,                ?MAILCHIMPLIST},
                     {double_optin,      false},
                     {update_existing,   true},
                     {replace_interests, false},
                     {batch,             {array, Batch2}}
                    ]},
    Json2 = lists:flatten(mochijson:encode(Json)),
    URL = ?MAILCHIMP ++ "?method=listBatchSubscribe",
    Ret = httpc:request(post, {URL, [], ["application/json"], Json2}, [], []),
    {ok, {{_, _Code, _}, _, _Body}} = Ret,
    ok.

%% need to munge the XML in a couple of different ways
get_highrise_id(XML, existinguser) ->
    #xmlElement{content = [_C1, C2 | _R]} = element(1, XML),
    #xmlElement{content = List} = C2,
    get(List, id);
get_highrise_id(XML, newuser) ->
    #xmlElement{content = List} = element(1, XML),
    get(List, id).

get([], _)                                            -> "";
get([#xmlElement{name = Id, content = [C]} | _T], Id) -> get_val(C);
get([_H | T], Id)                                     -> get(T, Id).

get_val(#xmlText{value = V}) -> V.

create_user_in_highrise(XML) ->
    Headers = get_highrise_headers(),
    URL = ?HIGHRISE ++ "/people.xml",
    Ret = httpc:request(post, {URL, Headers, ["application/xml"], XML}, [], []),
    {ok, {{_, 201, _}, _, Body}} = Ret,
    Details = xmerl_scan:string(Body),
    _Id = get_highrise_id(Details, newuser).

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
                comm_site = CS, comm_path = CP,
                comm_cell = CC} = Commission,
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
