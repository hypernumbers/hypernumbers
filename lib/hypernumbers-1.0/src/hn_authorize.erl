%%% @copyright 2008 Hypernumbers Ltd
%%% @doc authorize http requests
-module(hn_authorize).

-include("hypernumbers.hrl").

-include("hn_mochi.hrl").
-include("spriki.hrl").
-include("keyvalues.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(check_pt_vw(A, B, C, D), auth_srv:check_particular_view(A, B, C, D)).
-define(ISAUTH,     #api_auth{authorized = true, append_only = false}).
-define(ISNOTAUTH,  #api_auth{authorized = false}).
-define(ISADMIN,    #api_auth{authorized = true, admin = true, append_only = false}).
-define(ISNOTADMIN, #api_auth{authorized = true, admin = false, append_only = false}).
-define(APPENDONLY, #api_auth{authorized = true, append_only = true}).
-define(ANYAPI,     #api_auth{}).

-export([
         authorize/3,
         authorize_upload_again/3
        ]).

authorize(Env, #refX{site = S} = Ref, Qry) ->
    API = process_api(Ref, Env),
    Auth = case API of
               ?ANYAPI -> PubK = API#api_auth.publickey,
                          EMail = new_db_wu:make_api_email(S, PubK),
                          {ok, UID} = passport:email_to_uid(EMail),
                          {user, Env#env{uid = UID, email = EMail}};
               _       -> process_user(Ref, Env)
           end,
    case Auth of
        {user, Env2} ->
            #env{method = Method, body = Body} = Env,
            Return = case {Method, Body} of
                         {Req, _} when Req == 'GET'; Req == 'HEAD' ->
                             auth_get(Ref, Qry, Env2, API);
                         {'POST', multipart} ->
                             authorize_upload(Ref, Qry, Env2, API);
                         {'POST', _} ->
                             auth_post(Ref, Qry, Env2, API)
                     end,
            {Return, Env2};
        {303, Env2} ->
            {303, Env2}
    end.

%% Specifically allow access to the json permissions. Only the permissions
%% query may be present.
%% TODO: Only admins should be able to do this...
-spec auth_get(#refX{}, #qry{}, #env{}, [#api_auth{} | not_api])
-> {view, string()} | allowed | denied | not_found.
% dont think this is used any more...
% auth_get(_Ref,
%              #qry{permissions = [], _ = undefined},
%              #env{accept = html}) ->
%    allowed;

%% Authorize access to 'special' commands.
%% TODO put permissions access on _invite and _logout
auth_get(#refX{path = [X | Rest]}, _Qry, #env{accept = html}, not_api)
  when X == "_invite";
       X == "_mynewsite";
       X == "_validate";
       X == "_authorize" ->
    case Rest of
        [] -> denied;
        _  -> allowed
    end;

% never allowed from the api
auth_get(#refX{path = [X | Rest]}, _Qry, #env{accept = html}, ?ANYAPI)
  when X == "_invite";
       X == "_mynewsite";
       X == "_validate";
       X == "_authorize" ->
    case Rest of
        [] -> denied;
        _  -> denied
    end;

% the logout command
auth_get(#refX{path = [X | Rest]}, _Qry, #env{accept = Accept}, not_api)
  when X == "_logout" ->
    case {Rest, Accept} of
        {[], html} -> allowed;
        _          -> denied
    end;

% never allowed from the api
auth_get(#refX{path = ["_logout" |_Rest]}, _Qry, #env{}, ?ANYAPI) ->
    denied;

% deal with the reserved part of the namespace
% shows sites and pages
auth_get(#refX{path = [X | []]}, _Qry, #env{accept = Ac}, not_api)
  when X == "_site";
       X == "_pages" ->
    case Ac of
        json -> allowed;
        html -> denied
    end;

auth_get(#refX{path = [X | []]}, _Qry, #env{accept = Ac}, ?ISAUTH)
  when X == "_site";
       X == "_pages" ->
    case Ac of
        json -> allowed;
        html -> denied
    end;

% only let admins see the statistics pages
auth_get(#refX{site = S, path = ["_statistics" | []]}, _Qry,
         #env{accept = html, uid = UID}, not_api) ->
    case hn_groups:is_member(UID, S, ["admin"]) of
        true  -> allowed;
        false -> denied
    end;

auth_get(#refX{path = ["_statistics" | []]}, _Qry,
         #env{accept = json}, ?ANYAPI) ->
    denied;

% enable the _sites page and also _replies and _contacts for phone and form
% pages on the root page
%
% need to do it twice, once for no view set, and once for a specific view
% only '_audit' can have subpages
auth_get(#refX{site = Site, path = ["_" ++ X | Rest] = Path},
         #qry{_ = undefined}, #env{accept = Accept, uid = UID}, not_api)
  when Accept == html
       orelse Accept == json,
       X == "sites"    orelse
       X == "replies"  orelse
       X == "contacts" orelse
       X == "audit" ->
    case {X, Rest} of
        {"audit", _} ->
            auth_srv:check_get_view(Site, Path, UID);
        {_, []} ->
            auth_srv:check_get_view(Site, Path, UID);
        {_, _} ->
            not_found
    end;

auth_get(#refX{site = S, path = ["_" ++ X | Rest] = P},
         #qry{view = View}, #env{accept = Accept, uid = UID}, not_api)
  when Accept == html
       orelse Accept == json,
       X == "sites"    orelse
       X == "replies"  orelse
       X == "contacts" orelse
       X == "audit" ->
    case {X, Rest} of
        {"audit", _} ->
            auth_srv:check_particular_view(S, P, UID, View);
        {_, []} ->
            auth_srv:check_particular_view(S, P, UID, View);
        {_, _} ->
            not_found
    end;

%% allow the API
auth_get(#refX{path = ["_" ++ X | Rest]},
         #qry{_ = undefined}, #env{accept = Accept}, ?ISAUTH)
  when Accept == html
       orelse Accept == json,
       X == "sites"
       orelse X == "replies"
       orelse X == "contacts"
       orelse X == "audit" ->
    case {X, Rest} of
        {"audit", _} ->
            allowed;
        {_, []} ->
            allowed;
        {_, _} ->
            not_found
    end;

auth_get(#refX{path = ["_" ++ X | Rest]},
         #qry{view = _View}, #env{accept = Accept}, ?ANYAPI = API)
  when Accept == html
       orelse Accept == json,
       X == "sites"    orelse
       X == "replies"  orelse
       X == "contacts" orelse
       X == "audit" ->
    case {X, Rest, API} of
        {_, [], ?ISNOTAUTH} ->
            denied;
        {"audit", _, ?ISNOTAUTH} ->
            denied;
        {_, _, ?ISNOTAUTH} ->
            not_found;
        {"audit", _, _} ->
            allowed;
        {_, [], _} ->
            allowed;
        {_, _, _} ->
            not_found
    end;

% Feature Flag them out
% this path is hardwired into the module hn_twilio_mochi.erl
auth_get(#refX{path = ["_services", "phone" | _], obj = {page, _}},
         _Qry, #env{accept = html}, not_api) ->
    allowed;

auth_get(#refX{path = ["_services", "phone" | _], obj = {page, _}},
         _Qry, #env{accept = html}, ?ANYAPI) ->
    denied;

auth_get(#refX{path = ["_services", "phoneredirect" | _], obj = {page, _}},
         _Qry, #env{accept = html}, not_api) ->
    allowed;

% this api is NOT being tested yet!
auth_get(#refX{path = ["_services", "phoneredirect" | _], obj = {page, _}},
         _Qry, #env{accept = html}, ?ANYAPI) ->
    denied;

auth_get(#refX{site = "http://usability.hypernumbers.com:8080",
               path = ["_reprovision"], obj = {page, "/"}}, _Qry, _Env,
         not_api) ->
    allowed;

auth_get(#refX{site = "http://usability.hypernumbers.com:8080",
               path = ["_reprovision"], obj = {page, "/"}}, _Qry, _Env,
         ?ANYAPI) ->
    denied;

%% Only some sites have a forgotten password box
auth_get(#refX{path = [X | _Vanity]}, _Qry, #env{accept = Accept}, not_api)
  when X == "_forgotten_password" ->
    case {Accept, passport_running()} of
        {html, true} -> allowed;
        _            -> denied
    end;

auth_get(#refX{path = [X | _Vanity]}, _Qry, #env{}, ?ANYAPI)
  when X == "_forgotten_password" ->
    denied;

% deny all pages starting in _ that we haven't previously allowed
auth_get(#refX{path = ["_" ++ _X | _]}, _Qry, #env{accept = Ac}, not_api)
  when Ac == json;
       Ac == html ->
    denied;
auth_get(#refX{path = ["_" ++ _X | _]}, _Qry, #env{accept = Ac}, ?ANYAPI)
  when Ac == json;
       Ac == html ->
    denied;

%% Authorize update requests when the update is targeted towards a
%% spreadsheet. Since we have no closed security object, we rely on
%% 'run-time' checks.
auth_get(#refX{site = Site, path = [[Prefix | _Root] | _T] = Path},
         #qry{updates = U, view = ?SHEETVIEW, paths = More},
         #env{accept = json, uid = UID}, not_api)
  when U /= undefined andalso
       Prefix /= "_" ->
    case auth_srv:check_particular_view(Site, Path, UID, ?SHEETVIEW) of
        {view, ?SHEETVIEW} ->
            Fun1 = fun(X) ->
                           Tks = string:tokens(X, "/"),
                           auth_srv:get_any_main_view(Site, Tks, UID)
                   end,
            MoreViews = [Fun1(P) || P <- string:tokens(More, ",")],
            Fun2 = fun
                       ({view, _}) -> true;
                (_)         -> false
                           end,
    case lists:all(Fun2, MoreViews) of
        true  -> allowed;
        _Else -> denied
    end;
_Else ->
    denied
end;
% but dont give an http view
auth_get(#refX{path = [[Prefix | _Root] | _T] = _Path},
         #qry{updates = U, view = ?SHEETVIEW},
         #env{accept = html}, not_api)
  when U /= undefined andalso
       Prefix /= "_" ->
    not_found;
% or an api view
auth_get(#refX{path = [[Prefix | _Root] | _T]},
         #qry{updates = U, view = ?SHEETVIEW},
         #env{}, ?ANYAPI)
  when U /= undefined andalso
       Prefix /= "_" ->
    denied;

%% Authorize access to the DEFAULT page. Notice that no query
%% parameters have been set.
auth_get(#refX{site = Site, path = Path},
         #qry{_ = undefined},
         #env{accept = html, uid = UID}, not_api) ->
    auth_srv:check_get_view(Site, Path, UID);
% authorize the api
auth_get(#refX{}, #qry{_ = undefined}, #env{accept = html}, ?ISAUTH) ->
    allowed;

%% Scrap the challenger view
%% Authorize access to the challenger view.
%% auth_get(#refX{site = Site, path = Path},
%%               #qry{challenger = []},
%%               #env{accept = html, uid = UID}) ->
%%     auth_srv:check_get_challenger(Site, Path, UID);

% a recording is stored against a link with a hypertag signed with its url
% so authorize anyone with any view of that page to see it
auth_get(#refX{site = Site, path = Path},
         #qry{view = ?RECORDING}, #env{accept = html, uid = UID},
         not_api) ->
    case auth_srv:get_any_main_view(Site, Path, UID) of
        {view, _} -> allowed;
        _Else     -> denied
    end;
% deny json
auth_get(#refX{},#qry{view = ?RECORDING}, #env{accept = json}, not_api) ->
    denied;
% it is not stored in json so no point here
auth_get(#refX{}, #qry{view = ?RECORDING}, _Env, ?ISAUTH) ->
    denied;

% allow all DEBUG views
auth_get(#refX{site = S}, #qry{view = ?DEBUG},
         #env{accept = html, uid = UID}, not_api) ->
    case hn_groups:is_member(UID, S, ["admin"]) of
        true  -> allowed;
        false -> denied
    end;
% deny json view
auth_get(#refX{}, #qry{view = ?DEBUG}, #env{accept = json}, not_api) ->
    denied;
% deny api view
auth_get(#refX{}, #qry{view = ?DEBUG}, #env{}, ?ANYAPI) ->
    denied;

% you can only see the logs if you have the spreadsheet view
auth_get(R, #qry{view = ?LOGVIEW} = Q, E, not_api) ->
    case auth_get(R, Q#qry{view = ?SHEETVIEW}, E, not_api) of
        {view, ?SHEETVIEW} -> {view, ?LOGVIEW};
        Other              -> Other
    end;
auth_get(#refX{}, #qry{view = ?LOGVIEW}, #env{}, ?ISAUTH) ->
    allowed;

% you can only force a recalc if you have the spreadsheet view
% don't let the api do this
auth_get(R, #qry{view = ?RECALC} = Q, #env{accept = html} = E, not_api) ->
    case auth_get(R, Q#qry{view = ?SHEETVIEW}, E, not_api) of
        {view, ?SHEETVIEW} -> {view, ?RECALC};
        Other              -> Other
    end;
auth_get(_R, #qry{view = ?RECALC}, #env{accept = json}, not_api) ->
    denied;
auth_get(_R, #qry{view = ?RECALC}, _E, ?ANYAPI) ->
    denied;

%% Allow the softphone if there is a softphone control on the cell
%% for both html and json
% don't let the api have it
auth_get(#refX{site = S, path = P, obj = {cell, _}} = R,
         #qry{view = ?PHONE}, Env, not_api) ->
    case new_db_api:get_phone(R) of
        []       -> denied;
        [_Phone] -> case auth_srv:get_any_main_view(S, P, Env#env.uid) of
                        denied -> denied;
                        _Other -> allowed
                    end
    end;
auth_get(#refX{obj = {cell, _}} = _R, #qry{view = ?PHONE}, _E, ?ANYAPI) ->
    denied;

%% Authorize access to one particular view.
auth_get(#refX{site = Site, path = Path}, #qry{view = View},
         #env{uid = UID}, not_api)
  when View /= undefined ->
    auth_srv:check_particular_view(Site, Path, UID, View);
% ok for api
auth_get(#refX{}, #qry{view = View}, #env{}, #api_auth{authorized = true})
  when View /= undefined ->
    allowed;

%% As a last resort, we will authorize a GET request to a location
%% from which we have a view.
auth_get(#refX{site = Site, path = Path}, _Qry, Env, not_api) ->
    case auth_srv:get_any_main_view(Site, Path, Env#env.uid) of
        {view, _} -> allowed;
        _Else     -> denied
    end;
% api can get an authorised page
auth_get(#refX{}, #qry{}, #env{}, ?ISAUTH) ->
    allowed;
auth_get(#refX{}, #qry{}, #env{}, ?ISNOTAUTH) ->
    denied.

-spec auth_post(#refX{}, #qry{}, #env{}, [#api_auth{} | not_api])
-> allowed | denied | not_found.

%% Allow some special posts to occur
auth_post(#refX{path = [X]}, _Qry,
          #env{accept = json}, not_api)
  when X == "_login";
       X == "_forgotten_password" ->
    allowed;
auth_post(#refX{site = S, path = [X] = P}, _Qry,
          #env{uid = UID, accept = json}, not_api)
  when X == "_parse_expression" ->
    case auth_srv:check_particular_view(S, P, UID, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        _                  -> denied
    end;
% never allowed from the api
auth_post(#refX{path = [X]}, _Qry, #env{accept = json}, ?ANYAPI)
  when X == "_login";
       X == "_forgotten_password";
       X == "_parse_expression" ->
    denied;

auth_post(#refX{site = Site, path = ["_admin"]}, _Qry,
          #env{accept = json, uid = UID} = Env, not_api) ->
    case hn_groups:is_member(UID, Site, ["admin"]) of
        true  -> allowed;
        false -> authorize_admin(Site, Env#env.body, UID)
    end;
% check the admin on the api
auth_post(#refX{path = ["_admin"]}, _Qry, #env{accept = json}, ?ISADMIN) ->
    allowed;

% hammerban all other posts to paths starting in underscore, api or no
auth_post(#refX{path = [[$_ | _] | _]}, #qry{}, #env{accept = json}, _) ->
    denied;

% allow a post to a phone view for a cell - gonnae check it later
% auth_post(#refX{obj = {cell, _}}, #qry{view = ?PHONE},
%               #env{accept = json}) ->
%    allowed;

% always let jserrs through
auth_post(_Ref, _Qry, #env{body = [{"jserr", _}]}, not_api) ->
    allowed;
% but not for apis
auth_post(_Ref, _Qry, #env{body = [{"jserr", _}]}, ?ANYAPI) ->
    denied;

%% allow a lot of stuff through
auth_post(#refX{site = Site, path = Path}, _Qry,
          #env{body = [{Action, _}], uid = UID}, not_api)
  when Action == "load_template" orelse
       Action == "drag"          orelse
       Action == "insert"        orelse
       Action == "delete"        orelse
       Action == "copy"          orelse
       Action == "copystyle"     orelse
       Action == "copyvalue"     orelse
       Action == "revert_to" ->
    case ?check_pt_vw(Site, Path, UID, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        denied             -> denied
    end;
auth_post(#refX{}, _Qry, #env{body = [{Action, _}]}, ?ISAUTH)
  when Action == "load_template" orelse
       Action == "drag"          orelse
       Action == "insert"        orelse
       Action == "delete"        orelse
       Action == "copy"          orelse
       Action == "copystyle"     orelse
       Action == "copyvalue"     orelse
       Action == "revert_to" ->
    allowed;

%% allow the user to change aspects of the view or invite a user to a page if
%% they have a spreadsheet view
auth_post(#refX{site = Site, path = Path}, _Qry,
          #env{body = [{Action, _}]} = Env, not_api)
  when Action == "default_view" orelse
       Action == "set_view"     orelse
       Action == "invite_user" ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        not_found          -> not_found;
        denied             -> authorize_p2(Site, Path, Env)
    end;
%% some are allowed by api
auth_post(#refX{}, _Qry, #env{body = [{Action, _}]}, ?ISAUTH)
  when Action == "default_view" orelse
       Action == "set_view"     orelse
       Action == "invite_user" ->
    allowed;
%% some are never
auth_post(#refX{}, _Qry, #env{body = [{Action, _}]}, ?ISAUTH)
  when Action == "mark"           orelse
       Action == "postform"       orelse
       Action == "postinline"     orelse
       Action == "postrichinline" orelse
       Action == "postwebcontrols" ->
    denied;

%% allow a lot of stuff through
auth_post(#refX{}, _Qry, #env{body = [{Act, _}]}, not_api)
  when Act == "append" ->
    denied;
%% Yay! append_only gets to do something at last
auth_post(#refX{obj = {page, "/"}}, _Qry, #env{body = [{Act, _}]}, ?APPENDONLY)
  when Act == "append" ->
    allowed;
%% Ok, ok, normal API's can too
auth_post(#refX{obj = {page, "/"}}, _Qry, #env{body = [{Act, _}]}, ?ISAUTH)
  when Act == "append" ->
    allowed;
%% blah-blah drop the hammerban
auth_post(#refX{}, _Qry, #env{body = [{Act, _}]}, ?ANYAPI)
  when Act == "append" ->
    denied;

%% allow the user to save the page as a template or add groups or users
%% if they are the admin
auth_post(#refX{site = Site}, _Qry, #env{body = [{Action, _}],
                                         uid = UID}, not_api)
  when Action == "save_template" orelse
       Action == "add_group"     orelse
       Action == "add_user" ->
    case hn_groups:is_member(UID, Site, ["admin"]) of
        true  -> allowed;
        false -> denied
    end;
%% saving templates is reserved to the admin apis
auth_post(#refX{}, _Qry, #env{body = [{Action, _}]}, ?ISADMIN)
  when Action == "save_template" ->
    allowed;
%% adding users and groups, pff!, any old walk-in can do that!
auth_post(#refX{}, _Qry, #env{body = [{Action, _}]}, ?ISAUTH)
  when Action == "add_group" orelse
       Action == "add_user" ->
    allowed;

%% Allow a post to occur, if the user has access to a spreadsheet on
%% the target. But it might be a post from a form or an inline
%% update so you need to check for them too before allowing
%% the post to continue...
auth_post(#refX{site = Site, path = Path}, _Qry, Env, not_api) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?SHEETVIEW) of
        {view, ?SHEETVIEW} -> allowed;
        not_found          -> not_found;
        denied             -> authorize_p2(Site, Path, Env)
    end;
%% there is no api equivalent

%% kill all other postr
auth_post(_Ref, _Qry, _Env, not_api) ->
    denied;
%% stop all not authorised api calls
auth_post(_Ref, _Qry, _Env, ?ANYAPI) ->
    denied.

% WIKI's can take both 'postform' and 'postinline'
% 'mark's always get through
authorize_p2(Site, Path, Env) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?WIKI) of
        not_found ->
            authorize_p3(Site, Path, Env);
        {view, ?WIKI} ->
            case Env#env.body of
                [{"mark",   _}]          -> allowed;
                [{"postform",   _}]      -> allowed;
                [{"postinline", _}]      -> allowed;
                [{"postrichinline", _}]  -> allowed;
                [{"postwebcontrols", _}] -> allowed;
                _                        -> denied
            end;
        denied ->
            authorize_p3(Site, Path, Env)
    end.

% WEBPAGE's can only do 'postform'
% 'mark's always get through
authorize_p3(Site, Path, Env) ->
    case ?check_pt_vw(Site, Path, Env#env.uid, ?WEBPAGE) of
        not_found        ->
            denied;
        {view, ?WEBPAGE} ->
            case Env#env.body of
                [{"mark",   _}]          -> allowed;
                [{"postform",   _}]      -> allowed;
                [{"postwebcontrols", _}] -> allowed;
                _                        -> denied
            end;
        % jakub's clause
        denied           ->
            case Env#env.body of
                [{"read_user_fn~", _Args}]	-> denied;
                [{"delete_user_fn", _Args}]	-> denied;
                [{"write_user_fn", _Args}]	-> denied;
                _						        				-> denied
            end
    end.

authorize_upload(#refX{site = S, path = P}, _Qry, #env{uid = UID}, not_api) ->
    Views = auth_srv:get_views(S, P, UID),
    case has_appropriate_view(Views) of
        false -> {upload, denied};
        true  -> allowed
    end.

has_appropriate_view([])                -> false;
has_appropriate_view([?SHEETVIEW | _T]) -> true;
has_appropriate_view([?WEBPAGE | _T])   -> true;
has_appropriate_view([?WIKI | _T])      -> true;
has_appropriate_view([_H | T])          -> has_appropriate_view(T).

authorize_admin(_Site, [{"admin", {_, [{"set_password", _}]}}], UID) ->
    case passport:uid_to_email(UID) of
        {ok, "anonymous"} -> denied;
        _                 -> allowed
    end;

authorize_admin(Site, [{"admin", {_, [{Request, {_, List}}]}}], UID)
  when (Request == "set_view")
       orelse (Request == "set_champion")
       orelse (Request == "invite_user") ->
    case passport:uid_to_email(UID) of
        {ok, "anonymous"} -> denied;
        _                 ->
            case lists:keyfind("path", 1, List) of
                false       -> denied;
                {"path", P} -> P2 = string:tokens(P, "/"),
                               case ?check_pt_vw(Site, P2, UID, ?SHEETVIEW) of
                                   {view, ?SHEETVIEW} -> allowed;
                                   denied             -> denied
                               end
            end
    end.

-spec process_api(#refX{}, #env{}) -> #api_auth{}.
process_api(#refX{site = S, path = P}, Env) ->
    #env{mochi = Mochi,
         raw_body = RawBody, method = Method} = Env,
    case mochiweb_headers:lookup('Authorization', Mochi:get(headers)) of
        none ->
            not_api;
        {value, {'Authorization', _Auth}} ->
            case hmac_api_lib:authorize_request(S, Env#env.mochi) of
                {"match", #api{urls = API_URLs}, PublicKey} ->
                    case check_MD5(Method, RawBody, Mochi) of
                        valid   -> check_validity(S, P, API_URLs, PublicKey);
                        invalid -> tampered
                    end;
                {"no match", PublicKey} ->
                    #api_auth{publickey = PublicKey, authorized = false};
                {"no key", PublicKey} ->
                    #api_auth{publickey = PublicKey, authorized = false}
            end
    end.

check_validity(Site, Path, URLs, PublicKey) ->
    Admin = is_admin(URLs),
    {Auth, Append} = check_path(URLs, Site, Path),
    #api_auth{publickey = PublicKey, authorized = Auth,
              admin = Admin, append_only = Append}.

check_path([], _S, _P) ->
    {false, false};
check_path([URL | T], Site, Path) ->
    #api_url{path = P1, include_subs = IS, append_only = AO} = URL,
    Path1 = hn_util:parse_zpath(string:tokens(P1, "/")),
    case check_p2(Path1, Path, Site, [], IS) of
        true  -> {true, AO};
        false -> check_path(T, Site, Path)
    end.

% paths match
check_p2([], [], _, _, _) ->
    true;
% paths match to now, and sub-pages are included
check_p2([], _, _, _, true) ->
    true;
% paths match to now, and sub-pages are not included
check_p2([], _, _, _, false) ->
    false;
% run out of segments to match its a fail
check_p2(_P, [], _, _, _) ->
    false;
check_p2([{seg, S} | T1], [S | T2], Site, Htap, IS) ->
    check_p2(T1, T2, Site, [S | Htap], IS);
% paths don't match so terminate
check_p2([{seg, _S1} | _T1], [_S2 | _T2], _Site, _Htap, _IS) ->
    false;
check_p2([{zseg, Z} | T1], [S | T2], Site, Htap, IS) ->
    case new_db_api:run_zevalD(Site, lists:reverse(Htap), Z) of
        {_, true}               -> false;
        {match, false}          -> check_p2(T1, T2, Site, [S | Htap], IS);
        {nomatch, false}        -> false;
        {errval, _Err}          -> false; % dunno where this came from
        {{errval, _Err}, false} -> false;
        {{error, _}, false}     -> false % Old style errs from fns
                                   % (shouldn't exist!)
    end.

is_admin([])                            -> false;
is_admin([#api_url{admin = true} | _T]) -> true;
is_admin([_H | T])                      -> is_admin(T).

-spec process_user(#refX{}, #env{}) -> #env{} | no_return().
% for twilio api calls we spoof the username
process_user(#refX{path = ["_services", SubPath]}, E = #env{})
  when SubPath == "phone" orelse SubPath == "phoneredirect" ->
    Email = "api@twilio.com",
    {ok, _, UID} = passport:get_or_create_user(Email),
    {user, E#env{uid = UID, email = Email}};
process_user(#refX{site = Site} = Ref, E = #env{mochi = Mochi}) ->
    Auth = Mochi:get_cookie_value("auth"),
    try passport:inspect_stamp(Auth) of
        {ok, UID, Email} ->
            {user, E#env{uid = UID, email = Email}};
        {error, no_stamp} ->
            case is_wordpress(Site) of
                {true, Params} ->
                    Env2 = hn_wordpress:get_logon(Ref, E, Params),
                    {303, Env2};
                false ->
                    Return = cur_url(Site, E),
                    case hn_mochi:try_sync(["seek"], Site, Return, ?NO_STAMP) of
                        on_sync ->
                            Stamp = passport:temp_stamp(),
                            {ok, U2, E2} = passport:inspect_stamp(Stamp),
                            Cookie = hn_net_util:cookie("auth", Stamp, "never"),
                            {user, E#env{uid = U2, email = E2,
                                         headers = [Cookie | E#env.headers]}};
                        {redir, Redir} ->
                            Hs2 = [{"location", Redir}| E#env.headers],
                            E2 = E#env{headers = Hs2},
                            {303, E2}
                    end
            end;
        {error, _Reason} ->
            hn_mochi:cleanup(Site, cur_url(Site, E), E)
    catch error:
                _Other -> hn_mochi:cleanup(Site, cur_url(Site, E), E)
                         end.

passport_running() ->
    {ok, Services} = application:get_env(hypernumbers, services),
    case lists:keysearch(passport, 1, Services) of
        {value, {passport, false}} -> false;
        {value, {passport, true}}  -> true
    end.

is_wordpress(Site) ->
    case new_db_api:read_kv(Site, ?wordpress) of
        []                          -> false;
        [{kvstore, wordpress, List}] -> {true, List}
    end.

%% Returns the url representing the current location.s
-spec cur_url(string(), #env{}) -> string().
cur_url(Site, #env{mochi = Mochi}) ->
    hn_util:strip80(Site) ++ Mochi:get(raw_path).

%% TODO not sure if all these cases are used still...
authorize_upload_again(#refX{site = S, path = P}, file, UID) ->
    Views = auth_srv:get_views(S, P, UID),
    lists:member(?SHEETVIEW, Views);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {load_templates, Template}, _UID) ->
    Expected = new_db_api:matching_forms(RefX, 'load-template-button'),
    has_load_templates(Expected, Template);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {row, Map}, _UID) ->
    Expected = new_db_api:matching_forms(RefX, 'map-rows-button'),
    has_map_row(Expected, Map);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {sheet, Map, Page}, _UID) ->
    Expected = new_db_api:matching_forms(RefX, 'map-sheet-button'),
    has_map_sheet(Expected, Map, Page);
authorize_upload_again(#refX{site = _S, path = _P} = RefX,
                       {custom, Map}, _UID) ->
    Expected = new_db_api:matching_forms(RefX, 'map-custom-button'),
    has_map_custom(Expected, Map).

has_load_templates([], _Template) -> false;
has_load_templates([H | T], Template) ->
    case H of
        {form, _, {_, 'load-template-button', _}, _, _,
         {struct, [{"load_templates", Template}]}} ->
            true;
        _ ->
            has_load_templates(T, Template)
    end.

has_map_sheet([], _Map, _Page) -> false;
has_map_sheet([H | T], Map, Page) ->
    case H of
        {form, _, {_, 'map-sheet-button', _}, _, _,
         {struct, [{"map", Map}, {"page", Page}]}} ->
            true;
        _ ->
            has_map_sheet(T, Map, Page)
    end.

has_map_row([], _Map) -> false;
has_map_row([H | T], Map) ->
    case H of
        {form, _, {_, 'map-rows-button', _}, _, _,
         {struct, [{"map", Map}]}} ->
            true;
        _ ->
            has_map_row(T, Map)
    end.

has_map_custom([], _Map) -> false;
has_map_custom([H | T], Map) ->
    case H of
        {form, _, {_, 'map-custom-button', _}, _, _,
         {struct, [{"map", Map}]}} ->
            true;
        _ ->
            has_map_custom(T, Map)
    end.

check_MD5('GET', _Body, _Mochi) ->
    valid;
check_MD5('POST', Body, Mochi) ->
    {_, {_, GotMD5}} = mochiweb_headers:lookup('Content-MD5', Mochi:get(headers)),
    ExpectedMD5 = binary_to_list(crypto:md5(Body)),
    case GotMD5 of
        ExpectedMD5 -> valid;
        _           -> tampered
    end.

%%%===================================================================
%% EUnit Tests
%%%===================================================================
-define(SITE, "http://example.com").
test1([]) ->
    API = #api_url{path = "/bish/bash/bosh/"},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({true, true}, Got).

test2([]) ->
    API = #api_url{path = "/bish/bash/pish/"},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({false, false}, Got).

test3([]) ->
    API = #api_url{path = "/bish/bash/", include_subs = false},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({false, false}, Got).

test4([]) ->
    API = #api_url{path = "/bish/bash/", include_subs = true},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({true, true}, Got).

test5([]) ->
    API = #api_url{path = "/bish/bash/bosh/perk/", include_subs = false},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({false, false}, Got).

test6([]) ->
    API = #api_url{path = "/bish/bash/bosh/perk/", include_subs = true},
    Got = check_path([API], ?SITE, ["bish", "bash", "bosh"]),
    ?assertEqual({false, false}, Got).

test7([]) ->
    API = #api_url{path = "/", include_subs = true},
    Got = check_path([API], ?SITE, ["logout"]),
    ?assertEqual({true, true}, Got).

unit_test_() ->

    Setup = fun() -> ok end,

    SeriesA = [
               fun test1/1,
               fun test2/1,
               fun test3/1,
               fun test4/1,
               fun test5/1,
               fun test6/1,
               fun test7/1
              ],

    %{setup, Setup, Cleanup,
    {setup, Setup, [{with, [], SeriesA}]}.
