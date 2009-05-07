%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       This module implements the horizontal api that connects
%%%            two hypernumbers servers.
%%%            
%%%            It manages the <code>notify</code> and 
%%%            <code>notify_back</code> messages between servers as
%%%            well as the synchronisation of the page versions
%%%
%%% @TODO      write the synchronisation of the page version :(
%%%
%%% @end
%%% Created :  8 Mar 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(horiz_api).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-export([notify/1,
         notify_back/1,
         notify_back_create/1]).

-define(init, initialise_remote_page_vsn).
-define(api, hn_db_api).

%% @spec notify(Record::#dirty_notify_out{}) -> ok
%% @doc notifies any remote sites that a hypernumber has changed.
%% the reference must be for a cell
%% @todo generalise the references to row, column, range and page
%% the structure of the dirty_notify_out is leaking out here because
%% we use timestamps as identifiers instead of unique keys...
%% makes it hard to delete
%% @todo make me robust with retries...!
%% @todo spawn the notifies so they are concurrent not sequential...
notify(Record) when is_record(Record, dirty_notify_out) ->
    #dirty_notify_out{parent = Parent,
                      change = Change,
                      outgoing = Outgoing,
                      parent_vsn = PVsn} = Record,
    PVJson = json_util:jsonify(PVsn),
    PUrl = hn_util:refX_to_url(Parent),
    Fun2 = fun({X, ChildVsn}) ->
                   CVJson = json_util:jsonify(ChildVsn),
                   Server = X#outgoing_hn.child_proxy,
                   Biccie = X#outgoing_hn.biccie,
                   
                   {Type, P} = json_util:payload_to_json(Change),
                   Vars = {struct, [{"action",     "notify"},
                                    {"biccie",     Biccie},
                                    {"parent_url", PUrl},
                                    {"type",       Type},
                                    {"payload",    P},
                                    {"child_vsn",  CVJson},
                                    {"parent_vsn", PVJson}
                                   ]},
                   Actions = lists:flatten(mochijson:encode(Vars)),

                   % Logging code 
                   % Str = "POSTING £" ++ pid_to_list(self()) ++ "£" ++ PUrl ++
                   %    "£ To £" ++ Server ++ json_util:to_str([Vars]),
                   % bits:log(Str),
                   
                   "success" = hn_util:post(Server, Actions, "application/json"),
                   ok
           end,
    [ok = Fun2(X) || X <- Outgoing],
    ok.

%% @spec notify_back(Record::#dirty_notify_back_in{}) -> ok
%% @doc notify's a change of a cell back to its remote hypernumber parent
%% <code>#refX{}</code> can be a cell only
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents.
notify_back(Record) when is_record(Record, dirty_notify_back_in) ->
    #dirty_notify_back_in{parent = Parent, child = Child,
                          change = Change, biccie = Biccie,
                          parent_vsn = PVsn, child_vsn = CVsn} = Record,
    CUrl=hn_util:refX_to_url(Child),
    PUrl=hn_util:refX_to_url(Parent),
    CVsJson = json_util:jsonify(CVsn),
    PVsJson = json_util:jsonify(PVsn),
    Vars = {struct, [{"action",     "notify_back"},
                     {"biccie",     Biccie},
                     {"child_url",  CUrl},
                     {"parent_url", PUrl},
                     {"type",       Change},
                     {"parent_vsn", PVsJson},
                     {"child_vsn",  CVsJson}]},
    Actions = lists:flatten(mochijson:encode(Vars)),

    % Logging code 
    % Str = "POSTING £" ++ pid_to_list(self()) ++ "£" ++ CUrl ++ "£ To £"
    %    ++ PUrl ++ json_util:to_str([Vars]),
    % bits:log(Str),

    %% not very robust!
    "success" = hn_util:post(PUrl, Actions, "application/json"),
    ok.

%% @spec notify_back_create(Record::#dirty_inc_hn_create{}) -> ok
%% @doc creates a new hypernumbers.
%% Both the parent and the child references must point to a cell
notify_back_create(Record) when is_record(Record, dirty_inc_hn_create) ->

    #dirty_inc_hn_create{parent = Parent, child = Child,
                         parent_vsn = PVsn, child_vsn = CVsn} = Record,   

    PVsn2 = json_util:jsonify(PVsn),
    CVsn2 = json_util:jsonify(CVsn),
    PUrl = hn_util:refX_to_url(Parent),
    CUrl = hn_util:refX_to_url(Child),

    Biccie = util2:bake_biccie(),

    #refX{site = CSite, path = CPath} = Child,
    Proxy = CSite ++ "/" ++ string:join(CPath, "/") ++ "/",

    %PPage = Parent#refX{obj = {page, "/"}},
    
    Vars = {struct, [{"action",     "notify_back_create"},
                     {"biccie",     Biccie},
                     {"proxy",      Proxy},
                     {"child_url",  CUrl},
                     {"parent_vsn", PVsn2},
                     {"child_vsn",  CVsn2}]},
    Actions = lists:flatten(mochijson:encode(Vars)),

    % Logging code 
    % Str = "POSTING £" ++ pid_to_list(self()) ++ "£" ++ CUrl ++ "£ To £"
    %    ++ PUrl ++ json_util:to_str([Vars]),
    % bits:log(Str),

    case http:request(post, {PUrl, [], "application/json", Actions}, [], []) of
        {ok, {{_V, 200, _R}, _H, Json}} ->
            {struct, [{"value",            Value},
                      {"__dependecy-tree", DepTree},
                      {"parent_vsn",       NewPVsnJson}]}
                = mochijson:decode(Json),
            NewPVsn = json_util:unjsonify(NewPVsnJson),
            % check that the pages are in sync
            {xml, [], DepTree2} = simplexml:from_xml_string(DepTree),
            case ?api:check_page_vsn(CSite, NewPVsn) of
                synched         -> {Value, DepTree2, Biccie, NewPVsn};
                not_yet_synched -> {ok,ok} = ?api:?init(CSite, NewPVsn),
                                   {Value, DepTree2, Biccie, PVsn};
                unsynched       -> {error, unsynched, PVsn}
            end;
        {ok, {{_V, 503, _R}, _H, _Body}} ->
            io:format("-returned 503~n"),
            io:format("permission has been denied - need to write an error "++
                      "to the hypernumber here...~n"),
            {error, permission_denied}
    end.


