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

-export([notify/4,
         notify_back/4,
         notify_back_create/2]).
         
%% @spec notify(Parent::#refX{}, Outgoing, Val, DepTree) -> {ok, ok}
%% Val = term() 
%% Outgoing = [#outgoing_hn{}] 
%% DepTree = list()
%% @doc notifies any remote sites that a hypernumber has changed.
%% the reference must be for a cell
%% @todo generalise the references to row, column, range and page
%% the structure of the dirty_notify_out is leaking out here because
%% we use timestamps as identifiers instead of unique keys...
%% makes it hard to delete
%% @todo make me robust with retries...!
%% @todo spawn the notifies so they are concurrent not sequential...
notify(Parent, Outgoing, Value, DepTree)
  when is_record(Parent, refX) ->
    io:format("in horiz_api:notify Value is ~p~n", [Value]),
    
    %    error_logger:error_msg("in hn_db_wu:notify_outgoing_hn *WARNING* "++
    %                           "notify_outgoing_hn not using "++
    %                           "version number - ie it aint working - yet :("),
    ParentIdx = hn_util:index_from_refX(Parent),
    ParentUrl = hn_util:index_to_url(ParentIdx),
    Value2 = hn_util:to_xml(Value),
    Fun2 = fun(X) -> Server = X#outgoing_hn.child_proxy,
                     Version = tconv:to_s(X#outgoing_hn.version),
                     Biccie = X#outgoing_hn.biccie,
                     Actions = simplexml:to_json(
                                 {notify,[],
                                  [
                                   {biccie,            [], [Biccie]},
                                   {parent,            [], [ParentUrl]},
                                   {type,              [], ["change"]},
                                   {value,             [], Value2},
                                   {'dependency-tree', [], DepTree},
                                   {version,           [], [Version]}
                                  ]}),
                     
                     "<success/>" = hn_util:post(Server,Actions,"application/json"),
                     ok
           end,
    [ok = Fun2(X) || X <- Outgoing],
    {ok, ok}.

%% @spec notify_back(ParentRefX::#refX{}, ChildRefX::#refX{}, Change, 
%% Biccie) -> {ok, ok}
%% @doc notify's a change of a cell back to its remote hypernumber parent
%% <code>#refX{}</code> can be a cell only
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents.
notify_back(ParentRefX, ChildRefX, Change, Biccie)
  when is_record(ChildRefX, refX), is_record(ParentRefX, refX) ->
    io:format("in horiz_api:notify~n-ParentRefX is ~p~n-ChildRefX is ~p~n-"++
              "Change is ~p~n-Biccie is ~p~n",
              [ParentRefX, ChildRefX, Change, Biccie]),
    ChildIdx = hn_util:refX_to_index(ChildRefX),
    ParentIdx = hn_util:refX_to_index(ParentRefX),
    #index{site = Server} = ParentIdx,
    ChildUrl=hn_util:index_to_url(ChildIdx),
    ParentUrl=hn_util:index_to_url(ParentIdx),
    Actions = simplexml:to_json(
                   {notify_back, [], [
                                      {biccie,      [], [Biccie]},
                                      {child_url,   [], [ChildUrl]},
                                      {parent_url,  [], [ParentUrl]},
                                      {type,        [], [Change]}
                                     ]}),
    %% not very robust!
    "<success/>" = hn_util:post(Server,Actions,"application/json"),
    {ok, ok}.

%% @spec notify_back_create(Parent::#refX{}, Child::#refX{}) -> {ok, ok}
%% @doc creates a new hypernumbers.
%% Both the parent and the child references must point to a cell
notify_back_create(Parent, Child) ->

    Biccie = util2:bake_biccie(),
    ParentIdx = hn_util:index_from_refX(Parent),
    ChildIdx = hn_util:index_from_refX(Child),
    #index{site = S, path = P} = ChildIdx,
    Proxy = S ++"/"++ string:join(P,"/")++"/",
    ParentUrl = hn_util:index_to_url(ParentIdx),
    ChildUrl = hn_util:index_to_url(ChildIdx),

    %% Ignore simplexml it makes stuff a bit nastier
    Vars = {struct, [{"action","register"}, {"biccie", Biccie},
                     {"proxy", Proxy}, {"child_url", ChildUrl}]},
    Post = lists:flatten(mochijson:encode(Vars)),

    case http:request(post,{ParentUrl,[],"application/json",Post},[],[]) of
        {ok,{{_V,200,_R},_H,Json}} ->
            io:format("in horiz_api:notify_back_create Json is ~p~n", [Json]),

            {hypernumber,[],[
                             {value,[],              [Val]},
                             {'dependency-tree',[],  DepTree}]
            } = simplexml:from_json(Json),
            
            Value = hn_util:xml_to_val(Val),
            
            {Value, DepTree, Biccie};
        {ok,{{_V,503,_R},_H,_Body}} ->
            io:format("-returned 503~n"),
            io:format("permission has been denied - need to write an error "++
                      "to the hypernumber here...~n"),
            {error,permission_denied};
        Other -> io:format("Post failed with ~p~n", [Other])
    end.
