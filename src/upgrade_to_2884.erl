%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       code to manage backup and restore etc as well
%%%            as 'grabbing' sites and applications
%%%
%%% @end
%%% Created : 18 Dec 2009 by Gordon Guthrie gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(upgrade_to_2884).

-include("../lib/hypernumbers-1.0/include/spriki.hrl").

-export([
         grab_site/1,
         import_site/1
        ]).

import_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
        ++ Site ++ "&" ++ Port,
    [ ok = hn_import:json_file("http://" ++ Site ++ ":" ++ Port
        ++ create_path_from_name(Json), Json)
      || Json <- filelib:wildcard(Dir++"/data/*.json")],
    ok.

-spec grab_site(list()) -> ok.
grab_site(URL) ->
    "http://" ++ SiteAndPort = URL,
    [Site, Port] = string:tokens(SiteAndPort, ":"),
    Dir = code:lib_dir(hypernumbers) ++ "/../../var/" ++ "grab/"
        ++ Site ++ "&" ++ Port ++ "/data/",
    ok = case filelib:is_dir(Dir) of
             true  -> io:format("deleting directory ~p~n", [Dir]),
                      delete_directory(Dir);
             false -> ok
         end,
    ok = filelib:ensure_dir(Dir),
    ok = grab_pages(URL, Dir),
    ok.


%%
%% Internal Functions
%%

create_path_from_name(Name) ->
    [ "path" | Rest ] = string:tokens(filename:basename(Name, ".json"), "."),
    hn_util:list_to_path(Rest).

grab_pages(URL, Dir) ->
    RefX = hn_util:url_to_refX(URL),
    Pages = read_pages(RefX),
    grab_pages2(RefX, Pages, Dir).

grab_pages2(_RefX, [], _Dir)    -> ok;
grab_pages2(RefX, [H | T], Dir) ->
    Page = page_attributes(RefX#refX{path = H}, anonymous),
    io:format("grabbing page ~p~n", [H]),
    Page2 = (mochijson:encoder([{input_encoding, utf8}]))(Page),
    Name = path_to_json_path(H),
    Pg = io_lib:fwrite("~s", [lists:flatten(Page2)]),
    ok = file:write_file(Dir ++ Name, Pg),
    grab_pages2(RefX, T, Dir).

read_pages(#refX{site = Site} = RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  mnesia:dirty_all_keys(hn_db_wu:trans(Site, local_objs))
          end,
    mnesia:activity(transaction, Fun).    

delete_directory(From) ->
    {ok, Files} = file:list_dir(From),
    [ok = delete_dir(filename:join(From, File)) || File <- Files],
    ok = file:del_dir(From).

delete_dir(File) ->
    case filelib:is_dir(File) of
        true  -> delete_directory(File);
        false -> io:format("Deleting file ~p~n", [File]),
                 file:delete(File)
    end.

page_attributes(#refX{site = S, path = P} = Ref, User) ->
    % io:format("Page is ~p~n", [hn_db_api:read_whole_page(Ref)]),
    Name = hn_users:name(User),
    Groups = hn_users:groups(User),
    %% now build the struct
    Init   = [["cell"], ["column"], ["row"], ["page"], ["styles"]],
    Tree   = dh_tree:create(Init),
    Styles = styles_to_css(hn_db_api:read_styles(Ref), []),
    NTree  = add_styles(Styles, Tree),
    Dict   = to_dict(hn_db_api:read_whole_page(Ref), NTree),
    Time   = {"time", remoting_reg:timestamp()},
    Usr    = {"user", Name},
    Host   = {"host", S},
    Grps   = {"groups", {array, Groups}},
    Lang   = {"lang", get_lang(User)},
    Perms = case auth_srv:can_write(S, {Name, Groups}, P) of
                true  -> {"permissions", {array, ["read", "write"]}};
                false -> {"permissions", {array, ["read"]}}
            end,

    Views = {views, {array, auth_srv:get_views(S, {Name, Groups}, P)}},
    
    {struct, [Time, Usr, Host, Lang, Perms, Grps, Views
              | dict_to_struct(Dict)]}.

dict_to_struct(Dict) ->
    F = fun(X) -> dict_to_struct(X, dict:fetch(X, Dict)) end,
    case is_dict(Dict) of 
        true  -> lists:map(F, dict:fetch_keys(Dict));
        false -> Dict
    end.

dict_to_struct(X, Dict) -> 
    case is_dict(Dict) of
        true  -> {X, {struct, dict_to_struct(Dict)}};
        false -> {X, Dict}
    end.

styles_to_css([], Acc) ->
    Acc;
styles_to_css([H | T], Acc) ->
    styles_to_css(T, [style_to_css(H) | Acc]).

style_to_css({styles, _Ref, X, Rec}) ->
    style_to_css(X, Rec).

style_to_css(X, Rec) ->
    Num = ms_util2:no_of_fields(magic_style),
    {itol(X), style_att(Num + 1, Rec, [])}.

style_att(1, _Rec, Acc) ->
    lists:flatten(Acc);
style_att(X, Rec, Acc) ->
    case element(X, Rec) of
        [] ->
            style_att(X - 1, Rec, Acc);
        _Else -> 
            Name =  ms_util2:name_by_index(magic_style, X-1),
            A = io_lib:format("~s:~s;",[Name, element(X,Rec)]),
            style_att(X - 1, Rec, [A | Acc])
    end.

add_styles([], Tree) ->
    Tree;
add_styles([ {Name, CSS} | T], Tree) ->
    add_styles(T, dh_tree:set(["styles", Name], CSS, Tree)).

to_dict([], JSON) ->
    JSON;
to_dict([ {Ref, Val} | T], JSON) ->
    to_dict(T, add_ref(Ref, hn_util:jsonify_val(Val), JSON)).

get_lang(anonymous) ->
    "en_gb";
get_lang(User) ->
    case hn_users:get(User, "language") of
        {ok, Lang} -> Lang;
        undefined  -> "en_gb"
    end.

is_dict(Dict) when is_tuple(Dict) -> dict == element(1,Dict);
is_dict(_Else)                    -> false.

itol(X) -> integer_to_list(X).
ltoi(X) -> list_to_integer(X).

add_ref(#refX{ obj = {page,"/"}}, {Name, Val}, JSON) ->
    dh_tree:set(["page", Name], Val, JSON);
add_ref(#refX{ obj = {Ref, {X,Y}}}, Data, JSON) ->
    {Name, Val} = hn_util:jsonify_val(Data),
    dh_tree:set([atom_to_list(Ref), itol(Y), itol(X), Name], Val, JSON).

path_to_json_path([])                -> "path.json";
path_to_json_path(P) when is_list(P) -> "path." ++ string:join(P, ".")
                                            ++ ".json".
