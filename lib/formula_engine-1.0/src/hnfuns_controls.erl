%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers.com

-module(hnfuns_controls).

-export([
         'factory.'/1,
         'create.button'/1,
         'map.rows.button'/1,
         'map.sheet.button'/1,
         'map.custom.button'/1,
         'load.templates.button'/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

'factory.'([W, H]) ->
    'factory.'([W, H, "Create Site", "blank"]);
'factory.'([W, H, Title]) ->
    'factory.'([W, H, Title, "blank"]);
'factory.'([W, H, Title, Type | Rest]) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    [Title2, Type2] = typechecks:std_strs([Title, Type]),
    Rest2 = typechecks:std_strs(Rest),
    Type3 = list_to_existing_atom(Type2),
    % The Rest should consist of a set of doubles:
    % * a descriptive text
    % * a location to stick the text in
    case stdfuns_info:iseven([length(Rest)]) of
        true  -> ok;
        false -> ?ERR_VAL
    end,
    Id = "id_" ++ muin_util:create_name(),
    Payload = {struct, [{"signup",{struct,[{"sitetype", Type3}]}}]},
    Js = ["/webcomponents/hn.factory.js"],
    Reload = ["HN.Factory.reload();"],
    CSS = ["/webcomponents/hn.factory.css"],
    Incs = #incs{js = Js, js_reload = Reload, css= CSS},
    Form = #form{id = {factory, Type2},
                 kind = "factory",
                 attrs = Payload},
    HTML = "<div class='hn_factory'>"
        ++ "<input id='" ++ Id ++ "' class='button factory' "
        ++ "type='submit' value='" ++ Title ++ "' />"
        ++ "</div>",
    Preview = #preview{title = "Factory: " ++ Title2,
                       width = W2, height = H2},
    #spec_val{val = HTML, sp_webcontrol = Form, sp_incs = Incs,
              preview = Preview}.

'load.templates.button'(List) ->
    [Title, Template] = typechecks:std_strs(List),
    Templates = hn_util:get_templates(get(site)),
    Id = "id_" ++ muin_util:create_name(),
    case lists:member(Template, Templates) of
        false -> ?ERRVAL_VAL;
        true  -> Js = ["/hypernumbers/ajaxfileupload.js",
                       "/webcomponents/hn.loadtemplates.js"],
                 Reload = ["HN.LoadTemplates.reload();"],
                 Incs = #incs{js = Js, js_reload = Reload},
                 Payload = {struct, [{"load_templates", Template}]},
                 Form = #form{id = {'load-template-button', Title},
                              kind = "load-template-button",
                              attrs = Payload},
                 Html = "<input id='" ++ Id ++ "'type='submit' "
                     ++ "class='hn-loadtemplate' value='"
                     ++ Title ++ "' data-template='"
                     ++ Template ++ "' />",
                 {webcontrol, {Form, {Title, 2, 2, Incs}}, Html}
    end.

'map.custom.button'(List) ->
    [Title, Map] = typechecks:std_strs(List),
    Maps = hn_util:get_maps(get(site)),
    Id = "id_" ++ muin_util:create_name(),
    case lists:member(Map, Maps) of
        false -> ?ERRVAL_VAL;
        true  -> Js = ["/hypernumbers/ajaxfileupload.js",
                       "/webcomponents/hn.mapcustom.js"],
                 Reload = ["HN.MapCustom.reload();"],
                 Incs = #incs{js = Js, js_reload = Reload},
                 Payload = {struct, [{"map", Map}]},
                 Form = #form{id = {'map-custom-button', Title},
                              kind = "map-custom-button",
                              attrs = Payload},
                 Html = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-mapcustom' value='"
                     ++ Title ++ "' data-map-type='custom' data-map='"
                     ++ Map ++ "' />",
                 {webcontrol, {Form, {Title, 2, 2, Incs}}, Html}
    end.

'map.sheet.button'(List) ->
    [Title, Page, Map] = typechecks:std_strs(List),
    Maps = hn_util:get_maps(get(site)),
    Id = "id_" ++ muin_util:create_name(),
    case lists:member(Map, Maps) of
        false -> ?ERRVAL_VAL;
        true  -> Js = ["/hypernumbers/ajaxfileupload.js",
                       "/webcomponents/hn.mapsheet.js"],
                 Reload = ["HN.MapSheet.reload();"],
                 Incs = #incs{js = Js, js_reload = Reload},
                 Payload = {struct, [{"map", Map}, {"page", Page}]},
                 Form = #form{id = {'map-sheet-button', Title},
                              kind = "map-sheet-button",
                              attrs = Payload},
                 Html = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-mapsheet' value='"
                     ++ Title ++ "' data-map-type='sheet' data-map='"
                     ++ Map ++ "' data-map-page='" ++ Page ++"' />",
                 {webcontrol, {Form, {Title, 2, 2, Incs}}, Html}
    end.

'map.rows.button'(List) ->
    [Title, Map] = typechecks:std_strs(List),
    Maps = hn_util:get_maps(get(site)),
    Id = "id_" ++ muin_util:create_name(),
    case lists:member(Map, Maps) of
        false -> ?ERRVAL_VAL;
        true  -> Js = ["/hypernumbers/ajaxfileupload.js",
                       "/webcomponents/hn.maprows.js"],
                 Reload = ["HN.MapRows.reload();"],
                 Incs = #incs{js = Js, js_reload = Reload},
                 Payload = {struct, [{"map", Map}]},
                 Form = #form{id = {'map-rows-button', Title},
                              kind = "map-rows-button",
                              attrs = Payload},
                 Html = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-maprows' value='"
                     ++ Title ++ "' data-map-type='row' data-map='"
                     ++ Map ++ "' />",
                 {webcontrol, {Form, {Title, 2, 2, Incs}}, Html}
    end.

'create.button'(List) when is_list(List) ->
    Id = "id_" ++ muin_util:create_name(),
    [Title | Commands] = typechecks:std_strs(List),
    case Commands of
        [] -> ?ERRVAL_VAL;
        _  ->
            Origin = hn_util:list_to_path(muin:context_setting(path)),
            % first we create the list of commands records
            % this is the executable AST
            Fun1 = fun(Expr, {N, Acc}) ->
                           {N + 1, [{N, webc_parser:compile(Expr)} | Acc]}
                   end,
            Ret = lists:foldl(Fun1, {1, []}, Commands),
            {_, Commands2} = Ret,
            % no real reason to sort, just tidier!
            Commands3 = lists:sort(Commands2),
            % now we transform the commands in Json
            % this is the payload that is shipped to the button on the
            % browser
            Fun2 = fun({N, L}) ->
                           RecJson = [json_recs:rec_to_json(X) || X <- L],
                           {struct, [{N, {array, RecJson}}]}
                   end,
            Payload = [Fun2(X) || X <- Commands3],
            Pay2 = {struct, [{createpages, {array, Payload}}]},
            Json = mochijson:encode(Pay2),
            Html = lists:flatten("<input id='" ++ Id ++ "' type='submit' "
                                 ++ "class='hn-webcontrol' value='"
                                 ++ Title ++ "' data-payload='" ++ Json ++ "' "
                                 ++ "data-action='postcreatepages'"
                                 ++ "data-origin='" ++ Origin ++ "' />"),
            % we stash the record set as the form definition and when the POST
            % comes in from the button we will translate it back into a
            % record format and then compare the records to see if the
            % action is approved
            Form = #form{id = {'create-button', Title}, kind = "create-button",
                         attrs = Commands3},
            {webcontrol, {Form, {Title, 2, 2, #incs{}}}, Html}
    end.
