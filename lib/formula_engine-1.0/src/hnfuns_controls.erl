%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers.com

-module(hnfuns_controls).

-include("muin_proc_dict.hrl").

% careful adding 'factory.XXX' fns as factory.WxH will swallow them in muin
-export([
         'upload.file.'/1,
         'factory.'/1,
         'factory.info'/1,
         'create.button'/1,
         'map.rows.button'/1,
         'map.sheet.button'/1,
         'map.custom.button'/1,
         'load.templates.button'/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

'upload.file.'([W, H]) ->
    'upload.file.'([W, H, "Upload Files"]);
'upload.file.'([W, H, Title]) ->
    'upload.file.'([W, H, Title, "Upload Files >>"]);
'upload.file.'([W, H, Title, ButtonText]) ->
    'upload.file.'([W, H, Title, ButtonText, 0]);
'upload.file.'([W, H, Title, ButtonText, Options]) ->
    [W2, H2, O2] = typechecks:std_ints([W, H, Options]),
    % there are some minimum sizes for a file upload panel
    if
        W2 <  5                 -> ?ERR_VAL;
        H2 <  9                 -> ?ERR_VAL;
        W2 >= 5 andalso H2 >= 9 -> ok
    end,
    [Title2, BtnTxt2] = typechecks:std_strs([Title, ButtonText]),
    Restrictions = get_restrictions(O2),
    Files = filelib:wildcard(hn_util:userfilesroot(?msite) ++ "*"),
    HTML = "<div class='hn_site_admin' style='display:none;'>"
        ++ "<div class='hn_site_admin_top'>" ++ Title2 ++ "</div>"
        ++ "<div class='hn_scroll'>"
        ++ make_body(lists:sort(Files), [])
        ++ "</div>"
        ++ "<div class='hn_site_admin_container'>"
        ++ "<input class='hn_file_upload button' type='button' "
        ++ "value='" ++ BtnTxt2 ++ "' />"
        ++ "</div>"
        ++ "</div>",
    JS      = ["/webcomponents/hn.siteadmin.js",
              "/hypernumbers/ajaxfileupload.js"],
    Reload  = ["HN.SiteAdmin.reload();"],
    CSS     = ["/webcomponents/hn.siteadmin.css"],
    Incs    = #incs{js = JS, js_reload = Reload, css = CSS},
    Preview = #preview{title = "File Upload", width = W2, height = H2},
    #spec_val{val = HTML, preview = Preview, sp_incs = Incs, sp_site = true}.

make_body([], Acc) ->
    TBody = lists:flatten(lists:reverse(Acc)),
    "<table>"
        ++ "<thead><tr><td><em>Files</em></td><td></td></tr></thead>"
        ++ TBody ++ "</table>";
make_body([H | T], Acc) ->
    FileName = filename:basename(H),
    Ext = filename:extension(H),
    NewAcc = "<tr><td>" ++ FileName ++ "</td><td><code>"
        ++ make_fn(FileName, Ext) ++ "</code></td></tr>",
    make_body(T, [NewAcc | Acc]).

make_fn(FileName, Type)
  when Type == ".png" orelse Type == ".gif" orelse Type == ".jpg" ->
    "=img(\"" ++ FileName ++ "\")";
make_fn(FileName, _) ->
    "=link(\"" ++ FileName ++ "\", \"" ++ FileName ++ "\")".

get_restrictions(0)  -> [];
get_restrictions(1)  -> [".png", ".jpg", ".gif"];
get_restrictions(2)  -> [".doc", ".xls", ".pdf", ".ppt", ".xlsx", ".docx"];
get_restrictions(3)  -> lists:merge(get_restrictions(1), get_restrictions(2));
get_restrictions(_N) -> ?ERR_VAL.

'factory.info'([]) ->
    V = new_db_wu:read_kvD(?msite, factory),
    case V of
        [] ->
            #spec_val{val = ?ERRVAL_NOTFACT, sp_site = true};
        [{kvstore, factory, all}] ->
            #spec_val{val = "This site can create any site type",
                      sp_site = true};
        [{kvstore, factory, L}] when is_list(L) ->
            Msg = io_lib:format("This site can create ~p", [L]),
            #spec_val{val = lists:flatten(Msg), sp_site = true}
    end.

'factory.'([W, H]) ->
    'factory.'([W, H, "Create A New Vixo Site"]);
'factory.'([W, H, Title]) ->
    'factory.'([W, H, Title, "blank"]);
'factory.'([W, H, Title, Type]) ->
    Text = "Create a site of type: '" ++ Type ++ "'",
    'factory.'([W, H, Title, Type, Text, "Create Site >>"]);
'factory.'([W, H, Title, Type, Desc, ButtonTxt | Rest]) ->
    % first check if the site is able to be a factory
    [W2, H2] = typechecks:std_ints([W, H]),
    List = [Title, Type, Desc, ButtonTxt],
    [Title2, Type2, Desc2, BtnTxt2] = typechecks:std_strs(List),
    % make sure the sitetype exists
    Type3 = hn_util:site_type_exists(Type2),
    % check if the site is a factory site and which site types it
    % can create
    V = new_db_wu:read_kvD(?msite, factory),
    case V of
        [] ->
            #spec_val{val = ?ERRVAL_NOTFACT, sp_site = true};
        [{kvstore, factory, all}] ->
            factory2(W2, H2, Title2, Type3, Desc2, BtnTxt2, Rest);
        [{kvstore, factory, List}] when is_list(List) ->
            % now check if the site type is in the list
            case lists:member(Type3, List) of
                true  -> factory2(W2, H2, Title2, Type3, Desc2, BtnTxt2, Rest);
                false -> #spec_val{val = ?ERRVAL_VAL, sp_site = true}
            end
    end.

factory2(W, H, Title, Type, Desc, BtnTxt, Rest) ->
    Rest = typechecks:std_strs(Rest),
    % The Rest should consist of a set of doubles:
    % * a descriptive text
    % * a location to stick the text in
    case stdfuns_info:iseven([length(Rest)]) of
        true  -> factory3(W, H, Title, Type, Desc, BtnTxt, Rest);
        false -> #spec_val{val = ?ERRVAL_VAL, sp_site = true}
    end.

factory3(W, H, Title, Type, Desc, BtnTxt, Rest) ->
    Ref = hn_util:obj_to_ref({cell, {?mx, ?my}}),
    {Body, Payload} = make_body(Rest, [], []),
    Id     = "id_" ++ muin_util:create_name(),
    Js     = ["/webcomponents/hn.factory.js"],
    Reload = ["HN.Factory.reload();"],
    CSS    = ["/webcomponents/hn.factory.css",
              "/webcomponents/hn.siteadmin.css"],
    Incs   = #incs{js = Js, js_reload = Reload, css= CSS},
    Form   = #form{id = {factory, atom_to_list(Type)},
                 kind = "factory",
                 restrictions = {"sitetype", Type},
                 attrs = Payload},
    HTML = "<div class='hn_factory hn_site_admin'>"
        ++ "<div class='hn_site_admin_top'>" ++ Title ++ "</div>"
        ++ "<p>" ++ Desc ++ "</p>"
        ++ Body
        ++ "<p class='hn_factory_txt'>Please enter your email address:</p>"
        ++ "<input class='hn_factory_email' type='text' />"
        ++ "<div class='hn_site_admin_container'>"
        ++ "<input id='" ++ Id ++ "' class='button factory' "
        ++ "type='submit' data-type='" ++ atom_to_list(Type) ++ "' "
        ++ "data-ref='" ++ Ref ++ "' "
        ++ "value='" ++ BtnTxt ++ "' />"
        ++ "</div>"
        ++ "</div>",
    PreV = #preview{title = "Factory: " ++ Title, width = W, height = H},
    #spec_val{val = HTML, sp_webcontrol = Form, sp_incs = Incs,
              preview = PreV, sp_site = true}.

make_body([], Acc1, Acc2) -> {lists:flatten(lists:reverse(Acc1)),
                              lists:reverse(Acc2)};
make_body([Desc, Input | T], Acc1, Acc2) ->
    % test the input to see if it is a valid cell/path combo
    ok = is_valid(Input),
    NewAcc1 = make_b2(Desc, Input),
    make_body(T, [NewAcc1 | Acc1], [Input | Acc2]).

make_b2(Desc, Input) ->
    "<p class='hn_factory_txt'>" ++ Desc ++ "</p>"
        ++ "<input class='hn_factory_input' data-location='"
        ++ Input ++ "' type='text' />".

is_valid(Input) ->
    URL = case Input of
              "/" ++ _Rest -> ?msite ++ Input;
              _            -> ?msite ++ "/" ++ Input
          end,
    RefX = try hn_util:url_to_refX(URL)
           catch
               error:
               _ -> ?ERR_VAL;
               exit:
               _ -> ?ERR_VAL;
               throw:
               _ -> ?ERR_VAL
           end,
    case RefX of
        #refX{obj = {cell, _}} -> ok;
        _                      -> ?ERR_VAL
    end.

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
                 HTML = "<input id='" ++ Id ++ "'type='submit' "
                     ++ "class='hn-loadtemplate' value='"
                     ++ Title ++ "' data-template='"
                     ++ Template ++ "' />",
                 Preview = #preview{title = Title, width = 2, height = 2},
                 #spec_val{val = HTML, sp_webcontrol = Form, preview = Preview,
                           sp_incs = Incs}
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
                 HTML = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-mapcustom' value='"
                     ++ Title ++ "' data-map-type='custom' data-map='"
                     ++ Map ++ "' />",
                 Preview = #preview{title = Title, width = 2, height = 2},
                 #spec_val{val = HTML, sp_webcontrol = Form, preview = Preview,
                           sp_incs = Incs}
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
                 HTML = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-mapsheet' value='"
                     ++ Title ++ "' data-map-type='sheet' data-map='"
                     ++ Map ++ "' data-map-page='" ++ Page ++"' />",
                 Preview = #preview{title = Title, width = 2, height = 2},
                 #spec_val{val = HTML, sp_webcontrol = Form, preview = Preview,
                           sp_incs = Incs}
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
                 HTML = "<input id='" ++ Id ++ "' type='submit' "
                     ++ "class='hn-maprows' value='"
                     ++ Title ++ "' data-map-type='row' data-map='"
                     ++ Map ++ "' />",
                 Preview = #preview{title = Title, width = 2, height = 2},
                 #spec_val{val = HTML, sp_webcontrol = Form, preview = Preview,
                           sp_incs = Incs}
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
            HTML = lists:flatten("<input id='" ++ Id ++ "' type='submit' "
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
            Preview = #preview{title = Title, height = 2, width = 2},
            #spec_val{val = HTML, sp_webcontrol = Form, preview = Preview}
    end.
