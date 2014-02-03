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
         trial/1,
         'trial.if'/1,
         'factory.'/1,
         'factory.if.'/1,
         'factory.info'/1,
         'create.button'/1,
         'create.button.if'/1,
         'map.rows.button'/1,
         'map.rows.button.if'/1,
         'map.sheet.button'/1,
         'map.sheet.button.if'/1,
         'map.custom.button'/1,
         'map.custom.button.if'/1,
         'load.templates.button'/1,
         'load.templates.button.if'/1
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
    _Restrictions = get_restrictions(O2),
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
    Preview = "File Upload",
    Resize = #resize{width = W2, height = H2},
    #spec_val{val = HTML, preview = Preview, sp_incs = Incs,
              resize = Resize, sp_site = true}.

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
            #spec_val{val = ?ERRVAL_NOTSETUP, sp_site = true};
        [{kvstore, factory, all}] ->
            #spec_val{val = "This site can create any site type",
                      sp_site = true};
        [{kvstore, factory, L}] when is_list(L) ->
            Msg = io_lib:format("This site can create ~p", [L]),
            #spec_val{val = lists:flatten(Msg), sp_site = true}
    end.

trial(Args)      -> trial1([true | Args]).
'trial.if'(Args) -> trial1(Args).

trial1([Bool]) ->
    trial1([Bool, "Get A Trial"]);
trial1([Bool, Text]) ->
    trial1([Bool, Text, "blank"]);
trial1([Bool, Text, Type]) ->
    trial1([Bool, Text, Type, 0]);
trial1([Bool, Text, Type, Colour]) ->
    [Bool2] = typechecks:std_bools([Bool]),
    [Text2, Type2] = typechecks:std_strs([Text, Type]),
    Valid = is_site_valid(Type2),
    [Col2] = typechecks:std_ints([Colour]),
    Col3 = bootstrap_utils:get_colour(Col2),
    V = new_db_wu:read_kvD(?msite, factory),
    case {V, Valid} of
        {[], _} ->
            #spec_val{val = ?ERRVAL_NOTSETUP, sp_site = true};
        {_, error} ->
            #spec_val{val = ?ERRVAL_VAL, sp_site = true};
        {_, {sitetype, Type3}} ->
          case V of
              [{kvstore, factory, all}] ->
                  trial3(Bool2, Text2, Type2, Col3);
              [{kvstore, factory, List}] ->
                  case lists:member(Type3, List) of
                      true  ->
                          trial3(Bool2, Text2, Type2, Col3);
                      false ->
                          #spec_val{val = ?ERRVAL_VAL, sp_site = true}
                  end
          end
    end.

trial3(false, _, _, _) ->
    #spec_val{val = "", sp_site = true};
trial3(true, Text, Type, Colour) ->
    Id      = "id_" ++ muin_util:create_name(),
    Payload = "erk",
    Js      = ["/webcomponents/hn.factory.js"],
    Reload  = ["HN.Factory.Trial.reload();"],
    CSS     = ["/bootstrap/css/bootstrap.css",
               "/bootstrap/css/helper.css",
               "/webcomponents/hn.siteadmin.css"],
    Incs    = #incs{js = Js, js_reload = Reload, css= CSS},
    Form    = #form{id = {factory, Type},
                    kind = "factory",
                    restrictions = {"sitetype", Type},
                    attrs = Payload},
    HTML = "<div class='hn_trial'>"
        ++ "<input id='" ++ Id ++ "' class='btn btn-large hn-btn-large "
        ++ Colour ++ "' "
        ++ "type='submit' data-type='" ++ Type ++ "' "
        ++ "value='" ++ Text ++ "' />"
        ++ "</div>",
    PreV = "Trial: " ++ Text,
    Resize = #resize{width = 2, height = 2},
    #spec_val{val = HTML, sp_webcontrol = Form, sp_incs = Incs,
              resize = Resize, preview = PreV, sp_site = true}.

'factory.'([W, H | Rest]) -> factoryif([W, H, true | Rest]).
% syntax highlighter gets funky on these names
'factory.if.'(List) -> factoryif(List).

factoryif([W, H]) ->
    factoryif([W, H, true, "Create A New Vixo Site"]);
factoryif([W, H, Bool]) ->
    factoryif([W, H, Bool, "Create A New Vixo Site"]);
factoryif([W, H, Bool, Title]) ->
    factoryif([W, H, Bool, Title, "blank"]);
factoryif([W, H, Bool, Title, Type]) ->
    Text = "Create a site of type: '" ++ Type ++ "'",
    factoryif([W, H, Bool, Title, Type, Text	]);
factoryif([W, H, Bool, Title, Type, Text]) ->
    factoryif([W, H, Bool, Title, Type, Text, "Create Site >>"]);
factoryif([W, H, Bool, Title, Type, Desc, ButtonTxt]) ->
    factoryif([W, H, Bool, Title, Type, Desc, ButtonTxt,
               "Thanks, you will be emailed details of your new site."]);
factoryif([W, H, Bool, Title, Type, Desc, ButtonTxt, ResponseMsg]) ->
    factoryif([W, H, Bool, Title, Type, Desc, ButtonTxt, ResponseMsg, ""]);
factoryif([W, H, Bool, Title, Type, Desc, ButtonTxt, Response, Goto | Rest]) ->
    % first check if the site is able to be a factory
    [W2, H2] = typechecks:std_ints([W, H]),
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false -> "";
        true  -> factory1(W2, H2, Title, Type, Desc, ButtonTxt,
                         Response, Goto, Rest)
    end.

factory1(W2, H2, Title, Type, Desc, ButtonTxt, Response, Goto, Rest) ->
    % check if the site is a factory site and which site types it
    % can create
    V = new_db_wu:read_kvD(?msite, factory),
    List = [Title, Type, Desc, ButtonTxt, Response],
    [Tt2, Type2, Desc2, BTxt2, Rsp2] = typechecks:std_strs(List),
    G2 = case typechecks:std_strs([Goto]) of
             [""]    -> RefX = #refX{site = ?msite, path = ?mpath,
                                     obj = {page, "/"}},
                        hn_util:refX_to_url(RefX);
             [Other] -> Other
         end,
    Valid = is_site_valid(Type2),
    case V of
        [] ->
            #spec_val{val = ?ERRVAL_NOTSETUP, sp_site = true};
        [{kvstore, factory, all}] ->
            case Valid of
                {sitetype, _} ->
                    factory2(W2, H2, Tt2, Type2, Desc2, BTxt2,
                             Rsp2, G2, Rest);
                error ->
                    #spec_val{val = ?ERRVAL_VAL, sp_site = true}
            end;
        [{kvstore, factory, List}] when is_list(List) ->
            case Valid of
                {sitetype, Type3} ->
                    case lists:member(Type3, List) of
                        true  ->
                            factory2(W2, H2, Tt2, Type2, Desc2,
                                     BTxt2, Rsp2, G2, Rest);
                        false ->
                            #spec_val{val = ?ERRVAL_VAL, sp_site = true}
                    end;
                error ->
                    #spec_val{val = ?ERRVAL_VAL, sp_site = true}
            end
    end.

factory2(W, H, Title, Type, Desc, BtnTxt, Rsp2, Goto, Rest) ->
    Rest2 = typechecks:std_strs(Rest),
    % The Rest should consist of a set of doubles:
    % * a descriptive text
    % * a location to stick the text in
    case stdfuns_info:iseven([length(Rest2)]) of
        true  -> factory3(W, H, Title, Type, Desc, BtnTxt, Rsp2, Goto, Rest2);
        false -> #spec_val{val = ?ERRVAL_VAL, sp_site = true}
    end.

factory3(W, H, Title, Type, Desc, BtnTxt, Rsp, Goto, Rest) ->
    {Body, Payload} = make_body(Rest, [], []),
    Id     = "id_" ++ muin_util:create_name(),
    Js     = ["/webcomponents/hn.factory.js"],
    Reload = ["HN.Factory.reload();"],
    CSS    = ["/webcomponents/hn.factory.css",
              "/webcomponents/hn.siteadmin.css"],
    Incs   = #incs{js = Js, js_reload = Reload, css= CSS},
    Form   = #form{id = {factory, Type},
                 kind = "factory",
                 restrictions = {"sitetype", Type},
                 attrs = Payload},
    HTML = "<div class='hn_factory hn_site_admin'>"
        ++ "<div class='hn_site_admin_top'>" ++ Title ++ "</div>"
        ++ "<p>" ++ Desc ++ "</p>"
        ++ Body
        ++ "<p class='hn_factory_txt'>Please enter your email address:</p>"
        ++ "<input class='hn_factory_email' type='text' "
        ++ "data-input='" ++ Id ++ "' />"
        ++ "<div class='hn_site_admin_container'>"
        ++ "<input id='" ++ Id ++ "' class='button factory' "
        ++ "type='submit' data-type='" ++ Type ++ "' "
        ++ "data-goto='" ++ Goto ++ "' "
        ++ "data-response='" ++ Rsp ++ "' "
        ++ "value='" ++ BtnTxt ++ "' />"
        ++ "</div>"
        ++ "<div class='hn_factory_feedback'></div>"
        ++ "</div>",
    PreV = "Factory: " ++ Title,
    Resize = #resize{width = W, height = H},
    #spec_val{val = HTML, sp_webcontrol = Form, sp_incs = Incs,
              resize = Resize, preview = PreV, sp_site = true}.

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

% not sure if this works - need to make it work on subpages
'load.templates.button'(List) -> 'load.templates.buttonif'([true | List]).
% syntax heightlighter gets funky on these names
'load.templates.button.if'(List) -> 'load.templates.buttonif'(List).

'load.templates.buttonif'([Bool | List]) ->
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false ->
            "";
        true ->
            [Title, Template] = typechecks:std_strs(List),
            Templates = hn_util:get_templates(muin:pd_retrieve(site)),
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
                         Preview = Title,
                         Resize = #resize{width = 2, height = 2},
                         #spec_val{val = HTML, sp_webcontrol = Form,
                                   resize = Resize, preview = Preview,
                                   sp_incs = Incs}
            end
    end.

'map.custom.button'(List) -> 'map.custom.buttonif'([true | List]).
% syntax heightlighter gets funky on these names
'map.custom.button.if'(List) -> 'map.custom.buttonif'(List).

'map.custom.buttonif'([Bool | List]) ->
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false ->
            "";
        true ->
            [Title, Map] = typechecks:std_strs(List),
            Maps = hn_util:get_maps(muin:pd_retrieve(site)),
            Id = "id_" ++ muin_util:create_name(),
            case lists:member(Map, Maps) of
                false ->
                    ?ERRVAL_VAL;
                true  ->
                    Js = ["/hypernumbers/ajaxfileupload.js",
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
                    Preview = Title,
                    Resize = #resize{width = 2, height = 2},
                    #spec_val{val = HTML, sp_webcontrol = Form,
                              resize = Resize, preview = Preview,
                              sp_incs = Incs}
            end
    end.

'map.sheet.button'(List) -> 'map.sheet.buttonif'([true | List]).
% syntax heightlighter gets funky on these names
'map.sheet.button.if'(List) -> 'map.sheet.buttonif'(List).

'map.sheet.buttonif'([Bool | List]) ->
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false ->
            "";
        true ->
            [Title, Page, Map] = typechecks:std_strs(List),
            Maps = hn_util:get_maps(muin:pd_retrieve(site)),
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
                         Preview = Title,
                         Resize= #resize{width = 2, height = 2},
                         #spec_val{val = HTML, sp_webcontrol = Form,
                                   resize = Resize, preview = Preview,
                                   sp_incs = Incs}
            end
    end.

'map.rows.button'(List) -> 'map.rows.buttonif'([true | List]).
% syntax heightlighter gets funky on these names
'map.rows.button.if'(List) -> 'map.rows.buttonif'(List).

'map.rows.buttonif'([Bool | List]) ->
        [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false -> "";
        true ->
            [Title, Map] = typechecks:std_strs(List),
            Maps = hn_util:get_maps(muin:pd_retrieve(site)),
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
                         Preview = Title,
                         Resize = #resize{width = 2, height = 2},
                         #spec_val{val = HTML, sp_webcontrol = Form,
                                   resize = Resize, preview = Preview,
                                   sp_incs = Incs}
            end
    end.
'create.button'(List) -> 'create.buttonif'([true | List]).
% syntax heightlighter gets funky on these names
'create.button.if'(List) -> 'create.buttonif'(List).

'create.buttonif'([Bool | List])  ->
    [Bool2] = typechecks:std_bools([Bool]),
    case Bool2 of
        false -> "";
        true  -> create_b2(List)
    end.

create_b2(List) ->
    Id = "id_" ++ muin_util:create_name(),
    [Title | Commands] = typechecks:std_strs(List),
    case Commands of
        [] -> ?ERRVAL_VAL;
        _  ->
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
                                 ++ "data-action='postcreatepages' />"),
            % we stash the record set as the form definition and when the POST
            % comes in from the button we will translate it back into a
            % record format and then compare the records to see if the
            % action is approved
            Form = #form{id = {'create-button', Title}, kind = "create-button",
                         attrs = Commands3,
                         callback = {actions_hnfuns, run_actions}},
            Preview = Title,
            Resize = #resize{height = 2, width = 2},
            #spec_val{val = HTML, sp_webcontrol = Form,
                      resize = Resize, preview = Preview}
    end.

is_site_valid(Type) ->
        % can't just error because this is a site special formula
    % so you huftae catch it and pass out an #VALUE! error
    % manually...
    try
        {sitetype, hn_util:site_type_exists(Type)}
    catch
        error : _ -> error;
        exit  : _ -> error;
        throw : _ -> error
    end.
