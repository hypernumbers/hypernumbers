%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Make Bootstrap components available
%%%
%%% @end
%%% Created : 19 May 2012 by gordon@vixo.com

-module(hnfuns_bootstrap).

-export([
         'breadcrumbs.'/1,
         'small.goto.zdropdown'/1,
         'large.goto.zdropdown'/1,
         'small.goto.dropdown'/1,
         'large.goto.dropdown'/1,
         'small.buttonbar.'/1,
         'large.buttonbar.'/1,
         'small.goto.button'/1,
         'large.goto.button'/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

-define(SEP," <span class='divider'>/</span> ").

'breadcrumbs.'([W]) ->
    [W2] = typechecks:std_ints([W]),
    Path = get(path),
    HTML = breadcrumbs2(Path),
    Resize = #resize{width = W2, height = 2},
    Preview = "Breadcrumbs",
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{css = CSS},
    #spec_val{val = HTML, sp_incs = Incs, preview = Preview, resize = Resize}.

breadcrumbs2(Path) ->
    Trail = trail2(lists:reverse(Path), []),
    "<ul class='breadcrumb'>"
        ++ Trail
        ++ "</ul>".

trail2([], Acc) ->
    Trail2 = ["<a href='/'>home</a>" | Acc],
    "<li>" ++ string:join(Trail2, ?SEP) ++ "</li>";
trail2([H | T] = L, Acc) ->
    Path = "/" ++ string:join(lists:reverse(L), "/") ++ "/",
    NewAcc = " <a class='crumb' href='" ++ Path ++ "'>" ++ H ++ "</a>",
    trail2(T, [NewAcc | Acc]).

'small.goto.zdropdown'([Text, Colour, ZQuery]) ->
    'small.goto.zdropdown'([Text, Colour, ZQuery, 0]);
'small.goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(1, 1, Text, Colour, ZQuery, Style, "btn-small hn-btn-small").

'large.goto.zdropdown'([Text, Colour, ZQuery]) ->
    'large.goto.zdropdown'([Text, Colour, ZQuery, 0]);
'large.goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(2, 2, Text, Colour, ZQuery, Style, "btn-large hn-btn-large").

zdropdown(W, H, Text, Colour, ZQuery, Style, Button) ->
    [Text2] = typechecks:throw_std_strs([Text]),
    [C2] = typechecks:throw_std_ints([Colour]),
    C3 = bootstrap_utils:get_colour(C2),
    [S2] = typechecks:throw_std_ints([Style]),
    S3 = case S2 of
             0 -> vals;
             1 -> paths;
             _ -> ?ERR_VAL
         end,
    ZQuery2 = get_z(fun make_links/2, ZQuery, S3),
    HTML = "<a class='btn " ++ Button ++ " dropdown-toggle "
        ++ C3 ++ "' "
        ++ "data-toggle='dropdown' href='#'>" ++ Text2
        ++ "<span class='caret'></span>"
        ++ "</a>"
        ++ ZQuery2,
    Resize = #resize{width = W, height = H},
    Preview = "Dropdown: " ++ contact_utils:rightsize(Text2, 30),
    JS = ["/bootstrap/js/bootstrap.js", "/bootstrap/js/helper.js"],
    Reload = ["HN.BootstrapHelper.reload();"],
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{js = JS, js_reload = Reload, css = CSS},
    #spec_val{val = lists:flatten(HTML), sp_incs = Incs,
              resize = Resize, preview = Preview}.

'small.goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(1, 1, Text, Colour, Rest, "btn-small hn-btn-small").

'large.goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(2, 2, Text, Colour, Rest, "btn-large hn-btn-large").

dropdown(W, H, Text, Colour, Rest, Button) ->
    [Text2] = typechecks:throw_std_strs([Text]),
    [C2] = typechecks:throw_std_ints([Colour]),
    C3 = bootstrap_utils:get_colour(C2),
    Rest2 = typechecks:throw_std_strs(Rest),
    Drop = make_dropdown(Rest2, []),
    HTML = "<a class='btn " ++ Button ++ " dropdown-toggle "
        ++ C3 ++ "' "
        ++ "data-toggle='dropdown' href='#'>" ++ Text2
        ++ "<span class='caret'></span>"
        ++ "</a>"
        ++ Drop,
    Preview = "Dropdown: " ++ contact_utils:rightsize(Text2, 30),
    Resize = #resize{width = W, height = H},
    JS = ["/bootstrap/js/bootstrap.js", "/bootstrap/js/helper.js"],
    Reload = ["HN.BootstrapHelper.reload();"],
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{js = JS, js_reload = Reload, css = CSS},
    #spec_val{val = lists:flatten(HTML), resize = Resize,
              sp_incs = Incs, preview = Preview}.

'small.goto.button'([Link, Text]) ->
    'small.goto.button'([Link, Text, 0]);
'small.goto.button'([Link, Text, Colour]) ->
    button("btn-small hn-btn-small", "Small", 1, 2, Link, Text, Colour).

'large.goto.button'([Link, Text]) ->
    'large.goto.button'([Link, Text, 0]);
'large.goto.button'([Link, Text, Colour]) ->
    button("btn-large hn-btn-large", "Large", 2, 2, Link, Text, Colour).

'small.buttonbar.'([W | Rest]) ->
    buttonbar("btn-small", "Small", W, 2, Rest).

'large.buttonbar.'([W | Rest]) ->
    buttonbar("btn-large", "Large", W, 3, Rest).

button(Class, Size, W, H, Link, Text, Colour) ->
    [W2, H2] = typechecks:throw_std_ints([W, H]),
    [Link2, Text2] = typechecks:std_strs([Link, Text]),
    [C2] = typechecks:std_ints([Colour]),
    C3 = bootstrap_utils:get_colour(C2),
    [Btn] = make_buttons([Link2, Text2], Class ++ " " ++ C3, []),
    Resize = #resize{width = W2, height = H2},
    Preview = Size ++ " Menu Buttons: " ++ Text2,
    JS = ["/bootstrap/js/bootstrap.js", "/bootstrap/js/helper.js"],
    Reload = ["HN.BootstrapHelper.reload();"],
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{js = JS, js_reload = Reload, css = CSS},
    #spec_val{val = Btn, resize = Resize, sp_incs = Incs, preview = Preview}.

buttonbar(Class, Size, W, H, Rest) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    Rest2 = typechecks:throw_std_strs(Rest),
    Btns = make_buttons(Rest2, Class, []),
    HTML = lists:flatten("<div class='btn-group'>" ++ Btns ++ "</div>"),
    Resize = #resize{width = W2, height = H2},
    Preview = Size ++ " Menu Buttons: " ++ get_titles(Rest2, []),
    JS = ["/bootstrap/js/bootstrap.js", "/bootstrap/js/helper.js"],
    Reload = ["HN.BootstrapHelper.reload();"],
    CSS = ["/bootstrap/css/bootstrap.css", "/bootstrap/css/helper.css"],
    Incs = #incs{js = JS, js_reload = Reload, css = CSS},
    #spec_val{val = HTML, sp_incs = Incs, resize = Resize, preview = Preview}.

% will throw an error if odd length list passed in
get_titles([], Acc) ->
    lists:flatten(string:join(lists:reverse(Acc), " "));
get_titles([_, T | R], Acc) ->
    get_titles(R, [T | Acc]).

% will throw an error if odd length list passed in
make_dropdown([], Acc) ->
    "<ul class='dropdown-menu'>"
        ++ lists:reverse(Acc)
        ++ "</ul>";
make_dropdown([Link, Text | Rest], Acc) ->
    NewAcc = "<li><a " ++ "href='" ++ Link ++ "'>"
        ++ Text ++ "</a></li>",
    make_dropdown(Rest, [NewAcc | Acc]).

% will throw an error if odd length list passed in
make_buttons([], _Class, Acc) ->
    lists:reverse(Acc);
make_buttons([Link, Text | Rest], Class, Acc) ->
    C = case Class of
            [] -> "btn";
            _  -> "btn "
        end,
    NewAcc = "<a class='" ++ C ++ Class ++ "' " ++ "href='" ++ Link ++ "'>"
        ++ Text ++ "</a>",
    make_buttons(Rest, Class, [NewAcc | Acc]).

get_z(Fun, Z, Style) ->
    case muin_collect:col([Z], [fetch, fetch_z_debug, blank_as_str],
                          [return_errors]) of
        [{zeds, Matches, _, []}] -> Fun(Matches, Style);
        [{zeds, _, _, [H | _]}]  -> {error, _, {errval, Err}} = H,
                                    Err
    end.

make_links(List, paths) -> make_paths(List, []);
make_links(List, vals)  -> make_vals(List, []).

make_paths([], Acc) ->
    "<ul class='dropdown-menu'>"
        ++ lists:flatten(lists:reverse(Acc))
        ++ "</ul>";
make_paths([{{Path, _Cell}, _Val} | T], Acc) ->
    P2 = hn_util:list_to_path(Path),
    NewAcc = "<li><a href='" ++ P2 ++ "'>" ++ P2 ++ "</a></li>",
    make_paths(T, [NewAcc | Acc]).

make_vals([], Acc) ->
    "<ul class='dropdown-menu'>"
        ++ lists:flatten(lists:reverse(Acc))
        ++ "</ul>";
make_vals([{{Path, _Cell}, Val} | T], Acc) ->
    P2 = hn_util:list_to_path(Path),
    Val2 = case Val of
               blank -> "blank";
               _     -> tconv:to_s(Val)
           end,
    NewAcc = "<li><a href='" ++ P2 ++ "'>" ++ Val2 ++ "</a></li>",
    make_vals(T, [NewAcc | Acc]).
