%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Make Bootstrap components available
%%%
%%% @end
%%% Created : 19 May 2012 by gordon@vixo.com

-module(hnfuns_bootstrap).

-export([
         'breadcrumbs.'/1,
         'make.goto.buttonbar.'/1,
         'mini.goto.zdropdown'/1,
         'goto.zdropdown'/1,
         'small.goto.zdropdown'/1,
         'large.goto.zdropdown'/1,
         'mini.goto.dropdown'/1,
         'goto.dropdown'/1,
         'small.goto.dropdown'/1,
         'large.goto.dropdown'/1,
         'mini.buttonbar.'/1,
         'small.buttonbar.'/1,
         'buttonbar.'/1,
         'large.buttonbar.'/1,
         'mini.goto.button'/1,
         'small.goto.button'/1,
         'goto.button'/1,
         'large.goto.button'/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

-define(SEP," <span class='divider'>/</span> ").

'breadcrumbs.'([W]) ->
    [W2] = typechecks:std_ints([W]),
    Path = get(path),
    HTML = breadcrumbs2(Path),
    io:format("HTML is ~p~n", [HTML]),
    Resize = #resize{width = W2, height = 2},
    Preview = "Breadcrumbs",
    #spec_val{val = HTML, preview = Preview, resize = Resize}.

breadcrumbs2(Path) ->
    Trail = trail2(lists:reverse(Path), []),
    "<ul class='breadcrumb'>"
        ++ Trail
        ++ "</ul>".

trail2([], Acc) ->
    Trail = lists:reverse(Acc),
    Trail2 = ["<a href='/'>Home</a>" | Trail],
    "<li>" ++ string:join(Trail2, ?SEP) ++ "</li>";
trail2([H | T] = L, Acc) ->
    Path = "/" ++ string:join(lists:reverse(L), "/") ++ "/",
    NewAcc = " <a class='crumb' href='" ++ Path ++ "'>" ++ H ++ "</a>",
    trail2(T, [NewAcc | Acc]).

'mini.goto.zdropdown'([Text, Colour, ZQuery]) ->
    'mini.goto.zdropdown'([Text, Colour, ZQuery, 0]);
'mini.goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(Text, Colour, ZQuery, Style, "btn-mini").

'small.goto.zdropdown'([Text, Colour, ZQuery]) ->
    'small.goto.zdropdown'([Text, Colour, ZQuery, 0]);
'small.goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(Text, Colour, ZQuery, Style, "btn-small").

'goto.zdropdown'([Text, Colour, ZQuery]) ->
    'goto.zdropdown'([Text, Colour, ZQuery, 0]);
'goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(Text, Colour, ZQuery, Style, "").

'large.goto.zdropdown'([Text, Colour, ZQuery]) ->
    'large.goto.zdropdown'([Text, Colour, ZQuery, 0]);
'large.goto.zdropdown'([Text, Colour, ZQuery, Style]) ->
    zdropdown(Text, Colour, ZQuery, Style, "btn-large").

zdropdown(Text, Colour, ZQuery, Style, Button) ->
    [Text2] = typechecks:std_strs([Text]),
    [C2] = typechecks:std_ints([Colour]),
    C3 = get_colour(C2),
    [S2] = typechecks:std_ints([Style]),
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
    Preview = "Dropdown: " ++ contact_utils:rightsize(Text2, 30),
    #spec_val{val = lists:flatten(HTML), preview = Preview}.

'mini.goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(Text, Colour, Rest, "btn-mini").

'small.goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(Text, Colour, Rest, "btn-small").

'goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(Text, Colour, Rest, "").

'large.goto.dropdown'([Text, Colour | Rest]) ->
    dropdown(Text, Colour, Rest, "btn-large").

dropdown(Text, Colour, Rest, Button) ->
    [Text2] = typechecks:std_strs([Text]),
    [C2] = typechecks:std_ints([Colour]),
    C3 = get_colour(C2),
    Rest2 = typechecks:std_strs(Rest),
    Drop = make_dropdown(Rest2, []),
    HTML = "<a class='btn " ++ Button ++ " dropdown-toggle "
        ++ C3 ++ "' "
        ++ "data-toggle='dropdown' href='#'>" ++ Text2
        ++ "<span class='caret'></span>"
        ++ "</a>"
        ++ Drop,
    Preview = "Dropdown: " ++ contact_utils:rightsize(Text2, 30),
    #spec_val{val = lists:flatten(HTML), preview = Preview}.

'make.goto.buttonbar.'([W | Rest]) ->
    H = 2,
    [W2] = typechecks:std_ints([W]),
    R2 = typechecks:std_strs(Rest),
    HTML = lists:flatten("<div class='btn-group'>" ++ R2 ++ "</div>"),
    Resize = #resize{width = W2, height = H},
    Preview = "Menu Buttons",
    #spec_val{val = HTML, resize = Resize, preview = Preview}.

'mini.goto.button'([Link, Text]) ->
    'mini.goto.button'([Link, Text, 0]);
'mini.goto.button'([Link, Text, Colour]) ->
    button("btn-mini", "Mini", 1, 1, Link, Text, Colour).

'small.goto.button'([Link, Text]) ->
    'small.goto.button'([Link, Text, 0]);
'small.goto.button'([Link, Text, Colour]) ->
    button("btn-small", "Small", 1, 2, Link, Text, Colour).

% normal button don't need decoration
'goto.button'([Link, Text]) ->
    'goto.button'([Link, Text, 0]);
'goto.button'([Link, Text, Colour]) ->
    button("", "", 1, 2, Link, Text, Colour).

'large.goto.button'([Link, Text]) ->
    'large.goto.button'([Link, Text, 0]);
'large.goto.button'([Link, Text, Colour]) ->
    button("btn-large", "Large", 2, 2, Link, Text, Colour).

'mini.buttonbar.'([W | Rest]) ->
    buttonbar("btn-mini", "Mini", W, 1, Rest).

'small.buttonbar.'([W | Rest]) ->
    buttonbar("btn-small", "Small", W, 2, Rest).

% normal buttonbar don't need decoration
'buttonbar.'([W | Rest]) ->
    buttonbar("", "", W, 2, Rest).

'large.buttonbar.'([W | Rest]) ->
    buttonbar("btn-large", "Large", W, 3, Rest).

button(Class, Size, W, H, Link, Text, Colour) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    [Link2, Text2] = typechecks:std_strs([Link, Text]),
    [C2] = typechecks:std_ints([Colour]),
    C3 = get_colour(C2),
    [Btn] = make_buttons([Link2, Text2], Class ++ " " ++ C3, []),
    Resize = #resize{width = W2, height = H2},
    Preview = Size ++ " Menu Buttons: " ++ Text2,
    #spec_val{val = Btn, resize = Resize, preview = Preview}.

buttonbar(Class, Size, W, H, Rest) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    Rest2 = typechecks:std_strs(Rest),
    Btns = make_buttons(Rest2, Class, []),
    HTML = lists:flatten("<div class='btn-group'>" ++ Btns ++ "</div>"),
    Resize = #resize{width = W2, height = H2},
    Preview = Size ++ " Menu Buttons: " ++ get_titles(Rest2, []),
    #spec_val{val = HTML, resize = Resize, preview = Preview}.

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

get_colour(0) -> "btn-primary";
get_colour(1) -> "btn-warning";
get_colour(2) -> "btn-danger";
get_colour(3) -> "btn-success";
get_colour(4) -> "btn-info";
get_colour(5) -> "btn-inverse";
get_colour(_) -> ?ERR_VAL.

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
