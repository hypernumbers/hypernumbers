%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Make Bootstrap components available
%%%
%%% @end
%%% Created : 19 May 2012 by gordon@vixo.com

-module(hnfuns_bootstrap).

-export([
         'menu.mini.buttons.'/1,
         'menu.small.buttons.'/1,
         'menu.buttons.'/1,
         'menu.large.buttons.'/1
         ]).

-include("spriki.hrl").

'menu.mini.buttons.'([W | Rest]) ->
    buttons("btn-mini", "Mini", W, 1, Rest).

'menu.small.buttons.'([W | Rest]) ->
    buttons("btn-small", "Small", W, 2, Rest).

% normal buttons don't need decoration
'menu.buttons.'([W | Rest]) ->
    buttons("", "", W, 2, Rest).

'menu.large.buttons.'([W | Rest]) ->
    buttons("btn-large", "Large", W, 3, Rest).

buttons(Class, Size, W, H, Rest) ->
    [W2, H2] = typechecks:std_ints([W, H]),
    Rest2 = typechecks:std_strs(Rest),
    Btns = make_buttons(Rest2, Class, []),
    HTML = lists:flatten("<div class='btn-group'>" ++ Btns ++ "</div>"),
    CSS = ["/bootstrap/css/bootstrap.css"],
    Incs = #incs{css = CSS},
    Resize = #resize{width = W2, height = H2},
    Preview = Size ++ " Menu Buttons: " ++ get_titles(Rest2, []),
    #spec_val{val = HTML, resize = Resize, preview = Preview, sp_incs = Incs}.

% will throw an error if odd length list passed in
get_titles([], Acc) ->
    lists:flatten(string:join(lists:reverse(Acc), " "));
get_titles([_, T | R], Acc) ->
    get_titles(R, [T | Acc]).

% will throw an error if odd length list passed in
make_buttons([], _Class, Acc) ->
    lists:reverse(Acc);
make_buttons([Link, Text | Rest], Class, Acc) ->
    NewAcc = "<button class='btn " ++ Class ++ "' "
        ++ "onclick='javascript:window.location=\"" ++ Link ++ "\";' >"
        ++ Text ++ "</button>",
    make_buttons(Rest, Class, [NewAcc | Acc]).
