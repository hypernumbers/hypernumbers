%%% makes colour palettes
-module(make_palette).

-export([make/1]).

make(FileName) ->
    File = parse_csv:parse_file(FileName),
    Sets = sort(File),
    Background = [make_bg(X) || X <- Sets],
    Text = [flip(X) || X <- Background],
    io:format("Background is ~p~nText is ~p~n", [Background, Text]),
    ok.

flip([Auth, Title, Colours]) -> [Auth, Title, flip2(Colours)].

flip2([[B1, F1], [B2, F2], [B3, F3], [B4, F4], [B5, F5]]) ->
    [[F1, B1], [F2, B2], [F3, B3], [F4, B4], [F5, B5]].

make_bg({Auth, Title, Colours}) -> [Auth, Title, m_bg1(Colours)].

m_bg1([{C1, C1a}, {C2, C2a}, {C3, C3a}, {C4, C4a}, {C5, C5a}]) ->
    [[C1, black_or_white(C1a)],
     [C2, black_or_white(C2a)],
     [C3, black_or_white(C3a)],
     [C4, black_or_white(C4a)],
     [C5, black_or_white(C5a)]]. 

black_or_white({R, G, B}) ->
    I = (0.3*R + 0.59*G + 0.11 * B)/255,
    if
        I >  0.5 -> "000000";
        I =< 0.5 -> "FFFFFF"
    end.

sort(List) -> sort1(List, []).

sort1([], Acc) -> Acc;
sort1([H | T], Acc) ->
    {C1, C2, C3, C4, C5, Author, Title} = H,
    Colours = [{C1, rgb(C1)}, {C2, rgb(C2)}, {C3, rgb(C3)},
               {C4, rgb(C4)}, {C5, rgb(C5)}],
    P = lists:sort(fun palette_sort/2, Colours),
    sort1(T, [{Author, Title, P} | Acc]).

palette_sort({_X, {RX, GX, BX}}, {_Y, {RY, GY, BY}}) ->
    BrX = brightness(RX, GX, BX),
    BrY = brightness(RY, GY, BY),
    if
        (BrX >  BrY) -> true;
        (BrX =< BrY) -> false
    end.

%% http://particletree.com/notebook/calculating-color-contrast-for-legible-text/
brightness(Red, Green, Blue) -> (Red*299 + Green*587 + Blue*114)/1000.

rgb([R1, R2, G1, G2, B1, B2]) ->
    Red = erlang:list_to_integer([R1, R2], 16),
    Green = erlang:list_to_integer([G1, G2], 16),
    Blue = erlang:list_to_integer([B1, B2], 16),
    {Red, Green, Blue}.
    
    
