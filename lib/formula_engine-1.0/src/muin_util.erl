%%% Util functions used by Muin.
%%% TODO: Some of these might be useful outside Muin too...

-module(muin_util).
-export([
         just_path/1,
         just_ref/1,
         expand_cellrange/2,
         getxy/1,
         join/1,
         join/2,
         init/1,
         mid/1,
         fdbg/1,
         fdbg/2,
         puts/1,
         normalize_ssref/1,
         walk_path/2,
         is_alpha/1,
         get_frontend/0,
         to_i/1,
         to_i/2,
         to_f/1,
         to_s/1
        ]).

-import(lists, [all/2, append/1, append/2, dropwhile/2, filter/2, flatten/1, foldl/3,
                foreach/2, last/1, map/2, member/2, reverse/1, seq/2,
                takewhile/2]).
-import(string, [rchr/2, tokens/2]).

-compile(export_all).

-include("handy_macros.hrl").

%% Takes a same-site reference, returns path to the page it's on.
just_path(Ssref) when is_list(Ssref) ->
    append([?COND(hd(Ssref) == $/, "/", ""),
            join(init(string:tokens(Ssref, "/")), "/"),
            "/"]).

just_ref(Ssref) ->
    last(tokens(Ssref, "/")).

%% Absolute path to location -> absolute path to another location.
walk_path(_CurrLoc, Dest) when hd(Dest) == $/ ->
    Dest;

walk_path(CurrLoc, Dest) ->
    NewLocStack =
        foldl(fun(".",  Stack) -> Stack;
                 ("..", Stack) -> init(Stack);
                 (Word, Stack) -> append(Stack, [Word])
              end,
              string:tokens(CurrLoc, "/"),
              string:tokens(Dest, "/")),

    ?COND(length(NewLocStack) == 0,
          "/", %% Went up too far, son.
          "/" ++ muin_util:join(NewLocStack, "/") ++ "/").

expand_cellrange(Start_, End_) ->
    [Start, End] = map(fun(S) ->
                               {ok, NewS, _} = regexp:gsub(S, "\\$", ""),
                               NewS
                       end,
                       [Start_, End_]),
    
    StartCol = util2:mk_int_frm_b26(takewhile(fun is_alpha/1, Start)),
    EndCol   = util2:mk_int_frm_b26(takewhile(fun is_alpha/1, End)),
    StartRow = to_i(dropwhile(fun is_alpha/1, Start)),
    EndRow   = to_i(dropwhile(fun is_alpha/1, End)),
    
    %% Make a list of cells that make up this range.
    Cells = map(fun(X) ->
                        map(fun(Y) ->
                                    util2:make_b26(X) ++ integer_to_list(Y)
                            end,
                            seq(StartRow, EndRow))
                end,
                seq(StartCol, EndCol)),
    
    %% Cells is now a list of lists, which we need to flatten. lists:flatten()
    %% won't work (Erlang's lack of character type bites us in the ass again!)
    foldl(fun(X, Acc) ->
                  append([Acc, X])
          end,
          [],
          Cells).


fdbg(Val) ->
    fdbg(Val, "").


fdbg(Vals, Labels) when is_tuple(Vals) andalso is_tuple(Labels) ->
    foreach(fun({Val, Label}) ->
                    fdbg(Val, Label)
            end,
            lists:zip(tuple_to_list(Vals), tuple_to_list(Labels)));

fdbg(Val, Name) ->
    MakeLine = fun(Char, Length) ->
                       flatten(map(fun(_X) -> [Char] end, seq(1, Length)))
               end,
     
    StartLine = MakeLine($-, 7) ++ " " ++ Name,
    
    io:format("~s~n", [StartLine]),
    erlang:display(Val).


puts(Str) ->
    io:format(Str ++ "~n").


%% Given a list or tuple of strings, returns a new string with elements of the list 
%% separated by given separator, or by commas.
join(T, Sep) when is_tuple(T) ->
    join(tuple_to_list(T), Sep);

join([], _) ->
    "";
join(LoL, Sep) ->
    flatten(reverse(join1(LoL, Sep, []))).
join1([Hd | []], _Sep, Acc) ->
    [Hd | Acc];
join1([Hd | Tl], Sep, Acc) ->
    join1(Tl, Sep, [Sep, Hd | Acc]).

%% The default option is to use comma to separate items.
join(T) when is_tuple(T) ->
    join(tuple_to_list(T));

join(LoL) when is_list(LoL) ->
    join(LoL, ", ").

%% Checks if an integer or a list containing exactly one integer can represent
%% an alpha character.
is_alpha(Char) when is_integer(Char) ->
    (lists:member(Char, lists:seq($A, $Z)) orelse
     lists:member(Char, lists:seq($a, $z)));

is_alpha(Str) when is_list(Str) andalso (length(Str) == 1) ->
    is_alpha(hd(Str)).

%% Return list of all elements but the last one. (Name borrowed from Haskell.)
init([]) ->
    []; % This is not allowed in Haskell, but I'm not fussy.
init(L) ->
    reverse(tl(reverse(L))).

%% List middle: remove the first and last elements of the list.
mid(L) when length(L) > 1 ->
    reverse(tl(reverse(tl(L))));
mid(_L) ->
    [].

%% Given a cellref as string, return {Column, Row} integer tuple.
getxy(CellRef) ->
    ColName = takewhile(fun(X) ->
                                muin_util:is_alpha(X)
                        end,
                        string:to_lower(CellRef)),

    RowAsStr = string:substr(CellRef,
                             length(ColName) + 1,
                             length(CellRef) - length(ColName)),
    
    {ok, [Y], _} = io_lib:fread("~d", RowAsStr),
    {util2:mk_int_frm_b26(ColName), Y}.

%% Takes a same-site reference, and replaces all !s with /s.
%% Also converts the cell or range reference at the end of the ssref to lowercase.
normalize_ssref(Ssref) ->
    {ok, NewSsref, _} = regexp:gsub(Ssref, "!", "/"),
    Tokens = string:tokens(NewSsref, "/"),
    NewSsrefTokens = init(Tokens) ++ [string:to_lower(last(Tokens))],

    ?COND(hd(NewSsref) == $/,
          "/" ++ join(NewSsrefTokens, "/"),
          join(NewSsrefTokens, "/")).

%% Returns {lexer_module, parser_module} to use as frontend.
get_frontend() ->
    R = os:getenv("MUIN_FRONTEND"),
    ?COND(is_list(R),
          {list_to_atom(R ++ "_lexer"), list_to_atom(R ++ "_parser")},
          {muin_lexer, muin_parser}).

%% Convert string to integer.
to_i(Str) ->
    {ok, [Val], []} = io_lib:fread("~d", Str),
    Val.

to_i(Str, b26) ->
    util2:mk_int_frm_b26(Str).

%% Convert string to float.
to_f(Str) ->
    {ok, [Val], []} = io_lib:fread("~f", Str),
    Val.

%% Integer to string.
to_s(Int) ->
    integer_to_list(Int).

%%% =====================
%%% = Private functions =
%%% =====================
