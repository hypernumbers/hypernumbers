%%% Util functions used by Muin.
%%% TODO: Some of these might be useful outside Muin too...

-module(muin_util).
-export([
         error/1,
         split_ssref/1,
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
         get_frontend/0]).

-import(string, [rchr/2, tokens/2]).
-import(tconv, [to_i/1]).

-compile(export_all).

-include("handy_macros.hrl").


error('#NULL!')  -> throw({error, null});
error('#DIV/0!') -> throw({error, div0});
error('#VALUE!') -> throw({error, value});
error('#REF!')   -> throw({error, ref});
error('#NAME?')  -> throw({error, name});
error('#NUM!')   -> throw({error, num});
error('#N/A!')   -> throw({error, na}).

%% Splits ssref to [Path, Ref]
split_ssref(Ssref) ->
    {just_path(Ssref), just_ref(Ssref)}.

%% Takes a same-site reference, returns path to the page it's on.
just_path(Ssref) when is_list(Ssref) ->
    MbR = append([?COND(hd(Ssref) == $/,
                        "/",
                        ""),
                  join(init(string:tokens(Ssref, "/")), "/"),
                  "/"]),

    %% For Ssref like /a1.
    ?COND(MbR == "//", "/", MbR).
          
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
    %% won't work cos it'll flatten the strings.
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


%% Given a list of strings, returns a new string with elements of the list 
%% separated by given separator, or by commas.
%% RB12 has string:join()...
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
    (member(Char, seq($A, $Z)) orelse
     member(Char, seq($a, $z)));

is_alpha([Char]) ->
    is_alpha(Char).


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
%% Also lowercases the cell or range reference at the end of the ssref.
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


%%% =====================
%%% = Private functions =
%%% =====================
