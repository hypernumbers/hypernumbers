-module(muin_util).
-export([cast/2,
         split_ssref/1,
         just_path/1,
         just_ref/1,
         expand_cellrange/4,
         walk_path/2,
         attempt/3]).

-import(string, [rchr/2, tokens/2]).
-import(tconv, [to_i/1, to_s/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").

%% Booleans
cast(0, bool) ->
    false;
cast(0.0, bool) ->
    false;
cast(X, bool) when is_list(X) ->
    case string:to_upper(X) of
        "TRUE" -> true;
        "FALSE" -> false;
        _ -> {error, nab}
    end;
cast(X, bool) when is_number(X) ->
    true;
cast(X, bool) when is_boolean(X) ->
    X;
cast(_, bool) ->
    {error, nab};

%% Numbers
cast(X, num) when is_number(X) ->
    X;
cast(true, num) ->
    1;
cast(false, num) ->
    0;
cast(S, num) when is_list(S) ->
    tconv:to_num(S);
cast(_, num) ->
    ?ERR_VAL;

%% Strings
cast(true, str) ->
    "true";
cast(false, str) ->
    "false";
cast(N, str) when is_number(N) ->
    tconv:to_s(N);
cast(_, str) ->
    ?ERR_VAL.

%% Splits ssref to [Path, Ref]
split_ssref(Ssref) ->
    {just_path(Ssref), just_ref(Ssref)}.

%% Takes a same-site reference, returns path to the page it's on.
just_path(Ssref) when is_list(Ssref) ->
    MbR = append([?COND(hd(Ssref) == $/,
                        "/",
                        ""),
                  string:join(hslists:init(string:tokens(Ssref, "/")), "/"),
                  "/"]),

    %% For Ssref like /a1.
    ?COND(MbR == "//", "/", MbR).
          
just_ref(Ssref) ->
    last(tokens(Ssref, "/")).


%% Absolute path to location -> absolute path to another location.
walk_path(_, [$/ | _] = Dest) ->
    Dest;
walk_path(Currloc, Dest) ->
    Newstk = % New location stack
        foldl(fun(".",  Stk) -> Stk;
                 ("..", Stk) -> hslists:init(Stk);
                 (Word, Stk) -> append(Stk, [Word])
              end,
              string:tokens(Currloc, "/"),
              string:tokens(Dest, "/")),

    ?COND(length(Newstk) == 0,
          "/", %% too far up
          "/" ++ string:join(Newstk, "/") ++ "/").


expand_cellrange(StartRow, EndRow, StartCol, EndCol) ->    
    %% Make a list of cells that make up this range.
    Cells = map(fun(X) ->
                        map(fun(Y) -> {X, Y} end,
                            seq(StartRow, EndRow))
                end,
                seq(StartCol, EndCol)),
    %% Flatten Cells; can't use flatten/1 because there are strings in there.
    foldl(fun(X, Acc) -> append([Acc, X]) end,
          [], Cells).

%% Catch errors from error-throwing functions.
attempt(Mod, F, Args) ->
    try apply(Mod, F, Args) of
        Val -> {ok, Val}
    catch
        throw:X -> {error, X};
        exit:X  -> {error, X};
        error:X -> {error, X}
    end.
