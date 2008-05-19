-module(muin_util).
-export([error/1,
         split_ssref/1,
         just_path/1,
         just_ref/1,
         expand_cellrange/4,
         walk_path/2,
         attempt/2]).

-import(string, [rchr/2, tokens/2]).
-import(tconv, [to_i/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").

%% Booleans
cast(0, bool) ->
    false;
cast(X, bool) when is_list(X) ->
    string:to_upper(X) == "TRUE";
cast(X, bool) when is_number(X) ->
    true;
cast(X, bool) when is_boolean(X) ->
    X;

%% Numbers
cast(X, num) when is_number(X) ->
    X;
cast(true, num) ->
    1;
cast(false, num) ->
    0;
cast(S, num) when is_list(S) ->
    case tconv:to_num(S) of
        N when is_number(N) -> N;
        E = {error, nan}    -> E
    end.

error('#NULL!')  -> throw({error, '#NULL!'});
error('#DIV/0!') -> throw({error, '#DIV/0!'});
error('#VALUE!') -> throw({error, '#VALUE!'});
error('#REF!')   -> throw({error, '#REF!'});
error('#NAME?')  -> throw({error, '#NAME?'});
error('#NUM!')   -> throw({error, '#NUM'});
error('#N/A')    -> throw({error, '#N/A'}).

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
attempt(F, Args) ->
  case catch apply(F, Args) of
    {'EXIT', Reason} ->
          {error, Reason};
      Val ->
          {ok, Val}
  end.
