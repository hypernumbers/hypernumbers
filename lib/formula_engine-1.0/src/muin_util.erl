%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Various utility functions used by the formula engine.

-module(muin_util).
-export([array_at/3,
         cast/2,
         split_ssref/1,
         just_path/1,
         just_ref/1,
         expand_cellrange/4,
         walk_path/2,
         attempt/3,
         attempt/1]).

-import(string, [rchr/2, tokens/2]).
-import(tconv, [to_i/1, to_s/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").

array_at({array, Rows}, R, C) ->
    nth(C, nth(R, Rows)).

cast(X, Targtype) ->
    Xtype = get_type(X),
    cast(X, Xtype, Targtype).

get_type(X) when is_number(X) ->
    num;
get_type(X) when is_boolean(X) ->
    bool;
get_type(X) when is_record(X, datetime) ->
    date;
get_type(blank) ->
    blank;
get_type(X) ->
    case muin_collect:is_string(X) of
        true -> str;
        false -> unknown_type
    end.

%% X -> boolean
cast(X, num, bool) when X =< 0 -> false;
cast(X, num, bool) -> true;
cast(X, str, bool) ->
            case string:to_upper(X) of
                "TRUE"  -> true;
                "FALSE" -> false;
                _       -> {error, nab}
            end;    
cast(X, bool, bool)  -> X;
cast(_, date, bool)  -> true;
cast(_, blank, bool) -> false;
cast(_, _, bool) ->
    {error, nab};

%% X -> number
cast(X, num, num)      -> X;
cast(true, bool, num)  -> 1;
cast(false, bool, num) -> 0;
cast(X, str, num) ->
    tconv:to_num(X);
cast(_X, date, num) ->
    42; %% TODO:
cast(_, blank, num) -> 0;
cast(_, _, num) ->
    {error, nan};

%% X -> string
cast(X, num, str) ->
    tconv:to_s(X);
cast(X, str, str)      -> X;
cast(true, bool, str)  -> "true";  % STR!
cast(false, bool, str) -> "false"; % STR!
cast(_X, date, str) ->
    "1/1/1900"; % TODO:
cast(_, blank, str) -> ""; % STR!
cast(_, _, str) ->
    {error, nas};

cast(_X, _, date) ->
    #datetime{}. % TODO!
    
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
%% The first argument is a list of path components, the second is a string.
%% The first comes from the server side of things, and the second comes from
%% the path field in ref objects.
walk_path(_, Dest = [$/|_]) ->
    string:tokens(Dest, "/");
walk_path(Currloc, Dest) ->
    Newstk = foldl(fun(".",  Stk) -> Stk;
                      ("..", []) ->  [];
                      ("..", Stk) -> hslists:init(Stk);
                      (Word, Stk) -> append(Stk, [Word])
                   end,
                   Currloc,
                   string:tokens(Dest, "/")),
    Newstk.

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

attempt(Fun) when is_function(Fun) ->
    try Fun() of
        Val -> {ok, Val}
    catch
        throw:X -> {error, X};
        exit:X  -> {error, X};
        error:X -> {error, X}
    end.        
