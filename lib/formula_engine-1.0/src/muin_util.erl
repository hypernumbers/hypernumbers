%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Various utility functions used by the formula engine.

-module(muin_util).
-export([
         create_name/0,
         make_refX/3,
         array_at/3,
         cast/2,
         cast/3,
         split_ssref/1,
         just_path/1,
         just_ref/1,
         expand_cellrange/1,
         expand_cellrange/4,
         walk_path/2,
         walk_zpath/2,
         attempt/3,
         attempt/1,
         apply/2,
         run/2,
         run_or_err/2,
         bounds_indexes/1,
         tl_row/1,
         br_row/1,
         tl_col/1,
         br_col/1,
         normalize/1,
         get_type/1
        ]).

-define(SECS_IN_DAY, 86400).

-include("typechecks.hrl").
-include("muin_records.hrl").
-include("spriki.hrl").

create_name() ->
    Bin = crypto:rand_bytes(8),
    mochihex:to_hex(Bin).

apply(Args, Fun) ->
    case lists:keyfind(errval, 1, Args) of
        {errval, Val} -> {errval, Val};
        false         -> erlang:apply(Fun,Args)
    end.

run(Args, Fun) ->
    case lists:keyfind(errval, 1, Args) of
        {errval, Val} -> {errval, Val};
        false         -> Fun(Args)
    end.


run_or_err(Args, Fun) ->
    case lists:keyfind(errval, 1, Args) of
        {errval, Val} ->
            {errval, Val};
        false ->
            Fun([ hd(X) || X <- Args])
    end.

array_at({array, Rows}, R, C) ->
    lists:nth(C, lists:nth(R, Rows)).

get_type(X) when is_number(X)           -> num;
get_type(X) when is_boolean(X)          -> bool;
get_type(X) when is_record(X, datetime) -> date;
get_type(X) when ?is_array(X)           -> array;
get_type(X) when ?is_errval(X)          -> error;
get_type(blank)                         -> blank;
get_type(X) ->
    case muin_collect:is_string(X) of
        true  -> str;
        false -> unknown_type
    end.

cast(Val, Type) ->
    case get_type(Val) of
        Type -> Val;
        Else -> cast(Val, Else, Type)
    end.

%% X -> boolean
cast(0, num, bool)   -> false;
cast(0.0, num, bool)   -> false;
cast(_, num, bool)   -> true;
cast(_, date, bool)  -> true;
cast(_, blank, bool) -> false;
cast(X, str, bool) ->
    case string:to_upper(X) of
        "TRUE"  -> true;
        "FALSE" -> false;
        _       -> {error,  nab}
    end;
cast(_, _, bool) -> {error, nab};

cast(X, Type, int) ->
    case cast(X, Type, num) of
        {error, nan}       -> {error, nan};
        F when is_float(F) -> erlang:trunc(F);
        I                  -> I
    end;

%% X -> number
cast(X, num, num)      -> X;
cast(true, bool, num)  -> 1;
cast(false, bool, num) -> 0;
cast(X, str, num)      -> tconv:to_num(X);
cast(X, date, num)     ->
    #datetime{date = D, time = T} = X,
    Days = calendar:date_to_gregorian_days(D) -
        (calendar:date_to_gregorian_days({1900,1,1}) - 2),
    Secs = calendar:time_to_seconds(T),
    Days + Secs/?SECS_IN_DAY;
cast(_, blank, num)    -> 0;
cast(_, _, num)        -> {error, nan};

%% X -> string
cast(X, num, str)      -> tconv:to_s(X);
cast(X, str, str)      -> X;
cast(true, bool, str)  -> "TRUE";  % STR!
cast(false, bool, str) -> "FALSE"; % STR!
cast(X, date, str)     -> muin_date:to_rfc1123_string(X);
cast(_, blank, str)    -> "";      % STR!
cast(_, _, str)        -> {error, nas};

%% X -> date
cast(X, num, date) when X >= 0 ->

    % Hope to god the seconds doesnt get represented in
    % eng format
    Days = erlang:trunc(X),
    Secs = erlang:trunc((X - Days) * ?SECS_IN_DAY),

    % Offset days by excel epoch
    EDays = Days + (calendar:date_to_gregorian_days({1900,1,1}) - 2),
    Time  = calendar:seconds_to_time(Secs),

    #datetime{date= calendar:gregorian_days_to_date(EDays),
              time = Time};

cast(X, str, date) ->
    case tconv:to_num(X) of
        {error, nan} ->
            case dh_date:parse(X) of
                {error, bad_date} -> {error, nad}; %TODO Dont throw
                {Date, Time}      -> #datetime{date=Date, time=Time}
            end;
        Num ->
            cast(Num, num, date)
    end;

cast(false, bool, date) ->
    #datetime{date= {1900, 1, 1}, time = {0,0,0}};
cast(true, bool, date) ->
    #datetime{date= {1900, 1, 2}, time = {0,0,0}};
cast(_X, _, date)   ->
    {error, nad}.

%% Splits ssref to [Path, Ref]
split_ssref(Ssref) ->
    {just_path(Ssref), just_ref(Ssref)}.

%% Takes a same-site reference, returns path to the page it's on.
just_path(Ssref) when is_list(Ssref) ->
    H = case (hd(Ssref) == $/) of
            true  -> "/";
            false -> ""
        end,
    Str = string:join(hslists:init(string:tokens(Ssref, "/")), "/"),
    case {H, Str} of
        {[], []} -> [];
        _        -> MbR = lists:append([H, Str, "/"]),
                    % For Ssref like /a1.
                    case (MbR == "//") of
                        true  -> "/";
                        false -> MbR
                    end
    end.

just_ref(Ssref) ->
    lists:last(string:tokens(Ssref, "/")).

%% Absolute path to location -> absolute path to another location.
%% The first argument is a list of path components, the second is a string.
%% The first comes from the server side of things, and the second comes from
%% the path field in ref objects.
walk_path(_, Dest = [$/|_]) ->
    string:tokens(Dest, "/");
walk_path(Currloc, Dest) ->
    lists:foldl(fun(".",  Stk) -> Stk;
                   ("..", [])  ->  [];
                   ("..", Stk) -> hslists:init(Stk);
                   (Word, Stk) -> lists:append(Stk, [Word])
                end,
                Currloc,
                string:tokens(Dest, "/")).

walk_zpath(_Path, [{zseg, _, _} | _T] = Dest) ->
    Dest;
walk_zpath(Path, ZPath) ->
    Len = length(Path),
    Zips = lists:duplicate(Len, seg),
    Segs = lists:zip(Zips, Path),
    lists:foldl(fun({seg, "."}, Stk)    -> Stk;
                   ({seg, ".."}, [])    -> [];
                   ({seg, ".."}, Stk)   -> hslists:init(Stk);
                   (Word, Stk) -> lists:append(Stk, [Word])
                end,
                Segs,
                ZPath).

expand_cellrange(StartRow, EndRow, StartCol, EndCol) ->
    % Make a list of cells that make up this range.
    Cells = lists:map(fun(X) ->
                        lists:map(fun(Y) -> {X, Y} end,
                            lists:seq(StartRow, EndRow))
                end,
                lists:seq(StartCol, EndCol)),
    % Flatten Cells; can't use flatten/1 because there are strings in there.
    lists:foldl(fun(X, Acc) -> lists:append([Acc, X]) end,
          [], Cells).

make_refX(Site, Path, #rangeref{type = row, text = Txt}) ->
    Ref = strip(Txt),
    {row, {Y1, Y2}}= hn_util:parse_ref(Ref),
     #refX{site = Site, path = Path, type = url, obj = {row, {Y1, Y2}}};
make_refX(Site, Path, #rangeref{type = col, text = Txt}) ->
    Ref = strip(Txt),
    {column, {X1, X2}} = hn_util:parse_ref(Ref),
    #refX{site = Site, path = Path, type = url, obj = {column, {X1, X2}}}.

strip(Txt) -> [Ref | _T] = lists:reverse(string:tokens(Txt, "/")),
              Ref.

expand_cellrange(R) when ?is_rangeref(R) ->
    {{ColIndex1, RowIndex1}, {ColIndex2, RowIndex2}} = bounds_indexes(R),
    expand_cellrange(RowIndex1, RowIndex2, ColIndex1, ColIndex2).

%% Return static column & row indexes for a range reference.
bounds_indexes(#rangeref{type=finite} = R) ->
    {Col1, Row1} = R#rangeref.tl,
    {Col2, Row2} = R#rangeref.br,
    ColIndex1 = muin:col_index(Col1),
    RowIndex1 = muin:row_index(Row1),
    ColIndex2 = muin:col_index(Col2),
    RowIndex2 = muin:row_index(Row2),
    {{ColIndex1, RowIndex1}, {ColIndex2, RowIndex2}}.

tl_row(R) ->
    {{_, RowIndex}, {_, _}} = bounds_indexes(R),
    RowIndex.

br_row(R) ->
    {{_, _}, {_, RowIndex}} = bounds_indexes(R),
    RowIndex.

tl_col(R) ->
    {{ColIndex, _}, {_, _}} = bounds_indexes(R),
    ColIndex.

br_col(R) ->
    {{_, _}, {ColIndex, _}} = bounds_indexes(R),
    ColIndex.

%% Catch errors from error-throwing functions.
attempt(Mod, F, Args) ->
    try apply(Mod, F, Args) of
        Val -> {ok, Val}
    catch
        Error:Reason when Error =:= error orelse Error =:= throw ->
            error_logger:info_msg("attempt to eval ~p/~p/~p failed~n- for ~p : ~p~n"
                                   "-with stacktrace of ~p~n",
                                   [Mod, F, Args, Error, Reason, erlang:get_stacktrace()]),
            {error, Reason}
    end.

attempt(Fun) when is_function(Fun) ->
    try Fun() of
        Val -> {ok, Val}
    catch
        Error:Reason when Error =:= error orelse Error =:= throw ->
            error_logger:error_msg("attempt to eval a fun failed~n- for ~p : ~p~n"
                                   "-with stacktrace of ~p~n",
                                   [Error, Reason, erlang:get_stacktrace()]),
            {error, Reason}
    end.


%%% @doc Re-assemble a prettified and tidied up formula string from AST.
%%% Normalizations performed:
%%% * extraneous whitespace is removed
%%% * no whitespace between function name and list of arguments
%%% * one space after comma in argument lists
%%% * no whitespace around operators other than division "/"
%%% * logical values are upper-case
%%% * all references are upper-cased
%%% * floats in scientific format use lower-case "e"
%%% * one space after commas "," and semicolons ";" in array constants

%%% TODO: Handling translations? (Function names, logical values...)
%%% TODO: Excel preserves whitespace around operators.  We can't do that
%%%       without making the parser ridiculously more complex or throwing
%%%       Yecc away and using an ABNF/PEG parser.
%%% TODO: Write unit tests for this.

normalize([Op|Args]) when ?is_operator(Op) ->
    string:join([normalize(X) || X <- Args],
                case Op of
                    '/' -> " / ";           % insert spaces around division
                    _   -> atom_to_list(Op) % but not other ops
                end);
normalize([Func|Args]) when is_atom(Func) ->
    FuncStr = string:to_upper(atom_to_list(Func)),
    ArgsStr = string:join([normalize(X) || X <- Args], ", "),
    FuncStr ++ "(" ++ ArgsStr ++ ")";
normalize(X) when is_integer(X)    -> integer_to_list(X);
normalize(true)                    -> "TRUE";
normalize(false)                   -> "FALSE";
normalize(C) when ?is_cellref(C)   -> string:to_upper(C#cellref.text);
normalize(R) when ?is_rangeref(R)  -> string:to_upper(R#rangeref.text);
normalize(N) when ?is_namedexpr(N) -> string:to_upper(N#namedexpr.text);
normalize({F, OrigStr}) when is_float(F), ?is_string(OrigStr) -> string:to_lower(OrigStr);
normalize(A) when ?is_array(A)     ->
    {array, Rows} = A,
    RowsStr = lists:foldr(fun(Row, Acc) ->
                            Normalized = lists:map(fun normalize/1, Row),
                            Joined = string:join(Normalized, ", "),
                            [Joined|Acc]
                    end,
                    [],
                    Rows),
    "{" ++ string:join(RowsStr, "; ") ++ "}";
normalize(S) when ?is_string(S)    -> string:concat("\"", string:concat(S, "\""));
normalize(E) when ?is_errval(E)    -> atom_to_list(element(2, E));
normalize([])                      -> "()".
