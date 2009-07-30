%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Various utility functions used by the formula engine.
%%% @private

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

-compile(export_all).

-define(SECS_IN_DAY, 86400).

-import(string, [rchr/2, tokens/2]).
-import(tconv, [to_i/1, to_s/1, to_num/1]).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").

array_at({array, Rows}, R, C) ->
    nth(C, nth(R, Rows)).

get_type(X) when is_number(X)           -> num;
get_type(X) when is_boolean(X)          -> bool;
get_type(X) when is_record(X, datetime) -> date;
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

%% X -> number
cast(X, num, num)      -> X;
cast(true, bool, num)  -> 1;
cast(false, bool, num) -> 0;
cast(X, str, num)      -> to_num(X);
cast(X, date, num)     ->
    #datetime{date = D, time = T} = X,
    Days = calendar:date_to_gregorian_days(D) -
        (calendar:date_to_gregorian_days({1900,1,1}) - 2),
    Secs = calendar:time_to_seconds(T),
    Days + Secs/?SECS_IN_DAY;
cast(_, blank, num)    -> 0;
cast(_, _, num)        -> {error, nan};

%% X -> string
cast(X, num, str)      -> to_s(X);
cast(X, str, str)      -> X;
cast(true, bool, str)  -> "TRUE";  % STR!
cast(false, bool, str) -> "FALSE"; % STR!
cast(X, date, str)     -> muin_date:to_rfc1123_string(X);
cast(_, blank, str)    -> "";      % STR!
cast(_, _, str)        -> {error, nas};

%% X -> date   
cast(X, num, date) when X >= 0 ->
    
    % Hope to go the seconds doesnt get represented in
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
            % httpd date parser does nothing, need to replace
            {D1, T1} =
                case muin_date:from_rfc1123_string(X) of
                    {ok, {D, T}} -> {D, T};
                    _            -> ?ERR_VAL
                end,
            #datetime{date = D1, time = T1};
        Num ->
            cast(Num, num, date)
    end;

cast(false, bool, date) ->
    #datetime{date= {1900, 1, 1}, time = {0,0,0}};
cast(true, bool, date) ->
    #datetime{date= {1900, 1, 2}, time = {0,0,0}};
cast(_X, _, date)   ->
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

expand_cellrange(R) when ?is_rangeref(R) ->
    {{ColIndex1, RowIndex1}, {ColIndex2, RowIndex2}} = bounds_indexes(R),
    expand_cellrange(RowIndex1, RowIndex2, ColIndex1, ColIndex2).

%% Return static column & row indexes for a range reference.
bounds_indexes(R) when ?is_rangeref(R) ->
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
    RowsStr = foldr(fun(Row, Acc) ->
                            Normalized = map(fun normalize/1, Row),
                            Joined = string:join(Normalized, ", "),
                            [Joined|Acc]
                    end,
                    [],
                    Rows),
    "{" ++ string:join(RowsStr, "; ") ++ "}";
normalize(S) when ?is_string(S)    -> string:concat("\"", string:concat(S, "\""));
normalize(E) when ?is_errval(E)    -> atom_to_list(element(2, E));
normalize([])                      -> "()".
