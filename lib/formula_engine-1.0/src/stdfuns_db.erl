%%% @doc Built-in database functions.
%%% @author HV <hasan@hypernumbers.com>
%%% @private
%%% 
%%% NOTE: Computed criteria aren't supported yet (waiting on Muin refactoring).

-module(stdfuns_db).
-export([daverage/1, dcount/1, dcounta/1, dget/1, dmax/1, dmin/1, dproduct/1,
         dstdev/1, dstdevp/1, dsum/1, dvar/1, dvarp/1]).
-compile(export_all).

-import(muin_collect, [ col/2, col/3, col/4 ]).

-include("handy_macros.hrl").
-include("typechecks.hrl").

%% DbR = database range, Fld = field, CR = criteria range, A = action,
%% Vs = values

daverage([DbR, Fld, CR]) ->
    A = fun(Vs) ->
                stdfuns_stats:average([ X || X <- Vs, is_number(X) ])
        end,
    db_aggregate_func(DbR, Fld, CR, A) .

dcount([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:count(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A) .

dcounta([DbR, Fld, CR]) ->
    A = fun(Vs) ->
                stdfuns_stats:counta([ X || X <- Vs, not(?is_errval(X)) ])
        end,
    db_aggregate_func(DbR, Fld, CR, A) .

dget([DbR, Fld, CR]) ->
    A = fun([])  -> ?ERR_VAL;
           ([V]) -> V;
           (_L)  -> ?ERR_NUM
        end,
    db_aggregate_func(DbR, Fld, CR, A).

dmax([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:max(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dmin([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:min(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dproduct([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_math:product(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dstdev([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:stdev(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dstdevp([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:stdevp(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dsum([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_math:sum(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dvar([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:var(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).

dvarp([DbR, Fld, CR]) ->
    A = fun(Vs) -> stdfuns_stats:varp(Vs) end,
    db_aggregate_func(DbR, Fld, CR, A).


%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_aggregate_func(DbR, Fld, CR, A) ->
    
    F = fun([NDbR], [NFld], [NCR]) ->
                Vs = generic_select_values(NDbR, NFld, NCR),
                A(Vs)
        end,
    
    generic_argument_check(DbR, Fld, CR, F).

is_str_or_int(X) when ?is_string(X) orelse is_integer(X) ->
    true;
is_str_or_int(_) ->
    false.

generic_argument_check(DbR, Fld, CR, F) ->

    NDbr = col([DbR], [eval_funs, fetch, {conv, blank, ?ERRVAL_VAL}],
               [return_flat_errors, {all, fun is_range/1}]),
    NFld = col([Fld], [eval_funs, fetch, area_first, {cast, bool, num},
                       {conv, blank, 0}, {cast, num, int}],
               [return_errors, {all, fun is_str_or_int/1}]),
    NCr  = col([CR],[eval_funs, fetch],
               [return_flat_errors, {all, fun is_range/1}]),

    muin_util:apply([NDbr, NFld, NCr], F).


is_range(Range) when ?is_range(Range) orelse ?is_rangeref(Range) ->
    true;
is_range(_) ->
    false.
    
generic_select_values(DbR, Fld, CR) ->
    Db = odf_db:from_range(DbR),
    Criteriaset = odf_db:criteria_from_range(CR),
    Matched = odf_db:select(Db, Criteriaset),
    Vs = odf_db:db_field(Fld, Matched),
    case Vs of
        no_such_field -> ?ERR_VAL;
        _             -> Vs
    end.
    

%% tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
-define(eqlp(Computed, Expected, Msg),
        case (Computed) == (Expected) of
            true  -> io:format("ok~n");
            false -> io:format("FAIL: ~s~n", [Msg])
        end).

-define(DB, {range, [ % from dbfuncs.xls
                      ["Tree",   "Height", "Age", "Yield", "Profit"],
                      ["Apple",  18,       20,     14,     105.5],
                      ["Pear",   12,       12,     10,     96],
                      ["Cherry", 13,       14,     9,      105],
                      ["Apple",  14,       15,     10,     75],
                      ["Pear",   9,        8,      8,      76],
                      ["Apple",  8,        9,      6,      45],
                      ["Apple",  15,       19,     15,     110],
                      ["Apple",  15,       10,     14,     100],
                      ["Cherry", 4,        3,      3,      27]
                     ]}).

%%% First 3 are straightforward selectors.

%%% apple trees with height > 10
-define(CRIT1, {range, [
                        ["Tree",  "Height", "Age", "Yield", "Profit"],
                        ["Apple", ">10",    blank, blank,   blank]
                       ]}).
%%% all trees with height > 10
-define(CRIT2, {range, [
                        ["Tree",  "Height", "Age", "Yield", "Profit"],
                        [blank,   ">10",    blank, blank,   blank]
                       ]}).
%%% all pear trees, and apple trees with height > 10
-define(CRIT3, {range, [
                        ["Tree",  "Height", "Age", "Yield", "Profit"],
                        ["Pear",  blank,    blank, blank,   blank],
                        ["Apple", ">10",    blank, blank,   blank]
                       ]}).

%%% Advanced criteria:

%%% Multiple conditions on the same column.

-define(CRIT4, {range, [
                        ["Tree",  "Height", "Height", "Age", "Yield", "Profit"],
                        [blank,   ">=10",   "<=15",   blank, blank,   blank],
                        [blank,   "<5",     blank,    blank, blank,   blank]
                       ]}).

%%% Computed criteria.

-define(CRIT5, {range, []}).

simple_criteria_test() ->
    %% 53 = dsum([?DB, "Yield", ?CRIT1]),
    %% 53 = dsum([?DB, 4, ?CRIT1]), % same but with column index
    13.25 = daverage([?DB, "Yield", ?CRIT1]),
    %% 12.0 = daverage([?DB, "Yield", ?CRIT2]),
    %% 71 = dsum([?DB, "Yield", ?CRIT3]),
    ok.

advanced_criteria_test() ->
    61 = dsum([?DB, "Yield", ?CRIT4]),
    ok.
    
perf() ->
    Db = {range,
        [["Tree", "Height", "Age", "Yield", "Profit"]] ++
          lists:duplicate(1000, ["Apple", 18, 20, 14, 105.5]) ++
          lists:duplicate(1000, ["Pear", 12, 12, 10, 96]) ++
          lists:duplicate(1000, ["Cherry", 13, 14, 9, 105]) ++
          lists:duplicate(1000, ["Apple", 14, 15, 10, 75]) ++
          lists:duplicate(1000, ["Pear", 9, 8, 8, 76]) ++
          lists:duplicate(1000, ["Apple", 8, 9, 6, 45]) ++
          lists:duplicate(1000, ["Apple", 18, 20, 14, 105.5]) ++
          lists:duplicate(1000, ["Pear", 12, 12, 10, 96]) ++
          lists:duplicate(1000, ["Cherry", 13, 14, 9, 105]) ++
          lists:duplicate(1000, ["Apple", 14, 15, 10, 75]) ++
          lists:duplicate(1000, ["Pear", 9, 8, 8, 76]) ++
          lists:duplicate(1000, ["Apple", 8, 9, 6, 45])},
    {A,B,C} = erlang:now(),
    dsum([Db,"Yield",?CRIT4]),
    {A2,B2,C2} = erlang:now(),
    io:format("Time taken: {~p, ~p, ~p}~n", [A2-A,B2-B,C2-C]).
