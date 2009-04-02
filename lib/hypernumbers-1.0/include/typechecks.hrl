%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Type and error checking macros used by stdfuns modules.

%%% Even though a lot of macros here are simple wrappers around function calls,
%%% it makes sense to keep them that way to make the type checking and casting
%%% code stand out in stdfuns.

%%% TODO: This is not just about type checks anymore - rename.

-include("errvals.hrl").

%%% Guards

-define(is_array(A),
        (is_tuple(A) andalso (element(1, A) == array))).

-define(is_range(A),
        (is_tuple(A) andalso (element(1, A) == range))).

-define(is_area(A),
        (?is_array(A) orelse ?is_range(A))).

-define(is_string(X),
        ((is_list(X)) orelse (is_tuple(X) andalso element(1, X) == ustring))).

-define(is_cellref(X),
        element(1, X) == cellref).

-define(is_rangeref(X),
        element(1, X) == rangeref).

%%% Collectors

%% implicit iteration when not in array context :(
%% a call to this macro has to be added to every single-value collector (e.g. ?number, ?bool &c)
-define(iinaa(X, Rules, Collector),
        if ?is_array(X) -> Collector(element(2, area_util:at(1, 1, X)), Rules);
           ?is_range(X) -> ?ERR_VAL;
           true         -> Collector(X, Rules)
        end).

-define(numbers(Xs, Rules),
        muin_collect:collect_numbers(Xs, Rules)).

-define(number(X, Rules),
        ?iinaa(X, Rules, fun(A_, B_) -> muin_collect:collect_number(A_, B_) end)).

-define(int(X, Rules),
        erlang:trunc(?number(X, Rules))).

-define(ints(Xs, Rules),
        lists:map(fun(X) -> erlang:trunc(X) end,
                  ?numbers(Xs, Rules))).

-define(bool(X, Rules), ?iinaa(X, Rules, fun(A_, B_) -> muin_collect:collect_bool(A_, B_) end)).

-define(remove_errors(Xs),
        muin_collect:remove_errors(Xs)).

-define(bools(Xs, Rules),
        muin_collect:collect_bools(Xs, Rules)).

-define(flatten_all(Xs),
        muin_collect:flatten_areas(Xs)).

-define(date(X, Rules),
        muin_collect:collect_date(X, Rules)).

-define(dates(Xs, Rules),
        muin_collect:collect_dates(Xs, Rules)).

-define(string(X, Rules),
        muin_collect:collect_string(X, Rules)).

-define(strings(Xs, Rules),
        muin_collect:collect_strings(Xs, Rules)).

-define(edate(X),
        if is_record(X, datetime) -> ok;
           true -> ?ERR_VAL
        end).

-define(utf8l(Ustr),
        ustring:to_utf8(Ustr)).

%% Ensure X is a string.
%% Using element() rather than pattern matching to avoid creating new names
%% in the current scope.
-define(estring(X),
        if element(1, X) == ustring andalso is_binary(element(2, X)) -> X; % STR
           is_list(X) -> X; % STR
           true -> ?ERR_VAL
        end).

%% Ensure Xs are all strings.
-define(estrings(Xs),
        foreach(fun(X) -> ?estring(X) end, Xs)).

%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%% OLD STUFF BELOW.
%%% TODO: Clean up.
%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-define(ensure_number(N),
        muin_checks:number(N)).

-define(ensure_numbers(Ns),
        muin_checks:numbers(Ns)).

-define(ensure_nonzero(N),
        muin_checks:nonzero(N)).

-define(ensure_non_negative(N),
        muin_checks:gte0(N)).

-define(ensure_non_negatives(Ns),
        muin_checks:gte0s(Ns)).

-define(ensure_positive(N),
        muin_checks:gt0(N)).

-define(ensure_no_errvals(Vs),
        muin_checks:die_on_errval(Vs)).

-define(ensure(Test, Action),
        muin_checks:ensure(Test, fun() -> Action end)).

-define(filter_numbers(Vs),
        muin_checks:filter_numbers(Vs)).
        
-define(filter_numbers_with_cast(Vs),
        muin_checks:filter_numbers_all(Vs)).
        
-define(flatten(L),
        muin_checks:deck(L)).
