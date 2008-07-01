%%%----------------------------------------------------------------------------
%%% @doc Type and error checking macros used by stdfuns modules.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

%%% Even though a lot of macros here are simple wrappers around function calls,
%%% it makes sense to keep them that way to make the type checking and casting
%%% code stand out in stdfuns.

%%% TODO: This is not just about type checks anymore - rename.

-include("errvals.hrl").

%% Collect numbers from Xs.
-define(numbers(Xs, Rules),
        muin_collect:collect_numbers(Xs, Rules)).

%% Collect a date.
-define(date(X, Rules),
        muin_collect:collect_date(X, Rules)).

%% Collect dates from Xs.
-define(dates(Xs, Rules),
        muin_collect:collect_dates(Xs, Rules)).

%% Ensure X is a date.
-define(edate(X),
        if is_record(X, datetime) ->
                ok;
           true ->
                ?ERR_VAL
        end).

%% Convert a Ustring to a UTF-8 list.
-define(utf8l(Ustr),
        ustring:to_utf8(Ustr)).

%% Ensure X is a string.
%% Using element() rather than pattern matching to avoid creating new names
%% in the current scope.
-define(estring(X),
        if element(1, X) == ustring andalso is_binary(element(2, X)) ->
                ok;
           true ->
                ?ERR_VAL
        end).

%% Ensure Xs are all strings.
-define(estrings(Xs),
        foreach(fun(X) -> ?estring(X) end,
                Xs)).


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
        
