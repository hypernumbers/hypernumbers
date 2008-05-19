%%% Type and error checking macros used by stdfuns modules.
%%% Hasan Veldstra <hasan@hypernumbers.com>

-include("errvals.hrl").

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
        
