%%% Hasan Veldstra <hasan@hypernumbers.com>

%% solves some of your problems with match spec atoms of the form '$1' etc, etc
-define(MS(X),list_to_atom("$"++integer_to_list(X))). %" fix highlighting),%

%% Like cond, but lets you specify a pattern to match against (instead
%% of matching against true).
-define(ifmatch(X, Pat, Trueexpr, Falseexpr),
        case (X) of (Pat) -> Trueexpr; _ -> Falseexpr end).

%% Simpler lists:keysearch(). Returns the associated value directly, no messing
%% with pattern matching.
-define(KEYSEARCH(Key, List),
        element(2, element(2, keysearch(Key, 1, List)))).

%% Less typing. Handy for debugging.
-define(p(Str),
        io:format("~p~n", [Str])).

%% A fun that takes no arguments.
-define(L(BODY),
        fun() -> BODY end). 

%% A fun that takes one argument.
-define(Lx(BODY),
        fun(X) -> BODY end).

%% A fun that takes two arguments.
-define(Lxy(BODY),
        fun(X, Y) -> BODY end).

%% Just like funXY, nicer in folds.
-define(Lxacc(BODY),
        fun(X, Acc) -> BODY end).

%% Shorthand for ustring:pr(Ustr).
-define(pr(Ustr),
        ustring:pr(Ustr)).

%% Silly hack for ifs.
-define(else, true).

-define(DEFER(Expr), fun() -> Expr end).
