%%% Any macros that expand to **code** and are handy.
%%% Hasan Veldstra <hasan@hypernumbers.com>

%% Import most useful functions from lists.
-import(lists, [all/2,
                any/2,
                append/1,
                append/2,
                dropwhile/2,
                filter/2,
                flatten/1,
                flatmap/2,
                foldl/3,
                foreach/2,
                keysearch/3,
                last/1,
                map/2,
                member/2,
                nth/2,
                reverse/1,
                seq/2,
                sublist/3,
                takewhile/2,
                zip/2]).

%% Ternary if.
-define(COND(Test, TrueVal, FalseVal),
        case (Test) of true -> TrueVal; false -> FalseVal end).

%% Shortcut for if.
-define(IF(Test, TrueVal),
        ?COND(Test, TrueVal, nothing)).

%% Simpler lists:keysearch(). Returns the associated value directly, no messing
%% with pattern matching.
-define(KEYSEARCH(Key, List),
        element(2, element(2, keysearch(Key, 1, List)))).

%% Less typing. Handy for debugging.
-define(PUTS(Str),
        io:format(Str ++ "~n")).

%% A fun that takes no arguments.
-define(fun0(BODY),
        fun() -> BODY end). 

%% A fun that takes one argument.
-define(funX(BODY),
        fun(X) -> BODY end).

%% A fun that takes two arguments.
-define(funXY(BODY),
        fun(X, Y) -> BODY end).

%% Just like funXY, nicer in folds.
-define(funXAcc(BODY),
        fun(X, Acc) -> BODY end).

%% Shorthand for ustring:pr(Ustr).
-define(pr(Ustr),
        ustring:pr(Ustr)).
