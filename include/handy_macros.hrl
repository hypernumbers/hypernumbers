%%% Any macros that expand to **code** and are handy.
%%% Hasan Veldstra <hasan@hypernumbers.com>

%% Import most useful functions from lists.
-import(lists, [any/2,
                append/1,
                append/2,
                dropwhile/2,
                filter/2,
                flatten/1,
                foldl/3,
                foreach/2,
                keysearch/3,
                last/1,
                map/2,
                member/2,
                reverse/1,
                seq/2,
                takewhile/2]).

%% Ternary if.
-define(COND(Test, TrueVal, FalseVal),
        case (Test) of true -> TrueVal; false -> FalseVal end).


%% Simpler lists:keysearch(). Returns the associated value directly, no messing
%% with pattern matching.
-define(KEYSEARCH(Key, List),
        element(2, element(2, keysearch(Key, 1, List)))).

%% Less typing. Handy for debugging.
-define(PUTS(Str),
        io:format(Str ++ "~n")).
