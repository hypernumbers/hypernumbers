%%% Any macros that expand to **code** and are handy.
%%% Hasan Veldstra <hasan@hypernumbers.com>

%% Ternary if.
-define(COND(Test, TrueVal, FalseVal),
        case (Test) of true -> TrueVal; false -> FalseVal end).

%% Simpler lists:keysearch(). Returns the associated value directly. No messing
%% with pattern matching.
-define(KEYLOOKUP(Key, List),
        element(2, element(2, keysearch(Key, 1, List)))).

-define(PUTS(Str),
        io:format(Str ++ "~n")).
