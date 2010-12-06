%%% Hasan Veldstra <hasan@hypernumbers.com>

%% solves some of your problems with match spec atoms of the form '$1' etc, etc
-define(MS(X),list_to_atom("$"++integer_to_list(X))). %" fix highlighting),%

%% Like cond, but lets you specify a pattern to match against (instead
%% of matching against true).
%-define(ifmatch(X, Pat, Trueexpr, Falseexpr),
%        case (X) of (Pat) -> Trueexpr; _ -> Falseexpr end).

%% Simpler lists:keysearch(). Returns the associated value directly, no messing
%% with pattern matching.
%-define(KEYSEARCH(Key, List),
%        element(2, element(2, keysearch(Key, 1, List)))).
