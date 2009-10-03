
%%% @doc Tests for the XFL lexer.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% NB: This file is included into xfl_lexer.xrl

%% @doc Lexing functions for testing.
tlex(Input) ->
    ?ifmatch(string(Input),
             {ok, [{_Type, Val}], _},
             Val,
             foadplzkthx).
tlex(Input, Currcell) when is_list(Currcell) ->
    lex(Input, getxy(Currcell)).

-include_lib("eunit/include/eunit.hrl").
%% Check representation of cells C1 and C2 are same from cell Cfrom.
-define(check_same_from(C1, C2, Cfrom),
        ?_assert(tlex(C1, Cfrom) == tlex(C2, Cfrom))).

literals_test_() ->
    [
     %% **Floats**
     %% Floats can be in normal or scientific notation.
     %% Scientific notation is not case-sensitive.
     %% Scientific notation and normal notations are equivalent.
     ?_assert(tlex("1.2") == tlex("1.2E+0")),
     ?_assert(tlex("12.0") == tlex("1.2E+1")),
     ?_assert(tlex("12.0") == tlex("1.2e+1")),

     %% **Booleans**
     ?_assert(tlex(" TRUE ") == true),
     ?_assert(tlex(" FALSE  ") == false),
     %% Only stand-alone strings are matched.
     ?_assert(tlex("_TRUE_") =/= true),
     ?_assert(tlex("_FALSE") =/= false),
     ?_assert(tlex("TRUEfalse") =/= true),
     ?_assert(tlex("trueFALSE") =/= false)
    ].

a1refs_test_() ->
    [
     %% Straight-up same-page references can be done with ./ too.
     ?check_same_from("A1", "./A1", "B2"),
     ?check_same_from("a1", "./a1", "B2"),
     ?check_same_from("A1", "./a1", "B2")
    ].

rcrefs_test_() ->
    [
     ?check_same_from("R1C1", "r1C1", "B2"),
     ?check_same_from("R1C1", "./R1C1", "B2"),
     ?check_same_from("R[-1]C[+2]", "./r[-1]C[2]", "B2")
    ].

%% RC and A1-style refs are equivalent.
rc_a1_equiv_test_() ->
    [
     ?check_same_from("$A$1", "R1C1",       "B2"),
     ?check_same_from("A$1",  "R1C[-1]",    "B2"),
     ?check_same_from("$A1",  "R[-1]C1",    "B2"),
     ?check_same_from("A1",   "R[-1]C[-1]", "B2"),
     ?check_same_from("C3",   "R[+2]C[2]",  "A1")
    ].
