%%% @doc Implements ODF user-specified criteria (used in SUMIF, AVERAGEIF,
%%% database functions etc).
%%% @author HV <hasan@hypernumbers.com>

-module(odf_criteria).

-export([create/1]).

-include("typechecks.hrl").

-define(INVALID_SPEC, {error, invalid_spec}).

%%% @doc Create a comparator fun from a spec.
%%% @spec create(Spec) -> Selector | Error where
%%%   Spec = number() | boolean() | string()
%%%   Result = function() | {error, Reason :: atom()}
%%% @end

%%% For a number or logical value, the argument shall equal the given value.
%%% "argument" = argument to resulting fun, "value" = value specified by user.
%%% Value *must* be a constant, no references or expressions are allowed.
create(V) when is_number(V); is_boolean(V) ->
    fun(X) -> stdfuns_logical:'='([X, V]) end;
%%% For a string, first see if it begins with a logical operator in which case
%%% the argument must compare appropriately with the value following the
%%% operator. Otherwise, match the argument against the text.,
create(S) when ?is_string(S) ->
    Fun = case string:substr(S, 1, 2) of
              "<=" ->
                  C = lex_constant(string:substr(S, 3)),
                  fun(X) -> stdfuns_logical:'<='([X, C]) end;
              ">=" ->
                  C = lex_constant(string:substr(S, 3)),
                  fun(X) -> stdfuns_logical:'>='([X, C]) end;
              "<>" ->
                  C = lex_constant(string:substr(S, 3)),
                  fun(X) -> stdfuns_logical:'<>'([X, C]) end;
              _ ->
                  case string:substr(S, 1, 1) of
                      "<" ->
                          C = lex_constant(string:substr(S, 2)),
                          fun(X) -> stdfuns_logical:'<'([X, C]) end;
                      ">" ->
                          C = lex_constant(string:substr(S, 2)),
                          fun(X) -> stdfuns_logical:'>'([X, C]) end;
                      "=" ->
                          C = lex_constant(string:substr(S, 2)),
                          fun(X) -> stdfuns_logical:'='([X, C]) end;
                      _ ->
                          fun(X) -> stdfuns_logical:'='([X, S]) end
                  end          
          end,
    Fun;
create(_) ->
    ?INVALID_SPEC.

%%% FIXME: lexer expects booleans to be in uppercase (fix in the lexer).
%%% also: a potential source of confusion/errors is embedded string constants.
lex_constant(S) ->
    case xfl_lexer:lex(S, {1, 1}) of
        {ok, [{T, V}]} when T == int; T == bool; T == str ->
            V;
        {ok, [{float, {F, _OrigStr}}]} ->
            F;
        Else ->
            io:format("Else = ~p~n", [Else]),
            ?INVALID_SPEC
    end.


%%% tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").

-define(match_p(Spec, V),
        (create(Spec))(V)).

number_and_boolean_test_() ->
    [
     ?match_p(1, 1) == true,
     ?match_p(10.5, 10.5) == true,
     ?match_p(1, 0) == false,
     ?match_p(10.5, 7.5) == false,

     ?match_p(true, true) == true,
     ?match_p(false, true) == false,
     ?match_p(false, false) == true,
     ?match_p(false, true) == false
    ].

logical_comparison_test_() ->
    [
     ?match_p("=10", 10) == true,
     ?match_p("=10.5", 10.5) == true,
     ?match_p("=TRUE", true) == true,
     ?match_p("=FALSE", false) == true,

     ?match_p("<>10", 1) == true,
     ?match_p("<>10.5", 1.5) == true,
     ?match_p("<>TRUE", false) == true,
     ?match_p("<>FALSE", true) == true
    ].
