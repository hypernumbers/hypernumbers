%%% @doc Implements ODF user-specified criteria (used in SUMIF, AVERAGEIF,
%%% database functions etc).
%%% @author HV <hasan@hypernumbers.com>

-module(odf_criteria).

-export([create/1]).

-include("muin_records.hrl").
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
%% create(V) when is_number(V) ->
%%     fun(X) -> is_number(V) andalso X == V end;
%% create(V) when is_boolean(V) ->
%%     fun(X) -> is_boolean(V) andalso X == V end;
%% create(V) when ?is_errval(V) ->
%%     fun(X) -> X == V end;
%% create(V) when is_record(V, datetime) ->
%%     fun(X) -> X == V end;

%%% For a string, first see if it begins with a logical operator in which case
%%% the argument must compare appropriately with the value following the
%%% operator. Otherwise, match the argument against the text.,
create(">="++C) -> type_fun( fun stdfuns_logical:'>='/1, C);
create(">"++C)  -> type_fun( fun stdfuns_logical:'>'/1, C);
create("<>"++C) -> type_fun( fun stdfuns_logical:'<>'/1, C);
create("<="++C) -> type_fun( fun stdfuns_logical:'<='/1, C);
create("<"++C)  -> type_fun( fun stdfuns_logical:'<'/1, C);
create("="++C)  -> type_fun( fun stdfuns_logical:'='/1, C);

create(C) ->

    {DoRegex, Match} = conv_str(C),

    fun(X) when is_boolean(C) ->
            is_boolean(X) andalso X == C;
       (X) when X =:= C -> true;
       (X) when DoRegex == true ->
            not (nomatch == re:run(muin_util:cast(X, str),
                                   Match, [caseless, anchored]));
       (_) -> false
    end.

conv_str(Str) when is_number(Str); is_atom(Str); ?is_string(Str) ->
    Re = stdfuns_text:esc_rgx(tconv:to_s(Str)),
    Re1 = re:replace(Re, "\\?", "[a-z0-9]{1}", [{return, list}, global]),
    Re2 = re:replace(Re1, "\\*", "[a-z0-9]\\*", [{return, list}, global]),
    {true, "^"++Re2++"$"}; %"

conv_str(Str) ->
    {false, Str}.

    
                    

type_fun(Fun, C) ->
    Val = lex_constant(C),
    fun(X) ->
            muin_util:get_type(X) == muin_util:get_type(Val)
                andalso Fun([X, Val])
    end.

%%% FIXME: lexer expects booleans to be in uppercase (fix in the lexer).
%%% also: a potential source of confusion/errors is embedded string constants.
lex_constant(S) ->
    case xfl_lexer:lex(S, {1, 1}) of
        {ok, [{T, V}]} when T == int; T == bool; T == str ->
            V;
        {ok, [{float, {F, _OrigStr}}]} ->
            F;
        {ok, [{name, Str}]} ->
            Str;
        Else ->
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
