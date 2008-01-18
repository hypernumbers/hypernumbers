%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for the main lexer.
%%%
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @end
%%%-----------------------------------------------------------------------------

-module(lexer_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% Single cell references.
cellref_test_() ->
    [
     ?_assert(lex("aa11") == {cellref, "aa11"}),
     ?_assert(lex("AA11") == {cellref, "aa11"}),
     ?_assert(lex("aA11") == {cellref, "aa11"}),
     ?_assert(lex("Aa11") == {cellref, "aa11"})
    ].

%% General id tokens (can be function names or column names).
id_test_() ->
    [
     ?_assert(lex("rc")  == {id, "rc"}),
     ?_assert(lex("Bob") == {id, "bob"}),
     ?_assert(lex("BOB") == {id, "bob"}),
     ?_assert(lex("BoB") == {id, "bob"}),

     ?_assert(lex("AVeryLongStringReallyReallyReallyReallyReallyYesReallyReallyReallyReallyLong")
              == {id, "averylongstringreallyreallyreallyreallyreallyyesreallyreallyreallyreallylong"})
    ].

%% Single cell reference with an absolute aspect.
fixed_cellref_test_() ->
    [
     ?_assert(lex("$bf99")  == {cellref, "$bf99"}),
     ?_assert(lex("bf$99")  == {cellref, "bf$99"}),
     ?_assert(lex("$bf$99") == {cellref, "$bf$99"}),
     ?_assert(lex("$BF99")  == {cellref, "$bf99"}),
     ?_assert(lex("BF$99")  == {cellref, "bf$99"}),
     ?_assert(lex("$BF$99") == {cellref, "$bf$99"})
    ].

%% Ranges.
range_test_() ->
    [
     ?_assert(lex("aa7:b22") == {range, "aa7:b22"}),
     ?_assert(lex("A1:z99")  == {range, "a1:z99"}),
     ?_assert(lex("1:99999") == {range, "1:99999"}),
     ?_assert(lex("a:ZZZZZ") == {range, "a:zzzzz"})
    ].

%% Strings.
string_test_() ->
    [
     ?_assert(lex("\"No matter how much money you spend,\"")        == {string, "No matter how much money you spend,"}),
     ?_assert(lex("\" you can't make a racehorse out of a pig.\"")  == {string, " you can't make a racehorse out of a pig."}),
     ?_assert(lex("'You can, however, make an awfully fast pig.'")  == {string, "You can, however, make an awfully fast pig."}),
     ?_assert(lex("\"-- An old saying about program efficiency'\"") == {string, "-- An old saying about program efficiency'"}),

     ?_assert(lex("\" \"") == {string, " "})
     
    ].

%% Numbers.
number_test_() ->
    [
     ?_assert(lex("42")        == {integer, "42"}),
     ?_assert(lex("12.3400")   == {float, "12.3400"}),
     ?_assert(lex("12.34e+10") == {float, "12.34e+10"}),
     ?_assert(lex("12.23e-10") == {float, "12.23e-10"}),
     ?_assert(lex("1234567890.123456789E+1") == {float, "1234567890.123456789e+1"})
    ].

%% Booleans.
boolean_test_() ->
    [
     ?_assert(lex("true")  == {boolean, "true"}),
     ?_assert(lex("false") == {boolean, "false"}),
     ?_assert(lex("TRUE")  == {boolean, "true"}),
     ?_assert(lex("FALSE") == {boolean, "false"}),
     ?_assert(lex("TruE")  == {boolean, "true"}),
     ?_assert(lex("FaLSe") == {boolean, "false"})
    ].

%% Combinations of things.
combo_test_() ->
    [
     %% Numbers, arithmetic operators, whitespace insensitivity.
     ?_assert(lex("=1 + 2*3.5/ 4.1e-10") == [{equals_sign}, {integer, "1"}, {plus}, {integer, "2"},
                                              {times}, {float, "3.5"}, {slash}, {float, "4.1e-10"}]),

     ?_assert(lex("=IF ( SUM(a1:D12) >= e1, 0, 1 ) ") == [{equals_sign}, {id,"if"}, {open_paren},
                                                          {id,"sum"}, {open_paren}, {range,"a1:d12"}, {close_paren},
                                                          {gte}, {cellref,"e1"}, {comma}, {integer,"0"}, {comma},
                                                          {integer,"1"}, {close_paren}])
    ].

%% Invalid inputs.
invald_test_() ->
    [
     ?_assert(lex("$A$A") == invalid),
     ?_assert(lex("$$a44") == invalid),
     ?_assert(lex("aa$55$") == invalid)
    ].

%%% ----------------  %%%
%%% Private functions %%%
%%% ----------------- %%%

%% Simple wrapper around the call to lexer to make the tests shorter, and to insulate against interface changes.
lex(Str) ->
    {ok, TokenList, _} = muin_lexer:string(Str),

    %% Return error if lexing is not successful, otherwise return stripped token(s).
    case lists:keysearch(invalid_token, 1, TokenList) of
        {value, _} ->
            invalid;
        false ->
            case length(TokenList) of
                1 ->
                    strip_token(hd(TokenList));
                _ ->
                    lists:map(fun strip_token/1, TokenList)
            end
    end.

%% Strips token of anything unnecessary returning a tuple like ones used in tests.
strip_token(Token) ->
    case length(tuple_to_list(Token)) of
        2 ->
            %% Punctuation, e.g. an equals sign.
            {TokenName, _} = Token,
            {TokenName};
        3 ->
            %% Number, boolean, string, cellref etc.
            {TokenName, _LineIdx, TokenData} = Token,
            {TokenName, TokenData}
    end.
