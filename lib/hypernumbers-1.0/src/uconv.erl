%%% @doc Functions to convert between text in UTF-8 and UTF-32.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @end
%%%
%%% See:
%%%   * http://en.wikipedia.org/wiki/UTF-8
%%%   * http://www.ibm.com/developerworks/java/library/j-u-encode.html

-module(uconv).
-include_lib("eunit/include/eunit.hrl").

-export([utf8_to_32/1,
         utf16_to_32/1,
         utf32_to_8/1,
         utf32_to_16/1,
         utf16_to_8/1,
         utf8_to_16/1]).

%% @doc Binary with text encoded in UTF-8 -> list of UTF-32 code points.
utf8_to_32(Bin) ->
    u8to32(Bin, []).

%% @doc Binary with text encoded in UTF-16 -> list of UTF-32 code points.
utf16_to_32(List) ->
    u16to32(List, []).

%% @doc List of UTF-32 code points -> Binary with text encoded in UTF-8.
utf32_to_8(List) ->
    u32to8(List, []).

%% @doc List of UTF-32 code points -> Binary with text encoded in UTF-16.
utf32_to_16(List) ->
    u32to16(List, []).

%% @doc Binary with text encoded in UTF-16 -> Binary with text encoded in UTF-8
utf16_to_8(List) ->
    utf32_to_8(utf16_to_32(List)).

%% @doc Binary with text encoded in UTF-8 -> Binary with text encoded in UTF-16
utf8_to_16(List) ->
    utf32_to_16(utf8_to_32(List)).

%%% ----------------- %%%
%%% Private functions %%%
%%% ----------------- %%%

%% 0zzzzzzz -- one byte, ASCII equivalence range.
u8to32(<<2#0:1, CodePoint:7, Rest/binary>>, List) ->
     u8to32(Rest, [CodePoint | List]);

%% 110yyyyy 10zzzzzz -- two bytes, e.g. German or Russian.
u8to32(<<2#110:3, Y:5, 2#10:2, Z:6, Rest/binary>>, List) ->
    <<CodePoint:16>> = <<0:5, Y:5, Z:6>>,
    u8to32(Rest, [CodePoint | List]);

%% 1110xxxx 10yyyyyy 10zzzzzz -- three bytes, e.g. Chinese.
u8to32(<<2#1110:4, X:4,2#10:2, Y:6,2#10:2, Z:6, Rest/binary>>, List) ->
    <<CodePoint:16>> = <<X:4, Y:6, Z:6>>,
    u8to32(Rest, [CodePoint | List]);

%% 11110www 10xxxxxx 10yyyyyy 10zzzzzz -- four bytes, e.g. Gothic.
u8to32(<<2#11110:5, W:3, 2#10:2, X:6, 2#10:2, Y:6, 2#10:2, Z:6, Rest/binary>>, List) ->
    <<CodePoint:24>> = <<0:3, W:3, X:6, Y:6, Z:6>>,
    u8to32(Rest, [CodePoint | List]);

u8to32(<<>>, List) ->
    {ok, lists:reverse(List)}.


%% Pick out the surrogate pairs first
%% First  16 bytes match 110110xxxxxxxxxx (mask is 16#D800)
%% Second 16 bytes match 110111xxxxxxxxxx (mask is 16#DFFF)
u16to32(<<2#110110:6,Upper:10,2#110111:6,Lower:10,Rest/binary>>, Utf32) ->
    <<Interim:24/unsigned-integer>> = <<0:4,Upper:10,Lower:10>>,
    CodePoint = Interim+16#10000,
    u16to32(Rest, [CodePoint | Utf32]);

%% Otherwise the utf16 number is just the code point
u16to32(<<CodePoint:16/unsigned-integer,Rest/binary>>, Utf32) ->
    u16to32(Rest, [CodePoint | Utf32]);

u16to32(<<>>, Utf32) ->
    {ok, lists:reverse(Utf32)}.
    

%% 0-7F  0zzzzzzz -> 0zzzzzzz
u32to8([Utf32 | T], Utf8) when Utf32 < 16#80 ->
    u32to8(T, [Utf32 | Utf8]);

%% 80-7FF yyy yyzzzzzz -> 110yyyyy 10zzzzzz
u32to8([Utf32 | T], Utf8) when Utf32 < 16#800 ->
    <<_:5, Y:5, Z:6>> = <<Utf32:16>>,
    u32to8(T, [<<2#110:3, Y:5, 2#10:2, Z:6>> | Utf8]);

%% 800-FFFF xxxxyyyy yyzzzzzz -> 1110xxxx 10yyyyyy 10zzzzzz
u32to8([Utf32 | T], Utf8) when Utf32 < 16#10000 ->
    <<X:4, Y:6, Z:6>> = <<Utf32:16>>,
    u32to8(T, [<<2#1110:4, X:4, 2#10:2, Y:6, 2#10:2, Z:6>> | Utf8]);

%% 10000-10FFFF wwwxx xxxxyyyy yyzzzzzz -> 11110www 10xxxxxx 10yyyyyy 10zzzzzz
u32to8([Utf32 | T], Utf8) when Utf32 < 16#110000 ->
    <<0:3, W:3, X:6, Y:6, Z:6>> = <<Utf32:24>>,
    u32to8(T, [<<2#11110:5, W:3, 2#10:2, X:6, 2#10:2, Y:6, 2#10:2, Z:6>> | Utf8]);

u32to8([], Utf8) ->
    {ok, list_to_binary(lists:reverse(Utf8))}.

%% if the code point is outside the basic multilingual plane (ie > 16#FFFF)
%% then encode it as two 16 bit surrogate pairs
u32to16([Utf32 | T], Utf16) when Utf32 > 16#FFFF ->
    Interim = Utf32 - 16#10000,
    <<0:4, InterimU:10, InterimL:10>> = <<Interim:24>>,
    Upper = <<2#110110:6, InterimU:10>>,
    Lower = <<2#110111:6, InterimL:10>>,
    % remember when you list:reverse the code points you also list:reverse
    % the Utf16 pairs to load 'em into the accumulator backwards!
    u32to16(T, [Lower, Upper| Utf16]);

%% otherwise make it a 16 bit binary
u32to16([Utf32 | T], Utf16) ->
    NewAcc = <<Utf32:16>>,
    u32to16(T,[NewAcc | Utf16]);

u32to16([], Utf16) ->
    {ok, list_to_binary(lists:reverse(Utf16))}.


%%% ---------- %%%
%%% Unit tests %%%
%%% ---------- %%%

%%% NOTE: This file *must* be encoded in UTF-8 for tests to pass.

one_byte_test_() ->
    [
     ?_assert(utf8_to_32(<<"hello world">>) == {ok, "hello world"}),
     ?_assert(utf8_to_32(<<"unicode 12345 !@#$%">>) == {ok, "unicode 12345 !@#$%"}),

     ?_assert(utf32_to_8("hello world") == {ok, <<"hello world">>}),
     ?_assert(utf32_to_8("unicode 12345 !@#$%") == {ok, <<"unicode 12345 !@#$%">>})
    ].

two_bytes_test_() ->
    [
     ?_assert(utf8_to_32(<<"привет, как дела?">>) == {ok,[1087,1088,1080,1074,1077,1090,44,32,1082,1072,1082,32,1076,1077,1083,1072,63]}),

     ?_assert(utf32_to_8([1087,1088,1080,1074,1077,1090,44,32,1082,1072,1082,32,1076,1077,1083,1072,63]) == {ok, <<"привет, как дела?">>})
    ].

mixed_test_() ->
    [
     ?_assert(utf8_to_32(<<"абвгд abcde">>) == {ok, [1072, 1073, 1074, 1075, 1076, 32, 97, 98, 99, 100, 101]}),
     
     ?_assert(utf32_to_8([1072, 1073, 1074, 1075, 1076, 32, 97, 98, 99, 100, 101]) == {ok, <<"абвгд abcde">>})
    ].

utf16_test_() ->
    [
     ?_assert(utf32_to_16([122]) == {ok,<<" z">>}),

     ?_assert([97] == roundtrip([97])),

     ?_assert([16#1D11E] == roundtrip([16#1D11E])),

     ?_assert([16#64321] == roundtrip([16#64321]))
    ].

roundtrip(A) ->
    {ok, B} = utf32_to_16(A),
    {ok, C} = utf16_to_32(B),
    C.
    
           
