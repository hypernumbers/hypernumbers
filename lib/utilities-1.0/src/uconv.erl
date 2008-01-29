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
         utf32_to_8/1]).

%% @doc Binary with text encoded in UTF-8 -> list of UTF-32 code points.
utf8_to_32(Bin) ->
    u8to32(Bin, []).


%% @doc List of UTF-32 code points -> Binary with text encoded in UTF-8.
utf32_to_8(List) ->
    u32to8(List, []).


%%% ----------------- %%%
%%% Private functions %%%
%%% ----------------- %%%

%% 0zzzzzzz -- one byte, ASCII equivalence range.
u8to32(<<2#0:1, Cp:7, Rest/binary>>, List) ->
     u8to32(Rest, [Cp | List]);

%% 110yyyyy 10zzzzzz -- two bytes, e.g. German or Russian.
u8to32(<<2#110:3, Y:5, 2#10:2, Z:6, Rest/binary>>, List) ->
    <<Cp:16>> = <<0:5, Y:5, Z:6>>,
    u8to32(Rest, [Cp | List]);

%% 1110xxxx 10yyyyyy 10zzzzzz -- three bytes, e.g. Chinese.
u8to32(<<2#1110:4, X:4,2#10:2, Y:6,2#10:2, Z:6, Rest/binary>>, List) ->
    <<Cp:16>> = <<X:4, Y:6, Z:6>>,
    u8to32(Rest, [Cp | List]);

%% 11110www 10xxxxxx 10yyyyyy 10zzzzzz -- four bytes, e.g. Gothic.
u8to32(<<2#11110:5, W:3, 2#10:2, X:6, 2#10:2, Y:6, 2#10:2, Z:6, Rest/binary>>, List) ->
    <<Cp:24>> = <<0:3, W:3, X:6, Y:6, Z:6>>,
    u8to32(Rest, [Cp | List]);

u8to32(<<>>, List) ->
    {ok, lists:reverse(List)}.



%% 0-7F  0zzzzzzz -> 0zzzzzzz
u32to8([U32 | Tl], Utf8) when U32 < 16#80 ->
    u32to8(Tl, [U32 | Utf8]);

%% 80-7FF yyy yyzzzzzz -> 110yyyyy 10zzzzzz
u32to8([U32 | Tl], Utf8) when U32 < 16#800 ->
    <<_:5, Y:5, Z:6>> = <<U32:16>>,
    u32to8(Tl, [<<2#110:3, Y:5, 2#10:2, Z:6>> | Utf8]);

%% 800-FFFF xxxxyyyy yyzzzzzz -> 1110xxxx 10yyyyyy 10zzzzzz
u32to8([U32 | Tl], Utf8) when U32 < 16#10000 ->
    <<X:4, Y:6, Z:6>> = <<U32:16>>,
    u32to8(Tl, [<<2#1110:4, X:4, 2#10:2, Y:6, 2#10:2, Z:6>> | Utf8]);

%% 10000-10FFFF wwwxx xxxxyyyy yyzzzzzz -> 11110www 10xxxxxx 10yyyyyy 10zzzzzz
u32to8([U32 | Tl], Utf8) when U32 < 16#110000 ->
    <<0:3, W:3, X:6, Y:6, Z:6>> = <<U32:24>>,
    u32to8(Tl, [<<2#11110:5, W:3, 2#10:2, X:6, 2#10:2, Y:6, 2#10:2, Z:6>> | Utf8]);

u32to8([], Utf8) ->
    {ok, list_to_binary(lists:reverse(Utf8))}.
    


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
