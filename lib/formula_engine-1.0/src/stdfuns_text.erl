%%% @doc Excel-compatible text functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @end

%%% Doing the simplest thing that will work for now, which is: assuming that
%%% strings are lists of UTF-8 code units everywhere else in the application,
%%% and creating ustrings from them in these functions.

%%% TODO:
%%%   * Treat incoming strings as ustrings.
%%%   * Add type and error checks.

%%% INCOMPATIBILITY NOTES:
%%%
%%% 1. CHAR in Excel works with Windows ANSI or Mac character sets depending on
%%%    host platform. We work with Unicode.
%%%
%%% 2. Similarly, our implementation of CODE returns a list of the Unicode
%%%    codepoints of the first character in a string.
%%%
%%% Perhaps we can provide compatibility functions, like CHARWIN/CHARMAC and
%%% CODEWIN / CODEMAC, or maybe let our CHAR and CODE work as their namesakes
%%% on Windows Excel, and also have CHARMAC, CODEMAC, and CODEU and CHARU functions.
%%%
%%% In iWork Numbers CODE and CHAR work with Unicode, and there are no compatibility
%%% functions.

-module(stdfuns_text).
-compile(export_all).

-include("handy_macros.hrl").
-include("typechecks.hrl").

-import(tconv, [to_i/1, to_l/1, to_s/1]).

-import(ustring, [new/1, exact/2, eql/2, upcase/1, downcase/1, capitalize/1,
                  concat/2, substr/2, substr/3, index/2, rindex/2, gsub/3]).

%% Excel 2004 API.
-export([
         %% Comparison functions.
         exact/1,
         len/1,
         mid/1,

         %% Conversion functions.
         clean/1,
         %%dollar/2, TODO: Need formats.
         %%pound/2,  TODO: Need formats.
         fixed/1,
         lower/1,
         proper/1,
         %%t/1, TODO: Needs types.
         %%text/2, TODO: Needs formats.
         %%trim/1, TODO:
         upper/1,
         %%value/1, TODO: Need formats.
         %%yen/2, TODO: Need formats.

         %% Locate text.
         char/1,
         find/1,
         left/1,
         right/1,
         %%search/1, TODO:

         concatenate/1,
         %%replace/1, TODO:
         rept/1
         %% substitute/1, TODO:

         %% Work with full-width text TODO:
        ]).

exact([Str1, Str2]) ->
    exact(new(Str1), new(Str2)).

len([Str]) ->
    ustring:len(new(Str)).

mid([Str, Start, Len]) ->
    substr(new(Str), Start, Len).

%% FIXME: Make work with text in any language (with uregex).
clean([Str]) ->
    Clean = fun(X) -> (X > 31 andalso X < 123) end,
    filter(Clean, Str).

fixed([Num]) ->
    fixed([Num, 2]);
fixed([Num, Decimals]) when is_number(Decimals) ->
    fixed([Num, Decimals, false]);
fixed([Num, NoCommas]) when is_boolean(NoCommas) ->
    fixed([Num, 2, NoCommas]);
fixed([Num, Decimals, NoCommas]) ->
    RoundedNum = stdfuns_math:round(Num, Decimals) * 1.0,
    Str = ?COND(Decimals > 0,
                hd(io_lib:format("~." ++ to_l(Decimals) ++ "f", [RoundedNum])),
                to_l(erlang:trunc(RoundedNum))),

    case NoCommas of
        true ->
            Str;
        false ->
            commify(Str) ++
                ?COND(Decimals > 0,
                      begin
                          Idx = string:rchr(Str, $.),
                          "." ++ sublist(Str, Idx + 1, length(Str) - Idx)
                      end,
                      "")
    end.

lower([Str]) ->
    ?pr(downcase(new(Str))).

proper([Str]) ->
    ?pr(capitalize(new(Str))).

upper([Str]) ->
    ?pr(upcase(new(Str))).

char([Code]) ->
    xmerl_ucs:to_utf8([Code]).

code([Str]) ->
    xmerl_ucs:from_utf8(?pr(substr(new(Str), 1))).

find([Substr, Str]) ->
    find([Substr, Str, 1]);
find([Substr_, Str_, Start]) ->
    Str = new(Str_),
    Substr = new(Substr_),
    SearchStr = substr(Str, Start, ustring:len(Str) - Start + 1), %% Slice from Start to end
    Idx = index(SearchStr, Substr),
    Idx + (ustring:len(Str) - (ustring:len(Str) - Start + 1)).

left([Str, Len]) ->
    ?pr(substr(new(Str), 1, Len)).

right([Str_, Len]) ->
    Str = new(Str_),
    ?pr(substr(Str, ustring:len(Str) - Len + 1, Len)).
    
concatenate([Strs_]) ->
    Strs = map(?funX(new(X)), Strs_),
    ?pr(foldl(?funXAcc(concat(Acc, X)),
              ustring:empty(),
              Strs)).

rept([Str_, Reps]) ->
    Str = new(Str_),
    concatenate([map(fun(_) -> (Str) end, lists:seq(1, Reps))]).



%%% ----------------- %%%
%%% Private functions %%%
%%% ----------------- %%%

%% Takes an integer (as list), and gives it back formatted with commas.
commify(IntAsStr) when is_list(IntAsStr) ->
  commify(lists:reverse(IntAsStr), $,, []).
commify([A, B, C, D | T], P, Acc) ->
  commify([D|T], P, [P, C, B, A|Acc]);
commify(L, _, Acc) ->
  lists:reverse(L) ++ Acc.
