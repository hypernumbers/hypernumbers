%%% @doc Excel-compatible text functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

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

%% Excel 2004 API.
-export([
         %%dollar/2, TODO: Need formats.
         %%pound/2,  TODO: Need formats.
         %%replace/1, TODO:
         %%search/1, TODO:
         %%t/1, TODO: Needs types.
         %%trim/1, TODO:
         %%value/1, TODO: Need formats.
         %%yen/2, TODO: Need formats.
         '&'/1,
         char/1,
         clean/1,
         concatenate/1,
         exact/1,
         find/1,
         fixed/1,
         left/1,
         len/1,
         lower/1,
         mid/1,
         proper/1,
         rept/1,
         right/1,
         substitute/1,
         text/1,
         upper/1
        ]).

exact([Str1, Str1]) ->
    true;
exact(_) ->
    false.
    
len([Str]) ->
    length(Str).

mid([Str, Start, Len]) ->
    mid1(Str, ?ensure_number(Start), ?ensure_number(Len)).
mid1(Str, Start, Len) ->
    string:substr(Str, Start, Len).

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
    string:to_lower(Str).

proper([Str]) ->
    {ok, Words} = regexp:split(Str, "\\s+"),
    Capwords = map(fun([H|T]) -> string:to_upper([H]) ++ T end,
                   Words),
    string:join(Capwords, " ").

upper([Str]) ->
    string:to_upper(Str).

char([V1]) ->
    Code = ?number(V1, [cast_strings, cast_bools, ban_dates, ban_blanks]),
    xmerl_ucs:to_utf8([Code]).

code([Str]) ->
    xmerl_ucs:from_utf8(string:substr(Str, 1)).

find([Substr, Str]) ->
    find([Substr, Str, 1]);
find([Substr, Str, Start]) ->
    SearchStr = string:substr(Str, Start,
                              string:len(Str) - Start + 1), %% Slice from Start to end
    Idx = string:str(SearchStr, Substr),
    Idx + (string:len(Str) - (string:len(Str) - Start + 1)).

left([Str, Len]) ->
    string:substr(Str, 1, Len).

right([Str, Len]) ->
    string:substr(Str, string:len(Str) - Len + 1, Len).

'&'(Strs) ->
    concatenate(Strs).

concatenate(Vs) ->
    Strs = muin_collect:collect_strings(Vs, [cast_numbers, cast_bools, cast_dates, cast_blanks]),
    foldl(fun(X, Acc) ->
                  Acc ++ X
          end,
          "", Strs).

rept([Str, Reps]) ->
    concatenate([map(fun(_) -> (Str) end, lists:seq(1, Reps))]).

substitute([Text, Oldtext, Newtext]) ->
    {ok, Res, _Repcnt} = regexp:gsub(Text, Oldtext, Newtext),
    Res.

text([Value, Format]) ->
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {_Color, Fmtdstr}} = format:run_format(Value, Output),
    Fmtdstr.


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
