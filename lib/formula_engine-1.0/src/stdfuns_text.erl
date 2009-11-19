%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% @doc Excel-compatible text functions.
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
%%% on Windows Excel, and also have CHARMAC, CODEMAC, and CODEU and CHARU 
%%% functions.
%%%
%%% In iWork Numbers CODE and CHAR work with Unicode, and there are no 
%%% compatibility functions.
%%% @private

-module(stdfuns_text).
-compile(export_all).

-include("handy_macros.hrl").
-include("typechecks.hrl").

-import(tconv, [to_i/1, to_l/1, to_s/1]).

-import(muin_collect, [col/3, col/2, col/4]).

%% Excel 2004 API.
-export([value/1, t/1, replace/1, search/1, '&'/1, char/1, clean/1,
         concatenate/1,
         exact/1, find/1, fixed/1, left/1, len/1, lower/1, mid/1, proper/1,
         rept/1, right/1, substitute/1, text/1, trim/1, upper/1]).

%% Default set of rules for text
-define(default_num_rules, [first_array, cast_strings, cast_bools,
                            cast_blanks, cast_dates]).
-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).


t([V]) ->
    case col([V], [eval_funs, area_first, fetch], [return_errors]) of
        Err   when ?is_errval(Err) -> Err;
        [Str] when ?is_string(Str) -> Str;
        _Else -> ""
    end.


value([V]) ->
    col([V], [eval_funs, area_first, {cast, str, num}],
        [return_errors, {all, fun is_number/1}], fun value_/1).

value_([S]) ->
    S.

search([FindText, WithinText])->
    search([FindText, WithinText, 1]);

search([V1, V2, V3])->
    Str = col([V1, V2], [eval_funs, fetch, area_first, {cast, str}],
              [return_errors, {all, fun muin_collect:is_string/1}]),
    Num = col([V3], [eval_funs, fetch, area_first, {cast, num}, {cast, int}],
              [return_errors, {all, fun is_number/1}]),    
    muin_util:apply([Str, Num], fun search_/2).

search_([_Needle, Hay], [Start]) when Start < 1; Start > length(Hay) ->
    ?ERRVAL_VAL;
search_([Needle, Hay], [Start]) ->
    RegExp = wild_to_rgx(Needle),
    {_Start, End} = lists:split(Start-1, Hay),
    case re:run(End, RegExp, [{capture, first}, caseless]) of
        {match, [{S, _Length}]} -> S + Start;
        nomatch                 -> ?ERRVAL_VAL
    end.


replace([V1, V2, V3, V4]) ->
    Str = col([V1, V4], [eval_funs, fetch, area_first, {cast, str}],
              [return_errors, {all, fun muin_collect:is_string/1}]),
    Num = col([V2, V3], [eval_funs, fetch, area_first, {cast, num},
                         {cast, int}],
              [return_errors, {all, fun is_number/1}]),
    muin_util:apply([Str, Num], fun replace_/2).

replace_([_Str, _Replace], [Start, Len]) when Start =< 0; Len < 0 ->
    ?ERRVAL_VAL;
replace_([Str, Replace], [Start, _Len]) when Start > length(Str) ->
    lists:concat([Str, Replace]);
replace_([Str, Replace], [Start, Len]) ->
    {StartStr, Middle} = lists:split(Start-1, Str),
    case Len > length(Middle) of
        true ->
            lists:concat([StartStr, Replace]);
        false ->
            {_Del, End} = lists:split(Len, Middle),
            lists:concat([StartStr, Replace, End])
    end.
    

exact([V1, V2]) ->
    Rules = [first_array, cast_numbers, cast_bools, ban_dates, cast_blanks],
    Str1 = ?string(V1, Rules),
    Str2 = ?string(V2, Rules),
    string:equal(Str1, Str2).


len([Str]) when is_float(Str) ->
    case mochinum:digits(Str) of
          "0.0" -> 1;
          S     -> length(S)
         end;
len([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    length(NewStr).

mid([V1, V2, V3]) ->
    Str = col([V1], [eval_funs, fetch, area_first, {cast, str}],
              [return_errors, {all, fun muin_collect:is_string/1}]),
    Num = col([V2, V3], [eval_funs, fetch, area_first, {cast, num}, {cast, int}],
              [return_errors, {all, fun is_number/1}]),    
    muin_util:apply([Str, Num], fun mid_/2).

mid_([_Str], [Start, Len]) when Len < 0; Start =< 0 ->
    ?ERRVAL_VAL;
mid_([Str], [Start, _Len]) when Start > length(Str) ->
    [];
mid_([Str], [Start, Len]) ->
    string:substr(Str, Start, Len).

clean([Str])   ->
    col([Str], [eval_funs, fetch, area_first, {cast, str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun clean_/1).
clean_([Str]) ->
    Clean = fun(X) -> io_lib:printable_list([X])
                          andalso X =/= 10 andalso X =/= 8
            end,
    filter(Clean, Str).

%% Fixed is a bit of a mess
fixed([Num]) ->
    fixed([Num, 2, false]);
fixed([Num, Decimals]) ->
    fixed([Num, Decimals, false]);
fixed([N1, N2, N3]) ->
    Num = col([N1], [first_array, fetch_name,{cast,num}],
              [return_errors, {all, fun is_number/1}]),
    Dec = col([N2], [first_array, fetch_name,{cast,num}],
              [return_errors, {all, fun is_number/1}]),
    Com = col([N3], [first_array, fetch_name,{cast,bool}],
              [return_errors, {all, fun is_atom/1}]),
    muin_util:run_or_err([Num, Dec, Com], fun fixed_/1).

fixed_([Num, Dec, Com]) ->
    
    RoundedNum = stdfuns_math:round([Num, Dec]) * 1.0,
    Str = ?COND(Dec > 0,
                hd(io_lib:format("~." ++ to_l(Dec) ++ "f", [RoundedNum])),
                to_l(erlang:trunc(RoundedNum))),

    case Com of
        true ->
            Str;
        false ->
            [Int | Decs]=string:tokens(Str,"."),
            case Decs of
                [] -> commify(Int);
                _  -> commify(Int) ++ "."++hd(Decs)
            end
    end.

lower([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    string:to_lower(NewStr).

proper(Args) ->
    col(Args, [eval_funs, first_array, fetch_name, fetchdb, {cast, str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun proper_/1).

proper_([[]]) ->
    [];
proper_([Str]) ->
    make_proper(Str).

make_proper([$",H|Rest]) ->
    make_proper(Rest, [hd(string:to_upper([H])), $"]);
make_proper([H|Rest]) ->
    make_proper(Rest, [hd(string:to_upper([H]))]).

make_proper([], Acc) ->
    lists:reverse(Acc);
make_proper([Y, X | Rest], Acc) when Y == 32; Y == $-; Y == $" ->
    make_proper(Rest, [hd(string:to_upper([X])), Y | Acc]);
make_proper([X | Rest], Acc) ->
    make_proper(Rest, [hd(string:to_lower([X])) | Acc]).

upper([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    string:to_upper(NewStr).

char([V1]) ->
    col([V1], [eval_funs, area_first, fetch, {cast, int}],
        [return_errors, {all, fun is_integer/1}],
        fun char_/1).
char_([Code]) ->
    xmerl_ucs:to_utf8([Code]).

code([H|_Str]) ->
    hd(H).

find([Str, InStr]) ->
    find([Str, InStr, 1]);

find([V1, V2, V3]) ->

    Needle   = col([V1], [first_array, fetch_name,{cast,num,int},{cast,str}],
                   [return_errors, {all, fun muin_collect:is_string/1}]),
    HayStack = col([V2], [first_array, fetch_name, {cast, str}],
                   [return_errors, {all, fun muin_collect:is_string/1}]),
    Start    = col([V3], [first_array, fetch_name, {cast, int}],
                   [return_errors, {all, fun is_integer/1}]),
    
    muin_util:run_or_err([Needle, HayStack, Start], fun find_/1).

find_(["", _InStr, _Start]) ->
    1;
find_([_Str, _InStr, Start]) when Start < 1 ->
    ?ERRVAL_VAL;
find_([Str, InStr, Start]) ->
    InStr2 = string:substr(InStr, Start),
    case string:str(InStr2, Str) of
        0   -> ?ERRVAL_VAL;
        Idx -> (Idx + string:len(InStr) - (string:len(InStr) - Start+1))
    end.

left([Str])->
    NewStr=?string(Str,?default_str_rules),
    [lists:nth(1,NewStr)];
left([Str, Len]) ->
    NewStr=?string(Str,?default_str_rules),
    NewLen=erlang:trunc(?int(Len,?default_num_rules)),
    ?ensure(NewLen >= 0,?ERR_VAL),
    string:substr(NewStr, 1, NewLen).

right([Str])->
    NewStr=?string(Str,?default_str_rules),
    Len=length(NewStr),
    [lists:nth(Len,NewStr)];
right([Str, Len]) ->
    NewStr=?string(Str,?default_str_rules),
    NewLen=erlang:trunc(?int(Len,?default_num_rules)),
    ?ensure(NewLen >= 0,?ERR_VAL),
    TotalLen=length(NewStr),
    case (NewLen > TotalLen) of
        true ->
            NewStr;
        false ->
            string:substr(NewStr, TotalLen-NewLen+1, NewLen)
    end.

'&'(Strs) ->
    concatenate(Strs).

concatenate(Str) ->
    col(Str, [eval_funs, fetch, area_first, {cast,str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun concatenate_/1).

concatenate_(Str) ->
    lists:flatten(Str).
     
rept([Str, Reps]) ->
    A = col([Str],
            [first_array, fetch, {cast,str}],
            [return_errors, {all, fun muin_collect:is_string/1}]),
    
    B = col([Reps],
            [first_array, fetch, {conv, blank, 0},
             {cast, str, int}, {cast, bool, num}],
            [return_errors, {all, fun is_number/1}]),

    muin_util:apply([A, B], fun rept_/2).

rept_(_Str, [Reps]) when Reps < 0 ->
    ?ERRVAL_VAL;
rept_([Str], [Reps2]) ->
    Reps = erlang:trunc(Reps2),
    case length(Str) * Reps of
        X when X < 0; X > 32767 -> ?ERRVAL_VAL;
        _ ->
            Fun = fun(_) -> (Str) end,
            lists:flatten(concatenate(lists:map(Fun, lists:seq(1, Reps))))
    end.

substitute([_,_,_]=Args) ->
    col(Args, [first_array, fetch_name,{cast,str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun substitute_/1);

substitute([Text,OldText,NewText, N]) ->
    NArgs = col([Text,OldText,NewText],
                [first_array, fetch_name,{cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    
    Num = col([N], [first_array, fetch_name, {cast, num, int},
                    {cast, str, int}],
              [return_errors, {all, fun is_integer/1}]),

    muin_util:apply([NArgs, Num], fun substitute_/2).

substitute_([Text,OldText,NewText]) ->
    OldText2 = esc_rgx(OldText),
    re:replace(Text, OldText2, NewText, [global, {return, list}]).

substitute_([Text,OldText,NewText], [N]) when N > 0 ->
    OldText2 = lists:flatten(esc_rgx(OldText)),
    case re:run(Text, OldText2, [global]) of
        nomatch ->
            Text;
        {match, M} when length(M) < N ->
            Text;
        {match, Matches} ->
            [{Start, Len}] = lists:nth(N, Matches),
            {S1, E1}  = lists:split(Start, Text),
            {_S2, E2} = lists:split(Len, E1),
            S1 ++ NewText ++ E2
    end;

substitute_(_Args, _N) ->
    ?ERRVAL_VAL.

text([Value, Format]) ->
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {_Color, Fmtdstr}} = format:run_format(Value, Output),
    Fmtdstr.


trim(Args) ->
    col(Args, [first_array, fetch_name, {cast, str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun trim_/1).

trim_([S]) ->
    NewS = re:replace(S, "\\s+", " ", [global, {return, list}]),
    string:strip(NewS).

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

%% turns an Excel type wildcard into a regexp
wild_to_rgx(Str) -> wild_to_rgx(Str,[]).

%% converts Excel wild cards into Erlang RegExps
wild_to_rgx([],Acc)        -> lists:flatten(lists:reverse(Acc));
wild_to_rgx([$~,$*|T],Acc) -> wild_to_rgx(T,[$*,"\\"|Acc]);   
wild_to_rgx([$~,$?|T],Acc) -> wild_to_rgx(T,[$?,"\\"|Acc]);   
wild_to_rgx(["\\"|T],Acc)  -> wild_to_rgx(T,["\\","\\"|Acc]); 
wild_to_rgx([$^|T],Acc)    -> wild_to_rgx(T,[$^,"\\"|Acc]);
wild_to_rgx([$$|T],Acc)    -> wild_to_rgx(T,[$$,"\\"|Acc]);
wild_to_rgx([$[|T],Acc)    -> wild_to_rgx(T,[$[,"\\"|Acc]);
wild_to_rgx([$]|T],Acc)    -> wild_to_rgx(T,[$],"\\"|Acc]);
wild_to_rgx([$(|T],Acc)    -> wild_to_rgx(T,[$(,"\\"|Acc]);
wild_to_rgx([$)|T],Acc)    -> wild_to_rgx(T,[$),"\\"|Acc]);
wild_to_rgx([$||T],Acc)    -> wild_to_rgx(T,[$|,"\\"|Acc]);
wild_to_rgx([$+|T],Acc)    -> wild_to_rgx(T,[$+,"\\"|Acc]);
wild_to_rgx([$.|T],Acc)    -> wild_to_rgx(T,[$.,"\\"|Acc]);
wild_to_rgx([$*|T],Acc)    -> wild_to_rgx(T,[$+,$.|Acc]);  
wild_to_rgx([$?|T],Acc)    -> wild_to_rgx(T,[$.|Acc]);     
wild_to_rgx([H|T],Acc)     -> wild_to_rgx(T,[H|Acc]).      

%% escapes Erlang wild card characters for match expressions passed in from the
%% front then

esc_rgx(Str) -> esc_rgx(Str,[]).

esc_rgx([],Acc)        -> lists:reverse(Acc);
esc_rgx([$~,$?|T],Acc) -> esc_rgx(T,[$?,"\\"|Acc]);   % escape Excel wild card
esc_rgx(["\\"|T],Acc)  -> esc_rgx(T,["\\","\\"|Acc]); % escape Erlang RegExp
esc_rgx([$^|T],Acc)    -> esc_rgx(T,[$^,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$$|T],Acc)    -> esc_rgx(T,[$$,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$[|T],Acc)    -> esc_rgx(T,[$[,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$]|T],Acc)    -> esc_rgx(T,[$],"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([${|T],Acc)    -> esc_rgx(T,[${,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$}|T],Acc)    -> esc_rgx(T,[$},"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$(|T],Acc)    -> esc_rgx(T,[$(,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$)|T],Acc)    -> esc_rgx(T,[$0,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$||T],Acc)    -> esc_rgx(T,[$|,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$+|T],Acc)    -> esc_rgx(T,[$+,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$.|T],Acc)    -> esc_rgx(T,[$.,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([H|T],Acc)     -> esc_rgx(T,[H|Acc]).         % let the char through

