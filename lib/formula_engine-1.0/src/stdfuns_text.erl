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

%% Excel 2004 API.
-export([value/1, t/1, replace/1, search/1, '&'/1, char/1, clean/1, concatenate/1,
         exact/1, find/1, fixed/1, left/1, len/1, lower/1, mid/1, proper/1,
         rept/1, right/1, substitute/1, text/1, trim/1, upper/1]).

%% Default set of rules for text
-define(default_num_rules, [cast_strings, cast_bools,
                            cast_blanks, cast_dates]).
-define(default_str_rules, [cast_numbers, cast_bools,
                            cast_blanks, cast_dates]).


t([V]) when ?is_string(V) -> V;
t([_])                    -> "".


value([S]) when ?is_string(S) ->
    case tconv:to_num(S) of
        {error, nan} -> ?ERR_VAL;
        Num          -> Num
    end;
value([B]) when ?is_blank(B) ->
    0;
value([_]) ->
    ?ERR_VAL.


search([FindText, WithinText])-> search([FindText, WithinText, 1]);
search([FindText, WithinText, StartPoint])->
    NewFind = ?string(FindText, ?default_str_rules),
    NewWithin = ?string(WithinText, ?default_str_rules),
    NewStart = ?int(StartPoint, ?default_num_rules),
    ?ensure(NewStart >= 1, ?ERR_VAL),
    ?ensure(NewStart =< length(NewWithin), ?ERR_VAL),
    RegExp = wild_to_rgx(NewFind),
    {_Start, End} = lists:split(NewStart-1, NewWithin),
    case regexp:match(End, RegExp) of
        {match,Start, _Length} -> Start + NewStart - 1;
        nomatch                -> ?ERR_VAL
    end.

replace([Str,Start,Replace,InsertStr]) ->
    NewStr=?string(Str,?default_str_rules),
    StartInt=?int(Start,?default_num_rules),
    ?ensure(StartInt >= 0,?ERR_VAL),
    ReplaceInt=?int(Replace,?default_num_rules),
    ?ensure(ReplaceInt >= 0,?ERR_VAL),
    NewInsertStr=?string(InsertStr,?default_str_rules),
    if
        (StartInt >= length(NewStr)) -> lists:concat([Str,NewInsertStr]);
        
        true                          ->
            {StartStr,MiddleStr}=lists:split(StartInt-1,NewStr),
            {_Delete,EndStr}=lists:split(ReplaceInt,MiddleStr),
            lists:concat([StartStr,NewInsertStr,EndStr])
    end.


exact([V1, V2]) ->
    Rules = [cast_numbers, cast_bools, ban_dates, cast_blanks],
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

mid([Str, Start, Len]) ->
    NewStr=?string(Str,?default_str_rules),
    NewStart=erlang:trunc(?int(Start,?default_num_rules)),
    ?ensure(NewStart >0, ?ERR_VAL),
    NewLen=?int(Len,?default_num_rules),
    ?ensure(NewLen >0, ?ERR_VAL),
    if
        (NewLen >= length(NewStr)) -> NewStr;
        true                       -> mid1(NewStr,NewStart,NewLen)
    end.

mid1(Str, Start, Len) ->
    string:substr(Str, Start, Len).

clean([true])  -> "TRUE";
clean([false]) -> "FALSE";
clean([Str])   -> NewStr=?string(Str,?default_str_rules),
                  Clean = fun(X) -> (X > 31 andalso X < 123) end,
                  filter(Clean, NewStr).

%% Fixed is a bit of a mess
fixed([Num]) ->
    fixed([Num, 2, false]);
fixed([Num, Decimals]) ->
    fixed1([Num, Decimals, false]);
fixed([Num, Decimals, true]) ->
    fixed1([Num,Decimals,true]);
fixed([Num, Decimals, false]) ->
    fixed1([Num,Decimals,false]).
% all the casting etc is done in the helper
fixed1([Num,Decimals,NoCommas]) ->
    NewNum=?number(Num,?default_num_rules),
    NewDecs=?number(Decimals,?default_num_rules),
    ?ensure(NewDecs =< 127, ?ERR_VAL),
    RoundedNum = stdfuns_math:round([NewNum, NewDecs]) * 1.0,
    Str = ?COND(NewDecs > 0,
                hd(io_lib:format("~." ++ to_l(NewDecs) ++ "f", [RoundedNum])),
                to_l(erlang:trunc(RoundedNum))),
    case NoCommas of
        true ->
            Str;
        false ->
            [Int,Dec]=string:tokens(Str,"."),
            commify(Int) ++ "."++Dec
    end.

lower([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    string:to_lower(NewStr).

proper([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    {ok, Words} = regexp:split(NewStr, "\\s+"),
    Capwords = map(fun([H|T]) -> string:to_upper([H]) ++ T end,
                   Words),
    string:join(Capwords, " ").

upper([Str]) ->
    NewStr=?string(Str,?default_str_rules),
    string:to_upper(NewStr).

char([V1]) ->
    Code = ?int(V1, [cast_strings, cast_bools, ban_dates, ban_blanks]),
    xmerl_ucs:to_utf8([Code]).

code([Str]) ->
    xmerl_ucs:from_utf8(string:substr(Str, 1)).


find([Str, InStr]) ->
    find([Str, InStr, 1]);
find([V1, V2, V3]) ->
    Str = ?string(V1, ?default_str_rules),
    InStr = ?string(V2, ?default_str_rules),
    Start = ?int(V3, ?default_num_rules),
    case string:equal(Str, "") of
        true ->
            1; % Empty string matches the first character.
        false ->
            ?ensure(Start >= 1, ?ERR_VAL),
            ?ensure(Start =< string:len(InStr), ?ERR_VAL),
            InStr2 = string:substr(InStr, Start),
            Idx = string:str(InStr2, Str),
            case (Idx + string:len(InStr) - (string:len(InStr) - Start+1)) of
                0 -> ?ERR_VAL;
                N -> N
            end
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
    if
        (NewLen > TotalLen) -> NewStr;
        true                -> string:substr(Str,TotalLen-NewLen+1,NewLen)
    end.

'&'(Strs) ->
    concatenate(Strs).

concatenate(Str) ->
    ?ensure_string_under_limit(concatenate1(Str, [])).
    
concatenate1([],Acc) ->
    Acc;
concatenate1([H|T],Acc) when is_float(H) ->
    case mochinum:digits(H) of
        "0.0" -> concatenate1(["0"|T],Acc);
        S     -> concatenate1([S|T],Acc)
    end;
concatenate1([true|T],Acc) ->
    concatenate1(["TRUE"|T],Acc);
concatenate1([false|T],Acc) ->
    concatenate1(["FALSE"|T],Acc);
concatenate1(["0.0"|T],Acc) ->
    concatenate1(["0"|T],Acc);
concatenate1([H|T],Acc) ->
    Str = ?string(H,?default_str_rules),
    NewAcc=Acc++Str,
    concatenate1(T,NewAcc).

rept([Str, Reps]) ->
    NewStr=?string(Str,?default_str_rules),
    NewReps=?int(Reps,?default_num_rules),
    Len=length(NewStr)*NewReps,
    ?ensure(Len < 32767, ?ERR_VAL),
    ?ensure(Len >= 0,    ?ERR_VAL),
    Fun = fun(_) -> (NewStr) end,
    lists:flatten(concatenate([map(Fun, lists:seq(1, NewReps))])).

substitute([Text,OldText,NewText]) ->
    Text2=?string(Text,?default_str_rules),
    OldText2=?string(OldText,?default_str_rules),
    NewText2=?string(NewText,?default_str_rules),
    OldText3=esc_rgx(OldText2),
    case regexp:gsub(Text2, OldText3, NewText2) of
        {ok, Res, _Repcnt} -> Res;
        nomatch            -> Text2
    end;
substitute([Text,OldText,NewText,N]) ->
    Text2=?string(Text,?default_str_rules),
    OldText2=?string(OldText,?default_str_rules),
    NewText2=?string(NewText,?default_str_rules),
    List=string:tokens(Text2,OldText2),
    if
        (N > length(List)) ->
            Text;
        true ->
            {StartList,EndList}=list:split(N,List),
            string:join(StartList,OldText2)++NewText2++string:join(EndList,OldText2)
    end.


text([Value, Format]) ->
    {erlang, {_Type, Output}} = format:get_src(Format),
    {ok, {_Color, Fmtdstr}} = format:run_format(Value, Output),
    Fmtdstr.


trim([S]) when ?is_string(S) ->
    {ok, NewS, _} = regexp:gsub(S, "\\s+", " "),
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
wild_to_rgx([$~,$*|T],Acc) -> wild_to_rgx(T,[$*,"\\"|Acc]);   % escape Excel wild card
wild_to_rgx([$~,$?|T],Acc) -> wild_to_rgx(T,[$?,"\\"|Acc]);   % escape Excel wild card
wild_to_rgx(["\\"|T],Acc)  -> wild_to_rgx(T,["\\","\\"|Acc]); % escape Erlang RegExp
wild_to_rgx([$^|T],Acc)    -> wild_to_rgx(T,[$^,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$$|T],Acc)    -> wild_to_rgx(T,[$$,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$[|T],Acc)    -> wild_to_rgx(T,[$[,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$]|T],Acc)    -> wild_to_rgx(T,[$],"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$(|T],Acc)    -> wild_to_rgx(T,[$(,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$)|T],Acc)    -> wild_to_rgx(T,[$0,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$||T],Acc)    -> wild_to_rgx(T,[$|,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$+|T],Acc)    -> wild_to_rgx(T,[$+,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$.|T],Acc)    -> wild_to_rgx(T,[$.,"\\"|Acc]);   % escape Erlang RegExp
wild_to_rgx([$*|T],Acc)    -> wild_to_rgx(T,[$+,$.|Acc]);     % convert Excel wildcard
wild_to_rgx([$?|T],Acc)    -> wild_to_rgx(T,[$.|Acc]);        % convert Excel wildcard
wild_to_rgx([H|T],Acc)     -> wild_to_rgx(T,[H|Acc]).         % let the char through

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
esc_rgx([$(|T],Acc)    -> esc_rgx(T,[$(,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$)|T],Acc)    -> esc_rgx(T,[$0,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$||T],Acc)    -> esc_rgx(T,[$|,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$+|T],Acc)    -> esc_rgx(T,[$+,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([$.|T],Acc)    -> esc_rgx(T,[$.,"\\"|Acc]);   % escape Erlang RegExp
esc_rgx([H|T],Acc)     -> esc_rgx(T,[H|Acc]).         % let the char through

