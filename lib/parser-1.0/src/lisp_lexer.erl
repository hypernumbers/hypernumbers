%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module(lisp_lexer).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%%    {ok,Tokens,Line} | {error,ErrorInfo,Line}.

string([], L, [], Ts) ->			%No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
	{A,Alen,Ics1,L1} ->			%Accepting end state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{A,Alen,Ics1,L1,S1} ->		%After an accepting state
	    string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L1), Ts);
	{reject,Alen,Tlen,Ics1,L1,S1} ->
	    {error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
	{A,Alen,Tlen,Ics1,L1,S1} ->
	    string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L1), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%%  Test for and remove the end token wrapper.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {error,S}, Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(Chars, Line, yystate(), Chars, 0, reject, 0);
token({Line,State,Tcs,Tlen,Action,Alen}, Chars, _) ->
    token(Chars, Line, State, Tcs ++ Chars, Tlen, Action, Alen).

%% token(InChars, Line, State, TokenChars, TokenLen, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

token(Ics0, L0, S0, Tcs, Tlen0, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{L1,S1,Tcs,Alen1,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1));
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{L1,S1,Tcs,Tlen1,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{eof,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    {done,{error,{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},L1},Ics1};
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1))
    end.

%% tokens_cont(RestChars, Line, Token)
%%  Test if we have detected the end token, if so return done else continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, skip_token) ->
    token(Rest, Line, yystate(), Rest, 0, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(Chars, Line, yystate(), Chars, 0, [], reject, 0);
tokens({tokens,Line,State,Tcs,Tlen,Ts,Action,Alen}, Chars, _) ->
    tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Ts, Action, Alen);
tokens({skip_tokens,Line,State,Tcs,Tlen,Error,Action,Alen}, Chars, _) ->
    skip_tokens(Chars, Line, State, Tcs ++ Chars, Tlen, Error, Action, Alen).

%% tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(Ics0, L0, S0, Tcs, Tlen0, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{tokens,L1,S1,Tcs,Alen1,Ts,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Ts);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{tokens,L1,S1,Tcs,Tlen1,Ts,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,if Ts == [] -> {eof,L1};
		     true -> {ok,yyrev(Ts),L1} end,[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1+1), L1,
			{L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}});
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    tokens_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%%  Test if we have detected the end token, if so return done else continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(Rest, Line, yystate(), Rest, 0, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%% token_skip(InChars, Line, Error) -> {done,ReturnVal,RestChars}.
%%  Skip tokens until an end token, junk everything and return the error.

%%skip_tokens(Ics, Line, Error) -> {done,{error,Error,Line},Ics}.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(Ics, Line, yystate(), Ics, 0, Error, reject, 0).

%% skip_tokens(InChars, Line, State, TokenChars, TokenLen, Tokens, Accept) ->
%%    {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(Ics0, L0, S0, Tcs, Tlen0, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
	{A1,Alen1,Ics1,L1} ->			%Accepting end state
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,[],L1,S1} ->			%After an accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Alen1,Error,A1,Alen1}};
	{A1,Alen1,Ics1,L1,S1} ->
	    skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, L1), Error);
	{A1,Alen1,Tlen1,[],L1,S1} ->		%After a non-accepting state
	    {more,{skip_tokens,L1,S1,Tcs,Tlen1,Error,A1,Alen1}};
	{reject,Alen1,Tlen1,eof,L1,S1} ->
	    {done,{error,Error,L1},[]};
	{reject,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_tokens(yysuf(Tcs, Tlen1), L1, Error);
	{A1,Alen1,Tlen1,Ics1,L1,S1} ->
	    skip_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, L1), Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%%  Test if we have detected the end token, if so return done else continue.

skip_cont(Rest, Line, {token,T}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, {end_token,T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {error,S}, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0);
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(Rest, Line, yystate(), Rest, 0, Error, reject, 0).

yyrev(L) -> yyrev(L, []).

yyrev([H|T], Acc) -> yyrev(T, [H|Acc]);
yyrev([], Acc) -> Acc.

yypre([H|T], N) when N > 0 -> [H|yypre(T, N-1)];
yypre(L, N) -> [].

yysuf([H|T], N) when N > 0 -> yysuf(T, N-1);
yysuf(L, 0) -> L.

%% Generated state transition function.
yystate() -> 39.

yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {10,Tlen,Ics,Line};
yystate(45, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line};
yystate(44, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [$U|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $T ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $V, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(44, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,44};
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line};
yystate(42, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Q ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $S, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,42};
yystate(41, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line, Tlen+1, 17, Tlen);
yystate(41, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, 17, Tlen);
yystate(41, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line,41};
yystate(40, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line};
yystate(39, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line+1, Tlen+1, 7, Tlen);
yystate(39, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$#|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$&|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$(|Ics], Line, Tlen, Action, Alen) ->
    yystate(40, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$)|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$*|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$,|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$;|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$?|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$@|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$F|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(42, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(32, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$`|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$f|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [$t|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(31, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(31, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(6, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $E ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $G, C =< $S ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $[, C =< $] ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $e ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $g, C =< $s ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $u, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= ${, C =< $ÿ ->
    yystate(43, Ics, Line, Tlen+1, 7, Tlen);
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,39};
yystate(38, Ics, Line, Tlen, Action, Alen) ->
    {13,Tlen,Ics,Line};
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {20,Tlen,Ics,Line};
yystate(36, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(36, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(36, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $ÿ ->
    yystate(36, Ics, Line, Tlen+1, Action, Alen);
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,36};
yystate(35, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(35, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,35};
yystate(34, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,34};
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(33, Ics, Line, Tlen+1, 2, Tlen);
yystate(33, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,33};
yystate(32, Ics, Line, Tlen, Action, Alen) ->
    {15,Tlen,Ics,Line};
yystate(31, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line+1, Tlen+1, 7, Tlen);
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(31, Ics, Line, Tlen+1, 7, Tlen);
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(31, Ics, Line, Tlen+1, 7, Tlen);
yystate(31, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,31};
yystate(30, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line};
yystate(29, Ics, Line, Tlen, Action, Alen) ->
    {8,Tlen,Ics,Line};
yystate(28, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, 22, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(36, Ics, Line, Tlen+1, 22, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $& ->
    yystate(36, Ics, Line, Tlen+1, 22, Tlen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $(, C =< $ÿ ->
    yystate(36, Ics, Line, Tlen+1, 22, Tlen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line,28};
yystate(27, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 3, Tlen);
yystate(27, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 3, Tlen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 3, Tlen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 3, Tlen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 3, Tlen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,27};
yystate(26, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,26};
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,25};
yystate(24, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, [$a|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $b, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,24};
yystate(23, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, 22, Tlen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, 22, Tlen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(7, Ics, Line, Tlen+1, 22, Tlen);
yystate(23, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, 22, Tlen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line,23};
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line};
yystate(21, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, 16, Tlen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {16,Tlen,Ics,Line,21};
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(20, Ics, Line, Tlen+1, 5, Tlen);
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,20};
yystate(19, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $d ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $f, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,19};
yystate(18, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $R ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $T, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,18};
yystate(17, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,17};
yystate(16, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [$l|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $k ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $m, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,16};
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line};
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {14,Tlen,Ics,Line};
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {18,Tlen,Ics,Line};
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,12};
yystate(11, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [$u|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $t ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, [C|Ics], Line, Tlen, Action, Alen) when C >= $v, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,11};
yystate(10, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [$L|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $K ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $M, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,10};
yystate(9, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 1, Tlen);
yystate(9, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 1, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 1, Tlen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,9};
yystate(8, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [$s|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $r ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $t, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,8};
yystate(7, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,7};
yystate(6, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, 0, Tlen);
yystate(6, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(6, Ics, Line, Tlen+1, 0, Tlen);
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,6};
yystate(5, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $B, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,5};
yystate(4, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(4, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,4};
yystate(3, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [$r|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $q ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $s, C =< $z ->
    yystate(35, Ics, Line, Tlen+1, 6, Tlen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,3};
yystate(2, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(2, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(34, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(34, Ics, Line, Tlen+1, 5, Tlen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,2};
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 1, Tlen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,1};
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(4, Ics, Line, Tlen+1, 22, Tlen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(4, Ics, Line, Tlen+1, 22, Tlen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% Generated action function.
yyaction(0, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{integer,YYtext}};
yyaction(1, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{float,YYtext}};
yyaction(2, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{float_sci,YYtext}};
yyaction(3, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{boolean,YYtext}};
yyaction(4, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{string,YYtext}};
yyaction(5, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{cellref,YYtext}};
yyaction(6, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    case string:to_lower(YYtext) of
        "true" ->
            {token,{boolean,"true"}};
        "false" ->
            {token,{boolean,"false"}};
        _ ->
            {token,{id,YYtext}}
    end;
yyaction(7, YYlen, YYtcs, YYline) -> skip_token;
yyaction(8, YYlen, YYtcs, YYline) ->
    {token,{eq}};
yyaction(9, YYlen, YYtcs, YYline) ->
    {token,{open_paren}};
yyaction(10, YYlen, YYtcs, YYline) ->
    {token,{close_paren}};
yyaction(11, YYlen, YYtcs, YYline) ->
    {token,{plus}};
yyaction(12, YYlen, YYtcs, YYline) ->
    {token,{minus}};
yyaction(13, YYlen, YYtcs, YYline) ->
    {token,{times}};
yyaction(14, YYlen, YYtcs, YYline) ->
    {token,{divide}};
yyaction(15, YYlen, YYtcs, YYline) ->
    {token,{power}};
yyaction(16, YYlen, YYtcs, YYline) ->
    {token,{gt}};
yyaction(17, YYlen, YYtcs, YYline) ->
    {token,{lt}};
yyaction(18, YYlen, YYtcs, YYline) ->
    {token,{gte}};
yyaction(19, YYlen, YYtcs, YYline) ->
    {token,{lte}};
yyaction(20, YYlen, YYtcs, YYline) ->
    {token,{neq}};
yyaction(21, YYlen, YYtcs, YYline) ->
    {end_token,{'$end',YYline}};
yyaction(22, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{invalid_token,YYline,YYtext}};
yyaction(_, _, _, _) -> error.
