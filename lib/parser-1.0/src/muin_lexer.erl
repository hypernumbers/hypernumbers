%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module(muin_lexer).

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
yystate() -> 25.

yystate(88, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, 34, Tlen);
yystate(88, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, 34, Tlen);
yystate(88, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(78, Ics, Line, Tlen+1, 34, Tlen);
yystate(88, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(26, Ics, Line, Tlen+1, 34, Tlen);
yystate(88, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(26, Ics, Line, Tlen+1, 34, Tlen);
yystate(88, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line,88};
yystate(87, Ics, Line, Tlen, Action, Alen) ->
    {28,Tlen,Ics,Line};
yystate(86, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(80, Ics, Line, Tlen+1, 0, Tlen);
yystate(86, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(86, Ics, Line, Tlen+1, 0, Tlen);
yystate(86, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,86};
yystate(85, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(61, Ics, Line, Tlen+1, 34, Tlen);
yystate(85, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(45, Ics, Line, Tlen+1, 34, Tlen);
yystate(85, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(45, Ics, Line, Tlen+1, 34, Tlen);
yystate(85, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(45, Ics, Line, Tlen+1, 34, Tlen);
yystate(85, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line,85};
yystate(84, Ics, Line, Tlen, Action, Alen) ->
    {27,Tlen,Ics,Line};
yystate(83, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(83, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,83};
yystate(82, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(32, Ics, Line, Tlen+1, 34, Tlen);
yystate(82, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(32, Ics, Line, Tlen+1, 34, Tlen);
yystate(82, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line,82};
yystate(81, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [$U|Ics], Line, Tlen, Action, Alen) ->
    yystate(71, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [$u|Ics], Line, Tlen, Action, Alen) ->
    yystate(71, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $T ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [C|Ics], Line, Tlen, Action, Alen) when C >= $V, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $t ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, [C|Ics], Line, Tlen, Action, Alen) when C >= $v, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(81, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,81};
yystate(80, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(80, Ics, Line, Tlen+1, Action, Alen);
yystate(80, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(48, Ics, Line, Tlen+1, Action, Alen);
yystate(80, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(80, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(80, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,80};
yystate(79, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(75, Ics, Line, Tlen+1, Action, Alen);
yystate(79, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(79, Ics, Line, Tlen+1, Action, Alen);
yystate(79, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,79};
yystate(78, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(78, Ics, Line, Tlen+1, 4, Tlen);
yystate(78, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line,78};
yystate(77, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line};
yystate(76, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, 1, Tlen);
yystate(76, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(76, Ics, Line, Tlen+1, 1, Tlen);
yystate(76, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,76};
yystate(75, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, Action, Alen);
yystate(75, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(75, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(75, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,75};
yystate(74, Ics, Line, Tlen, Action, Alen) ->
    {32,Tlen,Ics,Line};
yystate(73, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(73, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,73};
yystate(72, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(50, Ics, Line, Tlen+1, 5, Tlen);
yystate(72, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(72, Ics, Line, Tlen+1, 5, Tlen);
yystate(72, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line,72};
yystate(71, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $D ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $d ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, [C|Ics], Line, Tlen, Action, Alen) when C >= $f, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(71, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,71};
yystate(70, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(76, Ics, Line, Tlen+1, Action, Alen);
yystate(70, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,70};
yystate(69, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [$L|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [$l|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $K ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [C|Ics], Line, Tlen, Action, Alen) when C >= $M, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $k ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, [C|Ics], Line, Tlen, Action, Alen) when C >= $m, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(69, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,69};
yystate(68, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 34, Tlen);
yystate(68, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, 34, Tlen);
yystate(68, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 34, Tlen);
yystate(68, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line,68};
yystate(67, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(67, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(67, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(67, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,67};
yystate(66, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line};
yystate(65, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(65, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(65, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,65};
yystate(64, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(64, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(64, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,64};
yystate(63, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(63, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(63, Ics, Line, Tlen+1, Action, Alen);
yystate(63, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,63};
yystate(62, Ics, Line, Tlen, Action, Alen) ->
    {21,Tlen,Ics,Line};
yystate(61, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line};
yystate(60, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(86, Ics, Line, Tlen+1, Action, Alen);
yystate(60, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,60};
yystate(59, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(63, Ics, Line, Tlen+1, Action, Alen);
yystate(59, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,59};
yystate(58, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(58, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(58, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(58, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(58, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,58};
yystate(57, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line};
yystate(56, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(56, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(56, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(56, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(56, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(56, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,56};
yystate(55, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(55, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(55, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,55};
yystate(54, Ics, Line, Tlen, Action, Alen) ->
    {31,Tlen,Ics,Line};
yystate(53, Ics, Line, Tlen, Action, Alen) ->
    {20,Tlen,Ics,Line};
yystate(52, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(52, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(52, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,52};
yystate(51, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(51, Ics, Line, Tlen+1, 2, Tlen);
yystate(51, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,51};
yystate(50, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 6, Tlen);
yystate(50, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,50};
yystate(49, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(65, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(49, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(49, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,49};
yystate(48, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(48, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(48, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,48};
yystate(47, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(79, Ics, Line, Tlen+1, Action, Alen);
yystate(47, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,47};
yystate(46, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(81, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [$r|Ics], Line, Tlen, Action, Alen) ->
    yystate(81, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Q ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $S, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $q ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, [C|Ics], Line, Tlen, Action, Alen) when C >= $s, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,46};
yystate(45, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(61, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(45, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,45};
yystate(44, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(44, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(44, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,44};
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(43, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,43};
yystate(42, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(80, Ics, Line, Tlen+1, 0, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(42, Ics, Line, Tlen+1, 0, Tlen);
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line,42};
yystate(41, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(41, Ics, Line, Tlen+1, 7, Tlen);
yystate(41, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line,41};
yystate(40, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line};
yystate(39, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(83, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(51, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,39};
yystate(38, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, Action, Alen);
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(76, Ics, Line, Tlen+1, Action, Alen);
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(38, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,38};
yystate(37, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(37, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(37, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(37, Ics, Line, Tlen+1, 3, Tlen);
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,37};
yystate(36, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(84, Ics, Line, Tlen+1, 25, Tlen);
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {25,Tlen,Ics,Line,36};
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {29,Tlen,Ics,Line};
yystate(34, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line+1, Tlen+1, 12, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(34, Ics, Line, Tlen+1, 12, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(34, Ics, Line, Tlen+1, 12, Tlen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line,34};
yystate(33, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(42, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(33, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(33, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,33};
yystate(32, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(32, Ics, Line, Tlen+1, 10, Tlen);
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(32, Ics, Line, Tlen+1, 10, Tlen);
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(32, Ics, Line, Tlen+1, 10, Tlen);
yystate(32, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(32, Ics, Line, Tlen+1, 10, Tlen);
yystate(32, Ics, Line, Tlen, Action, Alen) ->
    {10,Tlen,Ics,Line,32};
yystate(31, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(69, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [$a|Ics], Line, Tlen, Action, Alen) ->
    yystate(69, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $B, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, [C|Ics], Line, Tlen, Action, Alen) when C >= $b, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(31, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,31};
yystate(30, Ics, Line, Tlen, Action, Alen) ->
    {24,Tlen,Ics,Line};
yystate(29, Ics, Line, Tlen, Action, Alen) ->
    {16,Tlen,Ics,Line};
yystate(28, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(28, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,28};
yystate(27, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(59, Ics, Line, Tlen+1, Action, Alen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(63, Ics, Line, Tlen+1, Action, Alen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(27, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,27};
yystate(26, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(83, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(5, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(26, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(26, Ics, Line, Tlen+1, 3, Tlen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line,26};
yystate(25, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line+1, Tlen+1, 12, Tlen);
yystate(25, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(88, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(85, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$#|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(54, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$&|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$(|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$)|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$*|Ics], Line, Tlen, Action, Alen) ->
    yystate(77, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(53, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$,|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(62, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(68, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(74, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$;|Ics], Line, Tlen, Action, Alen) ->
    yystate(40, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(36, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$?|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$@|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$F|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(82, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$`|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$f|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$t|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [${|Ics], Line, Tlen, Action, Alen) ->
    yystate(66, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$||Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [$}|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(34, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\s ->
    yystate(34, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(72, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $E ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $G, C =< $S ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $U, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $[, C =< $] ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $e ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $g, C =< $s ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $u, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= $~, C =< $ÿ ->
    yystate(57, Ics, Line, Tlen+1, 12, Tlen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line,25};
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(24, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,24};
yystate(23, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(87, Ics, Line, Tlen+1, 26, Tlen);
yystate(23, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(35, Ics, Line, Tlen+1, 26, Tlen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {26,Tlen,Ics,Line,23};
yystate(22, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(73, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(73, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(41, Ics, Line, Tlen+1, Action, Alen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,22};
yystate(21, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(44, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(44, Ics, Line, Tlen+1, 1, Tlen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line,21};
yystate(20, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(20, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(78, Ics, Line, Tlen+1, Action, Alen);
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(20, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,20};
yystate(19, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(19, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(67, Ics, Line, Tlen+1, Action, Alen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(78, Ics, Line, Tlen+1, Action, Alen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(19, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,19};
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {30,Tlen,Ics,Line};
yystate(17, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(49, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,17};
yystate(16, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(79, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,16};
yystate(15, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [$S|Ics], Line, Tlen, Action, Alen) ->
    yystate(71, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [$s|Ics], Line, Tlen, Action, Alen) ->
    yystate(71, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $R ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $T, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $r ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, [C|Ics], Line, Tlen, Action, Alen) when C >= $t, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 11, Tlen);
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,15};
yystate(14, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,14};
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {15,Tlen,Ics,Line};
yystate(12, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(60, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(86, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,12};
yystate(11, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(11, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(52, Ics, Line, Tlen+1, Action, Alen);
yystate(11, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,11};
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {14,Tlen,Ics,Line};
yystate(9, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, 6, Tlen);
yystate(9, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, 6, Tlen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(9, Ics, Line, Tlen+1, 6, Tlen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line,9};
yystate(8, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, 23, Tlen);
yystate(8, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, 23, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(78, Ics, Line, Tlen+1, 23, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(26, Ics, Line, Tlen+1, 23, Tlen);
yystate(8, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(26, Ics, Line, Tlen+1, 23, Tlen);
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {23,Tlen,Ics,Line,8};
yystate(7, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, 8, Tlen);
yystate(7, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 8, Tlen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(21, Ics, Line, Tlen+1, 8, Tlen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(56, Ics, Line, Tlen+1, 8, Tlen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(56, Ics, Line, Tlen+1, 8, Tlen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {8,Tlen,Ics,Line,7};
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {18,Tlen,Ics,Line};
yystate(5, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(5, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(37, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(37, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line,5};
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {13,Tlen,Ics,Line};
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(3, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,3};
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(38, Ics, Line, Tlen+1, 34, Tlen);
yystate(2, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(38, Ics, Line, Tlen+1, 34, Tlen);
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line,2};
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(42, Ics, Line, Tlen+1, Action, Alen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,1};
yystate(0, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(0, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(64, Ics, Line, Tlen+1, Action, Alen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $A, C =< $Z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(0, [C|Ics], Line, Tlen, Action, Alen) when C >= $a, C =< $z ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% Generated action function.
yyaction(0, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{intersection,YYtext}};
yyaction(1, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{cellref,string:to_lower(YYtext)}};
yyaction(2, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{sscellref,muin_util:normalize_ssref(YYtext)}};
yyaction(3, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{sscolref,muin_util:normalize_ssref(YYtext)}};
yyaction(4, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{ssrowref,muin_util:normalize_ssref(YYtext)}};
yyaction(5, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{integer,muin_util:to_i(YYtext)}};
yyaction(6, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{float,muin_util:to_f(YYtext)}};
yyaction(7, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{float,muin_util:to_f(string:to_lower(YYtext))}};
yyaction(8, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,
     {boolean,
      case string:to_lower(YYtext) of
          "true" ->
              true;
          _ ->
              false
      end}};
yyaction(9, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{string,muin_util:mid(YYtext)}};
yyaction(10, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{var,string:to_lower(YYtext)}};
yyaction(11, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{id,string:to_lower(YYtext)}};
yyaction(12, YYlen, YYtcs, YYline) -> skip_token;
yyaction(13, YYlen, YYtcs, YYline) ->
    {token,{eq}};
yyaction(14, YYlen, YYtcs, YYline) ->
    {token,{comma}};
yyaction(15, YYlen, YYtcs, YYline) ->
    {token,{open_paren}};
yyaction(16, YYlen, YYtcs, YYline) ->
    {token,{close_paren}};
yyaction(17, YYlen, YYtcs, YYline) ->
    {token,{open_curly}};
yyaction(18, YYlen, YYtcs, YYline) ->
    {token,{close_curly}};
yyaction(19, YYlen, YYtcs, YYline) ->
    {token,{semicolon}};
yyaction(20, YYlen, YYtcs, YYline) ->
    {token,{plus}};
yyaction(21, YYlen, YYtcs, YYline) ->
    {token,{minus}};
yyaction(22, YYlen, YYtcs, YYline) ->
    {token,{times}};
yyaction(23, YYlen, YYtcs, YYline) ->
    {token,{slash}};
yyaction(24, YYlen, YYtcs, YYline) ->
    {token,{caret}};
yyaction(25, YYlen, YYtcs, YYline) ->
    {token,{gt}};
yyaction(26, YYlen, YYtcs, YYline) ->
    {token,{lt}};
yyaction(27, YYlen, YYtcs, YYline) ->
    {token,{gte}};
yyaction(28, YYlen, YYtcs, YYline) ->
    {token,{lte}};
yyaction(29, YYlen, YYtcs, YYline) ->
    {token,{neq}};
yyaction(30, YYlen, YYtcs, YYline) ->
    {token,{concat}};
yyaction(31, YYlen, YYtcs, YYline) ->
    {token,{percent}};
yyaction(32, YYlen, YYtcs, YYline) ->
    {token,{colon}};
yyaction(33, YYlen, YYtcs, YYline) ->
    {end_token,{'$end'}};
yyaction(34, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{invalid_token,YYtext}};
yyaction(_, _, _, _) -> error.
