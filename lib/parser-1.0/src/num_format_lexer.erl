%% THIS IS A PRE-RELEASE OF LEEX - RELEASED ONLY BECAUSE MANY PEOPLE
%% WANTED IT - THE OFFICIAL RELEASE WILL PROVIDE A DIFFERENT INCOMPATIBLE
%% AND BETTER INTERFACE - BE WARNED
%% PLEASE REPORT ALL BUGS TO THE AUTHOR.

-module(num_format_lexer).

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
yystate() -> 106.

yystate(109, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(107, Ics, Line, Tlen+1, Action, Alen);
yystate(109, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,109};
yystate(108, Ics, Line, Tlen, Action, Alen) ->
    {54,Tlen,Ics,Line};
yystate(107, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(103, Ics, Line, Tlen+1, Action, Alen);
yystate(107, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,107};
yystate(106, [$\n|Ics], Line, Tlen, Action, Alen) ->
    yystate(102, Ics, Line+1, Tlen+1, Action, Alen);
yystate(106, [$\s|Ics], Line, Tlen, Action, Alen) ->
    yystate(98, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$!|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(94, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$#|Ics], Line, Tlen, Action, Alen) ->
    yystate(82, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$$|Ics], Line, Tlen, Action, Alen) ->
    yystate(78, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$%|Ics], Line, Tlen, Action, Alen) ->
    yystate(74, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$&|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$'|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$(|Ics], Line, Tlen, Action, Alen) ->
    yystate(70, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$)|Ics], Line, Tlen, Action, Alen) ->
    yystate(66, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$*|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(62, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$,|Ics], Line, Tlen, Action, Alen) ->
    yystate(58, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(54, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(50, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(46, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$:|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$;|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$<|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$=|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$>|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$?|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$@|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$[|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$\\|Ics], Line, Tlen, Action, Alen) ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$^|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$_|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$`|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$a|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$b|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$c|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(40, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(56, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$f|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$g|Ics], Line, Tlen, Action, Alen) ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$h|Ics], Line, Tlen, Action, Alen) ->
    yystate(60, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(68, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$s|Ics], Line, Tlen, Action, Alen) ->
    yystate(84, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [$y|Ics], Line, Tlen, Action, Alen) ->
    yystate(92, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $\037 ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(42, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $B, C =< $D ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $F, C =< $Z ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $i, C =< $l ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $n, C =< $r ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $t, C =< $x ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, [C|Ics], Line, Tlen, Action, Alen) when C >= $z, C =< $ÿ ->
    yystate(108, Ics, Line, Tlen+1, Action, Alen);
yystate(106, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,106};
yystate(105, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(109, Ics, Line, Tlen+1, Action, Alen);
yystate(105, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,105};
yystate(104, Ics, Line, Tlen, Action, Alen) ->
    {37,Tlen,Ics,Line};
yystate(103, Ics, Line, Tlen, Action, Alen) ->
    {3,Tlen,Ics,Line};
yystate(102, Ics, Line, Tlen, Action, Alen) ->
    {55,Tlen,Ics,Line};
yystate(101, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(105, Ics, Line, Tlen+1, Action, Alen);
yystate(101, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,101};
yystate(100, [$y|Ics], Line, Tlen, Action, Alen) ->
    yystate(104, Ics, Line, Tlen+1, Action, Alen);
yystate(100, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,100};
yystate(99, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(95, Ics, Line, Tlen+1, Action, Alen);
yystate(99, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,99};
yystate(98, Ics, Line, Tlen, Action, Alen) ->
    {17,Tlen,Ics,Line};
yystate(97, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(101, Ics, Line, Tlen+1, Action, Alen);
yystate(97, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,97};
yystate(96, [$y|Ics], Line, Tlen, Action, Alen) ->
    yystate(100, Ics, Line, Tlen+1, 36, Tlen);
yystate(96, Ics, Line, Tlen, Action, Alen) ->
    {36,Tlen,Ics,Line,96};
yystate(95, [$G|Ics], Line, Tlen, Action, Alen) ->
    yystate(91, Ics, Line, Tlen+1, Action, Alen);
yystate(95, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,95};
yystate(94, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(90, Ics, Line, Tlen+1, 54, Tlen);
yystate(94, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(86, Ics, Line, Tlen+1, 54, Tlen);
yystate(94, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(86, Ics, Line, Tlen+1, 54, Tlen);
yystate(94, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(86, Ics, Line, Tlen+1, 54, Tlen);
yystate(94, Ics, Line, Tlen, Action, Alen) ->
    {54,Tlen,Ics,Line,94};
yystate(93, Ics, Line, Tlen, Action, Alen) ->
    {2,Tlen,Ics,Line};
yystate(92, [$y|Ics], Line, Tlen, Action, Alen) ->
    yystate(96, Ics, Line, Tlen+1, 54, Tlen);
yystate(92, Ics, Line, Tlen, Action, Alen) ->
    {54,Tlen,Ics,Line,92};
yystate(91, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(87, Ics, Line, Tlen+1, Action, Alen);
yystate(91, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,91};
yystate(90, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(90, Ics, Line, Tlen+1, 11, Tlen);
yystate(90, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(86, Ics, Line, Tlen+1, 11, Tlen);
yystate(90, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(86, Ics, Line, Tlen+1, 11, Tlen);
yystate(90, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(86, Ics, Line, Tlen+1, 11, Tlen);
yystate(90, Ics, Line, Tlen, Action, Alen) ->
    {11,Tlen,Ics,Line,90};
yystate(89, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(93, Ics, Line, Tlen+1, Action, Alen);
yystate(89, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,89};
yystate(88, Ics, Line, Tlen, Action, Alen) ->
    {49,Tlen,Ics,Line};
yystate(87, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(83, Ics, Line, Tlen+1, Action, Alen);
yystate(87, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,87};
yystate(86, [$"|Ics], Line, Tlen, Action, Alen) ->
    yystate(90, Ics, Line, Tlen+1, Action, Alen);
yystate(86, [C|Ics], Line, Tlen, Action, Alen) when C >= $\000, C =< $\t ->
    yystate(86, Ics, Line, Tlen+1, Action, Alen);
yystate(86, [C|Ics], Line, Tlen, Action, Alen) when C >= $\v, C =< $! ->
    yystate(86, Ics, Line, Tlen+1, Action, Alen);
yystate(86, [C|Ics], Line, Tlen, Action, Alen) when C >= $#, C =< $ÿ ->
    yystate(86, Ics, Line, Tlen+1, Action, Alen);
yystate(86, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,86};
yystate(85, [$N|Ics], Line, Tlen, Action, Alen) ->
    yystate(89, Ics, Line, Tlen+1, Action, Alen);
yystate(85, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,85};
yystate(84, [$s|Ics], Line, Tlen, Action, Alen) ->
    yystate(88, Ics, Line, Tlen+1, 48, Tlen);
yystate(84, Ics, Line, Tlen, Action, Alen) ->
    {48,Tlen,Ics,Line,84};
yystate(83, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(79, Ics, Line, Tlen+1, Action, Alen);
yystate(83, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,83};
yystate(82, Ics, Line, Tlen, Action, Alen) ->
    {19,Tlen,Ics,Line};
yystate(81, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(85, Ics, Line, Tlen+1, Action, Alen);
yystate(81, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,81};
yystate(80, Ics, Line, Tlen, Action, Alen) ->
    {41,Tlen,Ics,Line};
yystate(79, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(75, Ics, Line, Tlen+1, Action, Alen);
yystate(79, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,79};
yystate(78, Ics, Line, Tlen, Action, Alen) ->
    {29,Tlen,Ics,Line};
yystate(77, [$Y|Ics], Line, Tlen, Action, Alen) ->
    yystate(81, Ics, Line, Tlen+1, Action, Alen);
yystate(77, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,77};
yystate(76, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(80, Ics, Line, Tlen+1, 40, Tlen);
yystate(76, Ics, Line, Tlen, Action, Alen) ->
    {40,Tlen,Ics,Line,76};
yystate(75, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(71, Ics, Line, Tlen+1, Action, Alen);
yystate(75, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,75};
yystate(74, Ics, Line, Tlen, Action, Alen) ->
    {22,Tlen,Ics,Line};
yystate(73, Ics, Line, Tlen, Action, Alen) ->
    {1,Tlen,Ics,Line};
yystate(72, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(76, Ics, Line, Tlen+1, 39, Tlen);
yystate(72, Ics, Line, Tlen, Action, Alen) ->
    {39,Tlen,Ics,Line,72};
yystate(71, Ics, Line, Tlen, Action, Alen) ->
    {4,Tlen,Ics,Line};
yystate(70, Ics, Line, Tlen, Action, Alen) ->
    {31,Tlen,Ics,Line};
yystate(69, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(73, Ics, Line, Tlen+1, Action, Alen);
yystate(69, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,69};
yystate(68, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(72, Ics, Line, Tlen+1, 38, Tlen);
yystate(68, Ics, Line, Tlen, Action, Alen) ->
    {38,Tlen,Ics,Line,68};
yystate(67, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(63, Ics, Line, Tlen+1, Action, Alen);
yystate(67, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,67};
yystate(66, Ics, Line, Tlen, Action, Alen) ->
    {32,Tlen,Ics,Line};
yystate(65, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(69, Ics, Line, Tlen+1, Action, Alen);
yystate(65, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,65};
yystate(64, Ics, Line, Tlen, Action, Alen) ->
    {47,Tlen,Ics,Line};
yystate(63, [$D|Ics], Line, Tlen, Action, Alen) ->
    yystate(59, Ics, Line, Tlen+1, Action, Alen);
yystate(63, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,63};
yystate(62, Ics, Line, Tlen, Action, Alen) ->
    {27,Tlen,Ics,Line};
yystate(61, Ics, Line, Tlen, Action, Alen) ->
    {0,Tlen,Ics,Line};
yystate(60, [$h|Ics], Line, Tlen, Action, Alen) ->
    yystate(64, Ics, Line, Tlen+1, 46, Tlen);
yystate(60, Ics, Line, Tlen, Action, Alen) ->
    {46,Tlen,Ics,Line,60};
yystate(59, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(55, Ics, Line, Tlen+1, Action, Alen);
yystate(59, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,59};
yystate(58, Ics, Line, Tlen, Action, Alen) ->
    {23,Tlen,Ics,Line};
yystate(57, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(61, Ics, Line, Tlen+1, Action, Alen);
yystate(57, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,57};
yystate(56, Ics, Line, Tlen, Action, Alen) ->
    {24,Tlen,Ics,Line};
yystate(55, Ics, Line, Tlen, Action, Alen) ->
    {5,Tlen,Ics,Line};
yystate(54, Ics, Line, Tlen, Action, Alen) ->
    {26,Tlen,Ics,Line};
yystate(53, [$K|Ics], Line, Tlen, Action, Alen) ->
    yystate(57, Ics, Line, Tlen+1, Action, Alen);
yystate(53, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,53};
yystate(52, Ics, Line, Tlen, Action, Alen) ->
    {45,Tlen,Ics,Line};
yystate(51, [$H|Ics], Line, Tlen, Action, Alen) ->
    yystate(47, Ics, Line, Tlen+1, Action, Alen);
yystate(51, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,51};
yystate(50, Ics, Line, Tlen, Action, Alen) ->
    {21,Tlen,Ics,Line};
yystate(49, [$C|Ics], Line, Tlen, Action, Alen) ->
    yystate(53, Ics, Line, Tlen+1, Action, Alen);
yystate(49, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,49};
yystate(48, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(52, Ics, Line, Tlen+1, 44, Tlen);
yystate(48, Ics, Line, Tlen, Action, Alen) ->
    {44,Tlen,Ics,Line,48};
yystate(47, [$I|Ics], Line, Tlen, Action, Alen) ->
    yystate(43, Ics, Line, Tlen+1, Action, Alen);
yystate(47, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,47};
yystate(46, Ics, Line, Tlen, Action, Alen) ->
    {30,Tlen,Ics,Line};
yystate(45, [$A|Ics], Line, Tlen, Action, Alen) ->
    yystate(49, Ics, Line, Tlen+1, Action, Alen);
yystate(45, [$U|Ics], Line, Tlen, Action, Alen) ->
    yystate(65, Ics, Line, Tlen+1, Action, Alen);
yystate(45, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,45};
yystate(44, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(48, Ics, Line, Tlen+1, 43, Tlen);
yystate(44, Ics, Line, Tlen, Action, Alen) ->
    {43,Tlen,Ics,Line,44};
yystate(43, [$T|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(43, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,43};
yystate(42, [$.|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, 8, Tlen);
yystate(42, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(42, Ics, Line, Tlen+1, 8, Tlen);
yystate(42, Ics, Line, Tlen, Action, Alen) ->
    {8,Tlen,Ics,Line,42};
yystate(41, [$L|Ics], Line, Tlen, Action, Alen) ->
    yystate(45, Ics, Line, Tlen+1, Action, Alen);
yystate(41, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,41};
yystate(40, [$d|Ics], Line, Tlen, Action, Alen) ->
    yystate(44, Ics, Line, Tlen+1, 42, Tlen);
yystate(40, Ics, Line, Tlen, Action, Alen) ->
    {42,Tlen,Ics,Line,40};
yystate(39, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(35, Ics, Line, Tlen+1, Action, Alen);
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,39};
yystate(38, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(34, Ics, Line, Tlen+1, 9, Tlen);
yystate(38, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line,38};
yystate(37, [$B|Ics], Line, Tlen, Action, Alen) ->
    yystate(41, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$C|Ics], Line, Tlen, Action, Alen) ->
    yystate(77, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$G|Ics], Line, Tlen, Action, Alen) ->
    yystate(97, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$M|Ics], Line, Tlen, Action, Alen) ->
    yystate(99, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$R|Ics], Line, Tlen, Action, Alen) ->
    yystate(67, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$W|Ics], Line, Tlen, Action, Alen) ->
    yystate(51, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, [$Y|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, 12, Tlen);
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {12,Tlen,Ics,Line,37};
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {51,Tlen,Ics,Line};
yystate(35, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, Action, Alen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,35};
yystate(34, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 9, Tlen);
yystate(34, [$e|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, 9, Tlen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(34, Ics, Line, Tlen+1, 9, Tlen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {9,Tlen,Ics,Line,34};
yystate(33, Ics, Line, Tlen, Action, Alen) ->
    {25,Tlen,Ics,Line};
yystate(32, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(36, Ics, Line, Tlen+1, Action, Alen);
yystate(32, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,32};
yystate(31, Ics, Line, Tlen, Action, Alen) ->
    {6,Tlen,Ics,Line};
yystate(30, [$+|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [$-|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(30, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(30, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,30};
yystate(29, Ics, Line, Tlen, Action, Alen) ->
    {50,Tlen,Ics,Line};
yystate(28, [$p|Ics], Line, Tlen, Action, Alen) ->
    yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(28, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,28};
yystate(27, [$E|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(27, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,27};
yystate(26, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(26, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,26};
yystate(25, [$M|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,25};
yystate(24, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(24, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,24};
yystate(23, [$L|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(23, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,23};
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= $0, C =< $9 ->
    yystate(22, Ics, Line, Tlen+1, 10, Tlen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {10,Tlen,Ics,Line,22};
yystate(21, [$P|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,21};
yystate(20, Ics, Line, Tlen, Action, Alen) ->
    {53,Tlen,Ics,Line};
yystate(19, [$L|Ics], Line, Tlen, Action, Alen) ->
    yystate(15, Ics, Line, Tlen+1, Action, Alen);
yystate(19, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,19};
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {33,Tlen,Ics,Line};
yystate(17, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,17};
yystate(16, [$p|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(16, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,16};
yystate(15, [$O|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(15, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,15};
yystate(14, Ics, Line, Tlen, Action, Alen) ->
    {15,Tlen,Ics,Line};
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {52,Tlen,Ics,Line};
yystate(12, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, 54, Tlen);
yystate(12, [$m|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, 54, Tlen);
yystate(12, Ics, Line, Tlen, Action, Alen) ->
    {54,Tlen,Ics,Line,12};
yystate(11, [$W|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(11, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,11};
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {16,Tlen,Ics,Line};
yystate(9, [$P|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,9};
yystate(8, Ics, Line, Tlen, Action, Alen) ->
    {28,Tlen,Ics,Line};
yystate(7, [$]|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,7};
yystate(6, Ics, Line, Tlen, Action, Alen) ->
    {14,Tlen,Ics,Line};
yystate(5, [$/|Ics], Line, Tlen, Action, Alen) ->
    yystate(9, Ics, Line, Tlen+1, 54, Tlen);
yystate(5, [$M|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, 54, Tlen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {54,Tlen,Ics,Line,5};
yystate(4, Ics, Line, Tlen, Action, Alen) ->
    {13,Tlen,Ics,Line};
yystate(3, Ics, Line, Tlen, Action, Alen) ->
    {7,Tlen,Ics,Line};
yystate(2, Ics, Line, Tlen, Action, Alen) ->
    {20,Tlen,Ics,Line};
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {35,Tlen,Ics,Line};
yystate(0, Ics, Line, Tlen, Action, Alen) ->
    {34,Tlen,Ics,Line};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.


%% Generated action function.
yyaction(0, YYlen, YYtcs, YYline) ->
    {token,{colour,black}};
yyaction(1, YYlen, YYtcs, YYline) ->
    {token,{colour,blue}};
yyaction(2, YYlen, YYtcs, YYline) ->
    {token,{colour,cyan}};
yyaction(3, YYlen, YYtcs, YYline) ->
    {token,{colour,green}};
yyaction(4, YYlen, YYtcs, YYline) ->
    {token,{colour,magenta}};
yyaction(5, YYlen, YYtcs, YYline) ->
    {token,{colour,red}};
yyaction(6, YYlen, YYtcs, YYline) ->
    {token,{colour,white}};
yyaction(7, YYlen, YYtcs, YYline) ->
    {token,{colour,yellow}};
yyaction(8, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{number,YYtext}};
yyaction(9, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{number,YYtext}};
yyaction(10, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{number,YYtext}};
yyaction(11, YYlen, YYtcs, YYline) ->
    YYtext = yypre(YYtcs, YYlen),
    {token,{string,YYtext}};
yyaction(12, YYlen, YYtcs, YYline) ->
    {token,open_sq_brackets};
yyaction(13, YYlen, YYtcs, YYline) ->
    {token,close_sq_brackets};
yyaction(14, YYlen, YYtcs, YYline) ->
    {token,gt};
yyaction(15, YYlen, YYtcs, YYline) ->
    {token,lt};
yyaction(16, YYlen, YYtcs, YYline) ->
    {token,eq};
yyaction(17, YYlen, YYtcs, YYline) ->
    {token,space};
yyaction(18, YYlen, YYtcs, YYline) ->
    {token,zero};
yyaction(19, YYlen, YYtcs, YYline) ->
    {token,hash};
yyaction(20, YYlen, YYtcs, YYline) ->
    {token,question};
yyaction(21, YYlen, YYtcs, YYline) ->
    {token,dot};
yyaction(22, YYlen, YYtcs, YYline) ->
    {token,percent};
yyaction(23, YYlen, YYtcs, YYline) ->
    {token,comma};
yyaction(24, YYlen, YYtcs, YYline) ->
    {token,exponent};
yyaction(25, YYlen, YYtcs, YYline) ->
    {token,exponent};
yyaction(26, YYlen, YYtcs, YYline) ->
    {token,minus};
yyaction(27, YYlen, YYtcs, YYline) ->
    {token,plus};
yyaction(28, YYlen, YYtcs, YYline) ->
    {token,underscore};
yyaction(29, YYlen, YYtcs, YYline) ->
    {token,dollar};
yyaction(30, YYlen, YYtcs, YYline) ->
    {token,forwardslash};
yyaction(31, YYlen, YYtcs, YYline) ->
    {token,open_bra};
yyaction(32, YYlen, YYtcs, YYline) ->
    {token,close_ket};
yyaction(33, YYlen, YYtcs, YYline) ->
    {token,colon};
yyaction(34, YYlen, YYtcs, YYline) ->
    {token,backslash};
yyaction(35, YYlen, YYtcs, YYline) ->
    {token,at};
yyaction(36, YYlen, YYtcs, YYline) ->
    {token,{year,two_digit}};
yyaction(37, YYlen, YYtcs, YYline) ->
    {token,{year,four_digit}};
yyaction(38, YYlen, YYtcs, YYline) ->
    {token,{mon_min,no_zero}};
yyaction(39, YYlen, YYtcs, YYline) ->
    {token,{mon_min,zero}};
yyaction(40, YYlen, YYtcs, YYline) ->
    {token,{mon,abbr}};
yyaction(41, YYlen, YYtcs, YYline) ->
    {token,{mon,full}};
yyaction(42, YYlen, YYtcs, YYline) ->
    {token,{day,no_zero}};
yyaction(43, YYlen, YYtcs, YYline) ->
    {token,{day,zero}};
yyaction(44, YYlen, YYtcs, YYline) ->
    {token,{day,abbr}};
yyaction(45, YYlen, YYtcs, YYline) ->
    {token,{day,full}};
yyaction(46, YYlen, YYtcs, YYline) ->
    {token,{hour,no_zero}};
yyaction(47, YYlen, YYtcs, YYline) ->
    {token,{hour,zero}};
yyaction(48, YYlen, YYtcs, YYline) ->
    {token,{sec,no_zero}};
yyaction(49, YYlen, YYtcs, YYline) ->
    {token,{sec,zero}};
yyaction(50, YYlen, YYtcs, YYline) ->
    {token,{ampm,full_caps}};
yyaction(51, YYlen, YYtcs, YYline) ->
    {token,{ampm,full_lowercase}};
yyaction(52, YYlen, YYtcs, YYline) ->
    {token,{ampm,abbr_caps}};
yyaction(53, YYlen, YYtcs, YYline) ->
    {token,{ampm,abbr_lowercase}};
yyaction(54, YYlen, YYtcs, YYline) ->
    {token,duff};
yyaction(55, YYlen, YYtcs, YYline) ->
    {end_token,{'$end',YYline}};
yyaction(_, _, _, _) -> error.
