%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
FinalFormat
CondFormat
Col
Format
DateFormat
FullFormat
Tokens
Escaped
Cond
Bits
At
Semicolon
.

Terminals

%% format stuff
colour

%% conditions
condition


%% format characters
dollar
minus
plus
forwardslash
open_bra
close_ket
colon
space
underscore
percent
esc
at
year
mon_min
mon
day
hour
sec
ampm
string
format
fraction
semicolon

%% Other tokens
char

.

Rootsymbol Final.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators
%%Unary 50 Bits.
Unary 100 Escaped.

%% ----- Grammar definition.

Final -> FinalFormat : make_src('$1').

Semicolon -> semicolon           : '$1'.
Semicolon -> Semicolon semicolon : concat('$1','$2').

FinalFormat -> CondFormat Semicolon FinalFormat : lists:flatten(concat('$1','$2','$3')).

FinalFormat -> CondFormat Semicolon  : lists:flatten(concat('$1','$2')).
FinalFormat -> CondFormat            : '$1'.

Bits -> Escaped : to_tk('$1').
Bits -> Tokens  : to_tk('$1').

%% Set up the escaped character formats
Escaped -> esc Tokens  : esc('$2').
Escaped -> esc esc     : esc('$2').

%% you can skip either or both of Condition or Col with a format
CondFormat -> Cond FullFormat : concat2('$1','$2').
CondFormat -> FullFormat      : '$1'.

Cond -> condition : make_cond('$1').

Format -> format       : '$1'.
Format -> fraction     : '$1'.
Format -> Bits         : '$1'.
Format -> Format Bits  : concat2('$1','$2').

FullFormat -> Format          : '$1'.
FullFormat -> Format percent  : concat2('$1','$2').
FullFormat -> Bits Format     : concat2('$1','$2').
FullFormat -> Col FullFormat  : concat2('$1','$2').
FullFormat -> DateFormat      : '$1'.
FullFormat -> Bits DateFormat : concat2('$1','$2').
FullFormat -> At              : '$1'.
FullFormat -> Bits At         : concat2('$1','$2').

%% Set up the numerical formats
Tokens -> space        : '$1'.
Tokens -> forwardslash : '$1'.
Tokens -> minus        : '$1'.
Tokens -> underscore   : '$1'.
Tokens -> colon        : '$1'.
Tokens -> string       : strip('$1').
Tokens -> char         : '$1'.
Tokens -> dollar       : '$1'.
Tokens -> plus         : '$1'.
Tokens -> open_bra     : '$1'.
Tokens -> close_ket    : '$1'.

%% Date Formats
DateFormat -> year         : '$1'.
DateFormat -> mon_min      : '$1'.
DateFormat -> mon          : '$1'.
DateFormat -> day          : '$1'.
DateFormat -> hour         : '$1'.
DateFormat -> sec          : '$1'.
DateFormat -> ampm         : '$1'.

DateFormat -> DateFormat DateFormat : concat2('$1','$2').
DateFormat -> DateFormat Bits       : concat2('$1','$2').

Tokens -> Tokens Tokens  : concat_tk('$1','$2').

At -> at      : '$1'.
At -> at Bits : concat('$1','$2').

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

dump(N,Content) ->
  io:format("in Dump for ~p Content is ~p~n",[N,Content]),
  Content.

to_tk({_,A})       -> {tokens,A}.

concat_tk({_,A},{_,B}) -> {tokens,lists:concat([A,B])}.

%%concat({_,A},{_,B}) -> [A,B];
concat(A,B)         -> [A,B].

concat(A,B,C) -> [A,B,C].

concat2(A,B) -> [A,B].

esc(A) -> {char, A}.

make_cond({condition,String})->
        Str2=string:strip(String,left,$[),
        Str3=string:strip(Str2,right,$]),
        {condition,Str3}.

strip({string,A}) -> A1 = string:strip(A,both,$"),%" syntax highlighting fix
        {string2,A1}.

make_src(A) when is_list(A) -> make_src2(lists:flatten(A));
make_src(A)                 -> make_src2([A]).
  
make_src2(A) ->
  Clauses=organise_clauses(A,[],[]),
  gen_src(Clauses,[">0","<0","0"]).

gen_src(Clauses,Defaults) -> gen_src(Clauses,Defaults,[]).

%% generate the source from the list of clauses and the list of default clauses
%% if there aren't enough conditions repeat the first one
gen_src([],[],Acc)           -> gen_src2(lists:reverse(Acc));
gen_src([],[H1|T1],Acc)      -> gen_src([],T1,[swap_cond(H1,lists:last(Acc))|Acc]); 
gen_src([H1|T1],[],Acc)      -> gen_src(T1,[],[make_default(H1)|Acc]);
gen_src([H1|T1],[H2|T2],Acc) -> gen_src(T1,T2,[make_default(H1,H2)|Acc]).

gen_src2(Src)->
  io:format("in gen_src2 Src is ~p~n",[Src]),
  Src.  

swap_cond(Cond,[{condition,_OldCond}|Rest]) -> [{condition,Cond}|Rest].

make_default([{colour,Col}|Rest]) -> [{condition,text},{colour,Col}|Rest];
make_default(Plain)               -> [{condition,text},{colour,"black"}|Plain].
    
make_default([{condition,Cond},{colour,Col}|R],_Def)-> [{condition,Cond},{colour,Col}|R];
make_default([{colour,Col}|R],Def)                  -> [{condition,Def},{colour,Col}|R];
make_default([{condition,Cond}|R],_Def)             -> [{condition,Cond},{colour,"black"}|R];
make_default(Plain,Def)                             -> [{condition,Def},{colour,"black"}|Plain].

organise_clauses([],Acc1,Acc2)                -> lists:reverse([lists:reverse(Acc1)|Acc2]);
organise_clauses([{semicolon,_}|T],Acc1,Acc2) -> organise_clauses(T,[],[lists:reverse(Acc1)|Acc2]);
organise_clauses([Other|T],Acc1,Acc2)         -> organise_clauses(T,[Other|Acc1],Acc2).
