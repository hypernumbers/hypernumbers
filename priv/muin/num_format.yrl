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

FullFormat -> Format            : '$1'.
FullFormat -> Format percent    : concat2('$1','$2').
FullFormat -> Bits Format       : concat2('$1','$2').
FullFormat -> Col FullFormat    : concat2('$1','$2').
FullFormat -> DateFormat        : '$1'.
FullFormat -> Bits DateFormat   : concat2('$1','$2').
FullFormat -> format DateFormat : concat2(fix_up('$1'),'$2').
FullFormat -> At                : '$1'.
FullFormat -> Bits At           : concat2('$1','$2').

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
DateFormat -> DateFormat format     : concat2('$1',fix_up('$2')).

Tokens -> Tokens Tokens  : concat_tk('$1','$2').

At -> at      : '$1'.
At -> at Bits : concat('$1','$2').

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

-define(SPACE,32).
-define(LESSTHAN,60).
-define(EQUALS,61).
-define(GREATERTHAN,62).

dump(N,Content) ->
  io:format("in Dump for ~p Content is ~p~n",[N,Content]),
  Content.

fix_up({format,A}) -> {tokens,A}.

to_tk({_,A}) -> {tokens,A}.

concat_tk({_,A},{_,B}) -> {tokens,lists:concat([A,B])}.

concat(A,B) -> [A,B].

concat(A,B,C) -> [A,B,C].

concat2(A,B) -> [A,B].

esc(A) -> {char, A}.

make_cond({condition,String})->
        Str2=string:strip(String,left,$[),
        Str3=string:strip(Str2,right,$]),
        {condition,Str3}.

strip({string,A}) -> 
        A1 = string:strip(A,both,$"),%" syntax highlighting fix
        {string2,A1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% These functions all pertain to make_src                                   %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_src(A) when is_list(A) -> make_src2(lists:flatten(A));
make_src(A)                 -> make_src2([A]).
  
make_src2(A) ->
  Clauses=organise_clauses(A,[],[]),
  gen_src(Clauses,[">0","<0","=0"]).

gen_src(Clauses,Defaults) -> gen_src(Clauses,Defaults,[]).

%% generate the source from the list of clauses and the list of default clauses
%% if there aren't enough conditions repeat the first one
gen_src([],[],Acc)           -> gen_src2(lists:reverse(Acc));
gen_src([],[H1|T1],Acc)      -> gen_src([],T1,[swap_cond(H1,lists:last(Acc))|Acc]); 
gen_src([H1|T1],[],Acc)      -> gen_src(T1,[],[make_default(H1)|Acc]);
gen_src([H1|T1],[H2|T2],Acc) -> gen_src(T1,T2,[make_default(H1,H2)|Acc]).

gen_src2(Clauses)->
  io:format("in format:get_src2 Clauses are ~p~n",[Clauses]),
  Clause1=make_clause(lists:nth(1,Clauses)),
  Clause2=make_clause(lists:nth(2,Clauses)),
  Clause3=strip_semi(make_clause(lists:nth(3,Clauses))),  
  Clause4 = case length(Clauses) of
    3 -> default_clause();
    4 -> drop_cond(lists:nth(4,Clauses))
  end,
  io:format("in format:get_src2~n-1: ~p~n-2: ~p~n-3: ~p~n-4: ~p~n",
    [Clause1,Clause2,Clause3,Clause4]),
  Src="fun(X) -> "++
      "   Return=try tconv:to_i(X)"++
      "       catch"++
      "         error:_ ->"++
      "           try tconv:to_f(X)"++
      "           catch"++
      "               error:_ ->"++
      "                    {error, value}"++
      "           end"++
      "       end,"++
      "    case Return of"++
      "      {error,_} -> "++
                  Clause4++
      "      _ ->"++
      "          if "++
                     Clause1++" "++
                     Clause2++" "++
                     Clause3++" "++
      "          end"++
      "      end"++
      "   end.",
  lists:flatten(Src).

drop_cond([{condition,Cond},{colour,Col}|Rest]) -> [{colour,Col}|Rest].

strip_semi(String)->
  string:strip(String,right,$;).

make_clause([{condition,Cond},{colour,Col}|Rest])->
  Bits=io_lib:fwrite("~p",[Rest]),
  Bits2=lists:flatten(Bits),
  "X"++tart_up(Cond)++" -> {"++atom_to_list(Col)++",format:format(X,"++Bits2++")};".
  
default_clause() -> "{black,X};".

%% tart_up just makes the condition clauses well behaved
tart_up(text)                        -> "= text";
tart_up([?GREATERTHAN,?EQUALS|Rest]) -> [?SPACE,?GREATERTHAN,?EQUALS,?SPACE|Rest];
tart_up([?EQUALS,?LESSTHAN|Rest])    -> [?SPACE,?EQUALS,?LESSTHAN,?SPACE|Rest];
tart_up([?EQUALS|Rest])              -> [?SPACE,?EQUALS,?EQUALS,?SPACE|Rest]; % switch '=' to '=='
tart_up([Op|Rest])                   -> [?SPACE,Op,?SPACE|Rest].

swap_cond(Cond,[{condition,_OldCond}|Rest]) -> [{condition,Cond}|Rest].

make_default([{colour,Col}|Rest]) -> [{condition,text},{colour,Col}|Rest];
make_default(Plain)               -> [{condition,text},{colour,black}|Plain].
    
make_default([{condition,Cond},{colour,Col}|R],_Def)-> [{condition,Cond},{colour,Col}|R];
make_default([{colour,Col}|R],Def)                  -> [{condition,Def},{colour,Col}|R];
make_default([{condition,Cond}|R],_Def)             -> [{condition,Cond},{colour,black}|R];
make_default(Plain,Def)                             -> [{condition,Def},{colour,black}|Plain].

organise_clauses([],Acc1,Acc2)                -> lists:reverse([lists:reverse(Acc1)|Acc2]);
organise_clauses([{semicolon,_}|T],Acc1,Acc2) -> organise_clauses(T,[],[lists:reverse(Acc1)|Acc2]);
organise_clauses([Other|T],Acc1,Acc2)         -> organise_clauses(T,[Other|Acc1],Acc2).
