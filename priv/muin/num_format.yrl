%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
FinalFormat
CondFormat
Col
Format
FullFormat
Tokens
Token
Escaped
Cond
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

Semicolon -> semicolon           : tr('semicolon 1','$1').
Semicolon -> Semicolon semicolon : tr('semicolon 2',concat('$1','$2')).

FinalFormat -> CondFormat Semicolon FinalFormat : concat(verify('$1'),'$2','$3').

FinalFormat -> CondFormat Semicolon  : concat(verify('$1'),'$2').
FinalFormat -> CondFormat            : verify('$1').

%% Set up the escaped character formats
Escaped -> esc Token  : esc('$2').
Escaped -> esc esc    : esc('$2').

%% you can skip either or both of Condition or Col with a format
CondFormat -> Cond FullFormat : concat('$1','$2').
CondFormat -> FullFormat      : '$1'.

Cond -> condition : make_cond('$1').

Format -> Tokens        : '$1'.
Format -> fraction      : '$1'.
%%Format -> Format Format : concat('$1','$2').

FullFormat -> Format                : '$1'.
FullFormat -> Format percent        : concat('$1','$2').
FullFormat -> Col FullFormat        : concat('$1','$2').

%% Set up the numerical formats
Token -> format       : '$1'.
Token -> space        : '$1'.
Token -> forwardslash : '$1'.
Token -> minus        : '$1'.
Token -> underscore   : '$1'.
Token -> colon        : '$1'.
Token -> string       : strip('$1').
Token -> char         : '$1'.
Token -> dollar       : '$1'.
Token -> plus         : '$1'.
Token -> open_bra     : '$1'.
Token -> close_ket    : '$1'.

%% And Date Formats
Token -> year         : '$1'.
Token -> mon_min      : '$1'.
Token -> mon          : '$1'.
Token -> day          : '$1'.
Token -> hour         : '$1'.
Token -> sec          : '$1'.
Token -> ampm         : '$1'.

%% and text token
Token -> at           : '$1'.

Tokens -> Token Tokens : concat('$1','$2').
Tokens -> Token        : '$1'. 
Tokens -> Escaped      : '$1'.

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

-include("ascii.hrl").

%%
%% Some debugging functions
%%
dump([])    -> ok;
dump([H|T]) ->
  io:format("in dump H is ~p~n",[H]),
  dump(T).

tr(Location,Term) ->
  io:format("at ~p Term is ~p~n",[Location,Term]),
  Term.

%%
%% 'Proper' functions
%%
verify(A) when is_list(A) ->
  B=lists:flatten(A),
  Return=case is_text(B) of
    yes -> {text,B};
    no  -> case is_date(B) of
      yes -> {date,B};
      no  -> verify_num(B)
    end
  end,
  Return; % yes this is a clause end...
verify(A) -> verify([A]). %% If 'A' is not a list make it so

verify_num(A) when is_list(A) -> verify_num(A,[],0);
verify_num(A)                 -> verify_num([A],[],0).

%% There can be 0 or 1 decimal point - any more and its not valid...
verify_num([],Acc,0)    -> {number,conv_to_num(Acc)}; % don't reverse list!
verify_num([],Acc,1)    -> {number,conv_to_num(Acc)}; % don't reverse list!
verify_num([],Acc,N)    -> exit(invalid_format);
verify_num([H|T],Acc,N) ->
  NewAcc=[H|Acc],
  NewN=N+get_decimals(H),
  verify_num(T,[H|Acc],N+get_decimals(H)).

conv_to_num(List) -> io:format("in conv_to_num List is ~p~n",[List]),
                     conv_to_num(List,[]).

conv_to_num([],Acc)                        -> Acc; % don't reverse list!
conv_to_num([{colour,Colour}|T],Acc)       -> conv_to_num(T,[{colour,Colour}|Acc]);
conv_to_num([{condition,Cond}|T],Acc)      -> conv_to_num(T,[{condition,Cond}|Acc]);
conv_to_num([{percent,Perc}|T],Acc)        -> conv_to_num(T,[{percent,Perc}|Acc]);
conv_to_num([{fraction,Frac}|T],Acc)       -> conv_to_num(T,[{fraction,Frac}|Acc]);
conv_to_num([{string,String}|T],Acc)       -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{dollar,String}|T],Acc)       -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{minus,String}|T],Acc)        -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{plus,String}|T],Acc)         -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{forwardslash,String}|T],Acc) -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{open_bra,String}|T],Acc)     -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{close_ket,String}|T],Acc)    -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{colon,String}|T],Acc)        -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{space,String}|T],Acc)        -> conv_to_num(T,[{string,String}|Acc]);
conv_to_num([{format,Format}|T],Acc)       -> conv_to_num(T,[{format,Format}|Acc]);
conv_to_num([{char,"£"}|T],Acc)            -> conv_to_num(T,[{string,"£"}|Acc]);
conv_to_num([{char,"*"}|T],Acc)            -> conv_to_num(T,Acc); % silently drop "*"'s
conv_to_num([{underscore,UndS}|T],Acc)     -> conv_to_num(T,Acc); % silently drop "_"'s
conv_to_num([{char,"."}|T],Acc)            -> conv_to_num(T,[{format,"."}|Acc]); % convert to format!
conv_to_num([{char,","}|T],Acc)            -> conv_to_num(T,[{format,","}|Acc]); % convert to format!
conv_to_num(Other,_Acc)                    -> io:format("in conv_to_num Other is ~p~n",[Other]),
                                              exit(invalid_format).

is_text(A) when is_list(A) -> is_text2(A);
is_text(A)                 -> is_text2([A]).

is_text2([])          -> no;
is_text2([{at,_}|_T]) -> yes;
is_text2([H|T])       -> is_text2(T).

is_date(A) when is_list(A) -> is_date2(A);
is_date(A)                 -> is_date([A]).

is_date2([])               -> no;
is_date2([{year,_}|_T])    -> yes;
is_date2([{mon_min,_}|_T]) -> yes;
is_date2([{mon,_}|_T])     -> yes;
is_date2([{day,_}|_T])     -> yes;
is_date2([{hour,_}|_T])    -> yes;
is_date2([{sec,_}|_T])     -> yes;
is_date2([{ampm,_}|_T])    -> yes;
is_date2([H|T])            -> is_date2(T).

get_decimals({format,Format}) -> count_dec(Format);
get_decimals({char,"."})      -> 1;
get_decimals(_)               -> 0.

count_dec(Format) -> count_dec(Format,0).

count_dec([],N)             -> N;
count_dec([?ASC_FULLS|T],N) -> count_dec(T,N+1);
count_dec([H|T],N)          -> count_dec(T,N).

fix_up({format,A}) -> {tokens,A}.

to_tk({_,A}) -> {token,A}.

concat(A,B) -> [A,B].

concat(A,B,C) -> [A,B,C].

esc(A) -> {char, A}.

make_cond({condition,String})->
        Str2=string:strip(String,left,$[),
        Str3=string:strip(Str2,right,$]),
        {condition,Str3}.

strip({string,A}) -> 
        A1 = string:strip(A,both,$"),%" syntax highlighting fix
        {string,A1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% These functions all pertain to make_src                                   %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_src(A) when is_list(A) -> make_src2(lists:flatten(A));
make_src(A)                 -> make_src2([A]).
  
make_src2(A) ->
  Clauses=organise_clauses(A),
  io:format("in make_src Clauses are ~p~n",[Clauses]),
  Return=gen_src(Clauses,[">0","<0","=0"]),
  io:format("In make_src2 Return is ~p~n",[Return]),
  Return.

gen_src(Clauses,Defaults) -> 
  Return=gen_src(Clauses,Defaults,[]),
  io:format("in gen_src~n-Clauses are ~p~n-Defaults are ~p~n Return is ~p~n",[Clauses,Defaults,Return]),
  Return.

%% generate the source from the list of clauses and the list of default clauses
%% if there aren't enough conditions repeat the first one
gen_src([],[],Acc)           -> 
  io:format("in gen_src (1) Acc is ~p~n",[Acc]),
  gen_src2(lists:reverse(Acc));
%% this is nasty to fix the ghastly 'feature' of sign swapping for explicity
%% specified '<0' clauses - this one isn't and we 
gen_src([],["<0"|T2],Acc)      -> 
  io:format("in gen_src (2) T2 is ~p Acc is ~p~n",[T2,Acc]),  
  gen_src([],T2,[swap_cond("dont swap sign",lists:last(Acc))|Acc]); 
gen_src([],[H2|T2],Acc)      -> 
  io:format("in gen_src (3) H2 is ~p T2 is ~p Acc is ~p~n",[H2,T2,Acc]),  
  gen_src([],T2,[swap_cond(H2,lists:last(Acc))|Acc]); 
gen_src([H1|T1],[],Acc)      -> 
  io:format("in gen_src (4) H1 is ~p T1 is ~p Acc is ~p~n",[H1,T1,Acc]),  
  gen_src(T1,[],[make_default(H1)|Acc]);
gen_src([H1|T1],[H2|T2],Acc) ->
  io:format("in gen_src (5) H1 is ~p T1 is ~p H2 is ~p T2 is ~p Acc is ~p~n",[H1,T1,H2,T2,Acc]),  
  gen_src(T1,T2,[make_default(H1,H2)|Acc]).

gen_src2(Clauses)->
  io:format("in format:get_src2 Clauses are ~p~n",[Clauses]),
  Clause1=make_clause(lists:nth(1,Clauses)),
  Clause2=make_clause(lists:nth(2,Clauses)),
  Clause3=strip_semi(make_clause(lists:nth(3,Clauses))),  
  Clause4 = case length(Clauses) of
    3 -> default_clause();
    4 -> strip_format(lists:nth(4,Clauses))
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

strip_format([{condition,Text}|T]) -> T.

strip_semi(String)->
  string:strip(String,right,$;).

make_clause([{condition,Cond},{colour,Col}]) -> [{condition,Cond},{colour,Col}];
make_clause([{condition,"<0"},{colour,Col},{Type,Format}]) ->
   make_clause2([{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause([{condition,"=<0"},{colour,Col},{Type,Format}]) ->
   make_clause2([{condition,"=<0"},{colour,Col},{Type,Format}]);
%% this clause is a default clause and there should *NOT* have the sign change of
%% a custom specified <0 clause
%% you want someone to blame try billgates@microsoft.com
make_clause([{condition,"dont swap sign"},{colour,Col},{Type,Format}]) ->
   make_clause3([{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause([{condition,Cond},{colour,Col},{Type,Format}]) ->
   make_clause3([{condition,Cond},{colour,Col},{Type,Format}]).

%% if the conditional clause is less than (or in our case less than or equals) to zero
%% then the Excel format automatically changes the sign of the number to positive!
make_clause2([{condition,Cond},{colour,Col},{Type,Format}]) ->
  io:format("in make_clause2 Cond is ~p Format is ~p~n",[Cond,Format]),
  FormatClause={Type,Format},
  Bits=io_lib:fwrite("~p",[FormatClause]),
  Bits2=lists:flatten(Bits),
  "X"++tart_up(Cond)++" -> {"++atom_to_list(Col)++",format:format(-X,"++Bits2++")};".

%% this is the normal clause - but it also is picked up for automatically generated
%% <0 clauses
make_clause3([{condition,Cond},{colour,Col},{Type,Format}]) ->
  io:format("in make_clause3 Cond is ~p Format is ~p~n",[Cond,Format]),
  FormatClause={Type,Format},
  Bits=io_lib:fwrite("~p",[FormatClause]),
  Bits2=lists:flatten(Bits),
  "X"++tart_up(Cond)++" -> {"++atom_to_list(Col)++",format:format(X,"++Bits2++")};".


default_clause() -> "{black,X};".

%% tart_up just makes the condition clauses well behaved
tart_up(text)                  -> "= text";
tart_up([?ASC_GT,?ASC_EQ|Rest])->[?ASC_SPACE,?ASC_GT,?ASC_EQ,?ASC_SPACE|Rest];
tart_up([?ASC_EQ,?ASC_LT|Rest])->[?ASC_SPACE,?ASC_EQ,?ASC_LT,?ASC_SPACE|Rest];
%% switch '=' to '=='
tart_up([?ASC_EQ|Rest])        ->[?ASC_SPACE,?ASC_EQ,?ASC_EQ,?ASC_SPACE|Rest];
tart_up([Op|Rest])             ->[?ASC_SPACE,Op,?ASC_SPACE|Rest].

swap_cond(Cond,[{condition,_OldCond}|Rest]) -> [{condition,Cond}|Rest].

make_default([{colour,Col}|Rest]) -> [{condition,text},{colour,Col}|Rest];
make_default(Plain)               -> [{condition,text},{colour,black}|Plain].
    
make_default([{condition,Cond},{colour,Col}|R],_Def)-> [{condition,Cond},{colour,Col}|R];
make_default([{colour,Col}|R],Def)                  -> [{condition,Def},{colour,Col}|R];
make_default([{condition,Cond}|R],_Def)             -> [{condition,Cond},{colour,black}|R];
make_default(Plain,Def)                             -> [{condition,Def},{colour,black}|Plain].

organise_clauses(A) ->
  Clauses=organise_clauses(A,[],[]),
  Return=promote_subclauses(Clauses),
  io:format("in organise_clauses~n-Clauses are ~p~n-Return is ~p~n",[Clauses,Return]),
  Return.

organise_clauses([],Acc1,Acc2)                -> [lists:reverse(Acc1)|Acc2]; % don't reverse the Acc!
organise_clauses([{semicolon,_}|T],Acc1,Acc2) -> organise_clauses(T,[],[lists:reverse(Acc1)|Acc2]);
organise_clauses([Other|T],Acc1,Acc2)         -> organise_clauses(T,[Other|Acc1],Acc2).

promote_subclauses(A) -> promote_subclauses(A,[]).

promote_subclauses([],Acc) -> Acc; % don't reverse the Acc!
promote_subclauses([[{Type,[{condition,Cond},{colour,Col}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{condition,Cond},{colour,Col}|[{number,Rest}]]|Acc]);
promote_subclauses([[{Type,[{condition,Cond}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{condition,Cond}|[{number,Rest}]]|Acc]);
promote_subclauses([[{Type,[{colour,Col}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{colour,Col}|[{number,Rest}]]|Acc]);
promote_subclauses([H|T],Acc) ->
  promote_subclauses(T,[H|Acc]).