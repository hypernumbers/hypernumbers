%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
FinalFormat
ColFormat
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

%% general format
general

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
min
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
Final -> general     : get_general().

Semicolon -> semicolon           : tr('semicolon 1','$1').
Semicolon -> Semicolon semicolon : tr('semicolon 2',concat('$1','$2')).

FinalFormat -> ColFormat Semicolon FinalFormat : concat(verify('$1'),'$2','$3').

FinalFormat -> ColFormat Semicolon  : concat(verify('$1'),'$2').
FinalFormat -> ColFormat            : verify('$1').

%% Set up the escaped character formats
Escaped -> esc Token  : esc('$2').
Escaped -> esc esc    : esc('$2').

%% you can skip either or both of Condition or Col with a format
ColFormat -> Col FullFormat : concat('$1','$2').
ColFormat -> FullFormat      : '$1'.

Format -> Tokens          : '$1'.
Format -> fraction        : '$1'.

FullFormat -> Format          : '$1'.
FullFormat -> Format percent  : concat('$1','$2').
FullFormat -> Cond FullFormat : concat('$1','$2').

Cond -> condition : make_cond('$1').

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
Token -> min          : '$1'.
Token -> day          : '$1'.
Token -> hour         : '$1'.
Token -> sec          : '$1'.
Token -> ampm         : '$1'.

%% And text token
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
%%dump([])    -> ok;
%%dump([H|T]) ->
%%  io:format("in dump H is ~p~n",[H]),
%%  dump(T).

tr(Location,Term) ->
  io:format("at ~p Term is ~p~n",[Location,Term]),
  Term.

%%
%% 'Proper' functions
%%
verify(A) when is_list(A) ->
  B=lists:flatten(A),
  case is_text(B) of
    true  -> {text,clean_up_text(B)};
    false -> case is_date(B) of
      true  -> {date,B};
      false -> verify_num(B)
    end
  end; % yes this is a clause end...
verify(A) -> verify([A]). %% If 'A' is not a list make it so

verify_num(A) when is_list(A) -> verify_num(A,[],0);
verify_num(A)                 -> verify_num([A],[],0).

%% There can be 0 or 1 decimal point - any more and it's not valid...
verify_num([],Acc,0)    -> {number,conv_to_num(lists:reverse(Acc))};
verify_num([],Acc,1)    -> {number,conv_to_num(lists:reverse(Acc))};
verify_num([],_Acc,_N)  -> exit(invalid_format);
verify_num([H|T],Acc,N) -> verify_num(T,[H|Acc],N+get_decimals(H)).

%% clean_up_text just silently drops the following format commands from Excel
%% {char,"*"}
clean_up_text(List) -> clean_up_text(clear_underscores(List),[]).

clean_up_text([],Residuum)             -> lists:reverse(Residuum);
clean_up_text([{char,"*"}|T],Residuum) -> clean_up_text(T,Residuum);
clean_up_text([H|T],Residuum)          -> clean_up_text(T,[H|Residuum]).

conv_to_num(List) -> conv_to_num(clear_underscores(List),[]).

conv_to_num([],Acc)                        -> lists:reverse(Acc);
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
conv_to_num([{char,"."}|T],Acc)            -> conv_to_num(T,[{format,"."}|Acc]); % convert to format!
conv_to_num([{char,","}|T],Acc)            -> conv_to_num(T,[{format,","}|Acc]); % convert to format!
conv_to_num(Other,_Acc)                    -> io:format("in conv_to_num Other is ~p~n",[Other]),
                                              exit(invalid_format).

%% the underscore character in Excel is used to make a character non printing but to
%% have it space filling - this means that columns line up in an appropriate fashion
%% this is a mixed formula/GUI approach suitable for a file-based spreadsheet but
%% not a web-based one
%% We just delete the underscore and just replace the character with a space
%% (This will work with monospaced characters in the browser)
clear_underscores(List) -> clear_underscores(List,[]).

clear_underscores([],Residuum)                  -> lists:reverse(Residuum);
clear_underscores([{underscore,_UndS},_|T],Acc) -> clear_underscores(T,[{string," "}|Acc]);
clear_underscores([H|T],Acc)                    -> clear_underscores(T,[H|Acc]). 

is_text(A) when is_list(A) -> is_text2(A);
is_text(A)                 -> is_text2([A]).

is_text2([])          -> false;
is_text2([{at,_}|_T]) -> true;
is_text2([_H|T])      -> is_text2(T).

is_date(A) when is_list(A) -> is_date2(A);
is_date(A)                 -> is_date([A]).

is_date2([])               -> false;
is_date2([{year,_}|_T])    -> true;
is_date2([{mon_min,_}|_T]) -> true;
is_date2([{mon,_}|_T])     -> true;
is_date2([{min,_}|_T])     -> true;
is_date2([{day,_}|_T])     -> true;
is_date2([{hour,_}|_T])    -> true;
is_date2([{sec,_}|_T])     -> true;
is_date2([{ampm,_}|_T])    -> true;
is_date2([_H|T])            -> is_date2(T).

get_decimals({format,Format}) -> count_dec(Format);
get_decimals({char,"."})      -> 1;
get_decimals(_)               -> 0.

count_dec(Format) -> count_dec(Format,0).

count_dec([],N)                -> N;
count_dec([?ASC_FULLSTOP|T],N) -> count_dec(T,N+1);
count_dec([_H|T],N)            -> count_dec(T,N).

%fix_up({format,A}) -> {tokens,A}.

%to_tk({_,A}) -> {token,A}.

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

get_general() -> make_src([[{number,[{format,"0.00"}]}]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% These functions all pertain to make_src                                   %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_src(A) when is_list(A) -> make_src2(lists:flatten(A));
make_src(A)                 -> make_src2([A]).
  
make_src2(A) ->
  Type=get_type(A),
  Clauses=organise_clauses(A),
  {Type,gen_src(Clauses,[">0","<0","=0"])}.

get_type(A)                                                -> get_type(A,[]).
get_type([],Residuum)                                      -> verify_type(lists:reverse(Residuum));
get_type([{Type,_}|T],Residuum)                            -> get_type(T,[[Type]|Residuum]);
get_type([semicolon|T],Residuum)                           -> get_type(T,Residuum);
get_type([[{condition,_},{colour,_},{Type,_}]|T],Residuum) -> get_type(T,[[Type]|Residuum]);
get_type([[{condition,_},{Type,_}]|T],Residuum)            -> get_type(T,[[Type]|Residuum]);
get_type([[{colour,_},{Type,_}]|T],Residuum)               -> get_type(T,[[Type]|Residuum]).

verify_type([number]) -> number;
verify_type([date])   -> date;
verify_type([text])   -> text;
verify_type(A)        -> case lists:merge(A) of
				[number] -> number;
				[date]   -> date;
				[text]   -> text;
				_        -> verify_list(remove_semicolons(A))
  			end.

remove_semicolons(List) -> remove_semicolons(List,[]).

remove_semicolons([],Residuum)              -> lists:reverse(Residuum);
remove_semicolons([[semicolon]|T],Residuum) -> remove_semicolons(T,Residuum);
remove_semicolons([H|T],Residuum)           -> remove_semicolons(T,[H|Residuum]).

verify_list([[A]]    )            -> A;
verify_list([[A],[A]])            -> A;
verify_list([[A],[A],[A]])        -> A;
verify_list([[A],[A],[A],[text]]) -> A.

gen_src(Clauses,Defaults) -> gen_src(Clauses,Defaults,[]).

%% generate the source from the list of clauses and the list of default clauses
%% if there aren't enough conditions repeat the first one
gen_src([],[],Acc)           -> gen_src2(lists:reverse(Acc));
%% this is nasty to fix the ghastly 'feature' of sign swapping for explicity
%% specified '<0' clauses - this one isn't and we 
gen_src([],["<0"|T2],Acc)    -> gen_src([],T2,[swap_cond("dont swap sign",lists:last(Acc))|Acc]); 
gen_src([],[H2|T2],Acc)      -> gen_src([],T2,[swap_cond(H2,lists:last(Acc))|Acc]); 
gen_src([H1|T1],[],Acc)      -> gen_src(T1,[],[make_default(H1)|Acc]);
gen_src([H1|T1],[H2|T2],Acc) -> gen_src(T1,T2,[make_default(H1,H2)|Acc]).

gen_src2(Clauses)->
  io:format("in num_format.yrl:gen_src2~n-Clauses is ~p~n",[Clauses]),
  Clause1=make_clause(lists:nth(1,Clauses)),
  Clause2=make_clause(lists:nth(2,Clauses)),
  Clause3=make_clause(lists:nth(3,Clauses)),
  io:format("in num_format.yrl:gen_src2~n-Clause1 is ~p~n-Clause2 is ~p~n"++
	"-Clause3 is ~p~n",[Clause1,Clause2,Clause3]),
  NewClauses=rearrange_clauses(Clause1,Clause2,Clause3),
  io:format("in num_format.yrl:gen_src2 NewClauses is ~p~n",[NewClauses]),
  Clause4 = case length(Clauses) of
    3 -> io:format("in num_format.yrl:gen_src2 got to 3~n"), 
	 default_clause();
    4 -> io:format("in num_format.yrl:gen_src2 got to 4~n"),
	 RawClause=lists:nth(4,Clauses),
	 io:format("in num_format.yrl:gen_src2 got to 5~n"),
	 MadeClause=make_clause(RawClause), 
	 io:format("in num_format.yrl:gen_src2 got to 6~n"),
	 strip_condition(MadeClause)
  end,
  io:format("in num_format.yrl:gen_src2~n-Clause1 is ~p~n-Clause2 is ~p~n-Clause3 is ~p~n"++
	     "-Clause4 is ~p~n",[Clause1,Clause2,Clause3,Clause4]),
   Src="fun(X) -> "++
 	      "   Return=try tconv:to_i(X)"++
 	      "       catch"++
 	      "         error: _ ->"++
 	      "           try tconv:to_f(X)"++
 	      "           catch"++
 	      "               error: _ ->"++
 	      "                    {error, value}"++
 	      "           end"++
 	      "       end,"++
 	      "    case Return of"++
 	      "      {error,_} -> "++
 	                  Clause4++";"++
 	      "      _ ->"++
 	      "          if "++
 	                     NewClauses++
 	      "          end"++
 	      "      end"++
 	      "   end.",
  lists:flatten(Src).

%% Rearrange clauses to ensure that the equality clause appears first
rearrange_clauses(Clause1,Clause2,Clause3)->
  Order={is_eq(Clause1),is_eq(Clause2),is_eq(Clause3)},
  case Order of
    {true,false,false}  -> Clause1++"; "++Clause2++"; "++Clause3++" ";
    {false,true,false}  -> Clause2++"; "++Clause1++"; "++Clause3++" ";
    {false,false,true}  -> Clause3++"; "++Clause1++"; "++Clause2++" ";
    {false,false,false} -> Clause1++"; "++Clause2++"; "++Clause3++" "
  end.

is_eq([$X,?ASC_SPACE,?ASC_EQ,?ASC_EQ|_T]) -> true;
is_eq(_Other)                             -> false.

%% make sure there is a semicolon at the end of this list
%%ensure_semi(List) ->
%%  [H|_T]=lists:reverse(List),
%%  case H of
%%	$; -> List;
%%	_   -> lists:append([List,";"])
%%  end.

strip_condition(Clause) -> 
	Loc=string:str(Clause,"-> "),
	Len=string:len(Clause),
        string:right(Clause,Len-(Loc+2)).% 2 for the additional chars

%%strip_semi(String)->
%%  string:strip(String,right,$;).

make_clause([{condition,Cond},{colour,Col}]) -> [{condition,Cond},{colour,Col}];
make_clause([{condition,"<0"},{colour,Col},{Type,Format}]) ->
   make_clause2([{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause([{condition,"=<0"},{colour,Col},{Type,Format}]) ->
   make_clause2([{condition,"=<0"},{colour,Col},{Type,Format}]);
%% this clause is a default clause and should *NOT* have the sign change of
%% a custom specified <0 clause
%% you want someone to blame try billgates@microsoft.com
make_clause([{condition,"dont swap sign"},{colour,Col},{Type,Format}]) ->
   make_clause3([{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause([{condition,Cond},{colour,Col},{Type,Format}]) ->
   make_clause3([{condition,Cond},{colour,Col},{Type,Format}]).

%% if the conditional clause is less than (or in our case less than or equals) to zero
%% then the Excel format automatically changes the sign of the number to positive!
make_clause2([{condition,Cond},{colour,Col},{Type,Format}]) ->
  FormatClause={Type,Format},
  Bits=io_lib:fwrite("~p",[FormatClause]),
  Bits2=lists:flatten(Bits),
  "X"++tart_up(Cond)++" -> {"++atom_to_list(Col)++",format:format(-X,"++Bits2++")}".

%% this is the normal clause - but it also is picked up for automatically generated
%% <0 clauses
make_clause3([{condition,Cond},{colour,Col},{Type,Format}]) ->
  FormatClause={Type,Format},
  Bits=io_lib:fwrite("~p",[FormatClause]),
  Bits2=lists:flatten(Bits),
  "X"++tart_up(Cond)++" -> {"++atom_to_list(Col)++",format:format(X,"++Bits2++")}".


default_clause() -> "{black,X}".

%% tart_up just makes the condition clauses well behaved
tart_up(text)                  -> "= text";
tart_up([?ASC_GT,?ASC_EQ|Rest])->[?ASC_SPACE,?ASC_GT,?ASC_EQ,?ASC_SPACE|Rest];
tart_up([?ASC_EQ,?ASC_LT|Rest])->[?ASC_SPACE,?ASC_EQ,?ASC_LT,?ASC_SPACE|Rest];
%% Now make it work for arse backwards GE and LT operators
%% (Polite note Excel formats expect '<=' not '=<' so give it 'em both ways...)
tart_up([?ASC_EQ,?ASC_GT|Rest])->[?ASC_SPACE,?ASC_GT,?ASC_EQ,?ASC_SPACE|Rest];
tart_up([?ASC_LT,?ASC_EQ|Rest])->[?ASC_SPACE,?ASC_EQ,?ASC_LT,?ASC_SPACE|Rest];
%% switch '=' to '=='
tart_up([?ASC_EQ|Rest])        ->[?ASC_SPACE,?ASC_EQ,?ASC_EQ,?ASC_SPACE|Rest];
tart_up([Op|Rest])             ->[?ASC_SPACE,Op,?ASC_SPACE|Rest].

swap_cond(Cond,[{condition,_OldCond}|Rest]) -> [{condition,Cond}|Rest].

make_default([{colour,Col}|Rest]) -> [{condition,text},{colour,Col}|Rest];
make_default(Plain)               -> [{condition,text},{colour,black}|Plain].
    
make_default([{condition,Cond},{colour,Col}|R],_Def)-> [{condition,Cond},{colour,Col}|R];
make_default([{colour,Col}|R],Def)                  -> [{condition,Def},{colour,Col}|R];
make_default([{condition,Cond}|R],_Def)             -> [{condition,Cond},{colour,black}|R];
make_default([],Def)                                -> io:format("num_format.yrl:make_default (1)"++
								 "Def is ~p~n",[Def]),
				                       [{condition,Def},{colour,black},
							{number,[{format,""}]}];
make_default(Plain,Def)                             -> io:format("num_format.yrl:make_default "++
								 "Plain is ~p and Def is ~p~n",
								 [Plain,Def]),
				                       [{condition,Def},{colour,black}|Plain].

organise_clauses(A) ->
  Clauses=organise_clauses(A,[],[]),
 promote_subclauses(Clauses).

organise_clauses([],Acc1,Acc2)                -> [lists:reverse(Acc1)|Acc2]; % don't reverse the Acc!
organise_clauses([{semicolon,_}|T],Acc1,Acc2) -> organise_clauses(T,[],[lists:reverse(Acc1)|Acc2]);
organise_clauses([Other|T],Acc1,Acc2)         -> organise_clauses(T,[Other|Acc1],Acc2).

promote_subclauses(A) -> promote_subclauses(A,[]).

promote_subclauses([],Acc) -> Acc; % don't reverse the Acc!
%% swap the order of the colour and condition clauses!!
promote_subclauses([[{Type,[{colour,Col},{condition,Cond}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{condition,Cond},{colour,Col}|[{Type,Rest}]]|Acc]);
promote_subclauses([[{Type,[{condition,Cond}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{condition,Cond}|[{Type,Rest}]]|Acc]);
promote_subclauses([[{Type,[{colour,Col}|Rest]}]|T],Acc) ->
  promote_subclauses(T,[[{colour,Col}|[{Type,Rest}]]|Acc]);
promote_subclauses([H|T],Acc) ->
  promote_subclauses(T,[H|Acc]).