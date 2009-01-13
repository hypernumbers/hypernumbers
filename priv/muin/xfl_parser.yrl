%%% @doc Parser for the formula language.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% TODO: only same-page row & col ranges are supported right now.
%%% TODO: use list_to_existing_atom (& catch errors).
%%% TODO: handle invalid arrays propely.

Nonterminals

Formula E Uminus Uplus Funcall Args
ArrayLiteral ArrayRow ArrayRows Array
.

Terminals

atom name
'=' '<>' '>' '<' '>=' '<='
'+' '-' '*' '/' '^'
'&' '%' ':'
int float bool str
ref
errval
'(' ')' ',' '{' '}' ';'
ssatomref ssnumref
.

Rootsymbol Formula.
Endsymbol  '$end'.

%%% associativity & precedence rules.

Unary    50  Uplus.
Left     100 '=' '<>' '>' '<' '>=' '<='.
Left     200 '&'.
Left     300 '+' '-'.
Left     400 '*' '/'.
Left     500 '^'.
Left     600 '%'.
Unary    700 Uminus.
Nonassoc 800 ':'.

%%% production rules

Formula -> E : postproc('$1').

%%% operators

E -> E '='  E : op('$1', '$2', '$3').
E -> E '<>' E : op('$1', '$2', '$3').
E -> E '>'  E : op('$1', '$2', '$3').
E -> E '<'  E : op('$1', '$2', '$3').
E -> E '>=' E : op('$1', '$2', '$3').
E -> E '<=' E : op('$1', '$2', '$3').

E -> E '&' E : op('$1', {concatenate}, '$3').
    
E -> E '+' E : op('$1', '$2',  '$3').
E -> E '-' E : op('$1', '$2',  '$3').
E -> E '*' E : op('$1', '$2',  '$3').
E -> E '/' E : op('$1', '$2', '$3').
E -> E '^' E : op('$1', {power}, '$3').
E -> ref ref : special_div('$1', '$2').
E -> E ref   : special_div('$1', '$2').
E -> E ssnumref : special_div('$1', '$2').

E -> E '%' : ['/', '$1', [int, 100]].

E      -> Uminus : '$1'.
Uminus -> '-' E  : [negate, '$2'].
E      -> Uplus  : '$1'.
Uplus  -> '+' E  : '$2'.


%%% ranges

%%% cell ranges: A1 & RC, in combos too.
E -> ref  ':' ref    : [':', '$1', '$3'].

%%% row & col ranges.

%% same page

E -> atom ':' atom   : [':', '$1', '$3'].
E -> int  ':' int    : [':', '$1', '$3'].

%% other pages

E -> ssatomref ':' atom : [':', '$1', '$3'].
E -> ssnumref  ':' int  : [':', '$1', '$3'].

%% INDIRECT ranges.

E -> Funcall ':' ref : [':', '$1', '$3'].
E -> ref ':' Funcall : [':', '$1', '$3'].

%%% parenthesized expressions.

E -> '(' E ')' : '$2'.

%%% funcalls

E -> Funcall : '$1'.

%%% constants / literals

E -> int    : lit('$1').
E -> float  : lit('$1').
E -> bool   : lit('$1').
E -> str    : lit('$1').
E -> ref    : lit('$1').
E -> name   : lit('$1').
E -> errval : lit('$1').
E -> Array  : '$1'.

%%% funcall productions

Funcall -> atom '(' ')'      : [func_name('$1')].
Funcall -> atom '(' Args ')' : func('$1', '$3').
%% Special case for functions with names like ATAN2
Funcall -> ref '(' ')'       : [func_name('$1')].
Funcall -> ref '(' Args ')'  : func('$1', '$3').
    
Args -> E                    : ['$1'].
Args -> E ',' Args           : ['$1'] ++ '$3'.

%%% arrays ( = lists of rows, which are lists of values of allowed types)
Array -> '{' ArrayRows '}' : to_native_list('$2').

ArrayRows -> ArrayRow : [{row, '$1'}].
ArrayRows -> ArrayRow ';' ArrayRows : [{row, '$1'}] ++ '$3'.

ArrayRow -> ArrayLiteral : ['$1'].
ArrayRow -> ArrayLiteral ',' ArrayRow : ['$1'] ++ '$3'.

ArrayLiteral -> int       : lit('$1').
ArrayLiteral -> float     : lit('$1').
ArrayLiteral -> '-' int   : lit('$2', fun(X) -> -X end).
ArrayLiteral -> '-' float : lit('$2', fun(X) -> -X end).
ArrayLiteral -> '+' int   : lit('$2').
ArrayLiteral -> '+' float : lit('$2').
ArrayLiteral -> bool      : lit('$1').
ArrayLiteral -> str       : lit('$1').    


Erlang code.

-include("handy_macros.hrl").

%% Make a function name for the AST from lexer tokens.
func_name({atom, NameAsStr})      -> list_to_atom(NameAsStr);
func_name({ref, _, _, _, Refstr}) -> list_to_atom(string:to_lower(Refstr)). % For ATAN2 etc.

%%% stuff from lexer -> stuff for AST.

%% literals
lit({name, Data, Path}) -> [name, Data, Path];
lit({ref, R, C, P, _})  -> [ref, R, C, P];
lit({errval, Errval})   -> {errval, Errval};
lit({_Type, Data})      -> Data.
lit({_Type, Data}, Fun) -> Fun(Data).

%% operator function calls
op(Arg1, {Op}, Arg2) -> [Op, Arg1, Arg2].

%%% special cases for division / path separator ambiguity:

special_div(Ref1 = {ref, _, _, _, _}, Ref2 = {ref, _, _, "/", _}) ->
    Ref22 = setelement(4, Ref2, "."), % the cell is on current page, NOT root.
    op(hslists:init(tuple_to_list(Ref1)),
       {'/'},
       hslists:init(tuple_to_list(Ref22)));
special_div(E, Ref = {ref, _, _, "/", _}) ->
    Ref2 = setelement(4, Ref, "."), 
    op(E, {'/'}, hslists:init(tuple_to_list(Ref2)));
special_div(E, _Ref = {ssnumref, Str}) -> % Str is something like "/59"
    op(E, {'/'}, list_to_integer(tl(Str))).

%% token + list of args -> function call for AST.
func(Tuple, Args) -> [func_name(Tuple)] ++ Args.

%% Convert representation of array in AST into Erlang's native list-of-lists.
to_native_list(Ary) ->
    RowLen = length(element(2, hd(Ary))),
    Rectp = all(fun({row, Vals}) -> length(Vals) == RowLen end, Ary),
    ?IF(not(Rectp), throw(invalid_array)),

    %% Tail cos there'll be an extra [] in the list after the fold.
    {array, tl(foldl(fun(Row, Acc) ->
                             {row, Elts} = Row,
                             Acc ++ [Elts]
                     end,
                     [[]], %% <== See, here it is.
                     Ary))}.

postproc(Ast) ->
    Ast.