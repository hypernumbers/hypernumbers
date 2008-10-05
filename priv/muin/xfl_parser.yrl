%%% @doc Parser for the formula language.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

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
error
'(' ')' ',' '{' '}' ';'
.

Rootsymbol Formula.
Endsymbol  '$end'.

%%% **Associativity and precedence**

Unary    50  Uplus.
Left     100 '=' '<>' '>' '<' '>=' '<='.
Left     200 '&'.
Left     300 '+' '-'.
Left     400 '*' '/'.
Left     500 '^'.
Left     600 '%'.
Unary    700 Uminus.
Nonassoc 800 ':'.

%%% **Grammar definition**

Formula -> E : postproc('$1').

%% Comparison operators.
E -> E '='  E : op('$1', '$2', '$3').
E -> E '<>' E : op('$1', '$2', '$3').
E -> E '>'  E : op('$1', '$2', '$3').
E -> E '<'  E : op('$1', '$2', '$3').
E -> E '>=' E : op('$1', '$2', '$3').
E -> E '<=' E : op('$1', '$2', '$3').

%% Concatenation operator.
E -> E '&' E : op('$1', {concatenate}, '$3').
    
%% Arithmetic operators.
E -> E '+' E : op('$1', {sum},  '$3').
E -> E '-' E : op('$1', '$2',  '$3').
E -> E '*' E : op('$1', '$2',  '$3').
E -> E '/' E : op('$1', '$2', '$3').
E -> E '^' E : op('$1', {power}, '$3').
E -> ref ref : special_div('$1', '$2').
E -> E ref   : special_div('$1', '$2').

%% Percent.
E -> E '%' : ['/', '$1', [int, 100]].

%% Unary minus and plus.
E      -> Uminus : '$1'.
Uminus -> '-' E  : [negate, '$2'].

E      -> Uplus  : '$1'.
Uplus  -> '+' E  : '$2'.

%% Ranges
%% Cell ranges (A1/RC in all sorts of weird combos as well).
E -> ref  ':' ref    : [':', '$1', '$3'].
%% Row and column ranges. TODO: this only handles same-page.
E -> atom ':' atom   : [':', '$1', '$3'].
E -> int  ':' int    : [':', '$1', '$3'].
%% INDIRECT ranges.
E -> Funcall ':' ref : [':', '$1', '$3'].
E -> ref ':' Funcall : [':', '$1', '$3'].

%% Parenthesized expressions.
E -> '(' E ')' : '$2'.

%% Funcalls.
E -> Funcall : '$1'.
%% Literals.
E -> int   : lit('$1').
E -> float : lit('$1').
E -> bool  : lit('$1').
E -> str   : lit('$1').
E -> ref   : lit('$1').
E -> name  : lit('$1').
E -> error : lit('$1').
E -> Array : '$1'.

%% Funcalls.
Funcall -> atom '(' ')'      : [func_name('$1')].
Funcall -> atom '(' Args ')' : func('$1', '$3').
%% Special case for functions with names like ATAN2
Funcall -> ref '(' ')'       : [func_name('$1')].
Funcall -> ref '(' Args ')'  : func('$1', '$3').
    
Args -> E                    : ['$1'].
Args -> E ',' Args           : ['$1'] ++ '$3'.

%% Arrays: lists of rows, which are lists of values of allowed types.
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
func_name({atom, NameAsStr}) ->
    list_to_atom(NameAsStr);
func_name({ref, _, _, _, Refstr}) ->
    list_to_atom(string:to_lower(Refstr)). % For ATAN2 etc.
    
lit({name, Data}) ->
    [name, Data];
lit({ref, R, C, P, _}) ->
    [ref, R, C, P];
lit({error, Errval}) ->
    {errval, Errval};
lit({_Type, Data}) ->
    Data.
lit({_Type, Data}, Fun) ->
    Fun(Data).

%% Make an op function call for the AST from lexer tokens.
op(Arg1, {Op}, Arg2) ->
    [Op, Arg1, Arg2].

special_div(Ref1 = {ref, _, _, _, _}, Ref2 = {ref, _, _, "/", _}) ->
    op(Ref1, {'/'}, Ref2);
special_div(E, Ref = {ref, _, _, "/", _}) ->
    op(E, {'/'}, Ref).

%% Make a straight-up function call for the AST from a token and a
%% list of args.
func(Tuple, Args) ->
    [func_name(Tuple)] ++ Args.

%% Convert representation of array in AST into Erlang's native list-of-lists.
to_native_list(Ary) ->
    %% Check if the array is rectangular first.
    RowLen = length(element(2, hd(Ary))),
    Allok = all(fun({row, Vals}) -> length(Vals) == RowLen end,
                Ary),

    ?IF(not(Allok), throw(invalid_array)),

    %% Tail cos there'll be an extra [] in the list after the fold.
    {array, tl(foldl(fun(Row, Acc) ->
                             {row, Elts} = Row,
                             Acc ++ [Elts]
                     end,
                     [[]], %% <== See, here it is.
                     Ary))}.

postproc(Ast) ->
    Ast.
