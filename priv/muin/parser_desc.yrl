%%% @doc Muin parser definition. The parser produces an s-expression.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% **Grammar symbols**

Nonterminals

Formula E Uminus Uplus Literal Funcall Args
ArrayLiteral ArrayRow ArrayRows Array
.

Terminals

name
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

Formula -> E : '$1'.

%% Comparison operators.
E -> E '='  E : op('$1', '$2', '$3').
E -> E '<>' E : op('$1', '$2', '$3').
E -> E '>'  E : op('$1', '$2', '$3').
E -> E '<'  E : op('$1', '$2', '$3').
E -> E '>=' E : op('$1', '$2', '$3').
E -> E '<=' E : op('$1', '$2', '$3').

%% Concatenation operator.
E -> E '&' E : op('$1', '$2', '$3').
    
%% Arithmetic operators.
E -> E '+' E : op('$1', '$2',  '$3').
E -> E '-' E : op('$1', '$2',  '$3').
E -> E '*' E : op('$1', '$2',  '$3').
E -> E '/' E : op('$1', {'/'}, '$3').
E -> E '^' E : op('$1', {'*'}, '$3').

%% Percent.
E -> E '%' : ['/', '$1', [int, 100]].

%% Unary minus and plus.
E -> Uminus     : '$1'.
Uminus -> '-' E : [negate, '$2'].

E -> Uplus     : '$1'.
Uplus -> '+' E : '$2'.

%% Ranges
%% Cell ranges (A1/RC in all sorts of weird combos as well).
E -> ref  ':' ref  : [':', '$1', '$3'].
%% Row and column ranges. TODO: this only handles same-page.
E -> name ':' name : [':', '$1', '$3'].
E -> int  ':' int  : [':', '$1', '$3'].
%% INDIRECT ranges, this allows more madness than Excel.
E -> Funcall ':' ref : [':', '$1', '$3'].
E -> ref ':' Funcall : [':', '$1', '$3'].

%% Parenthesized expressions.
E -> '(' E ')' : '$2'.

%% Plain literals or funcalls.
E -> Literal : '$1'.
E -> Funcall : '$1'.
    
%% Literals.
Literal -> int   : lit('$1').
Literal -> float : lit('$1').
Literal -> bool  : lit('$1').
Literal -> str   : lit('$1').
Literal -> ref   : lit('$1').
Literal -> name  : lit('$1').
Literal -> error : lit('$1').
Literal -> Array : '$1'.

%% Arrays: lists of rows, which are lists of values of allowed types.
Array -> '{' ArrayRows '}' : to_native_list('$2').

ArrayRows -> ArrayRow : [{row, '$1'}].
ArrayRows -> ArrayRow ';' ArrayRows : [{row, '$1'}] ++ '$3'.

ArrayRow -> ArrayLiteral : ['$1'].
ArrayRow -> ArrayLiteral ',' ArrayRow : ['$1'] ++ '$3'.

ArrayLiteral -> int   : lit('$1').
ArrayLiteral -> float : lit('$1').
ArrayLiteral -> bool  : lit('$1').
ArrayLiteral -> str   : lit('$1').    

%% Funcalls.
Funcall -> name '(' ')'      : [func_name('$1')].
Funcall -> name '(' Args ')' : func('$1', '$3').

Args -> E          : ['$1'].
Args -> E ',' Args : ['$1'] ++ '$3'.


Erlang code.

-include("handy_macros.hrl").

%% Make a function name for the AST from lexer tokens.
func_name({name, NameAsStr}) ->
    list_to_atom(NameAsStr).

lit({error, Data}) ->
    [error, Data];
lit({var, Data}) ->
    [var, Data];
lit({ref, R, C, P}) ->
    [ref, R, C, P];
lit({_Type, Data}) ->
    Data.

%% Make an op function call for the AST from lexer tokens.
op(Arg1, OpTuple, Arg2) ->
    [element(1, OpTuple), Arg1, Arg2].

%% Make a straight-up function call for the AST from a token and a list of args.
func(IdTuple, Args) ->
    [func_name(IdTuple)] ++ Args.

all_rows_eql(MyArray) ->
    RowLen = length(element(2, hd(MyArray))),
    all(fun(X) ->
                {row, Values} = X,
                length(Values) == RowLen
        end,
        MyArray).

%% Convert representation of array in AST into Erlang's native list-of-lists.
to_native_list(MyArray) ->
    case all_rows_eql(MyArray) of
        false ->
            throw(invalid_array);
        true ->
            %% Tail cos there'll be an extra [] in the list after the fold.
            tl(foldl(fun(Row, Acc) ->
                             {row, Elts} = Row,
                             Acc ++ [Elts]
                     end,
                     [[]], %% <== See, here it is.
                     MyArray))
    end.
