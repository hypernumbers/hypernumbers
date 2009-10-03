%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
Cell
Conditional
Stringcond

.

Terminals

string
greaterthan
lessthan
gtorequal
ltorequal
equals
notequals

integer
float

.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> Cell : '$1'.

Cell -> string integer : make_cell('$1','$2').

Expr -> Cell Conditional integer    : make_cell_cond('$1', '$2', '$3').
Expr -> Cell Stringcond string      : make_cell_cond2('$1', '$2', '$3') .

Expr -> string  Conditional integer : make_col_cond('$1', '$2', '$3').
Expr -> string  Conditional float   : make_col_cond('$1', '$2', '$3').
Expr -> integer Conditional integer : make_row_cond('$1', '$2', '$3').
Expr -> integer Conditional float   : make_row_cond('$1', '$2', '$3').

Conditional -> greaterthan : '$1' .
Conditional -> lessthan    : '$1' .
Conditional -> gtorequal   : '$1' .
Conditional -> ltorequal   : '$1' .
Conditional -> Stringcond  : '$1' .

Stringcond -> equals      : '$1' .
Stringcond -> notequals   : '$1' .


Erlang code.

%% Erlang code follows here
make_cell({string,Col},{integer,Row})->
    {cell,{tconv:b26_to_i(Col),tconv:to_i(Row)}}.

make_cell_cond({cell,Cell},{_,Cond},{_,Val})->
    {{cell,Cell},Cond,tconv:to_num(Val)}.

make_cell_cond2({cell,Cell},{_,Cond},{_,Val})->
    {{cell,Cell},Cond,Val}.

make_row_cond({integer,Row},{_,Cond},{_,Val}) -> 
    {row,tconv:to_num(Row),Cond,tconv:to_num(Val)}.
make_col_cond({string,Col},{_,Cond},{_,Val})  -> 
    {col,Col,Cond,tconv:to_num(Val)}.