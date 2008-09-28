%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr

.

Terminals

string
condition
integer
float

.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> string condition integer  : make_col_cond('$1', '$2', '$3').
Expr -> string condition float    : make_col_cond('$1', '$2', '$3').
Expr -> integer condition integer : make_row_cond('$1', '$2', '$3').
Expr -> integer condition float   : make_row_cond('$1', '$2', '$3').

Erlang code.

%% Erlang code follows here
make_row_cond({integer,Row},{condition,Cond},{_,Val}) -> 
	{row,tconv:to_num(Row),Cond,tconv:to_num(Val)}.
make_col_cond({string,Col},{condition,Cond},{_,Val})  -> 
	{col,Col,Cond,tconv:to_num(Val)}.