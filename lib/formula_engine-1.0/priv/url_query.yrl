%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
Conditional
Stringcond
Query

.

Terminals

query
string
integer
float
open_bra
close_ket
open_curly
close_curl
open_sq
close_sq
greaterthan
lessthan
gtorequal
ltorequal
equals
notequals
semicolon
comma

.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> open_sq Query close_sq : '$2'.

Query -> Conditional integer : make_clause('$1', '$2') .
Query -> Stringcond string   : make_clause('$1', '$2') .

Conditional -> greaterthan : '$1' .
Conditional -> lessthan    : '$1' .
Conditional -> gtorequal   : '$1' .
Conditional -> ltorequal   : '$1' .
Conditional -> Stringcond  : '$1' .

Stringcond -> equals      : '$1' .
Stringcond -> notequals   : '$1' .


Erlang code.
-include("handy_macros.hrl").
%% Erlang code follows here
make_clause({_,Cond},{integer,Value}) -> 
    {Cond,?MS(get(matchspec)),list_to_integer(Value)};
make_clause({_,Cond},{string,Value}) -> 
    {Cond,?MS(get(matchspec)),Value}.
