%%%-----------------------------------------------------------------------------
%%% @doc    Parser for Lisp frontend for Muin.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @end
%%%
%%% Useful snippets:
%%%   * Parse = fun(Str) -> {ok, Tokens, _} = lisp_lexer:string(Str), lisp_parser:parse(Tokens) end.
%%%-----------------------------------------------------------------------------

Nonterminals
    formula
    expr
    funcall
    funname
    arg
    args
    rem_args
    literal
    special_function
    .

Terminals
    id

    %% --- Literals
    %% Data types
    integer
    float
    float_sci
    boolean
    string

    %% References
    cellref

    %%% --- Special functions
    %%% Arithmetic
    plus
    minus
    times
    divide
    power
    
    %%% Logical
    gt
    lt
    gte
    lte
    neq
  
    %% --- Punctuation.
    open_paren
    close_paren
    eq
    .

Rootsymbol formula.
Endsymbol '$end'.

%% ===== Fundamental grammar =====

%% Formula is an equals sign followed by an expression.
formula ->
    eq expr :
        %%muin_util:fdbg('$2', "Final expr"),
        {formula, hd(lists:append(['$2']))}.

%% Expression is either a function call or a literal.
expr ->
    funcall :
        '$1'.
expr ->
    literal :
        [get_literal('$1')]. % Without [], we'd have something like "integer", {"1"} in the AST.

%% Function call is something wrapped in parentheses.
funcall -> 
    open_paren funname args close_paren :
        [function_node('$2', '$3')]. % See comment for expr -> literal above.

funname ->
    id :
        X = element(2, '$1'), % Extract function name from the {id, "blabla"} token tuple.
        %%muin_util:fdbg(X, "X"),
        X.

funname ->
    special_function :
        Y = atom_to_list(element(1, '$1')), % Same as above, except the token is like {plus}.
        %%muin_util:fdbg(Y, "Y"),
        Y.

%% Arguments are an empty list, one argument, or many arguments.
args ->
    open_paren close_paren : % No arguments.
        [].
args ->
    arg rem_args :
        case '$2' of
            [] ->
                '$1';
            _  ->
                lists:append(['$1', '$2'])
        end.
       

rem_args ->
    arg rem_args :
        case '$2' of
            [] ->
                '$1';
            _  ->
                lists:append(['$1', '$2'])
        end.
rem_args ->
    '$empty' :
        [].
        

%% An argument is an expression, i.e. a literal or a function call.
arg ->
    expr :
        '$1'.

%% ===== Special functions =====

special_function -> plus   : '$1'.
special_function -> minus  : '$1'.
special_function -> times  : '$1'.
special_function -> divide : '$1'.
special_function -> power  : '$1'.
special_function -> eq     : '$1'.
special_function -> gt     : '$1'.
special_function -> lt     : '$1'.
special_function -> gte    : '$1'.
special_function -> lte    : '$1'.
special_function -> neq    : '$1'.


%% ===== Literals =====

literal -> integer   : '$1'.
literal -> float     : '$1'.
literal -> float_sci : '$1'.
literal -> string    : '$1'.
literal -> boolean   : '$1'.
literal -> cellref   : '$1'.


Erlang code.

%% Some functions require special treatment.
%% For example the tree for this code:
%%     (- 10 1 2 3) 
%% needs to be transformed to the tree for this (equivalent) code:
%%     (- (- (- 10 1) 2) 3)
%% This is because spriki_funs:minus() takes exactly two arguments.
%% Etc etc.
function_node("minus", Args) ->
    multiarg_to_biarg("minus", Args);

function_node("times", Args) ->
    multiarg_to_biarg("times", Args);

function_node("divide", Args) ->
    multiarg_to_biarg("divide", Args);

function_node("plus", Args) ->
    function_node("sum", Args); % Just make it into a call to SUM.

function_node(FunName, Args) ->
    %%io:format("function_node called with~n    ~s~n    ", [FunName]),
    %%erlang:display(Args),
    
    lists:foldl(
      fun(X, Acc) ->
              lists:append([Acc, [X]])
      end,
      [string:to_lower(FunName)],
      Args
      ).

%% This name sucks.
multiarg_to_biarg(FunName, Args) ->
    lists:foldl(
      fun(X, Acc) ->
              [FunName, Acc, X]
      end,
      hd(Args),
      tl(Args)
     ).

get_literal({Type, Data}) ->
    %%muin_util:fdbg("In get_literal"),
    [atom_to_list(Type), {Data}].
