%%% @doc Muin parser definition. The parser produces an s-expression.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

Nonterminals

Formula E Uminus Uplus Literal Funcall Args
Intersection
ArrayLiteral ArrayRow ArrayRows Array
Range CellRange ColRange RowRange
.

Terminals

%% Stuff!
id var

%% Operators.
eq neq gt lt gte lte
plus minus times slash caret
concat percent
colon

%% Basic types.
integer float boolean string date

%% References.
cellref sscellref sscolref ssrowref
intersection
rcref ssrcref

%% Errors
error

%% Punctuation.
open_paren close_paren comma
open_curly close_curly semicolon
.

Rootsymbol Formula.
Endsymbol  '$end'.


%% ----- Associativity and precedence rules for operators.

Unary    50  Uplus.
Left     100 eq neq gt lt gte lte.
Left     200 concat.              
Left     300 plus minus.
Left     400 times slash.
Left     500 caret.
Left     600 percent.
Unary    700 Uminus.


%% ----- Grammar definition.

Formula -> eq E : '$2'.

%% Comparison operators.
E -> E eq  E : op('$1', '$2', '$3').
E -> E neq E : op('$1', '$2', '$3').
E -> E gt  E : op('$1', '$2', '$3').
E -> E lt  E : op('$1', '$2', '$3').
E -> E gte E : op('$1', '$2', '$3').
E -> E lte E : op('$1', '$2', '$3').

%% Concatenation operator.
E -> E concat E : op('$1', '$2', '$3').
    
%% Arithmetic operators.
E -> E plus  E : op('$1', '$2', '$3').
E -> E minus E : op('$1', '$2', '$3').
E -> E times E : op('$1', '$2', '$3').
E -> E slash E : op('$1', {divide}, '$3').
E -> E caret E : op('$1', {power}, '$3').

%% Percent.
E -> E percent : [divide, '$1', [integer, 100]].

%% Unary minus and plus.
E -> Uminus       : '$1'.
Uminus -> minus E : [negate, '$2'].

E -> Uplus        : '$1'.
Uplus -> plus E   : '$2'.
    
%% Parenthesized expression.
E -> open_paren E close_paren : '$2'.

%% Plain literals or funcalls.
E -> Literal : '$1'.
E -> Funcall : '$1'.
    
%% Literals.
Literal -> integer    : lit('$1').
Literal -> float      : lit('$1').
Literal -> boolean    : lit('$1').
Literal -> string     : lit('$1').
Literal -> date       : lit('$1').
Literal -> cellref    : lit('$1').
Literal -> sscellref  : lit('$1').
Literal -> rcref      : lit('$1').
Literal -> ssrcref    : lit('$1').
Literal -> var        : lit('$1').
Literal -> error      : lit('$1').
Literal -> Array      : '$1'.


%% Arrays: lists of rows, which are lists of values of certain allowed types.
Array -> open_curly ArrayRows close_curly : to_native_list('$2').

ArrayRows -> ArrayRow : [{row, '$1'}].
ArrayRows -> ArrayRow semicolon ArrayRows : [{row, '$1'}] ++ '$3'.

ArrayRow -> ArrayLiteral : ['$1'].
ArrayRow -> ArrayLiteral comma ArrayRow : ['$1'] ++ '$3'.

ArrayLiteral -> integer : lit('$1').
ArrayLiteral -> float   : lit('$1').
ArrayLiteral -> boolean : lit('$1').
ArrayLiteral -> string  : lit('$1').    

%% Funcalls.
Funcall -> id open_paren close_paren           : [func_name('$1')].
Funcall -> id open_paren Args close_paren      : func('$1', '$3').

Args -> E            : ['$1'].
Args -> Intersection : '$1'.
Args -> E comma Args : ['$1'] ++ '$3'.
Args -> Intersection comma Args : ['$1'] ++ '$3'.
Args -> Range : ['$1'].
Args -> Range comma Args : ['$1'] ++ '$3'.

%% Kept here for future.
%% TODO: Move to parser/rt.
Intersection -> intersection : make_intersection('$1').

%% Ranges. Lions and tigers and bears, oh my!
Range -> CellRange : '$1'.
Range -> ColRange  : '$1'.
Range -> RowRange  : '$1'.
    
%% Cell ranges.
CellRange -> sscellref colon cellref : [':', '$1', '$3'].
CellRange -> cellref   colon cellref : % Special case of the previous rule.
                 [':', {sscellref, "./" ++ element(2, '$1')}, '$3'].
CellRange -> Funcall   colon Funcall : [':', '$1', '$3'].
CellRange -> Funcall   colon cellref : [':', '$1', '$3'].
CellRange -> Funcall   colon sscellref : [':', '$1', '$3'].
CellRange -> sscellref colon Funcall : [':', '$1', '$3'].
CellRange -> cellref   colon Funcall   : [':', '$1', '$3'].

%% Column ranges.
ColRange -> sscolref colon id      : [':', '$1', col('$3')].
ColRange -> id colon id            : % Special case of the previous rule.
                [':', {sscolref, "./" ++ element(2, '$1')}, col('$3')].
ColRange -> Funcall colon id       : [':', '$1', col('$3')].
ColRange -> id colon Funcall       : [':', col('$1'), '$3'].
ColRange -> sscolref colon Funcall : [':', '$1', '$3'].

%% Row ranges.
RowRange -> ssrowref colon integer : [':', '$1', row('$3')].
RowRange -> integer colon integer  : % Special case again.
                [':', {ssrowref, "./" ++ integer_to_list(element(2, '$1'))}, row('$3')].
RowRange -> Funcall colon integer  : [':', '$1', row('$3')].
RowRange -> integer colon Funcall  : [':', row('$1'), '$3'].
RowRange -> ssrowref colon Funcall : [':', '$1', '$3'].


%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

-import(lists, [all/2, filter/2, foldl/3, last/1, map/2, member/2]).

%% Make a function name for the AST from lexer tokens.
func_name({id, NameAsStr}) ->
    list_to_atom(NameAsStr).


%% TODO: Worth keeping?
lit({cellref, Data}) ->
    [sscellref, "./" ++ Data];

lit({sscellref, Data}) ->
    [sscellref, Data];

lit({error, Data}) ->
    [error, Data];

lit({var, Data}) ->
    [var, Data];

%% For fixed RC refs we can get the row/col straightaway and just pass it on to
%% the interpreter.
lit({rcref, Data}) ->
    lit({ssrcref, "./" ++ Data});

lit({ssrcref, Data}) ->
    Path = muin_util:just_path(Data),
    Ref = last(string:tokens(Data, "/")),

    {match, [{St1, Len1}, {St2, Len2}]} = regexp:matches(Ref, "([0-9]+)"),
    Row = tconv:to_i(string:substr(Ref, St1, Len1)),
    Col = tconv:to_i(string:substr(Ref, St2, Len2)),

    [ssrcref, {Path, Row, Col}];

lit({_Type, Data}) ->
    Data.


col({id, Data}) ->
    {col, Data}.


row({integer, Data}) ->
    {row, integer_to_list(Data)}. %% Makes things more straightforward in the interpreter.


%% Make an op function call for the AST from lexer tokens.
op(Arg1, OpTuple, Arg2) ->
    [element(1, OpTuple), Arg1, Arg2].


%% Make a straight-up function call for the AST from a token and a list of args.
func(IdTuple, Args) ->
    [func_name(IdTuple)] ++ Args.


make_intersection({intersection, YYtext}) ->
    {ok, Tmp, _} = regexp:gsub(YYtext, "(\s)+", " "), % Normalize whitespace.
    Ranges = map(fun string:to_lower/1, % lowercase
                 string:tokens(Tmp, " ")), % split into ranges
    Cells = map(fun muin_util:expand_cellrange/1, Ranges),

    %% Calculate intersections pairwise. At each step, Acc is the intersection
    %% of two previous ranges.
    Intersection = foldl(fun(L, Acc) ->
                                 filter(fun(X) ->
                                                member(X, Acc)
                                        end,
                                        L)
                         end,
                         hd(Cells),
                         tl(Cells)),
    
    map(fun(X) ->
                [sscellref, {"./" ++ X}]
        end,
        Intersection).
                      

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
