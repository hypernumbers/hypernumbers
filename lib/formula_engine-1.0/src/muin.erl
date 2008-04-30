%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%% @doc Interface to the formula engine. Also, the interpreter.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%% Formulas are currently compiled into simple s-exps.

-module(muin).
-export([compile/2, run/2]).

-include("spriki.hrl").
-include("builtins.hrl").
-include("handy_macros.hrl").
-include("typechecks.hrl").

-import(tconv, [to_b26/1, to_i/1, to_i/2, to_s/1]).

%% These are read-only.
-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).

%% Guard for eval() and plain_eval().
-define(isfuncall(X),
        is_atom(X) andalso X =/= true andalso X =/= false).

%%%--------------------%%%
%%%  Public functions  %%%
%%%--------------------%%%

%%% @doc Parses formula, and returns AST as an s-expression.
%%% @spec parse(Formula :: string()) -> {ok, {Ast :: list()}}
compile(Fla, {X, Y}) ->
    case (catch try_parse(Fla, {X, Y})) of
        {ok, Pcode} ->
            {ok, Pcode};
        _Else ->
            {error, error_in_formula}
    end.

try_parse(Fla, {X, Y}) ->
    Trans = translator:do(Fla),
    {ok, Toks} = muin_lexer:lex(Trans, {X, Y}),
    {ok, _Ast} = muin_parser:parse(Toks). % Match to enforce the contract.

%% @doc Runs compiled formula.
run(Pcode, Bindings) ->
    %% Populate the process dictionary.
    foreach(fun({K, V}) -> put(K, V) end,
            Bindings ++ [{retvals, {[], [], []}}]),
             
    case (catch eval(Pcode)) of
        {error, R}  -> {error, R};
        {'EXIT', R} -> throw(R); 
        Value ->
            {RefTree, Errors, References} = get(retvals),
            {ok, {Value, RefTree, Errors, References}}
    end.

%%%---------------------%%%
%%%  Private functions  %%%
%%%---------------------%%%

%% @doc Evaluates an s-expression, pre-processing subexps as needed.
eval([Fun__ | Args__]) when ?isfuncall(Fun__) ->
    %% Transform s-exp if needed.
    [Fun | Args] = preproc([Fun__ | Args__]),
    %% Evaluate args.
    CallArgs = [eval(X) || X <- Args],
    %% Call the function.
    funcall(Fun, CallArgs);
eval(Value) ->
    Value.


%% @doc Transforms certain types of s-exps. @end

%% Arguments to ':' calls are either tuples (literals) or lists (funcalls).
%% Not checking if funcalls are to INDIRECT as it'll fail later if they aren't.
%% FIXME: This doesn't work anymore with the new ref format.
preproc([':', StartExpr, EndExpr]) ->
    Eval = fun(Expr) when is_list(Expr) ->
                   Cellref = hd(plain_eval(tl(Expr))),
                   {sscellref, "./" ++ Cellref};
              (Expr) when is_tuple(Expr) ->
                   Expr
           end,

    [':', Eval(StartExpr), Eval(EndExpr)];

%% This clause is for standalone calls to INDIRECT. Ones that are part of
%% a range construction expression will be caught by the clause above.
%% FIXME: As the clause above.
preproc([indirect, Arg]) ->
    RefStr = plain_eval(Arg),
    {ok, Tokens, _} = muin_lexer:string(RefStr),% Yeh, screw the frontends.
                                                % FIXME: ^^^

    case Tokens of
        [{cellref, Ref}] -> % A1
            [sscellref, "./" ++ Ref];
        [{sscellref, Ref}] -> % ../A1
            [sscellref, Ref];
        [{cellref, Ref1}, {colon}, {cellref, Ref2}] -> % A1:B2
            [':', {sscellref, "./" ++ Ref1}, {cellref, Ref2}];
        [{sscellref, Ref1}, {colon}, {cellref, Ref2}] -> % ../A1:B2
            [':', {sscellref, Ref1}, {cellref, Ref2}];
        [{integer, Row1}, {colon}, {integer, Row2}] -> % 1:2
            [':', {ssrowref, "" ++ to_s(Row1)}, {row, to_s(Row2)}];
        [{ssrowref, Row1}, {colon}, {integer, Row2}] -> % ../1:2
            [':', {ssrowref, Row1}, {row, to_s(Row2)}];
        [{id, Col1}, {colon}, {id, Col2}] -> % A:B
            [':', {sscolref, "./" ++ Col1}, {col, Col2}];
        [{sscolref, Col1}, {colon}, {id, Col2}] -> % ../A:B
            [':', {sscolref, Col1}, {col, Col2}];
        _ -> % FOAD
            throw({error, ref})
    end;

%% Anything else -- don't need to transform.
preproc(Sexp) ->
    Sexp.


%% @doc Same as eval() but doesn't pre-process.
plain_eval([Fun | Args]) when ?isfuncall(Fun) ->
    CallArgs = [plain_eval(X) || X <- Args],
    funcall(Fun, CallArgs);
plain_eval(Value) ->
    Value.

%% Refs
funcall(ref, [Col, Row, Path]) ->
    Rowidx = toidx(Row),
    Colidx = toidx(Col),
    do_cell(Path, Rowidx, Colidx);

%% Cell ranges (A1:A5, R1C2:R2C10 etc).
%% In a range, the path of second ref **must** be ./
funcall(':', [{ref, Col1, Row1, Path1}, {ref, Col2, Row2, "./"}]) ->
    Rowidx1 = toidx(Row1),
    Colidx1 = toidx(Col1),
    Rowidx2 = toidx(Row2),
    Colidx2 = toidx(Col2),
    Cells = muin_util:expand_cellrange(Rowidx1, Rowidx2, Colidx1, Colidx2),
    map(fun({C, R}) -> do_cell(Path1, R, C) end,
        Cells);

%% TODO: Column & row ranges.

%% Hypernumber function and its shorthand.

funcall(hypernumber, [Url_]) ->
    %% Remove trailing ?hypernumber if needed.
    {ok, Url, _} = regexp:gsub(Url_, "\\?hypernumber$", ""),
    #page{site = RSite, path = RPath,
          ref = {cell, {RX, RY}}} = hn_util:parse_url(Url),
    F = ?L(hn_main:get_hypernumber(?msite, ?mpath, ?mx, ?my,
                                   Url, RSite, RPath, RX, RY)),
    get_value_and_link(F);

funcall(hn, [Url]) ->
    funcall(hypernumber, [Url]);

%% Function call, built-in or user-defined.
funcall(Fname, Args) ->
    case keysearch(Fname, 1, ?STDFUNS) of
        {value, {Fname, Modname}} ->
            Modname:Fname(Args);
        false ->
            userdef_call(Fname, Args)
    end.

%%% Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% Try the userdef module first, then Ruby, Gnumeric, R, whatever.
userdef_call(Fname, Args) ->
    case (catch userdef:Fname(Args)) of
        {'EXIT', {undef, _}} -> % Not there, try Ruby.
                    ?ERR_NAM
    end.

%% Returns value in the cell + get_value_and_link() is called behind the
%% scenes.
do_cell(RelPath, Rowidx, Colidx) ->
    ?IF(Colidx == ?mx andalso Rowidx == ?my,
        throw({error, self_reference})),
    Path = muin_util:walk_path(?mpath, RelPath),
    FetchFun = ?L(hn_main:get_cell_info(?msite, Path, Colidx, Rowidx)),
    get_value_and_link(FetchFun).

%% @doc Calls supplied fun to get a cell's value and dependence information,
%% saves the dependencies (linking it to current cell), and returns
%% the value to the caller (to continue the evaluation of the formula).
get_value_and_link(FetchFun) ->
    {Value, RefTree, Errs, Refs} = FetchFun(),

    ?IF(member({?msite, ?mpath, ?mx, ?my}, RefTree),
        throw({error, circular_reference})),

    {RefTree0, Errs0, Refs0} = get(retvals),
    put(retvals, {RefTree0 ++ RefTree, Errs0 ++ Errs, Refs0 ++ Refs}),
    Value.

%% Row or Col information --> index.
toidx(N) when is_number(N) ->
    N;
toidx({row, Offset}) ->
    ?my + Offset;
toidx({col, Offset}) ->
    ?mx + Offset.
