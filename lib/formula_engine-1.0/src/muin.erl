%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%% @doc Interface to the formula engine. Also, the interpreter.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
compile(Fla, {X, Y}) ->
    case (catch try_parse(Fla, {X, Y})) of
        {ok, Pcode} ->
            {ok, Pcode};
        Else ->
            error_logger:error_msg("muin:compile ~p~n",[Else]),
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
        Val ->
            {RefTree, Errors, References} = get(retvals),
            {ok, {?COND(Val == blank, 0, Val), %% Cells referencing blank cells become 0.
                  RefTree, Errors, References}}
    end.

%%%---------------------%%%
%%%  Private functions  %%%
%%%---------------------%%%

%% @doc Evaluates an s-expression, pre-processing subexps as needed.
eval([Func0 | Args0]) when ?isfuncall(Func0) ->
    [Func | Args] = preproc([Func0 | Args0]), %% transform sexp if needed
    CallArgs = [eval(X) || X <- Args], %% eval args
    funcall(Func, CallArgs);
eval(Value) ->
    Value.

%% @doc Same as eval() but doesn't preprocess.
plain_eval([Func | Args]) when ?isfuncall(Func) ->
    CallArgs = [plain_eval(X) || X <- Args],
    funcall(Func, CallArgs);
plain_eval(Value) ->
    Value.


%% @doc Transforms certain types of s-exps. @end
preproc([':', StartExpr, EndExpr]) ->
    %% Arguments to ':' calls are either tuples (literals) or lists (funcalls).
    %% Will fail later if funcalls aren't to INDIRECT, so not checking here.
    Eval = fun(Node) when is_list(Node) ->
                   Cellref = hd(plain_eval(tl(Node))),
                   {ok, [Ref]} = muin_lexer:lex(Cellref, {?mx, ?my}),
                   Ref;
              (Node) when is_tuple(Node) ->
                   Node
           end,

    [':', Eval(StartExpr), Eval(EndExpr)];
preproc([indirect, Arg]) ->
    Str = plain_eval(Arg),
    {ok, Toks} = muin_lexer:lex(Str, {?mx, ?my}),
    ?ifmatch(Toks,
             [{ref, R, C, P}],
             [ref, R, C, P],
             ?ERR_REF);
preproc(Sexp) ->
    Sexp.

%% Refs
funcall(ref, [Col, Row, Path]) ->
    Rowidx = toidx(Row),
    Colidx = toidx(Col),
    do_cell(Path, Rowidx, Colidx);

%% Cell ranges (A1:A5, R1C2:R2C10 etc).
%% In a range, the path of second ref **must** be ./
funcall(':', [{ref, Col1, Row1, Path1}, {ref, Col2, Row2, "./"}]) ->
    [Rowidx1, Colidx1, Rowidx2, Colidx2] = map(?Lx(toidx(X)),
                                               [Row1, Col1, Row2, Col2]),
    Cells = muin_util:expand_cellrange(Rowidx1, Rowidx2, Colidx1, Colidx2),
    map(fun({C, R}) -> do_cell(Path1, R, C) end,
        Cells);

%% TODO: Column & row ranges.

%% TODO: Names.
%% funcall(name, [Name, Path]) ->
%%     Addr = hn_db:get_ref_from_name(Name),
%%     Addr;

%% Hypernumber function and its shorthand.
funcall(hypernumber, [Url]) ->
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
        {'EXIT', {undef, _}} -> ?ERR_NAME;
        Val                  -> Val
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
