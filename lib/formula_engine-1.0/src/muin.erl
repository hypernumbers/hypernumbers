%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Interface to the formula engine and the interpreter.
%% TODO: Need a generic apply(Node, Fun), or better yet real macros.

-module(muin).
-export([run_formula/2, run_code/2]).
-export([eval/1]).

-compile(export_all).

-include("spriki.hrl").
-include("builtins.hrl").
-include("handy_macros.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-import(tconv, [to_b26/1, to_i/1, to_i/2, to_s/1]).
-import(muin_util, [attempt/3]).

-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).
-define(array_context, get(array_context)).

-define(is_fn(X),      % Is atom a function name?
        is_atom(X) andalso X =/= true andalso X =/= false).
-define(is_funcall(X), % Is list a function call?
        ?is_list(X) andalso ?is_fn(hd(X))).

-define(error_in_formula, {error, error_in_formula}).
-define(syntax_error, {error, syntax_error}).

-define(puts, io:format).

%%% PUBLIC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @doc Runs formula given as a string.
run_formula(Fla, Rti = #muin_rti{col = Col, row = Row}) ->
    case compile(Fla, {Col, Row}) of
        {ok, Ecode}       -> muin:run_code(Ecode, Rti);
        ?error_in_formula -> ?error_in_formula
    end.

%% @doc Runs compiled formula.
run_code(Pcode, #muin_rti{site=Site, path=Path, 
                          col=Col,   row=Row, 
                          array_context=AryCtx}) ->

    %% Populate the process dictionary.
    map(fun({K,V}) -> put(K, V) end,
        [{site, Site}, {path, Path}, {x, Col}, {y, Row}, 
         {array_context, AryCtx},
         {retvals, {[], [], []}}, {recompile, false}]),
    Fcode = ?COND(?array_context, loopify(Pcode), Pcode),
    Ev = eval(Fcode),
    {RefTree, _Errors, References} = get(retvals),
    case Ev of
        ?error_in_formula ->
            ?error_in_formula;
        _ ->
            Ev2 = ?COND(Ev == blank, 0, Ev),
            {ok, {Fcode, Ev2, RefTree, References, get(recompile)}}
    end.

%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @spec compile(Fla, {Col, Row}) -> something Fla = string(), 
%% Col = integer(), Row = integer()
%% @doc Compiles a formula against a cell.
compile(Fla, {Col, Row}) ->
    case parse(Fla, {Col, Row}) of
        {ok, Pcode}   -> {ok, Pcode};
        ?syntax_error -> ?error_in_formula
    end.

%% Formula -> sexp, relative to coord.
parse(Fla, {Col, Row}) ->
    Trans = translator:do(Fla),
    {ok, Toks} = xfl_lexer:lex(Trans, {Col, Row}),
    case catch(xfl_parser:parse(Toks)) of
        {ok, Ast} -> {ok, Ast};
        _         -> ?syntax_error
    end.

%% Evaluate a form in the current rti context.
eval(Node = [Func|Args]) when ?is_fn(Func) ->
    case preproc(Node) of
        false ->
            case member(Func, ['if', choose, column]) of
                true  -> call(Func, Args);
                false -> call(Func, [eval(X) || X <- Args])
            end;
        {reeval, Newnode} ->
            eval(Newnode);
        [Func2|Args2] ->
            CallArgs = [eval(X) || X <- Args2],
            call(Func2, CallArgs)
    end;
eval(Value) ->
    Value.

call(Func, Args) ->
    R = attempt(?MODULE, funcall, [Func, Args]),
    case R of
        {error, Errv = {errval, _}} -> Errv;
        {error, _E}                 -> ?error_in_formula;
        {ok, V}                     -> V
    end.

%% @doc Same as eval() but doesn't preprocess.
plain_eval([Func | Args]) when ?is_fn(Func) ->
    CallArgs = [plain_eval(X) || X <- Args],
    funcall(Func, CallArgs);
plain_eval(Value) ->
    Value.

%% @doc Used for direct manipulations on the AST (think Lisp macros).
%% E.g. OFFSET is done here because it needs to splice results directly back
%% into the AST.
%% Returns false if the node didn't need to be transformed.

preproc(['let', NameNode, ValueNode, BodyNode]) ->
    Value = eval(ValueNode),
    {reeval, let_transform(NameNode, BodyNode, Value)};
%% Ranges constructed with INDIRECT. No need for an explicit check, because
%% if either argument evaluates to something other than a ref, funcall clause
%% for ':' will fail at the next step.
preproc([':', StartExpr, EndExpr]) ->
    Eval = fun(Node) when is_list(Node) -> % Funcall
                   Cellref = hd(plain_eval(tl(Node))),
                   {ok, [Ref]} = xfl_lexer:lex(Cellref, {?mx, ?my}),
                   Ref;
            (Node) when is_tuple(Node) -> % Literal
                   Node
           end,

    R = [':', Eval(StartExpr), Eval(EndExpr)],
    R;
preproc([indirect, Arg]) ->
    case ?is_string(Arg) of
        true ->
            Str = plain_eval(Arg),
            {ok, Toks} = xfl_lexer:lex(Str, {?mx, ?my}),
            case Toks of
                [{ref, R, C, P, _}] ->
                    put(recompile, true),
                    [ref, R, C, P];
                _ ->
                    {reeval, ?ERRVAL_REF}
            end;
        false ->
            {reeval, ?ERRVAL_VAL}
    end;
%% OFFSET(Range, Rows, Cols) -- what if range is constructed with INDIRECT though...?
preproc([offset, _Base = [':', {ref, R1, C1, P}, {ref, R2, C2, _}], Rows, Cols]) ->
    H = toidx(C2)-toidx(C1)+1,
    W = toidx(R2)-toidx(R1)+1,
    preproc([offset, [ref, R1, C1, P], Rows, Cols, H, W]);
%% OFFSET(Cellref, Rows, Cols)
preproc([offset, Base, Rows, Cols]) ->
    preproc([offset, Base, Rows, Cols, 1, 1]);
preproc([offset, _Base = [ref, C, R, P], Rows, Cols, H, W]) ->
    RULES = [ban_strings, ban_bools, ban_dates, ban_blanks],
    % eval expressions, then try to cast them.
    [Rows2, Cols2, H2, W2] = ?numbers(map(fun muin:eval/1,
                                          [Rows, Cols, H, W]), RULES),

    Dr = toidx(R)+Rows2,
    Dc = toidx(C)+Cols2,

    Res =
        if
            Dr < 1 -> ?ERRVAL_REF;
            Dc < 1 -> ?ERRVAL_REF;
            H2 < 1 -> ?ERRVAL_VAL;
            W2 < 1 -> ?ERRVAL_VAL;
            true ->
                if H2 == 1 andalso W2 == 1 ->
                        [ref, Dc, Dr, P];
                   true ->
                        [':', {ref, Dc, Dr, P, ""}, {ref, Dc+W2, Dr+H2, P, ""}]
                end
        end,
    
    {reeval, Res};
preproc(_Node) -> false.

funcall(choose, [A|Vs]) when ?is_area(A) ->
    Flatvs = muin_collect:flatten_arrays([A]),
    map(fun(X) -> funcall(choose, [X|Vs]) end, Flatvs);
funcall(choose, [V|Vs]) ->
    Idx = ?number(V, [cast_strings, cast_bools, ban_dates, ban_blanks]),
    ?ensure(Idx > 0 andalso Idx =< length(Vs), ?ERR_VAL),
    eval(nth(Idx, Vs));

funcall(column, [])                     -> ?mx;
funcall(column, [[ref, Col, _Row, _]]) -> toidx(Col);
funcall(column, _)                     -> ?ERR_VAL;

funcall(make_list, Args) ->
    area_util:make_array([Args]); % horizontal array

%% Refs
funcall(ref, [Col, Row, Path]) ->
    Rowidx = toidx(Row),
    Colidx = toidx(Col),
    do_cell(Path, Rowidx, Colidx);
%% Cell ranges (A1:A5, R1C2:R2C10 etc).
%% In a range, the path of second ref **must** be ./
funcall(':', [{ref, Col1, Row1, Path1, _}, {ref, Col2, Row2, "./", _}]) ->
    [Rowidx1, Colidx1, Rowidx2, Colidx2] = map(fun(X) -> toidx(X) end,
                                               [Row1, Col1, Row2, Col2]),
    CellCoords = muin_util:expand_cellrange(Rowidx1, Rowidx2, Colidx1, Colidx2),
    Revrows = foldl(fun(X, Acc) -> % Curr row, result rows
                            RowCoords = filter(fun({_, R}) -> R == X end,
                                               CellCoords),
                            Row = map(fun({C, R}) -> do_cell(Path1, R, C) end,
                                      RowCoords),
                            [Row|Acc]
                    end,
                    [],
                    seq(Rowidx1, Rowidx2)),
    Resrange = {range, reverse(Revrows)},
    Resrange;

%% column & row ranges. the tokens come straight from the lexer.

funcall(':', [{ssatomref, _Ssa}, {atom, _A}]) ->
    ok; % TODO: column range

funcall(':', [{ssnumref, _Snr}, {atom, _A}]) ->
    ok; % TODO: row range

funcall(name, [Name, Path]) ->
    Refs = hn_db:get_ref_from_name(Name),
    NeedPath = muin_util:walk_path(?mpath, Path),
    case filter(fun(#ref{path = P}) -> NeedPath == P end, Refs) of
        [Ref] ->
            #ref{site = Site, path = Path2, ref = {cell, {Col, Row}}} = Ref,
            FetchFun = ?L(hn_main:get_cell_info(Site, Path2, Col, Row)),
            get_value_and_link(FetchFun);
        _ ->
            ?ERR_NAME
    end;

%% Hypernumber function and its shorthand.
funcall(hypernumber, [Url]) ->
    {ok,#ref{site = RSite, path = RPath,
             ref = {cell, {RX, RY}}}} = hn_util:parse_url(Url),
    F = fun() -> hn_main:get_hypernumber(?msite, ?mpath, ?mx, ?my, Url, RSite, RPath, RX, RY) end,
    get_value_and_link(F);

funcall(hn, [Url]) ->
    funcall(hypernumber, [Url]);

funcall(loop, [A, Fn]) when ?is_area(A) ->
    R = area_util:apply_each(fun(L) when is_list(L) -> % paired up
                                 funcall(Fn, L);
                            (X) -> % single value from array
                                 case Fn of
                                     {Lst} when is_list(Lst) -> % fn spec is reversed
                                         [Func|Revargs] = Lst,
                                         funcall(Func, [X|reverse(Revargs)]);
                                     _ -> % spec is atom (should probably be done on length)
                                         funcall(Fn, [X])
                                 end
                         end,
                         A),
    R;

funcall(pair_up, [A, B]) when ?is_area(A) andalso ?is_area(B) ->
    area_util:apply_each_with_pos(fun({X, {C, R}}) ->
                                          case area_util:at(C, R, B) of
                                              {ok, V}    -> [X, V];
                                              {error, _} -> ?ERRVAL_NA
                                          end
                                  end,
                                  A);
funcall(pair_up, [A, V]) when ?is_area(A) andalso not(?is_area(V)) ->
    Ev = eval(V),
    ?COND(?is_area(Ev),
          funcall(pair_up, [A, Ev]),
          area_util:apply_each(fun(X) -> [X, Ev] end, A));
funcall(pair_up, [V, A]) when ?is_area(A) andalso not(?is_area(V)) ->
    funcall(pair_up, [A, V]);

%% Formula function call (built-in or user-defined).
funcall(Fname, Args) ->
    R = foldl(fun(M, Acc = {F, A, not_found_yet}) ->
                      io:format("in muin:funcall M is ~p F is ~p A is ~p~n",
                                [M, F, A]),
                      case attempt(M, F, [A]) of
                          {error, undef} -> Acc;
                          {ok, V}        -> {F, A, V};
                          {error, Ev = {errval, _}} -> {F, A, Ev}
                      end;
               (_, Acc) ->
                      Acc
              end,
              {Fname, Args, not_found_yet},
              [stdfuns_math, stdfuns_stats, stdfuns_date, stdfuns_financial,
               stdfuns_info, stdfuns_lookup_ref, stdfuns_eng, stdfuns_gg,
               stdfuns_logical, stdfuns_text, stdfuns_db]),
    
    case R of
        {_, _, not_found_yet} -> userdef_call(Fname, Args);
        {_, _, V}             -> V
    end.

%%% Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% TODO: Beef up.
%% TODO: what's the real type of ':'? (vararg works for now).
fntype(Fn) ->
    ?COND(member(Fn, [transpose, mmult, munit, frequency]), matrix,
          ?COND(member(Fn, [sum, count, ':']),              vararg,
                                                            other)).

is_binop(X) ->
    member(X, ['>', '<', '=', '>=', '<=', '<>', '+', '*', '-', '/', '^']).

loopify(Node = [loop|_]) -> Node;
loopify(Node = [Fn|_]) when ?is_fn(Fn) ->
    case fntype(Fn) of
        matrix -> Node;
        vararg -> Node;
        _      -> loop_transform(Node)
    end;
loopify(Literal) -> Literal.

loop_transform([Fn|Args]) when ?is_fn(Fn) ->
    ArgsProcd = map(fun(X) -> loopify(X) end, Args),
    case is_binop(Fn) of
        true -> % operator -- no reversing.
            [A1|A2] = ArgsProcd,
            [loop] ++ [[pair_up] ++ [A1|A2]] ++ [Fn];
        false ->
            case length(ArgsProcd) of
                1 -> % ABS, SQRT &c
                    [loop] ++ ArgsProcd ++ [Fn];
                _ -> % IF, CONCATENATE &c (NOT binops)
                    [Area|Rst] = ArgsProcd,
                    [loop] ++ [Area] ++ [{[Fn|reverse(Rst)]}] % to wrap Area or not may depend on if it's node or literal?
                                                              % wrap in {} to prevent from being eval'd -- need a proper '
            end
    end.

%% Try the userdef module first, then Ruby, Gnumeric, R, whatever.
userdef_call(Fname, Args) ->
    % changed to apply because using the construction userdef:Fname failed
    % to work after hot code load (Gordon Guthrie 2008_09_08)
    case (catch apply(userdef,Fname,Args)) of
        {'EXIT', {undef, _}} -> ?ERR_NAME;
        Val                  -> Val
    end.

%% Returns value in the cell + get_value_and_link() is called behind the
%% scenes.
do_cell(RelPath, Rowidx, Colidx) ->
    Path = muin_util:walk_path(?mpath, RelPath),
    IsCircRef = (Colidx == ?mx andalso Rowidx == ?my andalso Path == ?mpath),
    ?IF(IsCircRef, ?ERR_CIRCREF),
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
toidx(N) when is_number(N) -> N;
toidx({row, Offset})       -> ?my + Offset;
toidx({col, Offset})       -> ?mx + Offset.

%% @doc AST transformation for LET function.

let_transform([name, N, P], [name, N, P], Repl)          -> Repl;
let_transform(NameNode, [Fn|Args], Repl) when ?is_fn(Fn) -> [Fn|[let_transform(NameNode, X, Repl) || X <- Args]];
let_transform(_NameNode, Literal, _Repl)                 -> Literal.
