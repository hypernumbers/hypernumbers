%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Interface to the formula engine/interpreter.

-module(muin).
-export([run_formula/2, run_code/2]).
-export([eval/1]).
-export([context_setting/1,
         col_index/1,
         row_index/1,
         col/1,
         row/1,
         path/1]).
-compile(export_all).

-include("spriki.hrl").
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
    Result = eval_formula(Fcode),
    {RefTree, _Errors, References} = get(retvals),
    {ok, {Fcode, Result, RefTree, References, get(recompile)}}.


%% evaluates a formula rather than a piece of AST, i.e. will do implicit
%% intersection, resolve a final cellref &c.

eval_formula(Fcode) ->
    case eval(Fcode) of
        ?error_in_formula ->
            ?error_in_formula;
        Value ->
            case Value of
                R when ?is_cellref(R) ->
                    case attempt(?MODULE, fetch, [R]) of
                        {ok,    blank}              -> 0;
                        {error, {aborted, _} = Err} -> exit(Err); % re-exit on lock
                        {ok,    Other}              -> Other;
                        {error, ErrVal}             -> ErrVal
                    end;
                R when ?is_rangeref(R); ?is_array(R) ->
                    % TODO need to get rid of these
                    case catch implicit_intersection(R) of
                        {error, {aborted, _} = Err} -> exit(Err); 
                        blank -> 0;
                        Other -> Other
                    end;
                R when ?is_namedexpr(R) ->
                    ?ERRVAL_NAME;
                Constant ->
                    Constant
            end
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

    case catch (xfl_lexer:lex(Trans, {Col, Row})) of
        {ok, Toks} -> case catch(xfl_parser:parse(Toks)) of
                          {ok, Ast} -> {ok, Ast};
                          _         -> ?syntax_error
                      end;
        _ -> ?syntax_error
    end.                     

%% Evaluate a form in the current rti context.
%% this function captures thrown errors - including those thrown
%% when Mnesia is unrolling a transaction. When the '{aborted, {cyclic...'
%% exit is caught it must be exited again...
eval(_Node = [Func|Args]) when ?is_fn(Func) ->
    case attempt(?MODULE, funcall, [Func, Args]) of
        {error, {errval, _}  = Err} -> Err;
        {error, {aborted, _} = Err} -> exit(Err); % re-exit
        {error, _E}                 -> ?error_in_formula;
        {ok, {error, _E}}           -> ?error_in_formula; % in stdfuns
        {ok, V}                     -> V
    end;

eval(Value) ->
    Value.

funcall(make_list, Args) ->
    area_util:make_array([Args]); % horizontal array

%% Hypernumber function and its shorthand.
funcall(hypernumber, [Url]) ->
    {ok, #refX{site = RSite, path = RPath,
             obj = {cell, {RX, RY}}}} = hn_util:parse_url(Url),
    F = fun() -> get_hypernumber(?msite, ?mpath, ?mx, ?my, Url, RSite, RPath, RX, RY) end,
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
%% TODO: If a function exists in one of the modules, but calling it returns
%%       no_clause, return #VALUE? (E.g. for when giving a list to ABS)
funcall(Fname, Args0) ->
    Args = case member(Fname, ['if', choose, column, row, cell, columns]) of
               true  -> Args0;
               false -> [eval(X) || X <- prefetch_references(Args0)]
           end,

    R = foldl(fun(M, Acc = {F, A, not_found_yet}) ->
                      case attempt(M, F, [A]) of
                          {error, undef} -> Acc;
                          {error, {aborted, _} = E}  -> exit(E); % rethrow on lock
                          {ok, V}        -> {F, A, V};
                          {error, Ev = {errval, _}} -> {F, A, Ev};
                          {error, Other} -> {F, A, {error, Other}}
                      end;
               (_, Acc) ->
                      Acc
              end,
              {Fname, Args, not_found_yet},
              [stdfuns_text, stdfuns_math, stdfuns_stats, stdfuns_date, stdfuns_financial,
               stdfuns_info, stdfuns_lookup_ref, stdfuns_eng, stdfuns_logical, stdfuns_text,
               stdfuns_db]),
    
    case R of
        {_, _, {error, Error}} -> {error, Error};
        {_, _, not_found_yet}  -> userdef_call(Fname, Args);
        {_, _, V}              -> V
    end.

%%% Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Intersect current cell with a range.
implicit_intersection(R) when ?is_rangeref(R) ->
    case R#rangeref.type of
        col    -> implicit_intersection_col(R);
        row    -> implicit_intersection_row(R);
        finite -> implicit_intersection_finite(R)
    end;
implicit_intersection(A) when ?is_array(A) ->
    {ok, V} = area_util:at(1, 1, A),
    V.

implicit_intersection_col(R) ->
    case R#rangeref.width of
        1 ->
            {col, Col} = R#rangeref.tl,
            ColIdx = col_index(Col),
            do_cell(R#rangeref.path, ?my, ColIdx);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_row(R) ->
    case R#rangeref.height of
        1 ->
            {row, Row} = R#rangeref.tl,
            RowIdx = row_index(Row),
            do_cell(R#rangeref.path, RowIdx, ?mx);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_finite(R) ->
    Dim = {R#rangeref.width, R#rangeref.height},
    case Dim of
        {1, 1} ->
            [{X, Y}] = muin_util:expand_cellrange(R),
            do_cell(R#rangeref.path, Y, X);
        {1, _H} -> % vertical vector
            CellCoords = muin_util:expand_cellrange(R),
            case filter(fun({_X, Y}) -> Y == ?my end, CellCoords) of
                [{X, Y}] -> do_cell(R#rangeref.path, Y, X);
                []       -> ?ERRVAL_VAL
            end;
        {_W, 1} -> % horizontal vector
            CellCoords = muin_util:expand_cellrange(R),
            case filter(fun({X, _Y}) -> X == ?mx end, CellCoords) of
                [{X, Y}] -> do_cell(R#rangeref.path, Y, X);
                []       -> ?ERRVAL_VAL
            end;
        {_, _} ->
            ?ERRVAL_VAL
    end.

context_setting(col)           -> ?mx;
context_setting(row)           -> ?my;
context_setting(path)          -> ?mpath;
context_setting(site)          -> ?msite;
context_setting(array_context) -> ?array_context.

col(#cellref{col = Col}) -> Col.
row(#cellref{row = Row}) -> Row.
path(#cellref{path = Path}) -> Path.
    
prefetch_references(L) ->
    foldr(fun(R, Acc) when ?is_cellref(R); ?is_rangeref(R); ?is_namedexpr(R) ->
                  [fetch(R)|Acc];
             (X, Acc) ->
                  [X|Acc]
          end,
          [],
          L).    

row_index(N) when is_integer(N) -> N;
row_index({offset, N}) -> ?my + N.

col_index(N) when is_integer(N) -> N;
col_index({offset, N}) -> ?mx + N.

fetch(N) when ?is_namedexpr(N) ->
    ?ERRVAL_NAME;
fetch(#cellref{col = Col, row = Row, path = Path}) ->
    RowIndex = row_index(Row),
    ColIndex = col_index(Col),
    do_cell(Path, RowIndex, ColIndex);                       
fetch(R) ->
    CellCoords = muin_util:expand_cellrange(R),
    Rows = foldr(fun(CurrRow, Acc) -> % Curr row, result rows
                         RowCoords = filter(fun({_, Y}) -> Y == CurrRow end, CellCoords),
                         Row = map(fun({X, Y}) -> do_cell(R#rangeref.path, Y, X) end, RowCoords),
                         [Row|Acc]
                 end,
                 [],
                 seq(muin_util:tl_row(R), muin_util:br_row(R))),
    {range, Rows}. % still tagging to tell stdfuns where values came from.

%% why are we passing in Url?
get_hypernumber(MSite, MPath, MX, MY, _Url, RSite, RPath, RX, RY) ->
    NewMPath = lists:filter(fun(X) -> not(X == $/) end, MPath),
    NewRPath = lists:filter(fun(X) -> not(X == $/) end, RPath),

    Child  = #refX{site = MSite, path = NewMPath, obj ={cell, {MX, MY}}},
    Parent = #refX{site = RSite, path = NewRPath, obj ={cell, {RX, RY}}},

    case hn_db_wu:read_incoming_hn(Parent, Child) of
        
        {error,permission_denied} ->
            {{errval,'#AUTH'},[],[],[]};
        
        {Val, DepTree} ->
            F = fun({url, [{type, Type}], [Url2]}) ->
                        {ok, Ref} = hn_util:parse_url(Url2),
                        #refX{site = S, path = P, obj = {cell, {X, Y}}} = Ref, 
                        {Type,{S, P, X, Y}}
                end,
            Dep = lists:map(F, DepTree) ++ [{"remote", {RSite, NewRPath, RX, RY}}],
            {Val, Dep, [], [{"remote", {RSite, NewRPath, RX, RY}}]}
    end.

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
        {'EXIT', _}          -> ?ERR_NAME; %% FIXME: should return a descriptive error message.
        Val                  -> Val
    end.

%% Returns value in the cell + get_value_and_link() is called behind the
%% scenes.

do_cell(RelPath, Rowidx, Colidx) ->
    Path = muin_util:walk_path(?mpath, RelPath),
    IsCircRef = (Colidx == ?mx andalso Rowidx == ?my andalso Path == ?mpath),
    ?IF(IsCircRef, ?ERR_CIRCREF),
    FetchFun = ?L(get_cell_info(?msite, Path, Colidx, Rowidx)),
    get_value_and_link(FetchFun).

%% @doc Calls supplied fun to get a cell's value and dependence information,
%% saves the dependencies (linking it to current cell), and returns
%% the value to the caller (to continue the evaluation of the formula).
get_value_and_link(FetchFun) ->
    {Value, RefTree, Errs, Refs}  = FetchFun(),
    RefX = #refX{site = ?msite, path = ?mpath, obj = {cell, {?mx, ?my}}},
    Idx = hn_db_wu:get_local_item_index(RefX),
    case member({"local", Idx}, RefTree) of
        true ->
            ?ERR_CIRCREF;
        false ->
            {RefTree0, Errs0, Refs0} = get(retvals),
            NewRefTree = hslists:uniq(lists:append([RefTree0, RefTree])),
            put(retvals, {NewRefTree, Errs0 ++ Errs, Refs0 ++ Refs}),
            Value
    end.

%% Row or Col information --> index.
toidx(N) when is_number(N) -> N;
toidx({row, Offset})       -> ?my + Offset;
toidx({col, Offset})       -> ?mx + Offset.

get_cell_info(S, P, Col, Row) ->
    RefX = #refX{site = string:to_lower(S), path = P, obj = {cell, {Col, Row}}},
    hn_db_wu:get_cell_for_muin(RefX).
