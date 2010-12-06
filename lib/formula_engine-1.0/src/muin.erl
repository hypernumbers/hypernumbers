%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Interface to the formula engine/interpreter.

-module(muin).

-export([test_formula/2, run_formula/2, run_code/2]).

-export([eval/1]).

-export([context_setting/1,
         col_index/1,
         row_index/1,
         col/1,
         row/1,
         path/1,
         get_modules/0 ]).

-compile(export_all).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).
-define(mar, get(auth_req)).
-define(array_context, get(array_context)).

-define(error_in_formula, {errval, '#FORMULA!'}).
-define(syntax_error, {error, syntax_error}).

%%% PUBLIC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @doc Test harness for running a formula inside transaction
test_formula(Fla) ->
    test_formula(Fla, #muin_rti{site = "http://localhost:9000", 
                                path = [],
                                col = 1, row = 1,
                                array_context = false,
                                auth_req = nil}).
test_formula(Fla, Rti) ->
    mnesia:activity(transaction, fun run_formula/2, [Fla, Rti]).

%% @doc Runs formula given as a string.
run_formula("#CIRCREF!", _) -> {error, '#CIRCREF!'};
run_formula(Fla, Rti = #muin_rti{col = Col, row = Row}) ->
    case compile(Fla, {Col, Row}) of
        {ok, Ecode}       -> run_code(Ecode, Rti);
        ?error_in_formula -> {error, ?ERRVAL_FORM}
    end.

%% @doc Runs compiled formula.
run_code(Pcode, #muin_rti{site=Site, path=Path,
                          col=Col,   row=Row,
                          array_context=AryCtx,
                          auth_req=AuthReq}) ->

    %% Populate the process dictionary.
    lists:map(fun({K,V}) -> put(K, V) end,
        [{site, Site}, {path, Path}, {x, Col}, {y, Row}, 
         {array_context, AryCtx},
         {retvals, {[], []}}, {recompile, false},
         {auth_req, AuthReq}]),
    
    Fcode = case ?array_context of
                true -> loopify(Pcode);
                false -> Pcode
            end,
    Result = eval_formula(Fcode),
    {_Errors, References} = get(retvals),
    {ok, {Fcode, Result, References, get(recompile)}}.

%% evaluates a formula rather than a piece of AST, i.e. will do implicit
%% intersection, resolve a final cellref &c.
eval_formula(Fcode) ->
    case eval(Fcode) of
        ?error_in_formula ->
            ?ERRVAL_FORM;
        Value ->
            case Value of
                R when ?is_cellref(R) ->
                    case muin_util:attempt(?MODULE, fetch, [R]) of
                        {ok,    blank}              -> 0;
                        {error, {aborted, _} = Err} -> exit(Err);
                        {ok,    Other}              -> Other;
                        {error, ErrVal}             -> ErrVal
                    end;
                R when ?is_rangeref(R); ?is_array(R) ->
                    try case implicit_intersection(R) of
                            blank -> 0;
                            Other -> Other
                        end
                    catch throw:Err -> Err end;
                R when ?is_namedexpr(R) ->
                    ?ERRVAL_NAME;
                Val -> Val
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
        {ok, Toks} ->
            case catch(xfl_parser:parse(Toks)) of
                {ok, Ast} -> 
                    {ok, Ast};
                _         -> ?syntax_error
            end;
        _ -> ?syntax_error
    end.                     

%% Evaluate a form in the current rti context.
%% this function captures thrown errors - including those thrown
%% when Mnesia is unrolling a transaction. When the '{aborted, {cyclic...'
%% exit is caught it must be exited again...
eval(_Node = [Func|Args]) when ?is_fn(Func) ->
    case muin_util:attempt(?MODULE, funcall, [Func, Args]) of
        {error, {errval, _}  = Err} -> Err;
        {error, {aborted, _} = Err} -> exit(Err); % re-exit - this is an mnesia transaction!
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
    #refX{site = RSite, path = RPath,
          obj = {cell, {RX, RY}}} = hn_util:parse_url(Url),
    F = fun() -> get_hypernumber(?msite, ?mpath, ?mx, ?my,
                                 Url, RSite, RPath, RX, RY) end,
    get_value_and_link(F);

funcall(hn, [Url]) ->
    funcall(hypernumber, [Url]);

funcall(loop, [A, Fn]) when ?is_area(A) ->
    area_util:apply_each(
      fun(L) when is_list(L) -> % paired up
              funcall(Fn, L);
         (X) -> % single value from array
              case Fn of
                  {Lst} when is_list(Lst) -> % fn spec is reversed
                      [Func|Revargs] = Lst,
                      funcall(Func, [X|lists:reverse(Revargs)]);
                  _ -> % spec is atom (should probably be done on length)
                      funcall(Fn, [X])
              end
      end,
      A);

funcall(pair_up, [A, B]) when ?is_area(A) andalso ?is_area(B) ->
    area_util:apply_each_with_pos(
      fun(X, {C, R}) ->
              case area_util:at(C, R, B) of
                  {ok, V}    -> [X, V];
                  {error, _} -> ?ERRVAL_NA
              end
      end,
      A);
funcall(pair_up, [A, V]) when ?is_area(A) andalso not(?is_area(V)) ->
    Ev = eval(V),
    case ?is_area(Ev) of
        true  -> funcall(pair_up, [A, Ev]);
        false -> area_util:apply_each(fun(X) -> [X, Ev] end, A)
    end;
funcall(pair_up, [V, A]) when ?is_area(A) andalso not(?is_area(V)) ->
    funcall(pair_up, [A, V]);

%% Formula function call (built-in or user-defined).
%% TODO: If a function exists in one of the modules, but calling it returns
%%       no_clause, return #VALUE? (E.g. for when giving a list to ABS)
funcall(Fname, Args0) ->
    
    % TODO, this should be taken out, no reason to strictly
    % evaluate arguments -- hahaha
    Funs = [ '+', '^^',
             abs, acos, 'and', asin, asinh, atan, atan2, atanh, avedev,
             average, averagea,
             ceiling, char, choose, clean, column, columns, combin,
             concatenate, cos, cosh, count, counta, countblank, countif,
             tan, tanh,
             sin, sinh,
             thelifeuniverseandeverything,
             row,
             cell, timevalue, npv, max, maxa, min, mina, syd,
             sumsq, sum, stdevpa, stdevp, stdeva, stdev, product, mdeterm,
             median, replace, rows, sumif,
             odd, int, degrees, radians, proper, index, var, steyx,
             small, skew, large, sumproduct, daverage, dcount, isref,
             irr, even,
             include],
    %% Funs = [include, index],
    
    Args = case lists:member(Fname, Funs) of
               true  -> Args0;
               false -> [eval(X) || X <- prefetch_references(Args0)]
           end,
    
    Modules = get_modules(),    
    
    case call_fun(Fname, Args, Modules) of
        {error, not_found} -> userdef_call(Fname, Args);
        {ok, Value}        -> Value
    end.

call_fun(Fun, Args, []) ->
    %% mebbies the VM has unloaded the function, so try and reload them
    case force_load(get_modules()) of
        reloaded -> call_fun(Fun, Args, get_modules());
        loaded   -> {error, not_found}
    end;

call_fun(Fun, Args, [Module | Rest]) ->
    case erlang:function_exported(Module, Fun, 1) of
        true  -> {ok, erlang:apply(Module, Fun, [Args])};
        false -> call_fun(Fun, Args, Rest)
    end.

force_load(Modules) ->
    Fun = fun(Element, Flag) ->
                  case code:is_loaded(Element) of
                      false    -> code:load_file(Element),
                                  ok = hn_net_util:email("gordon@hypernumbers.com", "",
                                                         atom_to_list(node()),
                                                         "Functions unloaded",
                                                         "reloading..."),
                                  reloaded;
                      {file,_} -> Flag
                  end
          end,
    lists:foldl(Fun, loaded, Modules).
                        
get_modules() ->
    [stdfuns_text, stdfuns_math, stdfuns_stats, stdfuns_date,
     stdfuns_financial, stdfuns_info, stdfuns_lookup_ref,
     stdfuns_eng, stdfuns_logical, stdfuns_text, stdfuns_db,
     hnfuns_graphs, hnfuns_web, hnfuns_integration].

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
            case lists:filter(fun({_X, Y}) -> Y == ?my end, CellCoords) of
                [{X, Y}] -> do_cell(R#rangeref.path, Y, X);
                []       -> ?ERRVAL_VAL
            end;
        {_W, 1} -> % horizontal vector
            CellCoords = muin_util:expand_cellrange(R),
            case lists:filter(fun({X, _Y}) -> X == ?mx end, CellCoords) of
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
    lists:foldr(fun(R, Acc) when ?is_cellref(R); ?is_rangeref(R); ?is_namedexpr(R) ->
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
    Rows = lists:foldr(fun(CurrRow, Acc) -> % Curr row, result rows
                         RowCoords = lists:filter(fun({_, Y}) -> Y == CurrRow end, CellCoords),
                         Row = lists:map(fun({X, Y}) -> do_cell(R#rangeref.path, Y, X) end, RowCoords),
                         [Row|Acc]
                 end,
                 [],
                 lists:seq(muin_util:tl_row(R), muin_util:br_row(R))),
    {range, Rows}. % still tagging to tell stdfuns where values came from.

%% why are we passing in Url?
get_hypernumber(MSite, MPath, MX, MY, _Url, RSite, RPath, RX, RY) ->
    % TODO get rid of
    NewMPath = lists:filter(fun(X) -> not(X == $/) end, MPath),
    NewRPath = lists:filter(fun(X) -> not(X == $/) end, RPath),

    Child  = #refX{site=MSite, path=NewMPath, obj={cell, {MX, MY}}},
    Parent = #refX{site=RSite, path=NewRPath, obj={cell, {RX, RY}}},

    case hn_db_api:read_incoming_hn(Parent, Child) of
        
        {error,permission_denied} ->
            {{errval,'#AUTH'},[],[],[]};
        
        {Val, DepTree} ->
            F = fun({url, [{type, Type}], [Url2]}) ->
                        Ref = hn_util:parse_url(Url2),
                        #refX{site = S, path = P, obj = {cell, {X, Y}}} = Ref, 
                        {Type,{S, P, X, Y}}
                end,
            Dep = lists:map(F, DepTree) ++
                [{"remote", {RSite, NewRPath, RX, RY}}],
            {Val, Dep, [], [{"remote", {RSite, NewRPath, RX, RY}}]};

        Else ->
            Else
    end.

%% TODO: Beef up.
%% TODO: what's the real type of ':'? (vararg works for now).
fntype(Fn) ->
    case lists:member(Fn, [transpose, mmult, munit, frequency]) of
        true  -> matrix;
        false -> case lists:member(Fn, [sum, count, ':']) of
                     true  -> vararg;
                     false -> other
                 end
    end.

is_binop(X) ->
    lists:member(X, ['>', '<', '=', '>=', '<=', '<>', '+', '*', '-', '/', '^']).

loopify(Node = [loop|_]) -> Node;
loopify(Node = [Fn|_]) when ?is_fn(Fn) ->
    case fntype(Fn) of
        matrix -> Node;
        vararg -> Node;
        _      -> loop_transform(Node)
    end;
loopify(Literal) -> Literal.

loop_transform([Fn|Args]) when ?is_fn(Fn) ->
    ArgsProcd = lists:map(fun(X) -> loopify(X) end, Args),
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
                    [loop] ++ [Area] ++ [{[Fn | lists:reverse(Rst)]}]
                    % to wrap Area or not may depend on if it's node or literal?
                    % wrap in {} to prevent from being eval'd -- need a proper '
            end
    end.

%% Try the userdef module first, then Ruby, Gnumeric, R, whatever.
userdef_call(Fname, Args) ->
    % changed to apply because using the construction userdef:Fname failed
    % to work after hot code load (Gordon Guthrie 2008_09_08)
    case (catch apply(userdef,Fname,Args)) of
        {'EXIT', {undef, _}} -> ?ERR_NAME;
        {'EXIT', _}          -> ?ERR_NAME;
        % FIXME: should return a descriptive error message.
        Val                  -> Val
    end.

%% Returns value in the cell + get_value_and_link() is called behind the
%% scenes.
do_cell(RelPath, Rowidx, Colidx) ->
    Path = muin_util:walk_path(?mpath, RelPath),
    IsCircRef = (Colidx == ?mx andalso Rowidx == ?my andalso Path == ?mpath),

    case IsCircRef of
        true ->
            ?ERRVAL_CIRCREF;
        false ->
            FetchFun = fun() -> get_cell_info(?msite, Path, Colidx, Rowidx) end,
            case ?mar of
                nil -> get_value_and_link(FetchFun);
                _Else -> case auth_srv:get_any_view(?msite, Path, ?mar) of
                             {view, _} -> get_value_and_link(FetchFun);
                             _Else -> ?ERR_AUTH
                         end
            end
    end.

%% @doc Calls supplied fun to get a cell's value and dependence information,
%% saves the dependencies (linking it to current cell), and returns
%% the value to the caller (to continue the evaluation of the formula).
get_value_and_link(FetchFun) ->
    {Value, Errs, Refs}  = FetchFun(),
    RefX = #refX{site = ?msite, path = ?mpath, obj = {cell, {?mx, ?my}}},
    hn_db_wu:ref_to_idx_create(RefX),
    {Errs0, Refs0} = get(retvals),
    put(retvals, {Errs0 ++ Errs, Refs0 ++ Refs}),
    Value.

%% Row or Col information --> index.
toidx(N) when is_number(N) -> N;
toidx({row, Offset})       -> ?my + Offset;
toidx({col, Offset})       -> ?mx + Offset.

get_cell_info(S, P, Col, Row) ->
    RefX = #refX{site=string:to_lower(S), path=P, obj={cell, {Col,Row}}},
    hn_db_wu:get_cell_for_muin(RefX).

