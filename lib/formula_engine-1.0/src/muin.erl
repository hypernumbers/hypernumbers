%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Interface to the formula engine/interpreter.

-module(muin).

%% test exports
-export([
         test_formula/2,
         test_xfl/0,
         run_formula/2,
         run_code/2,
         zeval/3
        ]).

-define('htmlbox', $h,$t,$m,$l,$.,$b,$o,$x,$.).
-define('htmlalert', $h,$t,$m,$l,$.,$a,$l,$e,$r,$t, $.).
-define('htmlblock', $h,$t,$m,$l,$.,$b,$l,$o,$c,$k,$.).

% these functions are wrappers for use externally
% they enable us to deny certain spreadsheet functions to
% ability to be called inside other fns
% used for fns line '=include(reference)' which operate
% on the presentation of some cells and not the values
-export([
         external_eval/1,
         external_eval_formula/1
        ]).

-export([
         context_setting/1,
         col_index/1,
         row_index/1,
         col/1,
         row/1,
         path/1,
         get_modules/0,
         test_formula/1,
         funcall/2,
         call_fun/3,
         force_load/1,
         prefetch_references/1,
         fetch/1,
         get_hypernumber/9,
         userdef_call/2,
         toidx/1,
         do_cell/4,
         parse/2
        ]).

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
-define(syntax_err, {error, syntax_error}).

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
run_formula("#CIRCREF!", _) -> {error, ?ERRVAL_CIRCREF};
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
         {array_context, AryCtx}, {infinite, []},
         {retvals, {[], []}}, {recompile, false},
         {auth_req, AuthReq}]),
    Fcode = case ?array_context of
                true -> loopify(Pcode);
                false -> Pcode
            end,
    Result = eval_formula(Fcode),
    {_Errors, References} = get(retvals),
    FiniteRefs = [{X, L} || {X, finite, L} <- References],
    InfiniteRefs = get(infinite),
    {ok, {Fcode, Result, FiniteRefs, InfiniteRefs, get(recompile)}}.

% not all functions can be included in other functions
external_eval_formula([include | _Rest]) -> ?error_in_formula;
external_eval_formula(X)                 -> eval_formula(X).

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
        {ok, Pcode} -> {ok, Pcode};
        ?syntax_err -> ?error_in_formula
    end.

%% Formula -> sexp, relative to coord.
parse(Fla, {Col, Row}) ->
    Trans = translator:do(Fla),
    case catch (xfl_lexer:lex(Trans, {Col, Row})) of
        {ok, Toks} ->
            case catch(xfl_parser:parse(Toks)) of
                {ok, Ast} -> {ok, Ast};
                _         -> ?syntax_err
            end;
        _ -> ?syntax_err
    end.                     

% not all functions can be included in other functions
external_eval([include | _Rest]) -> ?error_in_formula;
external_eval(X)                 -> eval(X).

%% Evaluate a form in the current rti context.
%% this function captures thrown errors - including those thrown
%% when Mnesia is unrolling a transaction. When the '{aborted, {cyclic...'
%% exit is caught it must be exited again...
eval(_Node = [Func|Args]) when ?is_fn(Func) ->
    {NewFunc, NewArgs} = transform(atom_to_list(Func), Args),
    case muin_util:attempt(?MODULE, funcall, [NewFunc, NewArgs]) of
        {error, {errval, _}  = Err} -> Err;
        {error, {aborted, _} = Err} -> exit(Err); % re-exit - this is an mnesia transaction!
        {error, _E}                 -> ?error_in_formula;
        {ok, {error, _E}}           -> ?error_in_formula; % in stdfuns
        {ok, V}                     -> V
    end;

eval(Value) ->
    Value.

transform([?htmlbox | R], Args) ->
    {W, H} = parse(R),
    {list_to_atom([?htmlbox]), [W , H | Args]};
transform([?htmlalert | R], Args) ->
    {W, H} = parse(R),
    {list_to_atom([?htmlalert]), [W , H | Args]};
transform([?htmlblock | R], Args) ->
    {W, H} = parse(R),
    {list_to_atom([?htmlblock]), [W , H | Args]};
transform(List, Args) -> {list_to_atom(List), Args}.

parse(String) -> [W, H] = string:tokens(String, "x"),
                 {W, H}.

funcall(make_list, Args) ->
    area_util:make_array([Args]); % horizontal array

%% Hypernumber function and its shorthand.
funcall(hypernumber, [Url]) ->
    #refX{site = RSite, path = RPath,
          obj = {cell, {RX, RY}}} = hn_util:url_to_refX(Url),
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
                                  ok = hn_net_util:email("gordon@hypernumbers.com",
                                                         "", atom_to_list(node()),
                                                         "Functions unloaded",
                                                         "reloading..."),
                                  reloaded;
                      {file,_} -> Flag
                  end
          end,
    lists:foldl(Fun, loaded, Modules).
                        
get_modules() ->
    [
     stdfuns_text,
     stdfuns_math,
     stdfuns_stats,
     stdfuns_date,
     stdfuns_financial,
     stdfuns_info,
     stdfuns_lookup_ref,
     stdfuns_eng,
     stdfuns_logical,
     stdfuns_text,
     stdfuns_db,
     hnfuns_graphs,
     hnfuns_web,
     hnfuns_integration,
     hnfuns_html,
     hnfuns_forms,
     hnfuns_controls
    ].

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
            do_cell(R#rangeref.path, ?my, ColIdx, finite);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_row(R) ->
    case R#rangeref.height of
        1 ->
            {row, Row} = R#rangeref.tl,
            RowIdx = row_index(Row),
            do_cell(R#rangeref.path, RowIdx, ?mx, finite);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_finite(R) ->
    Dim = {R#rangeref.width, R#rangeref.height},
    case Dim of
        {1, 1} ->
            [{X, Y}] = muin_util:expand_cellrange(R),
            do_cell(R#rangeref.path, Y, X, finite);
        {1, _H} -> % vertical vector
            CellCoords = muin_util:expand_cellrange(R),
            case lists:filter(fun({_X, Y}) -> Y == ?my end, CellCoords) of
                [{X, Y}] -> do_cell(R#rangeref.path, Y, X, finite);
                []       -> ?ERRVAL_VAL
            end;
        {_W, 1} -> % horizontal vector
            CellCoords = muin_util:expand_cellrange(R),
            case lists:filter(fun({X, _Y}) -> X == ?mx end, CellCoords) of
                [{X, Y}] -> do_cell(R#rangeref.path, Y, X, finite);
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

fetch(#zcellref{zpath = Z, cellref = C}) when is_record(C, cellref) ->
    {zpath, ZList} = Z,
    NewPath = muin_util:walk_zpath(?mpath, ZList),
    Length = length(NewPath),
    Paths = hn_db_api:read_pages(#refX{site = ?msite, path = [], obj = {page, "/"}}),
    FPaths = [X || X <- Paths, length(X) == Length],
    MPaths = match(?msite, FPaths, NewPath),
    OCol = C#cellref.col,
    ORow = C#cellref.row,
    Vals = fetch_vals(MPaths, ORow, OCol),
    % build the refX that describes the infinite reference
    RefX = make_inf_refX(C#cellref.path, C#cellref.text),
    Infinites = get(infinite),
    put(infinite, ordsets:add_element(RefX, Infinites)),
    {range, [Vals]};
    % pinch it off from working
    %error_logger:info_msg("(from muin) Somebody tried a z-order cellref~n"),
    %?ERRVAL_ERR;
fetch(Ref) when ?is_zrangeref(Ref) ->
    error_logger:info_msg("(from muin) Somebody tried a z-order rangeref~n"),
    ?ERRVAL_ERR;
fetch(N) when ?is_namedexpr(N) ->
    ?ERRVAL_NAME;
fetch(#cellref{col = Col, row = Row, path = Path}) ->
    RowIndex = row_index(Row),
    ColIndex = col_index(Col),
    do_cell(Path, RowIndex, ColIndex, finite);
fetch(#rangeref{type = Type, path = Path} = Ref)
  when Type == row orelse Type == col ->
    NewPath = muin_util:walk_path(?mpath, Path),
    #refX{obj = Obj} = RefX = muin_util:make_refX(?msite, NewPath, Ref),
    Infinites = get(infinite),
    put(infinite, ordsets:add_element(RefX,Infinites)),
    Refs = hn_db_wu:expand_ref(RefX),
    Rows = case Obj of
               {Type2, {_I, _I}} -> sort1D(Refs, Path, Type2);
               {Type2, {I,   J}} -> sort2D(Refs, Path, {Type2, I, J})
    end,
    % pinch out the functionality for a release
    {range, Rows};
    %error_logger:info_msg("Somebody tried a row or column rangeref~n"),
    %?ERRVAL_ERR;
fetch(#rangeref{type = finite} = Ref) ->
    CellCoords = muin_util:expand_cellrange(Ref),
    Fun1 = fun(CurrRow, Acc) -> % Curr row, result rows
                   Fun2 = fun({_, Y}) ->
                                  Y == CurrRow
                          end,
                   Fun3 = fun({X, Y}) ->
                                  do_cell(Ref#rangeref.path, Y, X, finite)
                          end,
                   RowCoords = lists:filter(Fun2, CellCoords),
                   Row = lists:map(Fun3, RowCoords),
                   [Row|Acc]
           end,
    Rows = lists:foldr(Fun1, [], lists:seq(muin_util:tl_row(Ref),
                                           muin_util:br_row(Ref))),
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

%% Returns value in the cell + get_value_and_link() is called`
do_cell(RelPath, Rowidx, Colidx, Type) ->
    Path = muin_util:walk_path(?mpath, RelPath),
    IsCircRef = (Colidx == ?mx andalso Rowidx == ?my andalso Path == ?mpath),

    case IsCircRef of
        true ->
            ?ERRVAL_CIRCREF;
        false ->
            FetchFun = fun() ->
                               get_cell_info(?msite, Path, Colidx, Rowidx, Type)
                       end,
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
    hn_db_wu:refX_to_idx_create(RefX),
    {Errs0, Refs0} = get(retvals),
    put(retvals, {Errs0 ++ Errs, Refs0 ++ Refs}),
    Value.

%% Row or Col information --> index.
toidx(N) when is_number(N) -> N;
toidx({row, Offset})       -> ?my + Offset;
toidx({col, Offset})       -> ?mx + Offset.

get_cell_info(S, P, Col, Row, Type) ->
    RefX = #refX{site=string:to_lower(S), path=P, obj={cell, {Col,Row}}},
    hn_db_wu:get_cell_for_muin(RefX, Type).

sort1D(Refs, Path, Type) -> sort1D_(Refs, Path, Type, orddict:new()).

sort1D_([], _Path, Type, Dict) -> Size = orddict:size(Dict),
                           List = orddict:to_list(Dict),
                           Filled = fill1D(List, 1, 1, Size + 1, [], 'to-last-key'),
                           % if it is row then you need to flatten the List
                           % by one degree and wrap it in a list
                           % (ie a 2d transposition)
                           case Type of
                               column -> Filled;
                               row    -> [[X || [X] <- Filled]]
                           end;
sort1D_([#refX{obj = {cell, {X, Y}}} | T], Path, row, Dict) ->
    V = do_cell(Path, Y, X, infinite),
    sort1D_(T, Path, row, orddict:append(X, V, Dict));
sort1D_([#refX{obj = {cell, {X, Y}}} | T], Path, column, Dict) ->
    V = do_cell(Path, Y, X, infinite),
    sort1D_(T, Path, column, orddict:append(Y, V, Dict)).

%% if all the cells are blank will return an array of arrays.
%% if type is 'column' this will be one array for each column each with
%% a 'blank' in it
%% if type is a 'row' this will be one list with as many 'blank's as there
%% are rows specified
sort2D(Refs, Path, Def) -> sort2D_(Refs, Path, Def, orddict:new()).

sort2D_([], _Path, {Type, Start, End}, Dict) ->
    Ret = case {Type, orddict:size(Dict)} of
              {row, 0}    -> [lists:duplicate(End - Start, blank)];
              {column, 0} ->  lists:duplicate(End - Start, [blank]);
              {_, _}      -> fill2D(Dict, Type, Start, End, [])
    end,
    Ret;
sort2D_([#refX{obj = {cell, {X, Y}}} | T], Path, Def, Dict) ->
    SubDict = case orddict:is_key(X, Dict) of
                  true  -> orddict:fetch(X, Dict);
                  false -> orddict:new()
              end,
    V = do_cell(Path, Y, X, infinite),
    NewSub  = orddict:append(Y, V, SubDict),  % works because there are no dups!
    NewDict = orddict:store(X, NewSub, Dict),
    sort2D_(T, Path, Def, NewDict).

%% for columns you infill to the number of columns
%% get the size of each row, and take the maximum of the
%% go back and 'top up' all rows that are too short
fill2D(Dict, column, Index, End, Acc) ->
    {Pass1, Max} = fill2D_a(Dict, Index, End + 1, Acc, 0),
    bulk_a(Pass1, [], Max);
%% for rows you walk over the outer set of columns up to the last
%% * if a column doesn't exist you fill it with blanks
%% * if it does you infill it
fill2D(Dict, row, Index, End, Acc) ->
    DictSize = orddict:size(Dict),
    fill2D_b(Dict, 1, 1, DictSize + 1, Index, End + 1, Acc).
   
%% this is the infill for row
fill2D_b(_Dict, _Key, Size, Size, _Index, _End, Acc) ->
    Lists = lists:reverse(Acc),
    [[X || [X] <- Y] || Y <- Lists];
fill2D_b(Dict, Key, N, Size, Index, End, Acc) ->
    {NewDict, NewN} = case orddict:is_key(Key, Dict) of
                          true  -> {orddict:fetch(Key, Dict), N + 1};
                          false -> {orddict:new(), N}
                      end,
    NewAcc = fill1D(NewDict, Index, Index, End, [], 'over-fill'),
    fill2D_b(Dict, Key + 1, NewN, Size, Index, End, [NewAcc | Acc]).

%% this is the infill for columns
fill2D_a(_Dict, End, End, Acc, Max) ->
    {lists:reverse(Acc), Max};
fill2D_a(Dict, Index, End, Acc, Max) ->
    {NewAcc, DictMax} =
        case orddict:is_key(Index, Dict) of
            true  -> SubDict = orddict:fetch(Index, Dict),
                     [{MyMax, _} | _T] = lists:reverse(orddict:to_list(SubDict)),
                     {{Index, SubDict}, MyMax};
            false -> {{Index, orddict:new()}, 0}
        end,
    NewMax = erlang:max(Max, DictMax),
    fill2D_a(Dict, Index + 1, End, [NewAcc | Acc], NewMax).

bulk_a([], Acc, _Max) ->
    Lists = lists:reverse(Acc),
    [[X || [X] <- Y] || Y <- Lists];
bulk_a([{_I, D} | T], Acc, Max) ->
    NewD = fill1D(D, 1, 1, Max + 1, [], 'over-fill'),
    bulk_a(T, [orddict:to_list(NewD) | Acc], Max).

fill1D(_Dict, _Index, Size, Size, Acc, 'to-last-key')  ->
    lists:reverse(Acc);
fill1D(_Dict, Size, _N, Size, Acc, 'over-fill')  ->
    lists:reverse(Acc);
fill1D(Dict, Index, N, Size, Acc, Type) ->
    {NewAcc, NewN} =
        case orddict:is_key(Index, Dict) of
            true  -> {[orddict:fetch(Index, Dict) | Acc], N + 1};
            false -> {[[blank] | Acc], N}
        end,
    fill1D(Dict, Index + 1, NewN, Size, NewAcc, Type).    

match(Site, Paths, ZPath) -> m1(Site, Paths, ZPath, []).

m1(_Site, [], _ZPath, Acc) -> Acc;
m1(Site, [H | T], ZPath, Acc) ->
    NewAcc = case m2(Site, H, ZPath, []) of
                 nomatch -> Acc;
                 match   -> [H | Acc]
             end,
    m1(Site, T, ZPath, NewAcc).

m2(_Site, [], [], _Htap) -> match;
m2(Site, [S | T1], [{seg, S}     | T2], Htap)   -> m2(Site, T1, T2, [S | Htap]);
m2(_Site, [_S | _T1], [{seg, _}  | _T2], _Htap) -> nomatch;
m2(Site, [S | T1], [{zseg, Z, _} | T2], Htap)   ->
    case zeval(Site, lists:reverse([S | Htap]), Z) of
        match   -> m2(Site, T1, T2, [S | Htap]);
        nomatch -> nomatch
    end.

% the execution context for expressions is stored in the process dictionary
% so here you need to rip it out and then stick it back in
% (not good, Damn you Hasan!).
zeval(Site, Path, Toks) ->
    % capture the process dictionary (it will get gubbed!)
    OldContext = get(),
    X = ?mx,
    Y = ?my,
    % we run in the context of call 'a1' - this is because z-order expressions
    % do not support r[]c[] format cell references (or is it vice-versa?)
    RefX = #refX{site = Site, path = Path, obj = {cell, {Y, X}}},
    % no array context (fine) or security context (erk!)
    RTI = hn_db_wu:refX_to_rti(RefX, nil, false),
    Return = case catch(xfl_parser:parse(Toks)) of
                 {ok, Ast}   -> {ok, {_, Rs, _, _, _}} = muin:run_code(Ast, RTI),
                                % cast to a boolean
                                Rs2 = typechecks:std_bools([Rs]),
                                case Rs2 of
                                    [true]  -> match;
                                    [false] -> nomatch;
                                    _       -> ?ERR_VAL
                                end;
                 ?syntax_err -> ?error_in_formula
             end,
    % restore the process dictionary (fugly! fugly! fugly!)
    [put(K, V) || {K, V} <- OldContext],
    Return.

fetch_vals(Paths, Row, Col) -> fetch_v1(Paths, Row, Col, []).

fetch_v1([], _Row, _Col, Acc) -> lists:reverse(Acc);
fetch_v1([H | T], Row, Col, Acc) ->
    Path = hn_util:list_to_path(H),
    RowIndex = row_index(Row),
    ColIndex = col_index(Col),
    NewAcc = do_cell(Path, RowIndex, ColIndex, infinite),
    fetch_v1(T, Row, Col, [NewAcc | Acc]).

make_inf_refX(Path, Text) ->
    NewPath = muin_util:walk_path(?mpath, Path),
    Segs = string:tokens(Text, "/"),
    {_, Ref} = lists:split(length(Segs) - 1, Segs),
    Obj = hn_util:parse_ref(Ref),
    #refX{site = ?msite, path = NewPath, type = gurl, obj = Obj}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Functions
test_xfl() ->
    Exprs = [
             % "sum(/_sites/a1, 2, 3)",
             % "sum(./_sites/a1, 2, 3)",
             % "sum(../_sites/a1, 2, 3)",
             % "sum(/blah/_sites/bleh/a1:b2, 3)",
             % "sum(/_SITES/a1, 2, 3)",
             % "sum(./_SITES/a1, 2, 3)",
             % "sum(../_SITES/a1, 2, 3)",
             % "sum(../_SITES/a:b, 2, 3)",
             % "sum(../_SITES/2:3, 2, 3)",
             % "sum(/blah/_SITES/a1:b2, 3)",
             % "sum(/blah/bleeh/[seg() = \"bluh\"]/bloh/a1)",
             % "sum(/blah/bleeh/a$1)",
             % "sum(atan2(3))",
             % "sum(/blah/bleeh/a1)",
             % "sum(/bb/[eg]/doggy/[or(A1,B2)]/a1)",
             % "sum(/bb/[eg]/doggy/[or(A1,a1/atan2(2))]/a1)",
             % "sum(/[or(BB)]/blah/a1)",
             % "sum([or(BB)]/a1)",
             % "sum([o]/a1)",
             % "sum(/blah/[or(BB)]/blah/a1)",
             % "sum(/blah/[or(BB)]/blah/a1) + atan(/[xx]/N2)".
             % "sum(/blah/[or(true,true)]/a1)+ sum(/blEh/[SUM(a1,B3)]/BLeh/A3)",
             % "sum(/blah/[or(1,2)]/a1, /[true = a1]/b99:bev90210)",
             % "/[or(1,2)]/a1",
             % "/[or(1,2)]/a1:b2",
             % "/[or(1,2)]/a:a",
             % "/[or(1,2)]/3:3"
             %"/bleh/1:1",
             %"/bleh/a:a"
             "sum(/bleh/gloh/dleh/1:1)"
            ],
    Fun= fun(X) ->
                  Fla = superparser:process("="++X),
                  io:format("~n~nStarting a new parse...~n"),
                  Trans = translator:do(Fla),
                  case catch (xfl_lexer:lex(Trans, {1, 1})) of
                      {ok, Toks} ->
                          case catch(xfl_parser:parse(Toks)) of
                              {ok, Ast} ->
                                  io:format("Sucess Expr is ~p Fla is ~p "++
                                            "Trans is ~p~n"++
                                            "Toks is ~p~nAst is ~p~n"++
                                            "Status is ~p~n",
                                            [X, Fla, Trans, Toks, Ast, "Ok"]),
                                  {ok, Ast};
                              O2         ->
                                  io:format("Parse fail: "++
                                            "Expr is ~p Fla is ~p Trans is ~p~n"++
                                            "Toks is ~p~nStatus is ~p~n",
                                            [X, Fla, Trans, Toks, O2]),
                                  ?syntax_err
                          end;
                      O1 -> io:format("Lex fail: Expr is ~p Fla is ~p "++
                                      "Trans is ~p~n"++
                                      "Status is ~p~n",
                                      [X, Fla, Trans, O1]),
                            ?syntax_err
                  end
         end,
    [Fun(X) || X <- Exprs],
    ok.
