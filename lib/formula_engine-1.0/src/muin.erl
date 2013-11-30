%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Interface to the formula engine/interpreter.

-module(muin).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").
-include("muin_proc_dict.hrl").

-define(notincfns, [include, tick, timestamp, snapshot,
                    input, fixedval,
                    textarea, button, radio, select, 'create.button',
                    'map.rows.button', 'map.sheet.button',
                    'map.custom.button',
                    'load.template.button']).

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
         fetch_for_select/2,
         run_formula/2,
         run_code/2,
         external_zeval/3,
         parse_expr_for_gui/1,
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
         fetch/2,
         fetch/3,
         prefetch_references/1,
         get_hypernumber/9,
         userdef_call/2,
         toidx/1,
         do_cell/6,
         parse/2,
         expand/1
        ]).

%% test exports
-export([
         test_formula/2,
         test_xfl/0
        ]).

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

-spec parse_expr_for_gui(Expr :: list()) -> [{atom(), list()}].
parse_expr_for_gui(Expr) when is_list(Expr) ->
    case superparser:process(Expr) of
        {formula, Fla} ->
            % not going to run this so compile in the
            % context of cell A1/{cell, {1, 1}}
            case compile(Fla, {1, 1}) of
                {ok, Expr2}   -> process_for_gui(Expr2);
                {errval, Err} -> {struct, [{"error", Err}]}
            end;
        [{_Type, NVal}, _, _] -> {struct, [{"value", NVal}]}
    end.

%% use in setting up selects
fetch_for_select(#rangeref{} = RangeRef, Rti) ->
    ok = init_proc_dict(Rti),
    {range, List} = fetch(RangeRef, "__rawvalue"),
    List2 = special_flatten(List, []),
    [X || X <- List2, X =/= blank];
fetch_for_select(#cellref{} = CellRef, Rti) ->
    ok = init_proc_dict(Rti),
    Val = fetch(CellRef, "__rawvalue"),
    % return a list 'cos the select expects a list
    [tconv:to_s(Val)];
%% use in setting up z-ref inline selects only
fetch_for_select(#zcellref{} = Z, Rti) ->
    ok = init_proc_dict(Rti),
    {zeds, Zeds, _, _} = fetch(Z, "__rawvalue"),
    {_Urls, Vals} = lists:unzip(Zeds),
    Vals2 = lists:delete(blank, hslists:uniq(Vals)),
    Vals3 = [tconv:to_s(XX) || XX <- Vals2],
    %% lists:delete don't mind if the value already exists...
    %% sooo, get rid of any blanks
    _Vals4 = lists:sort(Vals3);
%% TODO work out what you need to do to make a dynamic select
%% get a #REF element written into it
fetch_for_select(_Other, _Rti) ->
    ["#ERROR!"].

%% @doc Runs formula given as a string.
run_formula("#CIRCREF!", _) -> {error, ?ERRVAL_CIRCREF};
run_formula(Fla, Rti = #muin_rti{col = Col, row = Row}) ->
    case compile(Fla, {Col, Row}) of
        {ok, Ecode}       -> run_code(Ecode, Rti);
        ?error_in_formula -> {error, ?ERRVAL_FORM}
    end.

%% @doc Runs compiled formula.
run_code(Pcode, Rti) ->
    ok = init_proc_dict(Rti),
    Fcode = case ?array_context of
                true -> loopify(Pcode);
                false -> Pcode
            end,
    Result = eval_formula(Fcode),
    % this code borks NEED TO FIX
    % [Fname | Args] = Fcode,
    %case atom_to_list(Fname) of
    %    "user." ++ _Rest  ->  Fcode2 = [Fname, Site | Args],
    %                          Result = eval_formula(Fcode2);
    %    _        ->  Result = eval_formula(Fcode)
 	  %end,
    {_Errors, References} = get(retvals),
    FiniteRefs = [{X, L} || {X, _, L} <- References],
    InfiniteRefs = get(infinite),
    {ok, {Fcode, Result, FiniteRefs, InfiniteRefs,
          get(recompile), get(circref), get(selfreference)}}.

% not all functions can be included in other functions
external_eval_formula(X) ->
    case lists:member(X, ?notincfns) of
        true  -> ?ERRVAL_CANTINC;
        false -> eval_formula(X)
    end.

%% evaluates a formula rather than a piece of AST, i.e. will do implicit
%% intersection, resolve a final cellref &c.
eval_formula(Fcode) ->
    case eval(Fcode) of
        ?error_in_formula ->
            ?ERRVAL_FORM;
        Value ->
            case Value of
                R when ?is_cellref(R) ->
                    case muin_util:attempt(?MODULE, fetch, [R, "__rawvalue"]) of
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
external_eval([]) -> eval([]);
external_eval(X) when is_list(X)->
    case lists:member(hd(X), ?notincfns) of
        true  -> ?ERRVAL_CANTINC;
        false -> eval(X)
    end;
external_eval(X) -> eval(X).

%% Evaluate a form in the current rti context.
%% this function captures thrown errors - including those thrown
%% when Mnesia is unrolling a transaction. When the '{aborted, {cyclic...'
%% exit is caught it must be exited again...
eval(_Node = [Func|Args]) when ?is_fn(Func) ->
    {NewFunc, NewArgs} = transform(atom_to_list(Func), Args),
    case muin_util:attempt(?MODULE, funcall, [NewFunc, NewArgs]) of
        {error, {errval, _}  = Err} -> Err;
        {error, {aborted, _} = Err} -> exit(Err); % re-exit - an mnesia trans!
        {error, _E}                 -> ?error_in_formula;
        {ok, {error, _E}}           -> ?error_in_formula; % in stdfuns
        {ok, V}                     -> V
    end;

eval(Value) ->
    Value.

% I know it is fugly but you try and get it to work with guards and
% it won't - so stick it up yes
transform("user." ++ R, Args)  ->
    {user_defined_function, [list_to_atom("user." ++ R) | Args]};
transform("tim.alert." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("tim.alert."), [W, H | Args]};
transform("tim.box." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("tim.box."), [W, H | Args]};
transform("tim.plainbox." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("tim.plainbox."), [W, H | Args]};
transform("tim.ruledbox." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("tim.ruledbox."), [W, H | Args]};
transform("tim.tabs." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("tim.tabs."), [W, H | Args]};
transform("generic.integration." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("generic.integration."), [W, H | Args]};
transform("html.headline." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.headline."), [W , H | Args]};
transform("iframe." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("iframe."), [W , H | Args]};
transform("html.box." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.box."), [W , H | Args]};
transform("html.plainbox." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.plainbox."), [W , H | Args]};
transform("html.alert." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.alert."), [W , H | Args]};
transform("html.ruledbox." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.ruledbox."), [W , H | Args]};
transform("sparkline." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("sparkline."), [W , H | Args]};
transform("sparkbar." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("sparkbar."), [W , H | Args]};
transform("sparkhist." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("sparkhist."), [W , H | Args]};
transform("xy." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("xy."), [W , H | Args]};
transform("speedo." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("speedo."), [W , H | Args]};
transform("histogram." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("histogram."), [W , H | Args]};
transform("linegraph." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("linegraph."), [W , H | Args]};
transform("new.linegraph." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("new.linegraph."), [W , H | Args]};
transform("dategraph." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("dategraph."), [W , H | Args]};
transform("sequence.equigraph." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("sequence.equigraph."), [W , H | Args]};
transform("equigraph." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("equigraph."), [W , H | Args]};
transform("piechart." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("piechart."), [W , H | Args]};
transform("link.box." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("link.box."), [W , H | Args]};
transform("table." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("table."), [W , H | Args]};
transform("ztable." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("ztable."), [W , H | Args]};
transform("debug.array." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("debug.array."), [W , H | Args]};
% stop 'em getting swallowed by phone.menu.WxH
transform("phone.menu." ++ R = Fun, Args) ->
    case R of
        Fn when Fn == "say"
        orelse Fn == "play"
        orelse Fn == "record"
        orelse Fn == "phoneno"    % twilio #number{}
        orelse Fn == "extension"  % twilio #client{}
        orelse Fn == "conference" % twilio #conference{}
        orelse Fn == "dial"
        orelse Fn == "sms"
        orelse Fn == "redirect"
        orelse Fn == "record"
        orelse Fn == "transcribe"
        orelse Fn == "pause" -> {list_to_atom(Fun), Args};
        _                    -> {W, H} = get_dims(R),
                                {list_to_atom("phone.menu."), [W , H | Args]}
    end;
transform("manage.api.keys." ++ R, Args) ->
    {list_to_atom("manage.api.keys."), [R | Args]};
transform("users.and.groups." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("users.and.groups."), [W , H | Args]};
% stop 'em getting swallowed by factory.WxH
transform("factory." ++ R = Fun, Args) ->
    case R of
        "info"      -> {list_to_atom(Fun), Args};
        "if." ++ R2 -> {W, H} = get_dims(R2),
                       {list_to_atom("factory.if."), [W , H | Args]};
        _           -> {W, H} = get_dims(R),
                       {list_to_atom("factory."), [W , H | Args]}
    end;
transform("upload.file." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("upload.file."), [W , H | Args]};
transform("html.panel." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html.panel."), [W , H | Args]};
% single parameter stuff
transform("tim.headline." ++ R, Args) ->
    {list_to_atom("tim.headline."), [R | Args]};
transform("tim.vertical.line." ++ R, Args) ->
    {list_to_atom("tim.vertical.line."), [R | Args]};
transform("tim.horizontal.line." ++ R, Args) ->
    {list_to_atom("tim.horizontal.line."), [R | Args]};
transform("horizontal.line." ++ R, Args) ->
    {list_to_atom("horizontal.line."), [R | Args]};
transform("vertical.line." ++ R, Args) ->
    {list_to_atom("vertical.line."), [R | Args]};
transform("tim.menu." ++ R, Args) ->
    {list_to_atom("tim.menu."), [R | Args]};
transform("html.menu." ++ R, Args) ->
    {list_to_atom("html.menu."), [R | Args]};
transform("buttonbar." ++ R, Args) ->
    {list_to_atom("buttonbar."), [R | Args]};
transform("mini.buttonbar." ++ R, Args) ->
    {list_to_atom("mini.buttonbar."), [R | Args]};
transform("small.buttonbar." ++ R, Args) ->
    {list_to_atom("small.buttonbar."), [R | Args]};
transform("large.buttonbar." ++ R, Args) ->
    {list_to_atom("large.buttonbar."), [R | Args]};
transform("make.goto.buttonbar." ++ R, Args) ->
    {list_to_atom("make.goto.buttonbar."), [R | Args]};
transform("breadcrumbs." ++ R, Args) ->
    {list_to_atom("breadcrumbs."), [R | Args]};
% these clauses needs to be captured to stop the next one capturing it!
transform("html.submenu", Args) ->
    {list_to_atom("html.submenu"), Args};
transform("html.zsubmenu", Args) ->
    {list_to_atom("html.zsubmenu"), Args};
%% order matters to prevent premature matching!
transform("html." ++ R, Args) ->
    {W, H} = get_dims(R),
    {list_to_atom("html."), [W , H | Args]};
%% terminal clause
transform(List, Args) -> {list_to_atom(List), Args}.

get_dims(String) -> case string:tokens(String, "x") of
                        [W, H] -> {W, H};
                        _      -> {?ERRVAL_NAME, ?ERRVAL_NAME} % think about it!
                    end.

funcall(make_list, Args) ->
    area_util:make_array([Args]); % horizontal array

%% Hypernumber function and its shorthand.
funcall(hypernumber, [Url]) ->
    #refX{site = RSite, type = url, path = RPath,
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

% fun user-defined curie fns
funcall(user_defined_function, [Function_Name, Site | Args])  ->
    case curie:read_user_fn(Site, atom_to_list(Function_Name)) of
        {ok, DB_Entry}  ->
            [{user_fns, _name, AST, _page, _wizard}] = DB_Entry,
            Params_in_AST = length(curie_arity:walk_AST(lists:flatten(AST), [])),
            case Params_in_AST =:= length(Args) of
                true  ->  io:format("TRUE~n~p~n", [AST]);
                % TODO
                % Now need to swap AST for function parameters.
                % Walk through the AST and for each cellref grab its cell
                % Address,
                % than compare it with walk_AST result (get its position) and
                % grab argument with the same position from Args.
                % TODO
                false  ->  ?ERRVAL_VAL
            end;
        {error, _Message}  ->  ?ERRVAL_NAME
    end;

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
             row, cellref,
             cell, timevalue, npv, max, maxa, min, mina, syd,
             sumsq, sum, stdevpa, stdevp, stdeva, stdev, product, mdeterm,
             median, replace, rows, sumif,
             odd, int, degrees, radians, proper, index, var, steyx,
             small, skew, large, sumproduct, daverage, dcount, isref,
             irr, even,
             include, 'tim.tabs.', 'table.',
             'phone.menu.', 'phone.menu.dial',
             'make.goto.buttonbar.'],

    Args = case lists:member(Fname, Funs) of
               true  -> Args0;
               false -> [external_eval(X) || X <- prefetch_references(Args0)]
           end,
    Modules = get_modules(),
    case call_fun(Fname, Args, Modules) of
        {error, not_found} -> userdef_call(Fname, Args);
        {ok, Value}        -> Value
    end.

call_fun(Fun, Args, []) ->
    % mebbies the VM has unloaded the function, so try and reload them
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
     stdfuns_db,
     hnfuns_graphs,
     hnfuns_graphs2,
     hnfuns_web,
     hnfuns_integration,
     hnfuns_html,
     hnfuns_forms,
     hnfuns_contacts,
     hnfuns_controls,
     hnfuns_z,
     hnfuns_special,
     hnfuns_site,
     hnfuns_bootstrap
    ].

%%% Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
process_for_gui([Fn | []])  ->
    {struct, [{fn, {struct, [{name, Fn}, {type, "prefix"}]}}, {args, []}]};
process_for_gui([Fn| Args])
  when ?is_fn(Fn) andalso (Fn == '='
                           orelse Fn == '<>'
                           orelse Fn == '>'
                           orelse Fn == '<'
                           orelse Fn == '>='
                           orelse Fn == '<='
                           orelse Fn == '+'
                           orelse Fn == '-'
                           orelse Fn == '*'
                           orelse Fn == '/'
                           orelse Fn == '^'
                           orelse Fn == '&'
                           orelse Fn == '%'
                           orelse Fn == '^^') ->
    {struct, [{fn, {struct, [{name, Fn}, {type, "infix"}]}},
              {args, {array, process_args(Args, [])}}]};
process_for_gui([Fn| Args]) when ?is_fn(Fn) ->
    {struct, [{fn, {struct, [{name, Fn}, {type, "prefix"}]}},
              {args, {array, process_args(Args, [])}}]};
% so its not a fn - must be a constant
process_for_gui({cellref, _, _, _, Text}) ->
    {struct, [{cellref, Text}]};
process_for_gui({rangeref, _, _, _, _, _, _, Text}) ->
    {struct, [{rangeref, Text}]};
process_for_gui({zcellref,  _, {cellref, _, _, _, Text}}) ->
    {struct, [{zcellref, Text}]};
process_for_gui({zrangeref,  _, {rangeref, _, _, _, _, _, _,  Text}}) ->
    {struct, [{zrangeref, Text}]};
process_for_gui({errval, Err}) ->
    {struct, [{errval, Err}]};
process_for_gui({namedexpr, _, Name}) ->
    {struct, [{name, Name}]};
process_for_gui({array, [Array]}) ->
    {struct, [{"array", {array, Array}}]};
process_for_gui(H) ->
    {struct, [{constant, H}]}.

process_args([], Acc)      -> lists:reverse(Acc);
process_args([H | T], Acc) -> NewAcc = process_for_gui(H),
                              process_args(T, [NewAcc | Acc]).

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
            xx1(),
            do_cell(R#rangeref.path, ?my, ColIdx, finite, "__rawvalue", false);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_row(R) ->
    case R#rangeref.height of
        1 ->
            {row, Row} = R#rangeref.tl,
            RowIdx = row_index(Row),
            xx2(),
            do_cell(R#rangeref.path, RowIdx, ?mx, finite, "__rawvalue", false);
        _ ->
            ?ERRVAL_VAL
    end.

implicit_intersection_finite(R) ->
    Dim = {R#rangeref.width, R#rangeref.height},
    case Dim of
        {1, 1} ->
            [{X, Y}] = muin_util:expand_cellrange(R),
            xx3(),
            do_cell(R#rangeref.path, Y, X, finite, "__rawvalue", false);
        {1, _H} -> % vertical vector
            CellCoords = muin_util:expand_cellrange(R),
            case lists:filter(fun({_X, Y}) -> Y == ?my end, CellCoords) of
                [{X, Y}] -> xx4(),
                            do_cell(R#rangeref.path, Y, X, finite,
                                    "__rawvalue", false);
                []       -> ?ERRVAL_VAL
            end;
        {_W, 1} -> % horizontal vector
            CellCoords = muin_util:expand_cellrange(R),
            case lists:filter(fun({X, _Y}) -> X == ?mx end, CellCoords) of
                [{X, Y}] -> xx5(),
                            do_cell(R#rangeref.path, Y, X, finite,
                                    "__rawvalue", false);
                []       -> ?ERRVAL_VAL
            end;
        {_, _} ->
            ?ERRVAL_VAL
    end.

context_setting(cell)          -> {?mx, ?my};
context_setting(col)           -> ?mx;
context_setting(row)           -> ?my;
context_setting(path)          -> ?mpath;
context_setting(site)          -> ?msite;
context_setting(array_context) -> ?array_context.

col(#cellref{col = Col}) -> Col.
row(#cellref{row = Row}) -> Row.
path(#cellref{path = Path}) -> Path.

prefetch_references(L) ->
    Fun = fun(R, Acc) when ?is_cellref(R);
                           ?is_rangeref(R);
                           ?is_namedexpr(R) ->
                  [fetch(R, "__rawvalue") | Acc];
             ([], Acc) ->
                  [[] | Acc];
             (X, Acc) when is_list(X)->
                  % not all functions can be included in other functions
                  case lists:member(hd(X), ?notincfns) of
                      true  -> io:format("cant inc (2)~n"),
                               ?ERRVAL_CANTINC;
                      false -> [X | Acc]
                  end;
             (X, Acc) ->
                  [X | Acc]
          end,
    lists:foldr(Fun, [], L).

row_index(N) when is_integer(N) -> N;
row_index({offset, N}) -> ?my + N.

col_index(N) when is_integer(N) -> N;
col_index({offset, N}) -> ?mx + N.

fetch(Ref, ValType) -> fetch(Ref, ValType, false).

% Override makes no sense on a zquery
fetch(#zcellref{zpath = Z, cellref = C}, ValType, false)
  when is_record(C, cellref) ->
    {zpath, ZList} = Z,
    NewPath = muin_util:walk_zpath(?mpath, ZList),
    PageTree = page_srv:get_pages(?msite),
    {MPaths, NoMatch, Err} = match(?msite, PageTree, NewPath),
    OCol = C#cellref.col,
    ORow = C#cellref.row,
    Vals = fetch_vals(MPaths, ORow, OCol, ValType),
    % build the refX that describes the infinite reference
    XRefX = make_inf_xrefX(C#cellref.path, C#cellref.text),
    Infinites = get(infinite),
    NewInfinites = ordsets:add_element({local, XRefX}, Infinites),
    put(infinite, NewInfinites),
    {zeds, Vals, NoMatch, Err};
fetch(#zrangeref{zpath = Z, rangeref = R}, ValType, false)
  when is_record(R, rangeref) ->
    {zpath, ZList} = Z,
    ZP = make_zpath(ZList, []),
    NewPath = muin_util:walk_zpath(?mpath, ZList),
    PageTree = page_srv:get_pages(?msite),
    {MPaths, NoMatch, Err} = match(?msite, PageTree, NewPath),
    {XRefXs, Ranges} = fetch_ranges(MPaths, ZP, R, ValType),
    Infinites = get(infinite),
    Fun = fun(X, Acc) ->
                  ordsets:add_element({local, X}, Acc)
          end,
    NewInfinites = lists:foldl(Fun, Infinites, lists:flatten(XRefXs)),
    put(infinite, NewInfinites),
    {zeds, Ranges, NoMatch, Err};
fetch(N, _ValType, _Override) when ?is_namedexpr(N) ->
    ?ERRVAL_NAME;
fetch(#cellref{col = Col, row = Row, path = Path}, ValType, Override) ->
    RowIndex = row_index(Row),
    ColIndex = col_index(Col),
    xx6(),
    do_cell(Path, RowIndex, ColIndex, finite, ValType, Override);
fetch(#rangeref{type = Type, path = Path} = Ref, ValType, Oride)
  when Type == row orelse Type == col ->
    NewPath = muin_util:walk_path(?mpath, Path),
    #refX{obj = Obj} = RefX = muin_util:make_refX(?msite, NewPath, Ref),
    Infinites = get(infinite),
    put(infinite, ordsets:add_element(RefX,Infinites)),
    Refs = new_db_wu:expand_ref(RefX),
    Rows = case Obj of
               {T2, {_I, _I}} -> sort1D(Refs, Path, T2, ValType, Oride);
               {T2, {I,   J}} -> sort2D(Refs, Path, {T2, I, J}, ValType, Oride)
           end,
    % pinch out the functionality for a release
    {range, Rows};
% error_logger:info_msg("Somebody tried a row or column rangeref~n"),
% ?ERRVAL_ERR;
fetch(#rangeref{type = finite} = Ref, ValType, Override) ->
    CellCoords = muin_util:expand_cellrange(Ref),
    Fun1 = fun(CurrRow, Acc) -> % Curr row, result rows
                   Fun2 = fun({_, Y}) ->
                                  Y == CurrRow
                          end,
                   Fun3 = fun({X, Y}) ->
                                  xx7(),
                                  do_cell(Ref#rangeref.path, Y, X,
                                          finite, ValType, Override)
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

    Child  = #refX{site=MSite, type = url, path=NewMPath, obj={cell, {MX, MY}}},
    Parent = #refX{site=RSite, type = url, path=NewRPath, obj={cell, {RX, RY}}},

    % Yup it is not implemented...
    case new_db_api:read_incoming_hn(Parent, Child) of

        {error,permission_denied} ->
            {{errval,'#AUTH'},[],[],[]};

        {Val, DepTree} ->
            F = fun({url, [{type, Type}], [Url2]}) ->
                        % yeah parse_url is now deprecated...
                        Ref = hn_util:parse_url(Url2),
                        #refX{site = S, type = url, path = P,
                              obj = {cell, {X, Y}}} = Ref,
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
do_cell(RelPath, Rowidx, Colidx, Type, ValType, OverrideCantInc) ->
    Path = muin_util:walk_path(?mpath, RelPath),

    FetchFun = fun() ->
                       get_cell_info(?msite, Path, Colidx, Rowidx, Type,
                                     ValType, OverrideCantInc)
               end,
    case ?mar of
        nil   -> get_value_and_link(FetchFun);
        _Else -> case auth_srv:get_any_main_view(?msite, Path, ?mar) of
                     {view, _} -> get_value_and_link(FetchFun);
                     _Other    -> exit("This is a security bug"),
                                  ?ERR_AUTH
                 end
    end.

%% @doc Calls supplied fun to get a cell's value and dependence information,
%% saves the dependencies (linking it to current cell), and returns
%% the value to the caller (to continue the evaluation of the formula).
get_value_and_link(FetchFun) ->
    {Value, Errs, Refs} = FetchFun(),
    % this checks for DIRECT circular references where a formula
    % refers to itself (ie =A1 in cell A1). INDIRECT circular references
    % (eg =A2 in cell A1 and =A1 in cell A2) are found in dbsrv.erl
    % the process dictionary entry 'oldcontextpath' only exists
    % if this is a z-reference so check if it exists and then
    % look for a circular reference against a the original path
    % not the current one
    {MPath, MX, MY} = case get(oldcontextpath) of
                          undefined -> {?mpath, ?mx, ?my};
                          OldCP     -> {OldCP, get(oldcontextcol),
                                        get(oldcontextrow)}
                                  end,
    case Refs of
        [{_, _, {xrefX, _, _, MPath, {cell, {MX, MY}}}}] ->
            put(circref, true);
        _ ->
            ok
    end,
    {Errs0, Refs0} = get(retvals),
    put(retvals, {Errs0 ++ Errs, Refs0 ++ Refs}),
    Value.

%% Row or Col information --> index.
toidx(N) when is_number(N) -> N;
toidx({row, Offset})       -> ?my + Offset;
toidx({col, Offset})       -> ?mx + Offset.

get_cell_info(S, P, Col, Row, Type, ValType, OverrideCantInclude) ->
    RefX = #refX{site = string:to_lower(S), path = P, type = url,
                 obj = {cell, {Col, Row}}},
    new_db_wu:get_cell_for_muin(RefX, Type, ValType, OverrideCantInclude).

sort1D(Refs, Path, Type, ValType, Override) ->
    sort1D_(Refs, Path, Type, orddict:new(), ValType, Override).

sort1D_([], _Path, Type, Dict, _ValType, _Override) ->
    Size = orddict:size(Dict),
    List = orddict:to_list(Dict),
    Filled = fill1D(List, 1, 1, Size + 1, [],
                    'to-last-key'),
    % if it is row then you need to flatten the
    % List by one degree and wrap it in a list
    % (ie a 2d transposition)
    case Type of
        column -> Filled;
        row    -> [[X || [X] <- Filled]]
    end;
sort1D_([#xrefX{obj = {cell, {X, Y}}} | T], Path, row, Dict, ValType, Oride) ->
    xx8(),
    V = do_cell(Path, Y, X, infinite, ValType, Oride),
    sort1D_(T, Path, row, orddict:append(X, V, Dict), ValType, Oride);
sort1D_([#xrefX{obj = {cell, {X, Y}}} | T], Path, column, Dict, ValType, Oride) ->
    xx9(),
    V = do_cell(Path, Y, X, infinite, ValType, Oride),
    sort1D_(T, Path, column, orddict:append(Y, V, Dict), ValType, Oride).

%% if all the cells are blank will return an array of arrays.
%% if type is 'column' this will be one array for each column each with
%% a 'blank' in it
%% if type is a 'row' this will be one list with as many 'blank's as there
%% are rows specified
sort2D(Refs, Path, Def, ValType, Override) ->
    sort2D_(Refs, Path, Def, orddict:new(), ValType, Override).

sort2D_([], _Path, {Type, Start, End}, Dict, _ValType, _Override) ->
    Ret = case {Type, orddict:size(Dict)} of
              {row, 0}    -> [lists:duplicate(End - Start, blank)];
              {column, 0} ->  lists:duplicate(End - Start, [blank]);
              {_, _}      -> fill2D(Dict, Type, Start, End, [])
          end,
    Ret;
sort2D_([#xrefX{obj = {cell, {X, Y}}} | T], Path, Def, Dict, ValType, Oride) ->
    SubDict = case orddict:is_key(X, Dict) of
                  true  -> orddict:fetch(X, Dict);
                  false -> orddict:new()
              end,
    xx10(),
    V = do_cell(Path, Y, X, infinite, ValType, Oride),
    NewSub  = orddict:append(Y, V, SubDict),  % works because there are no dups!
    NewDict = orddict:store(X, NewSub, Dict),
    sort2D_(T, Path, Def, NewDict, ValType, Oride).

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
                     List = orddict:to_list(SubDict),
                     [{MyMax, _} | _T] = lists:reverse(List),
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

match(Site, PageTree, ZPath) ->
    Segments = dh_tree:segments_below([], PageTree),
    m1(Site, PageTree, Segments, ZPath, [], [], [], []).

%% this clause terminates the second iteration
%% we have run out of ZSegs and Segs to test and all have passed
m1(_Site, _PageTree, [], [], Htap, Match, NoMatch, Err) ->
    {[lists:reverse(Htap) | Match], NoMatch, Err};
%% this clause terminates the first iteration
%% we have run out of segments at a particular level to test
m1(_Site, _PageTree, [], _ZPath, Htap, Match, NoMatch, Err) ->
    {Match, [{nomatch, lists:reverse(Htap)} | NoMatch], Err};
%% this clause terminates the first iteration
%% we still have subtrees but there the Segs/ZSegs are not deep enough
m1(_Site, _PageTree, _Segs, [], Htap, Match, NoMatch, Err) ->
    {[lists:reverse(Htap) | Match],  NoMatch, Err};
m1(Site, PageTree, [H | T], [ZH | ZT], Htap, Match, NoMatch, Err) ->
    {NewM, NewNM, NewE} =
        case m2(Site, H, ZH, Htap) of
            {partmatch, Path}  ->
                Segs = dh_tree:segments_below(Path, PageTree),
                m1(Site, PageTree, Segs, ZT, [H | Htap], Match,
                   NoMatch, Err);
            {nomatch, Path} ->
                {Match, [{nomatch, Path} | NoMatch], Err};
            {error, Path, V} ->
                {Match, NoMatch, [{error, Path, V} | Err]}
        end,
    m1(Site, PageTree, T, [ZH | ZT], Htap, NewM, NewNM, NewE).

m2(_Site,  S, {seg, S}, Htap)     ->
    {partmatch, lists:reverse([S | Htap])};
m2(_Site, S, {seg, _}, Htap)     ->
    {nomatch, lists:reverse([S | Htap])};
m2(Site,  S, {zseg, Z, _}, Htap) ->
    case zeval(Site, lists:reverse([S | Htap]), Z) of
        match        -> {partmatch, lists:reverse([S | Htap])};
        nomatch      -> {nomatch,   lists:reverse([S | Htap])};
        {error, Val} -> {error,     lists:reverse([S | Htap]), Val}
    end.

%% used in the zinf server and the api evaluator.
% Neither has any context to execute at this stage!
external_zeval(Site, Path, Toks) ->
    % zeval evaluates an expression in a cell context - but that cell
    % cannot actually exists - so use {0,0} which is the cell 1 up and 1
    % right of A1
    % NOTE that we create an xrefX{} with a fake idx this xrefX{}
    % doesn't go anywhere near the database
    XRefX = #xrefX{idx = 0, site = Site, path = Path, obj = {cell, {0, 0}}},
    Return = zeval2(XRefX, Toks),
    {Return, get(circref)}.

zeval(Site, Path, Toks) ->
    X = ?mx,
    Y = ?my,
    % NOTE that we create an xrefX{} with a fake idx this xrefX{}
    % doesn't go anywhere near the database
    XRefX = #xrefX{idx = 0, site = Site, path = Path, obj = {cell, {X, Y}}},
    zeval2(XRefX, Toks).

%% the execution context for expressions is stored in the process dictionary
%% so here you need to rip it out and then stick it back in
%% (not good, Damn you Hasan!).
zeval2(XRefX, Toks) ->
    % set the oldcontext path
    put(oldcontextpath, ?mpath),
    put(oldcontextrow, ?my),
    put(oldcontextcol, ?mx),
    % capture the process dictionary (it will get gubbed!)
    OldContext = get(),
    % no array context (fine) or security context (erk!)
    RTI = new_db_wu:xrefX_to_rti(XRefX, nil, false),
    {Return, CircRef} =
        case catch(xfl_parser:parse(Toks)) of
            {ok, Ast} ->
                {ok, {_, Rs, _, _, _, CR, _}} = muin:run_code(Ast, RTI),
                % cast to a boolean
                Rs2 = typechecks:std_bools([Rs]),
                case Rs2 of
                    [true]  -> {match, CR};
                    [false] -> {nomatch, CR};
                    Val     -> {{error, Val}, CR}
                end;
            ?syntax_err ->
                {?error_in_formula, false}
        end,
    % restore the process dictionary (fugly! fugly! fugly!)
    {_Errs, ZRetVals} = get(retvals),
    [put(K, V) || {K, V} <- OldContext],
    erase(oldcontextpath),
    erase(oldcontextrow),
    erase(oldcontextcol),
    % need to flag up the circular reference (which you have just erased)
    case CircRef of
        true  -> put(circref, true);
        false -> ok
    end,
    % also add in the missing parents from the sub-context
    {Errs, OrigRetVals} = get(retvals),
    put(retvals, {Errs, ZRetVals ++ OrigRetVals}),
    Return.

make_blank_infinites([], _ZPath, Acc) ->
    Acc;
make_blank_infinites([{cell, {X, Y}} | T], ZPath, Acc) ->
    Cell = tconv:to_b26(X) ++ integer_to_list(Y),
    NewAcc1 = make_inf_xrefX(ZPath, Cell),
    make_blank_infinites(T, ZPath, [NewAcc1 | Acc]).

fetch_ranges([], ZPath, Range, _ValType) ->
    ExpRange = expand(Range),
    {make_blank_infinites(ExpRange, ZPath, []), []};
fetch_ranges(Paths, ZPath, Range, ValType) ->
    ExpRange = expand(Range),
    fetch_r1(Paths, ExpRange, ZPath, ValType, [], []).

fetch_r1([], _Ranges, _ZPath, _ValType, Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
fetch_r1([H | T], Ranges, ZPath, ValType, Acc1, Acc2) ->
    Path = hn_util:list_to_path(H),
    {NewAcc1, NewAcc2} = fetch_r2(Ranges, Path, ZPath, ValType, [], []),
    fetch_r1(T, Ranges, ZPath, ValType, [NewAcc1 | Acc1], [NewAcc2 | Acc2]).

fetch_r2([], _Path, _ZPath, _ValType, Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
fetch_r2([{cell, {X, Y}} | T], Path, ZPath, ValType, Acc1, Acc2) ->
    Cell = tconv:to_b26(X) ++ integer_to_list(Y),
    NewAcc1 = make_inf_xrefX(ZPath, Cell),
    URL = {Path, Cell},
    xx11(),
    NewAcc2 = do_cell(Path, Y, X, infinite, ValType, false),
    fetch_r2(T, Path, ZPath, ValType, [NewAcc1 | Acc1],
             [{URL, NewAcc2} | Acc2]).

fetch_vals(Paths, Row, Col, ValType) -> fetch_v1(Paths, Row, Col, ValType, []).

fetch_v1([], _Row, _Col, _ValType, Acc) -> lists:reverse(Acc);
fetch_v1([H | T], Row, Col, ValType, Acc) ->
    Path = hn_util:list_to_path(H),
    RowIndex = row_index(Row),
    ColIndex = col_index(Col),
    URL = {H, tconv:to_b26(ColIndex) ++ integer_to_list(RowIndex)},
    xx12(),
    NewAcc = do_cell(Path, RowIndex, ColIndex, infinite, ValType, false),
    fetch_v1(T, Row, Col, ValType, [{URL, NewAcc} | Acc]).

make_inf_xrefX(Path, Text) ->
    NewPath = muin_util:walk_path(?mpath, Path),
    Segs = hn_util:path_tokens(Text),
    {_, Ref} = lists:split(length(Segs) - 1, Segs),
    Obj = hn_util:parse_ref(Ref),
    RefX = #refX{site = ?msite, type = gurl, path = NewPath, obj = Obj},
    new_db_wu:refX_to_xrefX_createD(RefX).

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
                                 io:format("Success Expr is ~p Fla is ~p "++
                                           "Trans is ~p~n"++
                                           "Toks is ~p~nAst is ~p~n"++
                                           "Status is ~p~n",
                                           [X, Fla, Trans, Toks, Ast, "Ok"]),
                                 {ok, Ast};
                             O2         ->
                                 io:format("Parse fail: "++
                                           "Expr is ~p Fla is ~p Trans is ~p~n"
                                           ++ "Toks is ~p~nStatus is ~p~n",
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

expand(#rangeref{tl = {{_, C1}, {_, R1}}, br = {{_, C2}, {_, R2}}}) ->
    X = ?mx,
    Y = ?my,
    X1 = X + C1,
    Y1 = Y + R1,
    X2 = X + C2,
    Y2 = Y + R2,
    expand2(X1, X1, Y1, X2, Y2, []).

expand2(_, X, Y, X, Y, Acc) ->
    lists:reverse([{cell, {X, Y}} | Acc]);
expand2(StartX, X, Y1, X, Y2, Acc) ->
    NewAcc = [{cell, {X, Y1}} | Acc],
    expand2(StartX, StartX, Y1 + 1, X, Y2, NewAcc);
expand2(StartX, X1, Y1, X2, Y2, Acc) ->
    NewAcc = [{cell, {X1, Y1}} | Acc],
    expand2(StartX, X1 + 1, Y1, X2, Y2, NewAcc).

make_zpath([], Acc) ->
    [Hd | List] = lists:reverse(Acc),
    case Hd of
        "."  -> "." ++ hn_util:list_to_path(List);
        ".." -> ".." ++ hn_util:list_to_path(List);
        _    -> hn_util:list_to_path([Hd | List])
    end;
make_zpath([{seg, Seg} | T], Acc) ->
    make_zpath(T, [Seg | Acc]);
make_zpath([{zseg, _, ZSeg} | T], Acc) ->
    make_zpath(T, [ZSeg | Acc]).

init_proc_dict(#muin_rti{site = Site, path = Path, col = Col,
                         row = Row, idx = Idx,array_context = AryCtx,
                         auth_req = AuthReq}) ->
    % Populate the process dictionary.
    lists:map(fun({K,V}) -> put(K, V) end,
              [{site, Site}, {path, Path}, {x, Col}, {y, Row},
               {idx, Idx},
               {array_context, AryCtx}, {infinite, []},
               {retvals, {[], []}}, {recompile, false},
               {auth_req, AuthReq},
               {circref, false},
               {selfreference, false}]),
    ok.

special_flatten([], Acc)                      -> lists:reverse(Acc);
special_flatten([H | T], Acc) when is_list(H) -> NewAcc = special_f2(H, Acc),
                                                 special_flatten(T, NewAcc);
special_flatten([H | T], Acc)                 -> special_flatten(T, [H | Acc]).

special_f2([], Acc)      -> Acc;
special_f2([H | T], Acc) -> special_f2(T, [H | Acc]).

% these funs are just to enable to me track different paths through
% muin when running cprof - bit of a perf testing artefact
xx1() -> ok.
xx2() -> ok.
xx3() -> ok.
xx4() -> ok.
xx5() -> ok.
xx6() -> ok.
xx7() -> ok.
xx8() -> ok.
xx9() -> ok.
xx10() -> ok.
xx11() -> ok.
xx12() -> ok.

