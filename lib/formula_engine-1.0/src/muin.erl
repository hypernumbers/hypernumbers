%%% @doc        Interface to the formula engine. Also, the interpreter.
%%% @author     Hasan Veldstra <hasan@hypernumbers.com>

%%% Formulas are currently compiled into simple sexps.

-module(muin).
-export([compile/1, run/2, update/2]).

-include("spriki.hrl").
-include("builtins.hrl").
-include("handy_macros.hrl").

-import(muin_util, [expand_cellrange/2, fdbg/1, fdbg/2, get_frontend/0,
                    getxy/1, init/1, join/1, join/2, just_path/1,
                    just_ref/1, split_ssref/1, walk_path/2]).

-import(tconv, [to_b26/1, to_i/1, to_i/2, to_s/1]).

%% These are read-only.
-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).

%% Guard for eval() and plain_eval().
-define(IS_FUNCALL(X),
        is_atom(X) andalso X =/= true andalso X =/= false).

-define(NEED_BINDINGS,
        [sscellref, ssrcref, rcrelref, ':', hn, hypernumber, isref, cell]).

%%%--------------------%%%
%%%  Public functions  %%%
%%%--------------------%%%

%%% @doc Parses formula, and returns AST as an s-expression.
%%% @spec parse(Formula :: string()) -> {ok, {Ast :: list()}}
compile(Fla) ->
    case (catch try_parse(Fla)) of
        {ok, Pcode} ->
            {ok, Pcode};
        _Else ->
            {error, error_in_formula}
    end.

try_parse(Fla) ->
    FlaTr = translator:do(Fla),
    io:format("~s~n", [FlaTr]),
    {LexMod, ParseMod} = get_frontend(),
    {ok, Tokens, _} = LexMod:string(FlaTr),
    {ok, _Ast} = ParseMod:parse(Tokens). % Match to enforce the contract.
    

%% @doc Runs compiled formula.
run(Pcode, Bindings) ->
    %% Populate the process dictionary.
    foreach(fun({K, V}) -> put(K, V) end,
            Bindings ++ [{retvals, {[], [], []}}]),
             
    case (catch eval(Pcode, Bindings)) of
        {error, R} ->
            {error, R};
        Value ->
            {RefTree, Errors, References} = get(retvals),
            {ok, {Value, RefTree, Errors, References}}
    end.


%%% @doc Updates formula for structural change.
%%% @spec update(Formula::string(), UpdateMsg::tuple()) ->
%%%            {ok, {NewFormula :: string(), Ast :: list()}}
update(Formula, UpdateMsg) ->
    {ok, NewFormula} = muin_supd:do(Formula, UpdateMsg),
    {ok, Pcode} = compile(NewFormula),
    {ok, {NewFormula, Pcode}}.


%%%---------------------%%%
%%%  Private functions  %%%
%%%---------------------%%%

%% @doc Evaluates an s-expression, pre-processing subexps as needed.
eval([Fun__ | Args__], Bindings) when ?IS_FUNCALL(Fun__) ->
    %% Transform s-exp if needed.
    [Fun | Args] = preproc([Fun__ | Args__], Bindings),

    %% Evaluate args first.
    CallArgs = [eval(X, Bindings) || X <- Args],

    %% And then evaluate the whole s-exp.
    %% Some functions need access to bindings, some don't...
    ?COND(member(Fun, ?NEED_BINDINGS),
          funcall(Fun, CallArgs, Bindings),
          funcall(Fun, CallArgs));

eval(Value, _Bindings) ->
    Value.


%% @doc Transforms certain types of s-exps. @end

%% Arguments to ':' calls are either tuples (literals) or lists (funcalls).
%% Not checking if funcalls are to INDIRECT as it'll fail later if they aren't.
preproc([':', StartExpr, EndExpr], Bindings) ->
    Eval = fun(Arg) when is_list(Arg) ->
                   Cellref = hd(plain_eval(tl(Arg), Bindings)),
                   {sscellref, "./" ++ Cellref};
              (Arg) when is_tuple(Arg) ->
                   Arg
           end,

    [':', Eval(StartExpr), Eval(EndExpr)];

%% This clause is for standalone calls to INDIRECT. Ones that are part of
%% a range construction expression will be caught by the clause above.
preproc([indirect, Arg], Bindings) ->
    RefStr = plain_eval(Arg, Bindings),
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
preproc(Sexp, _Bindings) ->
    Sexp.


%% @doc Same as eval() but doesn't pre-process.
plain_eval([Fun | Args], Bindings) when ?IS_FUNCALL(Fun) ->
    CallArgs = [plain_eval(X, Bindings) || X <- Args],
    
    ?COND(member(Fun, ?NEED_BINDINGS),
          funcall(Fun, CallArgs, Bindings),
          funcall(Fun, CallArgs));

plain_eval(Value, _Bindings) ->
    Value.


%%% ----- Functions defined in spriki_funs or userdef.

%% Picks a module name, calls
funcall(Fname, Args) ->
    %% Find module name corresponding to function; default to userdef.
    [Modname | _] = foldl(fun({M, L}, Acc) ->
                                  ?COND(member(Fname, L), [M], Acc)
                          end,
                          [userdef],
                          ?STDFUNS),
    Modname:Fname(Args).

%%% ----- Reference functions.

funcall(sscellref, [Ssref], Bindings) ->
    {Path_, Ref} = split_ssref(Ssref),
    Path = walk_path(?mpath, Path_),
    do_cell(Path, Ref, Bindings);

funcall(ssrcref, [{RelPath, Row, Col}], Bindings) ->
    Path = walk_path(?mpath, RelPath),
    do_cell(Path, Row, Col, Bindings);

funcall(rcrelref, [{RelPath, RowOffset, ColOffset}], Bindings) ->
    Path = walk_path(?mpath, RelPath),
    do_cell(Path, ?my + RowOffset, ?mx + ColOffset, Bindings);

%% -- Range functions.

%% Cellranges, e.g. A1:B10, ../page/A1:B10 etc.
funcall(':', [{sscellref, Ref1}, {cellref, Ref2}], Bindings) ->
    [do_cell(just_path(Ref1), X, Bindings) ||
        X <- expand_cellrange(just_ref(Ref1), Ref2)];

%% Column ranges.
funcall(':', [{sscolref, Ref1}, {col, Ref2}], Bindings) ->
    Path = walk_path(?mpath, just_path(Ref1)),
    Cols = seq(to_i(just_ref(Ref1), b26), to_i(Ref2, b26)),
    %% generate a fun to read each column
    Funs = [fun() -> db:read_column(?msite, Path, X) end || X <- Cols],
    do_cells(Funs, Bindings);

%% Row ranges.
funcall(':', [{ssrowref, Ref1}, {row, Ref2}], Bindings) ->
    FullPath = walk_path(?mpath, just_path(Ref1)),
    Rows = seq(to_i(just_ref(Ref1)), to_i(Ref2)),
    Funs = [?fun0(db:read_row(?msite, FullPath, X)) || X <- Rows],
    do_cells(Funs, Bindings);

%% -- Hypernumber function and its shorthand.

funcall(hypernumber, [Url_], Bindings) ->
    %% Remove trailing ?hypernumber if needed.
    {ok, Url, _} = regexp:gsub(Url_, "\\?hypernumber$", ""),
    
    #page{site = RSite, path = RPath,
          ref = {cell, {RX, RY}}} = hn_util:parse_url(Url),

    get_value_and_link(fun() ->
                               spriki:get_hypernumber(?msite, ?mpath,
                                                      ?mx, ?my, Url,
                                                      RSite, RPath, RX, RY)
                       end);

funcall(hn, [Url], Bindings) ->
    funcall(hypernumber, [Url], Bindings);

%%% -- Information functions

%% Unsupported info types: color, filename, parentheses (WTF?), prefix, width.

funcall(cell, ["address", Ref], _Bindings) ->
    Ref;
funcall(cell, ["col", Ref], _Bindings) ->
    {_Row, Col} = getxy(Ref),
    Col;
funcall(cell, ["contents", Ref], Bindings) ->
    Val = do_cell(".", Ref, Bindings),
    ?COND(Val == blank, 0, Val);
funcall(cell, ["format", _Ref], _Bindings) ->
    throw(tantrum); %% TODO: Implement me.
funcall(cell, ["protect", _Ref], _Bindings) ->
    throw(tantrum); %% TODO: Implement me.
funcall(cell, ["row", Ref], _Bindings) ->
    {Row, _Col} = getxy(Ref),
    Row;
funcall(cell, ["type", Ref], Bindings) ->
    Val = do_cell(".", Ref, Bindings),
    ?COND(Val == blank, "b",
          ?COND(is_binary(Val), "l",
                "v"));

funcall(info, _, _Bindings) ->
    throw({error, unsupported}); %% TODO: What to do with this?

funcall(isref, [_MaybeRef], _Bindings) ->
    throw(tantrum). %% TODO: Implement me. With cellref, range, and name support.


%%% ----- Utility functions.

%% Returns value in the cell + get_value_and_link() is called behind the
%% scenes.
do_cell(RelPath, Ref, Bindings) ->
    {X, Y} = getxy(Ref),
    
    ?IF(X == ?mx andalso Y == ?my,
        throw({error, self_reference})),

    Path = walk_path(?mpath, RelPath),
    FetchFun = ?fun0(spriki:calc(?msite, Path, X, Y)),
    get_value_and_link(FetchFun).

do_cell(Path, Row, Col, Bindings) ->
    do_cell(Path, to_b26(Col) ++ to_s(Row), Bindings).

%% @doc Takes a list of funs that fetch a list of cell records each.
%% Calls do_cell() on each of those cells.
%% TODO: The records already contain all required information, don't really
%% need to call do_cell().
do_cells(Funs, Bindings) ->
    DoRec = fun(CellRec) ->
                    do_cell((CellRec#spriki.index)#index.path,
                            (CellRec#spriki.index)#index.row,
                            (CellRec#spriki.index)#index.column,
                            Bindings)
            end,
    
    map(fun(GetCellRecFun) ->
                map(DoRec, GetCellRecFun())
        end,
        Funs).

%% @doc Calls supplied fun to get a cell's value and dependence information,
%% saves the dependencies away (linking it to current cell), and returns
%% the value to the caller (to continue the evaluation of the formula).
get_value_and_link(FetchFun) ->
    {Value, RefTree, Errs, Refs} = FetchFun(),

    ?IF(member({?msite, ?mpath, ?mx, ?my}, RefTree),
        throw({error, circular_reference})),

    {RefTree0, Errs0, Refs0} = get(retvals),
    put(retvals, {RefTree0 ++ RefTree, Errs0 ++ Errs, Refs0 ++ Refs}),
    Value.
