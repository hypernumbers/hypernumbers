%%% @doc        Interface to the formula engine. Also, the interpreter.
%%% @author     Hasan Veldstra <hasan@hypernumbers.com>

-module(muin).
-export([parse/1, run/2, update/2]).
-compile(export_all).

-include("spriki.hrl").
-include("builtins.hrl").
-include("handy_macros.hrl").

-import(muin_util, [expand_cellrange/2, fdbg/1, fdbg/2, get_frontend/0, getxy/1,
                    init/1, join/1, join/2, just_path/1, just_ref/1, puts/1,
                    walk_path/2]).

-import(tconv, [to_b26/1, to_i/1, to_i/2, to_s/1]).

%% Bind values from Bindings to vars in current scope. Vars repeated after
%% creation to remove "Unused variable" warnings.
-define(CREATE_BINDINGS_VARS,
        MSite = ?KEYSEARCH(site, Bindings), MSite,
        MPath = ?KEYSEARCH(path, Bindings), MPath,
        MX    = ?KEYSEARCH(x, Bindings), MX,
        MY    = ?KEYSEARCH(y, Bindings), MY).

%% Guard for eval() and plain_eval().
-define(IS_FUNCALL(X),
        is_atom(X) andalso X =/= true andalso X =/= false).

-define(NEED_BINDINGS,
        [sscellref, ssrcref, ':', hn, hypernumber]).

%%%--------------------%%%
%%%  Public functions  %%%
%%%--------------------%%%

%%% @doc Parses formula, and returns AST as an s-expression.
%%% @spec parse(Formula :: string()) -> {ok, {Ast :: list()}}
parse(Formula) ->
    {LexMod, ParseMod} = get_frontend(),
    {ok, Tokens, _} = erlang:apply(LexMod, string, [Formula]),
    {ok, Ast} = erlang:apply(ParseMod, parse, [Tokens]),
    {ok, Ast}.


%% @doc Runs the s-expression.
%% TODO: Move the interpreter to muin_interpreter, which will also be
%% spawnable?
%% Otherwise, I can haz problems with concurrency?
run(Ast, Bindings) ->
    put(retvals, {[], [], []}),
    Value = eval(Ast, Bindings),
    {RefTree, Errors, References} = get(retvals),
    {ok, {Value, RefTree, Errors, References}}.


%%% @doc Updates formula for structural change.
%%% @spec update(Formula::string(), UpdateMsg::tuple()) ->
%%%            {ok, {NewFormula :: string(), Ast :: list()}}
update(Formula, UpdateMsg) ->
    {ok, NewFormula} = muin_supd:do(Formula, UpdateMsg),
    {ok, Ast} = parse(NewFormula),
    {ok, {NewFormula, Ast}}.


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
            [':', {cellref, Ref1}, {cellref, Ref2}];
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

funcall(Fun, Args) ->
    Modname = ?COND(member(Fun, ?BUILT_IN_FUNCTIONS), stdfuns, userdef),
    erlang:apply(Modname, Fun, (case length(Args) of
                                    0 -> [];
                                    1 -> [hd(Args)];
                                    2 -> [hd(Args), last(Args)];
                                    _ -> [Args]
                                end)).


%%% ----- Reference functions.

funcall(sscellref, [Ssref], Bindings) ->
    ?CREATE_BINDINGS_VARS,
    Path = walk_path(MPath, just_path(Ssref)),
    Ref = just_ref(Ssref),
    do_cell(Path, Ref, Bindings);

funcall(ssrcref, [{RelPath, Row, Col}], Bindings) ->
    ?CREATE_BINDINGS_VARS,
    Path = walk_path(MPath, RelPath),
    do_cell(Path, Row, Col, Bindings);

%% -- Range functions.

%% Cellranges, e.g. A1:B10, ../page/A1:B10 etc.
funcall(':', [{sscellref, Ref1}, {cellref, Ref2}], Bindings) ->
    [do_cell(just_path(Ref1), X, Bindings) ||
        X <- expand_cellrange(just_ref(Ref1), Ref2)];

%% These two clauses are for preproc'd ranges. Redirect to the one above.
%% TODO: Cut if possible.
funcall(':', [{cellref, Ref1}, {cellref, Ref2}], Bindings) ->
    funcall(':', [{sscellref, "./" ++ Ref1}, {cellref, Ref2}], Bindings);

funcall(':', [{sscellref, Ref1}, {sscellref, Ref2}], Bindings) ->
    funcall(':', [{sscellref, Ref1}, {cellref, just_ref(Ref2)}], Bindings);

%% Column ranges.
funcall(':', [{sscolref, Ref1}, {col, Ref2}], Bindings) ->
    ?CREATE_BINDINGS_VARS,
    Path = walk_path(MPath, just_path(Ref1)),
    Cols = seq(to_i(just_ref(Ref1), b26), to_i(Ref2, b26)),
    Funs = [?FUN(db:read_column(MSite, Path, X)) || X <- Cols],
    do_cells(Funs, Bindings);

%% Row ranges.
funcall(':', [{ssrowref, Ref1}, {row, Ref2}], Bindings) ->
    ?CREATE_BINDINGS_VARS,
    FullPath = walk_path(MPath, just_path(Ref1)),
    Rows = seq(to_i(just_ref(Ref1)), to_i(Ref2)),
    Funs = [?FUN(db:read_row(MSite, FullPath, X)) || X <- Rows],
    do_cells(Funs, Bindings);

%% -- Hypernumber function and its shorthand.

funcall(hypernumber, [Url_], Bindings) ->
    ?CREATE_BINDINGS_VARS,

    %% Remove trailing ?hypernumber if needed.
    {ok, Url, _} = regexp:gsub(Url_, "\\?hypernumber$", ""),
    
    #page{site = RSite, path = RPath, ref = {cell, {RX, RY}}} =
        hn_util:parse_url(Url),

    fetch_update_return(?FUN(spriki:get_hypernumber(MSite, MPath, MX, MY, Url,
                                                    RSite, RPath, RX, RY)));


funcall(hn, [Url], Bindings) ->
    funcall(hypernumber, [Url], Bindings).


%%% ----- Utility functions.

%% Returns value in the cell + fetch_update_return() is called behind the
%% scenes.
do_cell(RelPath, Ref, Bindings) ->
    ?CREATE_BINDINGS_VARS,
    {X, Y} = getxy(Ref),
    Path = walk_path(MPath, RelPath),
    FetchFun = ?FUN(spriki:calc(MSite, Path, X, Y)),
    fetch_update_return(FetchFun).

do_cell(Path, Row, Col, Bindings) ->
    do_cell(Path, to_b26(Col) ++ to_s(Row), Bindings).

%% @doc Takes a list of funs that fetch a list of cell records each.
%% Calls do_cell() on each of those cells.
%% TODO: The records already contain all required information, don't really
%% need to call do_cell().
do_cells(Funs, Bindings) ->
    map(fun(GetCellRecFun) ->
                map(fun(Rec) ->
                            do_cell((Rec#spriki.index)#index.path,
                                    (Rec#spriki.index)#index.row,
                                    (Rec#spriki.index)#index.column,
                                    Bindings)
                    end,
                    GetCellRecFun())
        end,
        Funs).



%% @doc Calls supplied fun to get value and dependencies, stashes dependencies
%% away, and returns the value.
fetch_update_return(FetchFun) ->
    {Value, RefTree, Errors, References} = FetchFun(),

    {RefTree0, Errors0, References0} = get(retvals),
    put(retvals,
        {RefTree0 ++ RefTree, Errors0 ++ Errors, References0 ++ References}),

    Value.
