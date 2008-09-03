%%% @doc Interface to the formula engine and the interpreter.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(muin).
-export([compile/2, run_formula/2, run_code/2]).

-compile(export_all).

-include("spriki.hrl").
-include("builtins.hrl").
-include("handy_macros.hrl").
-include("typechecks.hrl").

-import(tconv, [to_b26/1, to_i/1, to_i/2, to_s/1]).
-import(muin_util, [attempt/3]).

-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).

%% Guard for eval() and plain_eval().
-define(isfuncall(X),
        is_atom(X) andalso X =/= true andalso X =/= false).

-define(puts, io:format).

%%% PUBLIC ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @spec compile(string(), {integer(), integer()})
%% @doc Compiles a formula against a cell.
compile(Fla, {X, Y}) ->
    case attempt(?MODULE, try_parse, [Fla, {X, Y}]) of
        {ok, {ok, Pcode}} ->
            {ok, Pcode};
        {error, Reason} ->
            error_logger:error_msg("muin:compile ~p~n", [Reason]),
            {error, error_in_formula}
    end.

%% @doc Runs formula given as a string.
run_formula(Fla, Bindings) ->
    {cell, {X, Y}} = Bindings#ref.ref,
    case compile(Fla, {X, Y}) of
        {ok, Ecode} ->
            case muin:run_code(Ecode, Bindings) of
                {ok, {Val, Deptree, _, Parents, Recompile}} ->
                    {Ecode, Val, Deptree, Parents, Recompile};
                %% FIXME: This is an ugly fix, and it's wrong.
                {error, {errval, Errval}} ->
                    {Ecode, {errval, Errval}, [], [], false};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, error_in_formula} ->
            {error, error_in_formula}
    end.

%% @doc Runs compiled formula.
run_code(Pcode, #ref{site = Site, path = Path, ref = {cell, {X, Y}}}) ->
    %% Populate the process dictionary.
    map(fun({K,V}) -> put(K, V) end,
        [{site, Site}, {path, Path}, {x, X}, {y, Y},
         {retvals, {[], [], []}}, {recompile, false}]),

    case attempt(?MODULE, eval, [Pcode]) of
        {ok, Val} ->
            {RefTree, Errors, References} = get(retvals),
            Val2 = ?COND(Val == blank, 0, Val), % Links to blanks become 0.
            {ok, {Val2, RefTree, Errors, References, get(recompile)}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try_parse(Fla, {X, Y}) ->
    Trans = translator:do(Fla),
    {ok, Toks} = xfl_lexer:lex(Trans, {X, Y}),
    {ok, _Ast} = xfl_parser:parse(Toks). % Match to enforce the contract.

%% @doc Evaluates an s-expression, pre-processing subexps as needed.
eval(Node = [Func|Args]) when ?isfuncall(Func) ->
    case preproc(Node) of
        false ->
            CallArgs = [eval(X) || X <- Args],
            funcall(Func, CallArgs);
        {reeval, Newnode} ->
            eval(Newnode);
        [Func2|Args2] ->
            CallArgs = [eval(X) || X <- Args2],
            funcall(Func2, CallArgs)
    end;
eval(Value) ->
    Value.

%% @doc Same as eval() but doesn't preprocess.
plain_eval([Func | Args]) when ?isfuncall(Func) ->
    CallArgs = [plain_eval(X) || X <- Args],
    funcall(Func, CallArgs);
plain_eval(Value) ->
    Value.

%% @doc Transforms certain types of sexps. Returns false if the sexp didn't
%% need to be transformed. @end
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

    [':', Eval(StartExpr), Eval(EndExpr)];
preproc([indirect, Arg]) ->
    Str = plain_eval(Arg),
    {ok, Toks} = xfl_lexer:lex(Str, {?mx, ?my}),
    case Toks of
        [{ref, R, C, P, _}] ->
            put(recompile, true),
            [ref, R, C, P];
        _ ->
            ?ERR_REF
    end;
preproc(['query', Arg]) ->
    Toks = string:tokens(Arg, "/"),
    Idx = find("*", Toks),
    Under = sublist(Toks, Idx - 1),
    Pages = get_pages_under(Under),
    %% io:format("in muin:preproc~n-Arg is ~p~n-Toks is ~p~n-Idx is ~p~n"++
    %%	      "-Under is ~p~n-Pages is ~p~n",[Arg,Toks,Idx,Under,Pages]),
    R = map(fun(X) ->
                    %% omgwtfbbq
                    Refstr = "/" ++ flatten(hslists:join("/", Under ++ [X] ++ [last(Toks)])),
                    {ok, Ts} = xfl_lexer:lex(Refstr, {?mx, ?my}),
                    case Ts of
                        [{ref, R, C, P, _}] ->
                            [ref, R, C, P];
                        _ ->
                            ?ERR_REF
                    end
            end,
            Pages),
    Node = [make_list | R],
    %% Stick a special parent in.
    {ok, Refobj} = xfl_lexer:lex(last(Toks), {?mx, ?my}),
    [{ref, C2, R2, _, _}] = Refobj,
    Rowidx = toidx(R2),
    Colidx = toidx(C2),
    {Oldparents, Errs, Refs} = get(retvals),
    Newparent = {"local", {?msite, hslists:init(Toks), Colidx, Rowidx}},
    %% io:format("in muin:preproc~n-Node is ~p~n-C2 is ~p~n-R2 is ~p~n"++
    %%	      "-Rowidx is ~p~n-Colidx is ~p~n-Oldparents are ~p~n"++
    %%	      "-Errs are ~p~nRefs are ~p~n-NewParent is ~p~n",
    %%	      [Node,C2,R2,Rowidx,Colidx,Oldparents,Errs,Refs,Newparent]),
    put(retvals, {[Newparent | Oldparents], Errs, Refs}),

    {reeval, Node};
%% preproc(['query',Arg1,Arg2])->
%%    io:format("in muin:preproc for query/2~n");
preproc(_) ->
    false.

funcall(make_list, Args) ->
    Args; % shame, shame...
%% Refs
funcall(ref, [Col, Row, Path]) ->
    Rowidx = toidx(Row),
    Colidx = toidx(Col),
    do_cell(Path, Rowidx, Colidx);
%% Cell ranges (A1:A5, R1C2:R2C10 etc).
%% In a range, the path of second ref **must** be ./
funcall(':', [{ref, Col1, Row1, Path1, _}, {ref, Col2, Row2, "./", _}]) ->
    
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
    Path = muin_util:walk_path(?mpath, RelPath),
    ?IF(Colidx == ?mx andalso Rowidx == ?my andalso Path == ?mpath,
        throw({error, self_reference})),

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

%% TODO: Move to hslists.
%% @doc Return position of element X in the list. Returns 0 if the list does
%% not contain such element.
find(_, []) ->
    0;
find(What, [What|_]) ->
    1;
find(What, [_|Tl]) ->
    find1(What, Tl, 2). % position of current hd in the original list
find1(What, [What|_], Origpos) ->
    Origpos;
find1(What, [_|Tl], Origpos) ->
    find1(What, Tl, Origpos + 1);
find1(_, [], _) ->
    0.

%% @spec get_pages_under(list()) -> list()
%% @doc Get list of pages under path specified by a list of path components.
get_pages_under(Pathcomps) ->
    %%io:format("in muin:get_pages_under Pathcomps are ~p~n",[Pathcomps]),
    Match = fun() ->
                    M = #hn_item{addr = #ref{site = '_',
                                             path = '_',
                                             ref  = '_',
                                             name = '_',
                                             auth = '_'},
                                 val = '_'},
                    mnesia:match_object(hn_item, M, read)
            end,
    {atomic, Res} = mnesia:transaction(Match),
    %% List of expansions for the "*" wildcard.
    Starexp = foldl(fun(X, Acc) ->
			    Path = (X#hn_item.addr)#ref.path, % assume 1 site
			    Init = sublist(Path, length(Pathcomps)),
			    if (Pathcomps == Init) and (Path =/= Init) ->
				    %%io:format("in muin:get_pages Path is ~p "++
				    %% 	      "Init is ~p~n",[Path,Init]),
				    [last(Path) | Acc];
			       true ->
				    Acc
			    end
                    end,
                    [],
                    Res),
    %%io:format("in muin:get_pages_under Starexp is ~p~n",[Starexp]),
    hslists:uniq(Starexp).
