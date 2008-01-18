%%% @doc        Interface to the formula engine. Also, the interpreter.
%%% @author     Hasan Veldstra <hasan@hypernumbers.com>

-module(muin).
-export([parse/1, run/2, update/2]).
-compile(export_all).

-include("spriki.hrl").
-include("builtins.hrl").

-import(muin_util, [expand_cellrange/2, fdbg/1, fdbg/2, get_frontend/0, getxy/1,
                    init/1, join/1, join/2, just_path/1, just_ref/1, to_i/1,
                    to_i/2, to_s/1, puts/1, walk_path/2]).
-import(lists, [any/2, append/2, flatten/1, foldl/3, foreach/2, keysearch/3,
                last/1, map/2, member/2, seq/2]).

%% Ternary if.
-define(COND(Test, TrueVal, FalseVal),
        case (Test) of true -> TrueVal; false -> FalseVal end).

%% Simpler lists:keysearch().
-define(LOOKUP(Key, List),
        element(2, element(2, keysearch(Key, 1, List)))).

%% Bind values from Bindings to names in current scope.
-define(CREATE_BINDINGS_VARS,
        MSite = ?LOOKUP(site, Bindings), MSite,
        MPath = ?LOOKUP(path, Bindings), MPath,
        MX = ?LOOKUP(x, Bindings), MX,
        MY = ?LOOKUP(y, Bindings), MY).

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


%%% @doc Runs the s-expression.
%%% TODO: Move the interpreter to muin_interpreter, which will also be spawnable?
%%% Otherwise, I can haz problems with concurrency?
run(Ast, Bindings) ->
    put(retvals, {[], [], []}),
    Value = eval(Ast, Bindings),
    {RefTree, Errors, References} = get(retvals),
    {ok, {Value, RefTree, Errors, References}}.


%%% @doc Updates formula for structural change.
%%% @spec update(Formula::string(), UpdateMsg::tuple()) ->
%%            {ok, {NewFormula :: string(), Ast :: list()}}
update(Formula, UpdateMsg) ->
    {ok, NewFormula} = muin_supd:do(Formula, UpdateMsg),
    {ok, Ast} = parse(NewFormula),
    {ok, {NewFormula, Ast}}.


%%%---------------------%%%
%%%  Private functions  %%%
%%%---------------------%%%

%% @doc Evaluates an s-expression, pre-processing subexps as needed.
eval([Fun_ | Args_], Bindings) when is_atom(Fun_) ->
    [Fun | Args] = preproc([Fun_ | Args_], Bindings),

    %% Eval any funcalls in args first if needed.
    CallArgs = ?COND(any_funcalls(Args),
                     map(fun(X) -> eval(X, Bindings) end, Args),
                     Args),
    
    %% Some functions need access to bindings, some don't...
    ?COND(member(Fun, [sscellref, ':', hn, hypernumber]),
          funcall(Fun, CallArgs, Bindings),
          funcall(Fun, CallArgs));

eval(Value, Bindings) ->
    Value.


%% @doc Transforms certain types of s-exps.
%% StartExpr and EndExpr may be tuples or indirect s-exps *only*.
preproc([':', StartExpr, EndExpr], Bindings) ->
    Start = ?COND(is_list(StartExpr), % An INDIRECT node?
                  hd(plain_eval(tl(StartExpr), Bindings)), % Eval args.
                  StartExpr),
    End   = ?COND(is_list(EndExpr),
                  hd(plain_eval(tl(EndExpr), Bindings)),
                  EndExpr),

    TryParse = fun(S) when is_list(S)  -> {sscellref, "./" ++ S};
                  (T) when is_tuple(T) -> T
               end,
    
    [':', TryParse(Start), TryParse(End)];

preproc([indirect, Arg], Bindings) ->
    RefStr = plain_eval(Arg, Bindings),
    {ok, Tokens, _} = muin_lexer:string(RefStr), % Yeh, screw the frontends. FIXME:

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
preproc(Sexp, Bindings) ->
    Sexp.


%% @doc Same as eval() but doesn't pre-process.
plain_eval([Fun | Args], Bindings) when is_atom(Fun) ->
    %% Eval any funcalls in args first if needed.
    CallArgs = ?COND(any_funcalls(Args),
                     map(fun(X) -> plain_eval(X, Bindings) end, Args),
                     Args),
    
    %% Some functions need access to bindings, some don't...
    ?COND(member(Fun, [sscellref, ':', hn, hypernumber]),
          funcall(Fun, CallArgs, Bindings),
          funcall(Fun, CallArgs));

plain_eval(Value, Bindings) ->
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
    DestPath = walk_path(MPath, just_path(Ssref)),
    Cellref = just_ref(Ssref),
    {X, Y} = getxy(Cellref),
    FetchFun = fun() -> spriki:calc(MSite, DestPath, X, Y) end,
    fetch_update_return(FetchFun);

%% -- Range functions.

%% Cellranges, e.g. A1:B10, ../page/A1:B10 etc.
funcall(':', [{sscellref, Ref1}, {cellref, Ref2}], Bindings) ->
    Path = just_path(Ref1),
    Ref  = just_ref(Ref1),
    DoCell = fun(X) -> funcall(sscellref, [Path ++ X], Bindings) end,
    map(DoCell, expand_cellrange(Ref, Ref2));

%% These two clauses are for preproc'd ranges. Redirect to the one above.
funcall(':', [{cellref, Ref1}, {cellref, Ref2}], Bindings) ->
    funcall(':', [{sscellref, "./" ++ Ref1}, {cellref, Ref2}], Bindings);

funcall(':', [{sscellref, Ref1}, {sscellref, Ref2}], Bindings) ->
    funcall(':', [{sscellref, Ref1}, {cellref, just_ref(Ref2)}], Bindings);

%% Column ranges. FIXME: will break on cells containing strings becase of flatten().
funcall(':', [{sscolref, Ref1}, {col, Ref2}], Bindings) ->
    ?CREATE_BINDINGS_VARS,

    flatten(
      map(fun(X) ->
                  map(fun(CellRec) ->
                              #spriki{index = _, value = Value,
                                      val_type = _, status = _,
                                      num_format = _, disp_format = _} = CellRec,
                              Value
                      end,
                      db:read_column(MSite, walk_path(MPath, just_path(Ref1)),
                                     X))
          end,
          seq(to_i(just_ref(Ref1), b26), to_i(Ref2, b26))));

%% Row ranges. FIXME: as above.
funcall(':', [{ssrowref, Ref1}, {row, Ref2}], Bindings) ->
    ?CREATE_BINDINGS_VARS,

    flatten(
      map(fun(X) ->
                  map(fun(CellRec) ->
                              #spriki{index = _, value = Value,
                                      val_type = _, status = _,
                                      num_format = _, disp_format = _} = CellRec,
                              Value
                      end,
                      db:read_row(MSite, walk_path(MPath, just_path(Ref1)), X))
          end,
          seq(to_i(just_ref(Ref1)), to_i(Ref2))));

funcall(hypernumber, [Url], Bindings) ->
    ?CREATE_BINDINGS_VARS,

    %% Append ?hypernumber if needed.
    {match, Matches} = regexp:matches(Url, "\\?hypernumber$"),
    Url2 = ?COND(Matches == [], Url ++ "?hypernumber", Url),
    
    #page{site = RSite, path = RPath, ref = {cell, {RX, RY}}} =
        hn_util:parse_url(Url2),

    fetch_update_return(fun() ->
                                spriki:get_hypernumber(MSite, MPath, MX, MY, Url,
                                                       RSite, RPath, RX, RY)
                        end);

funcall(hn, [Url], Bindings) ->
    funcall(hypernumber, [Url], Bindings).


%%% ----- Utility functions.

%% Checks if any elements of an s-exp are funcalls.
any_funcalls(Sexp) when is_list(Sexp) ->
    any(fun(Elt) ->
                %% List where head is an atom, which isn't true/false? Bingo!
                ?COND(is_list(Elt) andalso is_atom(hd(Elt)),
                      (hd(Elt) =/= true) and (hd(Elt) =/= false),
                      false)
        end,
        Sexp);

%% For formulas like "=1".
any_funcalls(_) ->
    false.


%% Call fun to get value and deps, stash deps away, return value.
fetch_update_return(FetchFun) ->
    {Value, RefTree, Errors, References} = FetchFun(),

    {RefTreeLst, ErrorsLst, ReferencesLst} = get(retvals),
    put(retvals,
        {RefTreeLst ++ RefTree,
         ErrorsLst  ++ Errors,
         ReferencesLst ++ References}),

    Value.
