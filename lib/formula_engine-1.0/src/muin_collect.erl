%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Type casting/coercion functions.
%%%
%%% Source values flow through functions that process them according
%%% to the appropriate rule. #VALUE! is returned if the resulting list
%%% is empty.
%%% A rule MUST be supplied for each type (number, string, bool, date,
%%% blank). Rules for error values are optional.
%%%
%%% There are two basic kinds of rules:
%%% 1. Try to cast and throw error if not possible, e.g. "bob" -> int
%%% 2. Try to cast and return default value if not possible. "bob" -> int = 0.
%%%
%%% All collector functions are stable.
%%%
%%% TODO: Collectors for areas are very inefficient.

-module(muin_collect).

-export([flatten_ranges/1,  flatten_arrays/1, flatten_areas/1,
         collect_numbers/2, collect_number/2,
         collect_strings/2, collect_string/2,
         collect_bools/2,   collect_bool/2,
         collect_dates/2,   collect_date/2,
         remove_errors/1]).

-export([is_string/1, is_date/1, is_blank/1]).

-compile(export_all). % For testing / to kill warnings.

-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("typechecks.hrl").

-import(muin_util, [cast/2]).

col(Args, Rules, Passes) ->
    pass(col(Args, Rules), Passes).

col(Args, Rules) ->
    
    F1 = fun(X, List) ->
                 case lists:foldl(fun rl/2, X, Rules) of
                     ignore       -> List;
                     {list, Vals} -> Vals ++ List;
                     Else         -> [Else | List]
                 end
         end,
    
    lists:foldr(F1, [], Args).

%% This is the list of rules as rl(Rule, Value), each rule
%% is called against each value in left to right order, unrecognised
%% rules are ignored, the function should return the new value following
%% the rule

% If anything has been marked ignore, ignore
rl(_, ignore) ->
    ignore;

% Ignore Blanks
rl(ignore_blanks, blank) ->
    ignore;
rl(ignore_strings, String) when ?is_string(String) ->
    ignore;
rl(ignore_errors, Err) when ?is_errval(Err) ->
    ignore;

% Evaluate functions
rl(eval_funs, Fun) when ?is_funcall(Fun) ->
    muin:eval(Fun);

rl(area_first, {array,[[X|_]|_]}) -> X;
rl(area_first, {range,[[X|_]|_]}) -> X;

rl(first_array, {array,[[X|_]|_]}) -> X;

rl(first_array_as_bool, {array,[[X|_]|_]}) when X == false; X == 0 ->
    false;
rl(first_array_as_bool, {array,[[_Val|_]|_]}) ->
    true;

rl(flatten_as_str, {range,[X]}) ->
    {list, col(X, [ignore_blanks, cast_str, cast_num, ignore_strings])};
rl(flatten_as_str, {array,[X]}) ->
    {list, col(X, [ignore_blanks, cast_str, cast_num, ignore_strings])};

rl(num_as_bool, X) when is_number(X) andalso X==0; X==0.0 ->
    false;
rl(num_as_bool, X) when is_number(X) ->
    true;

rl(str_as_bool, Str) when ?is_string(Str) ->
    case string:to_upper(Str) of
        "TRUE"  -> true;
        "FALSE" -> false;
        _Else   -> ?ERRVAL_VAL
    end;

rl(ref_as_bool, Ref) when ?is_cellref(Ref) ->
    case muin:fetch(Ref) of
        blank                     -> blank;
        X when X == 0; X == false -> false;
        _Else                     -> true
    end;

rl({cast, Type}, X) ->
    case muin_util:cast(X, Type) of
        {error, _} -> X;
        Num        -> Num
    end;

rl(cast_num, X) ->
    case muin_util:cast(X, num) of
        {error, _} -> X;
        Num  -> Num
    end;

rl(cast_str, X) ->
    case muin_util:cast(X, str) of
        {error, _} -> X;
        Num  -> Num
    end;


rl(name_as_bool, Name) when ?is_namedexpr(Name) ->
    ?ERRVAL_NAME;

% No Rules for this element
rl(_Rule, Value) ->
    Value.

%% Passes are a list of rules to perform on arguments once casting
%% and such has happened
pass(Args, []) ->
    Args;

% if there are any errors in the parameters, return these
pass(Args, [ return_errors | Rules ]) ->
    case lists:keyfind(errval, 1, Args) of
        false -> pass(Args, Rules);
        Err   -> Err
    end;

% Typically a type check, checks that all elements return true
% for F(X)
pass(Args, [ {all, F} | Rules ]) ->
    case lists:all(F, Args) of
        true  -> pass(Args, Rules);
        false -> ?ERRVAL_VAL
    end.

%% for each arguments, check wether is should be ignored
%% and if not, go through a list of collection rules, the ordering
%% of the rules can be important (pick_first_array needs to be called
%% before cast_or_die, etc)
collect(Args, Type, Rules, Filters) ->
    io:format("~p",[collect(Args, Type, Rules)]),
    [ X || X <- collect(Args, Type, Rules), ignor(X, Filters) ].
                                   
collect(Args, Type, Rules) ->
    [ casts(X, Type, Rules) || X <- Args ].

ignor(_X, []) ->
    true;
ignor(blank, [ignore_blanks | _Filters]) ->
    false;
ignor(X, [ignore_blanks | Filters]) ->
    ignor(X, Filters).



%% Rules
% pick_first_array, ignore_blank_refs, cast_to_X_or_err, die_on_err,
% [1,1] = [true,true]
% [=,A,B] = [true]
% ["X"] = [false]
% ["TRUE"] = [true]
% [1/0, 2] = [{err}]



%% List of clauses to ignore values
casts(Val, _Type, []) ->
    Val;

% die_on_err
% causes the expression to throw an error when one of the params fails
casts(Err, _Type, [die_on_err | _Rules]) when ?is_errval(Err)->
    throw(Err);
casts(Val, Type, [die_on_err | Rules]) ->
    casts(Val, Type, Rules);

% fetch_refs
casts(Val, Type, [fetch_refs | Rules]) when ?is_namedexpr(Val) ->
    casts(?ERRVAL_NAME, Type, Rules);
casts(Ref, Type, [fetch_refs | Rules]) when ?is_cellref(Ref) ->
    io:format("~p ~n", [muin:fetch(Ref)]), 
    casts(muin:fetch(Ref), Type, Rules);
casts([Fun | Args], Type, [fetch_refs | Rules]) when ?is_fn(Fun) ->
    io:format("evalling ~p ~p~n", [Fun, Args]),
    io:format("~p ~n", [muin:eval([Fun | Args])]), 
    casts(muin:eval([Fun | Args]), Type, Rules);
casts(Val, Type, [fetch_refs | Rules])  ->
    casts(Val, Type, Rules);

% fetch_refs_as_bool
casts(Val, Type, [fetch_refs_as_bool | Rules]) when ?is_namedexpr(Val) ->
    casts(?ERRVAL_NAME, Type, Rules);
casts(Ref, Type, [fetch_refs_as_bool | Rules]) when ?is_cellref(Ref) ->
    Val = case muin:fetch(Ref) of
              X when X == "0", X == false, X == "FALSE" -> false;
              blank -> blank;
              _Else -> true
          end,
    casts(Val, Type, Rules);
casts([Fun | Args], Type, [fetch_refs_as_bool | Rules]) when ?is_fn(Fun) ->
    casts(muin:eval([Fun | Args]), Type, Rules);
casts(Val, Type, [fetch_refs_as_bool | Rules])  ->
    casts(Val, Type, Rules);

% fetch_refs_as_str
casts({namedexpr, _, _}, Type, [fetch_refs_as_str | Rules]) ->
    casts(?ERRVAL_NAME, Type, Rules);
casts({cellref, _X, _T, _Path, _Name}, Type, [fetch_refs_as_str | Rules]) ->
    casts("RefStr", Type, Rules);
casts(Val, Type, [fetch_refs_as_str | Rules]) ->
    casts(Val, Type, Rules);

% pick first array
casts({array,[[Val|_]|_]}, Type, [pick_first_array | Rules]) ->
    casts(Val, Type, Rules);
casts(Val, Type, [pick_first_array | Rules]) ->
    casts(Val, Type, Rules);

casts(Val, _Type, [cast_or_err | _Rules]) when ?is_errval(Val) ->
    throw(Val);
casts(Val, Type, [cast_or_err | Rules]) ->
    case muin_util:cast(Val, Type) of
        {error, _Err} -> ?ERR_VAL;
        Else          -> casts(Else, Type, Rules)
    end.

%% @doc removes any errors
remove_errors(Xs) ->
    Fun  = fun(X) ->
                   case X of
                       {error, _} -> false;
                       _          -> true
                   end
           end,
    Return = lists:filter(Fun, Xs),
    io:format("in remove_errors~n-Xs is ~p~n-Return is ~p~n", [Xs, Return]),
    Return.

%% @doc Replaces array objects with values they contain.
flatten_arrays([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_array(X) end);
flatten_arrays(A) ->
    flatten_arrays([A]).

%% @doc Replaces range objects with values they contain.
flatten_ranges([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_range(X) end);
flatten_ranges(A) ->
    flatten_ranges([A]).

%% @doc Replaces both array and range objects with values they contain.
flatten_areas([Hd|Tl]) ->
    flatten_areas(Hd, Tl, [], fun(X) -> ?is_area(X) end);
flatten_areas([]) ->
    [];
flatten_areas(A) ->
    flatten_areas([A]).

%% Rules:
%% ignore_strings | cast_strings | cast_strings_zero | 
%%  cast_strings_or_ignore | ban_strings
%% ignore_bools | cast_bools | ban_bools
%% ignore_dates | cast_dates | ban_dates
%% ignore_blanks | cast_blanks | ban_blanks
collect_numbers(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_number(X, Rules) end, A);
collect_numbers(Vs, Rules) ->
    generic_collect(Vs, Rules, fun erlang:is_number/1, num).

%% @doc Same as <code>collect_numbers</code>
collect_number(V, Rules) ->
    hd(collect_numbers([V], Rules)).

%% Rules:
%% cast_numbers | ignore_numbers | ban_numbers
%% cast_bools | ignore_bools | ban_bools
%% cast_dates | ignore_dates | ban_dates
%% cast_blanks | ignore_blanks | ban_blanks
collect_strings(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_string(X, Rules) end, A);
collect_strings(Vs, Rules) ->
    generic_collect(Vs, Rules, fun is_string/1, str).

%% @doc Same as collect_strings but for one value.
collect_string(V, Rules) ->
    hd(collect_strings([V], Rules)).

%% @doc
collect_bools(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_bool(X, Rules) end, A);
collect_bools(Vs, Rules) ->
    generic_collect(Vs, Rules, fun erlang:is_boolean/1, bool).

%% @doc Same as <code>collect_bools/2</code> but for one value.
collect_bool(V, Rules) -> 
    hd(collect_bools([V], Rules)).

%% @doc
collect_dates(A, Rules) when ?is_area(A) ->
    area_util:apply_each(fun(X) -> collect_date(X, Rules) end, A);
collect_dates(Vs, Rules) ->
    generic_collect(Vs, Rules, fun is_date/1, date).

%% @doc Same as <code>collect_dates/2</code> but only for one value.
collect_date(V, Rules) ->
    hd(collect_dates([V], Rules)).

%%% PRIVATE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%% TODO: Rewrite this.
%%% Instead of working rule-by-rule, go value-by-value and die on the first encountered error value.
%%% Checking for error values after everything has been [attempted to be] cast works but is not
%%% efficient.
%%% Clients of the interface aren't affected by these changes so the quick fix is ok for now.
%%% FIXME: PartitionFun is not used.
generic_collect(Vs, Rules, _PartitionFun, Targtype) ->

    %io:format("hello ~p ~p ~p ~n", [Vs, Rules, Targtype]),
    
    Res = foldl(fun(cast_numbers, Acc) -> cast_numbers(Acc, Targtype);
                   (cast_strings, Acc) -> cast_strings(Acc, Targtype);
                   (cast_bools, Acc)   -> cast_bools(Acc, Targtype);
                   (cast_dates, Acc)   -> cast_dates(Acc, Targtype);
                   (cast_blanks, Acc)  -> cast_blanks(Acc, Targtype);
                   (Func, Acc)         -> ?MODULE:Func(Acc)
                end,
                Vs,
                Rules),

    %io:format("bye ~p~n",[Res]),
    
    muin_checks:die_on_errval(Res),

    if(Res == []) -> ?ERR_VAL;
      true        -> Res
    end.



pick_first({array, [[Val|_]|_]}) ->
    Val;
pick_first(Val) ->
    Val.

first_array(Vals) ->
    [ pick_first(X) || X <- Vals ].

%%% Ignores ~~~~~

ignore_numbers(Xs) ->
    ignore(fun erlang:is_number/1, Xs).

ignore_strings(Xs) ->
    ignore(fun is_string/1, Xs).

ignore_bools(Xs) ->
    ignore(fun erlang:is_boolean/1, Xs).

ignore_dates(Xs) ->
    ignore(fun is_date/1, Xs).

ignore_blanks(Xs) ->
    ignore(fun(X) -> X == blank end, Xs).

%% Complement of lists:filter/2
ignore(Fun, Xs) ->
    filter(fun(X) -> not(Fun(X)) end, Xs).

%%% Casts ~~~~~

cast_strings(Xs, Targtype) ->
    cast_strings_with_opt(Xs, Targtype, fun() -> ?ERRVAL_VAL end).

cast_strings_false(Xs) ->
    cast_strings_with_opt(Xs, bool, fun() -> false end).

cast_strings_zero(Xs) ->
    cast_strings_with_opt(Xs, num, fun() -> 0 end).

cast_strings_or_ignore(Xs) ->
             cast_strings_with_opt(Xs, num, ignore).

cast_strings_with_opt(Xs, Targtype, Action) ->
    Res = generic_cast(Xs, Targtype, fun is_string/1),
    % Swap all {error, _} for Action if Action is a fun()
    % if action is the atom 'ignore' will return nothing
    Fun = fun(X, Acc) ->
                  case X of
                      {error, _} -> case Action of
                                        ignore    -> Acc;
                                        _         -> [Action() | Acc]
                                    end;
                      _          -> [X | Acc]
                  end  
          end,
    reverse(foldl(Fun, [], Res)).

cast_numbers(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_number/1).

cast_bools(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_boolean/1).

cast_dates(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun is_date/1).

cast_blanks(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun is_blank/1).

generic_cast(Xs, Targtype, Guardfun) ->
    R = foldl(fun(X, Acc) ->
                      case Guardfun(X) of
                          true  -> [cast(X, Targtype) | Acc];
                          false -> [X | Acc]
                      end
              end,
              [], Xs),
    reverse(R).

%%% Bans ~~~~~

ban_numbers(Xs) ->
    generic_ban(Xs, fun erlang:is_number/1).

ban_strings(Xs) ->
    generic_ban(Xs, fun is_string/1).

ban_bools(Xs) ->
    generic_ban(Xs, fun erlang:is_boolean/1).

ban_dates(Xs) ->
    generic_ban(Xs, fun is_date/1).

ban_blanks(Xs) ->
    generic_ban(Xs, fun is_blank/1).

generic_ban(Xs, Detectorf) ->
    case any(Detectorf, Xs) of
        true  -> ?ERR_VAL; % For this to work properly, generic_collect needs to go value-by-value.
        false -> Xs
    end.

%%% Type checks ~~~~~
is_bool(true)  -> true;
is_bool(false) -> true;
is_bool(_)     -> false.

is_string({ustr, Bin}) when is_binary(Bin) ->
    true;
is_string(L) when is_list(L) ->
    io_lib:char_list(L);
is_string(_) ->
    false.

is_date(X) when is_record(X, datetime) ->
    true;
is_date(_) ->
    false.

is_blank(blank) ->
    true;
is_blank(_) ->
    false.

%%% Generic flattener ~~~~~~~~~~

flatten_areas(Hd, [], Acc, Test) ->
    case Test(Hd) of
        true  -> Acc ++ area_util:to_list(Hd);
        false -> Acc ++ [Hd]
    end;
flatten_areas(Hd, [NHd|Tl], Acc, Test) ->
    case Test(Hd) of
        true  -> flatten_areas(NHd, Tl, Acc ++ area_util:to_list(Hd), Test);
        false -> flatten_areas(NHd, Tl, Acc ++ [Hd], Test)
    end.

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    [
     % Basic tests
     ?_assertEqual( col([1,2,3], [num_as_bool]), [true, true, true]),
     ?_assertEqual( col([0,2,3], [num_as_bool]), [false, true, true]),

     % Test casting string to bools
     ?_assertEqual( [?ERRVAL_VAL, true, true],
                    col(["1",2,3],
                        [num_as_bool, str_as_bool])),
     ?_assertEqual( [false, true, true],
                    col(["FaLsE",2,3],
                        [num_as_bool, str_as_bool])),

     % Check Evalling functions
     ?_assertEqual( [false],
                    col([['-',1,1]], [eval_funs, num_as_bool])),     
     ?_assertEqual( [?ERRVAL_DIV],
                    col([['/',1,0]], [eval_funs, num_as_bool])),

     % Check reading arrays
     ?_assertEqual( [true, false, true, false],
                    col([{array,[["FALSE",2,3]]},{array,[[0,5,4]]},
                         {array,[[1,5,4]]}, {array,[[false,2,3]]}],
                        [eval_funs, first_array_as_bool])),

     % Check ignore blanks
     ?_assertEqual( [true, false],
                    col([true, blank, false], [ignore_blanks])),

     % Check pass filters
     ?_assertEqual( ?ERRVAL_VAL,
                    pass([true, false, "1"], [{all, fun is_bool/1}])),
     
     ?_assertEqual( ?ERRVAL_DIV,
                    pass([true, false, ?ERRVAL_DIV], [return_errors])),
     
     
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true])
     ?_assert(1==1)
    ].
