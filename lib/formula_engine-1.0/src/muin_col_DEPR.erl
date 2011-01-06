%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc    Deprecated type casting/coercion functions.
%%%         DO NOT USE - USE muin_collect.erl instead

-module(muin_col_DEPR).

-include("muin_records.hrl").
-include("typechecks.hrl").

-export([
         collect/3,
         collect/4,
         ignor/2,
         casts/3,
         pick_first/1,
         first_array/1,
         ignore_numbers/1,
         ignore_strings/1,
         ignore_bools/1,
         ignore_dates/1,
         ignore_blanks/1,
         ignore/2,
         cast_strings_false/1,
         cast_strings_zero/1,
         cast_strings_or_ignore/1,
         ban_numbers/1,
         ban_strings/1,
         ban_bools/1,
         ban_dates/1,
         ban_blanks/1,
         generic_ban/2
        ]).

-export([flatten_ranges/1,  flatten_arrays/1, flatten_areas/1,
         collect_numbers/2, collect_number/2,
         collect_strings/2, collect_string/2,
         collect_bools/2,   collect_bool/2,
         collect_dates/2,   collect_date/2,
         remove_errors/1]).


%% Everything below this should be replaced
%% (by ^)

%% for each arguments, check wether is should be ignored
%% and if not, go through a list of collection rules, the ordering
%% of the rules can be important (pick_first_array needs to be called
%% before cast_or_die, etc)
collect(Args, Type, Rules, Filters) ->
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
    casts(muin:fetch(Ref), Type, Rules);
casts([Fun | Args], Type, [fetch_refs | Rules]) when ?is_fn(Fun) ->
    casts(muin:eval([Fun | Args]), Type, Rules);
casts(Val, Type, [fetch_refs | Rules])  ->
    casts(Val, Type, Rules);

% fetch_refs_as_bool
casts(Val, Type, [fetch_refs_as_bool | Rules]) when ?is_namedexpr(Val) ->
    casts(?ERRVAL_NAME, Type, Rules);
casts(Ref, Type, [fetch_refs_as_bool | Rules]) when ?is_cellref(Ref) ->
    Val = case muin:fetch(Ref) of
              X when X == "0"; X == false; X == "FALSE" -> false;
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
    lists:filter(Fun, Xs).

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
    io:format("in collect_numbers Vs is ~p Rules is ~p~n", [Vs, Rules]),
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
    generic_collect(Vs, Rules, fun muin_collect:is_string/1, str).

%% @doc Same as collect_strings but for one value.
collect_string(V, Rules) ->
    io:format("V is ~p Rules is ~p~n", [V, Rules]),
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
    generic_collect(Vs, Rules, fun muin_collect:is_date/1, date).

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
    io:format("hello ~p ~p ~p ~n", [Vs, Rules, Targtype]),
    Res = lists:foldl(fun(cast_numbers, Acc) -> cast_numbers(Acc, Targtype);
                         (cast_strings, Acc) -> cast_strings(Acc, Targtype);
                         (cast_bools, Acc)   -> cast_bools(Acc, Targtype);
                         (cast_dates, Acc)   -> cast_dates(Acc, Targtype);
                         (cast_blanks, Acc)  -> cast_blanks(Acc, Targtype);
                         (Func, Acc)         -> ?MODULE:Func(Acc)
                      end,
                      Vs,
                      Rules),
    io:format("bye ~p~n",[Res]),    
    muin_checks:die_on_errval(Res),

    if(Res == []) -> ?ERR_VAL;
      true        -> Res
    end.

pick_first({array, [[Val|_]|_]}) ->
    Val;
pick_first(Val) ->
    Val.

first_array(Vals) ->
    io:format("in first_array for ~p~n", [Vals]),
    [ pick_first(X) || X <- Vals ].

%%% Ignores ~~~~~

ignore_numbers(Xs) ->
    ignore(fun erlang:is_number/1, Xs).

ignore_strings(Xs) ->
    ignore(fun muin_collect:is_string/1, Xs).

ignore_bools(Xs) ->
    ignore(fun erlang:is_boolean/1, Xs).

ignore_dates(Xs) ->
    ignore(fun muin_collect:is_date/1, Xs).

ignore_blanks(Xs) ->
    ignore(fun(X) -> X == blank end, Xs).

%% Complement of lists:filter/2
ignore(Fun, Xs) ->
    lists:filter(fun(X) -> not(Fun(X)) end, Xs).

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
    Res = generic_cast(Xs, Targtype, fun muin_collect:is_string/1),
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
    lists:reverse(lists:foldl(Fun, [], Res)).

cast_numbers(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_number/1).

cast_bools(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun erlang:is_boolean/1).

cast_dates(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun muin_collect:is_date/1).

cast_blanks(Xs, Targtype) ->
    generic_cast(Xs, Targtype, fun muin_collect:is_blank/1).

generic_cast(Xs, Targtype, Guardfun) ->
    R = lists:foldl(fun(X, Acc) ->
                      case Guardfun(X) of
                          true  -> [muin_util:cast(X, Targtype) | Acc];
                          false -> [X | Acc]
                      end
              end,
              [], Xs),
    lists:reverse(R).

%%% Bans ~~~~~

ban_numbers(Xs) ->
    generic_ban(Xs, fun erlang:is_number/1).

ban_strings(Xs) ->
    generic_ban(Xs, fun muin_collect:is_string/1).

ban_bools(Xs) ->
    generic_ban(Xs, fun erlang:is_boolean/1).

ban_dates(Xs) ->
    generic_ban(Xs, fun muin_collect:is_date/1).

ban_blanks(Xs) ->
    generic_ban(Xs, fun muin_collect:is_blank/1).

generic_ban(Xs, Detectorf) ->
    case lists:any(Detectorf, Xs) of
        true  -> ?ERR_VAL; % For this to work properly, generic_collect needs to go value-by-value.
        false -> Xs
    end.

%%% Type checks ~~~~~

%% is_area(Area) when ?is_area(Area) ->
%%     true;
%% is_area(_Area) ->
%%     false.

%% is_string({ustr, Bin}) when is_binary(Bin) ->
%%     true;
%% is_string(L) when is_list(L) ->
%%     io_lib:char_list(L);
%% is_string(_) ->
%%     false.

%% is_date(X) when is_record(X, datetime) ->
%%     true;
%% is_date(_) ->
%%     false.

%% is_blank(blank) ->
%%     true;
%% is_blank(_) ->
%%     false.

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
