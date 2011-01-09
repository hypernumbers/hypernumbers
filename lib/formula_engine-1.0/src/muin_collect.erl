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

%%% The available rules are:
%%% * ignore_blanks
%%% * ignore_strings
%%% * ignore_errors
%%% * {ignore, Type} where Type === num, bool, date, array
%%%                                 error, blank, str, unknown_type
%%% * eval_funs             evaluates a function (ie non-lazy eval)
%%% * area_first            gets the top left value of an array or range
%%% * first_array           gets the first element of an array
%%% * first_array_as_bool   gets the top left of an array as
%%%                         a false-specific boolean
%%% * flatten_as_str        turns a range into a string
%%% * flatten               turns a list, range or array into a flat list
%%%                         NOTE a list APPEARS to be an internal state of
%%%                         the collector and not a type in its own right
%%% * {flatten, range}      flattens a range
%%% * num_as_bool           casts an integer/float into
%%%                         a false-specific boolean
%%% * str_as_bool           casts true/TRUE/false/False or throws err
%%% * err_as_str            returns a string represenntation of an error
%%%                         THIS IS ONLY TO BE USE USED IN DISPLAY FUNCTIONS WHOSE
%%%                         INPUT IS NOT EXPECTED TO BE USED IN CALCULATIONS
%%%                         eg hnfuns_web:table and hnfuns_html etc, etc,
%%%                         STANDS OUTSIDE NORMAL CASTING (ie YOU DON'T CAST
%%%                         ERRORS TO STRINGS OR NUMS NORMALLY...)
%%% * ref_as_bool           presumes that a cellref has not been fetched,
%%%                         then fetches it and casts it as bool
%%% * fetch_ref             fetches a cellref or rangeref - that is to say
%%%                         turns 'A1' into the value in 'A1'
%%% * cast_def              NOT A REAL CAST - USED INTERNALLY
%%% * {cast, Type}          casts all values to 'Type' (see ignore_type)
%%% * {cast, From, To}      only casts some types to the the 'To' type
%%% * {cast, From, To, Default} if the cast fails return the default
%%% * cast_num
%%% * cast_str
%%% * fetch_name            fetches the value of a named object
%%%                         (names not properly implemented yet)
%%% * name_as_bool          always fails (at the mo!, mebbies?)
%%% * fetch                 gets values of cell/range references
%%% * fetchdb               fetch for database fns
%%% * {conv, Type, Value}   converts objects of a particular type to a val
%%% * {convflat, Type, Value} same as conv? WTF?

%%% Passes are what happens after all the rules have been applied
%%% * return_flat_errors    returns all the errors
%%% * return_errors
%%% * {all, Type}           checks that all the values are of the type

-module(muin_collect).

-include("muin_records.hrl").
-include("typechecks.hrl").

-export([
         col/2,
         col/3,
         col/4,
         is_bool/1,
         is_area/1,
         is_string/1,
         is_date/1,
         is_blank/1
        ]).

col(Args, Rules, Passes, Fun) ->
    case col(Args, Rules, Passes) of
        Error when ?is_errval(Error) -> Error;
        Else -> Fun(Else)
    end.

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

rl(Rule, {list, Vals}) ->    
    F = fun(X, Acc) ->
                case rl(Rule, X) of
                    ignore       -> Acc;
                    Else         -> [Else | Acc]
                end
        end,
                
    {list, lists:foldl(F, [], Vals)};
                          
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

rl({ignore, Type}, {Area, Rows}=Val) when ?is_area(Val) ->
    {Area, [ muin_collect:col(X, [{ignore, Type}]) || X <- Rows ]};

rl({ignore, Type}, Val) ->
    case muin_util:get_type(Val) of
        Type  -> ignore;
        _Else -> Val
    end;

% Evaluate functions
rl(eval_funs, Fun) when ?is_funcall(Fun) ->
    muin:external_eval(Fun);

rl(area_first, {array,[[X|_]|_]}) -> X;
rl(area_first, {range,[[X|_]|_]}) -> X;

rl(first_array, {array,[[X|_]|_]}) -> X;

rl(first_array_as_bool, {array,[[X|_]|_]}) when X == false; X == 0 ->
    false;
rl(first_array_as_bool, {array,[[_Val|_]|_]}) ->
    true;

rl(flatten_as_str, {range,X}) ->
    {list, col(lists:concat(X), [ignore_blanks, cast_str, cast_num, ignore_strings])};
rl(flatten_as_str, {array,[X]}) ->
    {list, col(X, [ignore_blanks, cast_str, cast_num, ignore_strings])};

rl(flatten, {range,X}) ->
    {list, flat(X, [])};
rl(flatten, {array,X}) ->
    {list, flat(X,[])};
rl(flatten, {list,X}) ->
    {list, X};

rl({flatten, range}, {range,X}) ->
    {list, flat(X, [])};

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

rl(err_as_str, {errval, Err}) -> atom_to_list(Err);

rl(ref_as_bool, Ref) when ?is_cellref(Ref) ->
    case muin:fetch(Ref) of
        blank                     -> blank;
        X when X == 0; X == false -> false;
        _Else                     -> true
    end;
rl(fetch_ref, Ref) when ?is_cellref(Ref); ?is_rangeref(Ref) ->
    muin:fetch(Ref);

rl({cast_def, _Type, _Def}, X) when ?is_errval(X) ->
    X;
rl({cast_def, Type, Def}, X) ->
    case muin_util:cast(X, Type) of
        {error, _} -> Def;
        Num        -> Num
    end;
rl(cast_blank, blank) ->
    1;
rl({cast, Type}, X) ->
    case muin_util:cast(X, Type) of
        {error, _} -> X;
        Num        -> Num
    end;
rl({cast, From, To, Default}, X) ->
    case muin_util:get_type(X) of
        From  -> rl({cast_def, To, Default}, X);
        _Else -> X
    end;
rl({cast, From, To}, X) ->    
    case muin_util:get_type(X) of
        From  -> rl({cast, To}, X);
        _Else -> X
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

rl(fetch_name, Name) when ?is_namedexpr(Name) ->
    ?ERRVAL_NAME;
rl(name_as_bool, Name) when ?is_namedexpr(Name) ->
    ?ERRVAL_NAME;

rl(fetch, Name) when ?is_namedexpr(Name) ->
    ?ERRVAL_NAME;
rl(fetch, Ref) when ?is_cellref(Ref); ?is_rangeref(Ref) ->
    muin:fetch(Ref);
%% WTF? (why are ranges tagged as arrays)
rl(fetch, {array, [[Ref]]}) when ?is_cellref(Ref); ?is_rangeref(Ref) ->
    muin:fetch(Ref);

rl(fetchdb, Ref) when ?is_rangeref(Ref) ->
    case {Ref#rangeref.height, Ref#rangeref.width} of
        {1, _} ->
            0;
        {_, 1} ->
            {range, Rows} = muin:fetch(Ref),
            {_,{offset, X}} = Ref#rangeref.tl,
            [Val] = lists:nth(erlang:abs(X) + 1, Rows),
            Val;
        _ ->
            ?ERRVAL_VAL
    end;

rl(fetchdb, Ref) when ?is_cellref(Ref) ->
    muin:fetch(Ref);

rl({conv, Type, Val}, {Area, Rows}=Va) when ?is_area(Va) ->
    {Area, [ muin_collect:col(X, [{conv, Type, Val}]) || X <- Rows ]};

rl({conv, Type, Value}, X) ->
    case muin_util:get_type(X) of
        Type  -> Value;
        _Else -> X
    end;

rl({convflat, Type, Value}, X) ->
    case muin_util:get_type(X) of
        Type  -> Value;
        _Else -> X
    end;

% No Rules for this element
rl(_Rule, Value) ->
    Value.

flat([], Acc)          -> Acc;
flat([Head|Tail], Acc) -> flat(Tail, Acc ++ Head).
    
%% Passes are a list of rules to perform on arguments once casting
%% and such has happened
pass(Args, []) ->
    Args;

pass(Args, [ return_flat_errors | Rules ]) ->
    F = fun(X, _Acc) when ?is_errval(X) -> X;
           (_X, Acc) -> Acc
        end,
    
    case lists:foldr(F, false, Args) of
        false -> pass(Args, Rules);
        Err   -> Err
    end;

% if there are any errors in the parameters, return these
pass(Args, [ return_errors | Rules ]) ->
    F = fun(X, _Acc) when ?is_errval(X) -> X;
           ({_, Rows} = X, Acc)  when ?is_area(X) ->
                L = [[ Y || Y <- Z, muin_util:get_type(Y) == error ] || Z <- Rows ],
                case lists:flatten(L) of
                    []        -> Acc;
                    [Err | _] -> Err
                end;
           (_X, Acc) -> Acc
        end,
   
    case lists:foldr(F, false, Args) of
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

%%%%%%%%%%%%%%%%%%%
%%% Type Checks %%%
%%%%%%%%%%%%%%%%%%%

is_bool(true)  -> true;
is_bool(false) -> true;
is_bool(_)     -> false.

is_area(Area) when ?is_area(Area) ->
    true;
is_area(_Area) ->
    false.

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
                    pass([true, false, ?ERRVAL_DIV], [return_errors]))
     
     
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true]),
%% ?_assertEqual( col([1,2,3], [cast_num_as_bool]), [true, true, true])
%% ?_assert(1==1)
    ].
