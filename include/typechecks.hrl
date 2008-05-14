%%% Type and error checking macros used by stdfuns modules.
%%% Hasan Veldstra <hasan@hypernumbers.com>

-define(ERR_NULL, muin_util:error('#NULL!')).
-define(ERR_DIV,  muin_util:error('#DIV/0!')).
-define(ERR_VAL,  muin_util:error('#VALUE!')).
-define(ERR_REF,  muin_util:error('#REF!')).
-define(ERR_NAME, muin_util:error('#NAME?')).
-define(ERR_NUM,  muin_util:error('#NUM!')).
-define(ERR_NA,   muin_util:error('#N/A')).

-define(ensure_number(Num),
        ?IF(not(is_number(Num)), ?ERR_VAL)).

-define(ensure_numbers(L),
        Allokx = lists:all(fun(X_x) -> is_number(X_x) end, L),
        ?IF(not(Allokx), ?ERR_VAL)).

-define(ensure_nonzero(Num),
        ?IF(Num == 0, ?ERR_DIV)).

%% Ensures number is >= 0.
-define(ensure_non_negative(Num),
        ?ensure_non_negative_ex(Num, ?ERR_NUM)).

-define(ensure_non_negatives(L),
        Allokx = lists:all(fun(X_x) -> is_number(X_x) andalso X_x >= 0 end,
                           L),
        ?IF(not(Allokx), ?ERR_NUM)).
        

-define(ensure_non_negative_ex(Num, Action),
        ?IF(not(is_number(Num) andalso Num >= 0), Action)).

%% Ensures number is > 0.
-define(ensure_positive(Num),
        ?ensure_positive_ex(Num, ?ERR_NUM)).

-define(ensure_positive_ex(Num, Action),
        ?IF(not(is_number(Num) andalso Num > 0), Action)).

%% Looks at a list of values, and if an error value is found, that error
%% value is returned.
-define(ensure_no_errvals(Values),
        lists:map(fun(X_x) ->
                          ?COND(stdfuns_info:iserror([X_x]),
                                muin_util:error(X_x),
                                X_x)
                  end,
                  Values)).

-define(ensure(Test, Action),
        case (Test) of
            true  -> nothing;
            false -> Action
        end).

-define(filter_numbers(Vals),
        [X || X <- Vals, is_number(X)]).

-define(filter_numbers_with_cast(Vals),
        [X || X <- Vals, is_number(muin_util:cast(X, num))]).
        
%% L is a list that may contain {matrix, _, [X]} tuples.
-define(flatten(L),
        (foldl(fun({matrix, _, Xs}, Acc) ->
                       append([Acc, Xs]);
                  (X, Acc) ->
                       append([Acc, [X]])
               end,
               [], L))).
