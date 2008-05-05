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
        AllNumbers_x = lists:all(fun(X_x) ->
                                       is_number(X_x)
                               end,
                               L),
        ?IF(not(AllNumbers_x), ?ERR_VAL)).

-define(ensure_nonzero(Num),
        ?IF(Num == 0, ?ERR_DIV)).

%% Ensures number is not less than 0.
-define(ensure_non_negative_ex(Num, Action),
        ?IF(not(is_number(Num) andalso Num >= 0), Action)).

-define(ensure_positive_ex(Num, Action),
        ?IF(not(is_number(Num) andalso Num > 0), Action)).

%% Looks at a list of values, and if an error value is found, that error
%% value is returned.
-define(ensure_no_errvals(Values),
        lists:foreach(fun(X_x) ->
                              ?IF(stdfuns_info:iserror([X_x]),
                                  muin_util:error(X_x))
                      end,
                      Values)).

-define(ensure(Test, Action),
        case (Test) of
            true  -> nothing;
            false -> Action
        end).
