%%% Type and error checking macros used by stdfuns modules.
%%% Hasan Veldstra <hasan@hypernumbers.com>

-define(ERR_VAL, stdfuns:error('#VALUE!')).
-define(ERR_DIV, stdfuns:error('#DIV/0!')).
-define(ERR_NUM, stdfuns:error('#NUM!')).

-define(ensure_number(Num_x),
        ?IF(not(is_number(Num_x)), ?ERR_VAL)).

-define(ensure_numbers(L_x),
        AllNumbers_x = lists:all(fun(X_x) ->
                                       is_number(X_x)
                               end,
                               L_x),
        ?IF(not(AllNumbers_x), ?ERR_VAL)).

-define(ensure_nonzero(Num_x),
        ?IF(Num_x == 0, ?ERR_DIV)).

-define(ensure_non_negative_ex(Num_x, Action_x),
        ?IF(not(is_number(Num_x) andalso Num_x >= 0), Action_x)).

-define(ensure_positive_ex(Num_x, Action_x),
        ?IF(not(is_number(Num_x) andalso Num_x > 0), Action_x)).

%% Looks at a list of values, and if an error value is found, that error
%% is returned.
-define(maybe_throw_up(Values_x),
        lists:foreach(fun(X_x) ->
                              ?IF(stdfuns_info:iserror([X_x]),
                                  muin_util:error(X_x))
                      end,
                      Values_x)).

-define(ensure(Test, Action),
        case Test of
            true ->
                nothing;
            false ->
                Action
        end).
