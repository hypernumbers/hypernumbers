%%% Engineering functions.
%%% <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_eng).

-include("handy_macros.hrl").
-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

-export([
         delta/1,
         gestep/1
        ]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

%% @todo non-Excel 97 function - needs a test suite
delta([V1, V2]) ->
    [Num1, Num2] = ?numbers([V1, V2], ?default_rules),
    ?COND(Num1 == Num2, 1, 0).

gestep([V1]) ->
    gestep([V1, 0]);
gestep([V1, V2]) ->
    % gestep is wierd - it evaluates back to front for the purpose 
    % of throwing errors so reverse the arguements in the cast
    [Step, Num] = ?numbers([V2, V1], [cast_strings, ban_bools,
                                       cast_blanks, cast_dates]),
    io:format("in gestep~n-V1 is ~p~n-V2 is ~p~n-Num is ~p~n-Step is ~p~n",
              [V1, V2, Num, Step]),
    ?COND(Num >= Step, 1, 0).
    
