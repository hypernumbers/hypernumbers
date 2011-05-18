%%% Engineering functions.
%%% <hasan@hypernumbers.com>
%%% @private

-module(stdfuns_eng).

-include("typechecks.hrl").
-import(muin_util, [conv/2, cast/2]).

% passing test suite
-export([
         gestep/1
        ]).

% no test suite
%-export([
%         delta/1
%         ]).

-define(default_rules, [cast_strings, cast_bools, cast_blanks, cast_dates]).

%% @todo non-Excel 97 function - needs a test suite
delta([V1, V2]) ->
    [Num1, Num2] = muin_col_DEPR:collect_number([V1, V2], ?default_rules),
    case (Num1 == Num2) of
        true  -> 1;
        false -> 0
    end.

gestep([V1]) ->
    gestep([V1, 0]);
gestep([V1, V2]) ->
    % gestep is wierd - it evaluates back to front for the purpose
    % of throwing errors so reverse the arguements in the cast
    [Step, Num] = muin_col_DEPR:collect_numbers([V2, V1],
                                                [cast_strings, ban_bools,
                                                 cast_blanks, cast_dates]),
    case (Num >= Step) of
        true  -> 1;
        false -> 0
    end.

