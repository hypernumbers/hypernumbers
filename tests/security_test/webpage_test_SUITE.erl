%%%-----------------------------------------------------------------------------
%%% File        : webpage_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests security for a webpage view
%%%
%%% Created     : 30 August 2010 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(webpage_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").


-define(test(Name, Input, Expected),
        Name(_Config) ->
               Got = Input,
               %Got = run(Input),
               %Results = compare(Input, Expected),
               Res = false,
               case Res of
                   true ->
                       {test, ok};
                   false ->
                       io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                 [Expected, Got]),
                       exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                            ++ " of " ++ atom_to_list(?MODULE))
               end).

%% callbacks
init_per_suite(Config)               -> Config.
end_per_suite(_Config)               -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% tests to run
all() -> 
    [
     wp_test1
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------

?test(wp_test1, input, output).


