%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Validate form posts. 
-module(hn_security).

-export([
         validate_form/2
        ]).

%% APIs-export([validate/2]).
-include("spriki.hrl").
-include_lib("eunit/include/eunit.hrl").

validate_form(Expected0, Submitted0) ->
    Expected = lists:keysort(#form.id, Expected0),
    Submitted = lists:keysort(1, [{pget("label",L), pget("formula",L)} 
                                  || {struct, L} <- Submitted0]),
    run_validate(Expected, Submitted).

run_validate([], []) ->
    true;
run_validate([#form{kind = button}|LT], RT) ->
    run_validate(LT, RT);
run_validate([E=#form{id={_,_,L}}|LT], [{L,Val}|RT]) ->
    case E#form.restrictions of 
        none -> run_validate(LT, RT);
        Restricted -> case lists:member(Val, Restricted) of
                          true -> run_validate(LT, RT);
                          false -> false
                      end
    end;
run_validate(_, _) ->
    false.
                    
pget(K,L) -> proplists:get_value(K,L,undefined).


%% Simple test with sane input.
basic_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []},
                #form{key=2, id = {[],common,"two"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []},
                #form{key=3, id = {[],common,""},
                      kind = button,
                      restrictions = none,
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","a"}]},
                 {struct,[{"label","two"},{"formula","b"}]}],
    ?assert(validate_form(Expected, Submitted)).

%% Submit too many values.
too_much_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","1"}]},
                 {struct,[{"label","two"},{"formula","2"}]}],
    ?assertNot(validate_form(Expected, Submitted)).

not_enough_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []},
                #form{key=2, id = {[],common,"two"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","#"}]}],
    ?assertNot(validate_form(Expected, Submitted)).

fake_name_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []},
                #form{key=2, id = {[],common,"two"}, 
                      kind = input, 
                      restrictions = none, 
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","a"}]},
                 {struct,[{"label","haha"},{"formula","b"}]}],
    ?assertNot(validate_form(Expected, Submitted)).

basic_select_radio_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = select, 
                      restrictions = ["green", "blue"], 
                      attrs = []},
                #form{key=2, id = {[],common,"two"}, 
                      kind = radio, 
                      restrictions = ["red"],
                      attrs = []},
                #form{key=3, id = {[],common,""},
                      kind = button,
                      restrictions = none,
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","blue"}]},
                 {struct,[{"label","two"},{"formula","red"}]}],
    ?assert(validate_form(Expected, Submitted)).

basic_select_bad_val_test() ->
    Expected = [#form{key=1, id = {[],common,"one"}, 
                      kind = select, 
                      restrictions = ["green", "blue"], 
                      attrs = []},
                #form{key=2, id = {[],common,"two"}, 
                      kind = input, 
                      restrictions = ["red"],
                      attrs = []},
                #form{key=3, id = {[],common,""},
                      kind = button,
                      restrictions = none,
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","blue"}]},
                 {struct,[{"label","two"},{"formula","IndianRed"}]}],
    ?assertNot(validate_form(Expected, Submitted)).
