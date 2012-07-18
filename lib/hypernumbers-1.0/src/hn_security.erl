%%% @copyright 2010 Hypernumbers Ltd
%%% @doc Validate form posts.
-module(hn_security).

-export([
         validate_factory/3,
         validate_form/2,
         validate_create_pages/2
        ]).

%% APIs-export([validate/2]).
-include("spriki.hrl").
-include_lib("eunit/include/eunit.hrl").

validate_factory(Expected, Type, Data) ->
    #form{restrictions = {"sitetype", EType}, attrs = Attrs} = Expected,
    EType2 = list_to_existing_atom(EType),
    case {EType2, valid_fac_attrs(Data, Attrs)} of
        {Type, true} -> true;
        _Other       -> false
    end.

% needent supply all the attrs, can only supply the listed attrs
valid_fac_attrs([], _)               -> true;
valid_fac_attrs([{K, _V} | T], List) -> case lists:member(K, List) of
                                            false -> false;
                                            true  -> valid_fac_attrs(T, List)
                                        end.

validate_create_pages([], _Submitted)     -> false;
validate_create_pages([H | T], Submitted) ->
    Exp = lists:sort(H#form.attrs),
    Sub = lists:sort(Submitted),
    if
        Exp =:= Sub -> true;
        Exp =/= Sub -> validate_create_pages(T, Submitted)
    end.

validate_form(Expected0, Submitted0) ->
    Expected = lists:keysort(#form.id, Expected0),
    Submitted = lists:keysort(1, [{pget("label",L), pget("formula",L)}
                                  || {struct, L} <- Submitted0]),
    run_validate(Expected, Submitted).

run_validate([], []) ->
    true;
run_validate([#form{kind = button} | LT], RT) ->
    run_validate(LT, RT);
run_validate([E = #form{id = {_, _, L}} | LT], [{L, Val} | RT]) ->
    case E#form.restrictions of
        none -> run_validate(LT, RT);
        Restricted -> case lists:member(Val, Restricted) of
                          true  -> run_validate(LT, RT);
                          false -> false
                      end
    end;
run_validate(_E, _S) ->
    false.

pget(K, L) -> proplists:get_value(K, L, undefined).

%%% EUnit tests

%% Simple test with sane input.
basic_test_() ->
    Expected = [#form{key=1, id = {[], common, "one"},
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
    ?_assert(validate_form(Expected, Submitted)).

%% Submit too many values.
too_much_test_() ->
    Expected = [#form{key=1, id = {[],common,"one"},
                      kind = input,
                      restrictions = none,
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","1"}]},
                 {struct,[{"label","two"},{"formula","2"}]}],
    ?_assertNot(validate_form(Expected, Submitted)).

not_enough_test_() ->
    Expected = [#form{key=1, id = {[],common,"one"},
                      kind = input,
                      restrictions = none,
                      attrs = []},
                #form{key=2, id = {[],common,"two"},
                      kind = input,
                      restrictions = none,
                      attrs = []}],
    Submitted = [{struct,[{"label","one"},{"formula","#"}]}],
    ?_assertNot(validate_form(Expected, Submitted)).

fake_name_test_() ->
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
    ?_assertNot(validate_form(Expected, Submitted)).

basic_select_radio_test_() ->
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
    ?_assert(validate_form(Expected, Submitted)).

basic_select_bad_val_test_() ->
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
    ?_assertNot(validate_form(Expected, Submitted)).

-define(E1, [{form, 1296210633529548,
              {["dla-piper", "initiatives", "new"], common,
               "Start date"}, input, none, []},
             {form, 1296210633879010,
              {["dla-piper", "initiatives", "new"], common,
               "Net carbon saving"}, input, none, []},
             {form, 1296210634159589,
              {["dla-piper", "initiatives", "new"], common,
               "Approved date"}, input, none, []},
             {form, 1296210633381975,
              {["dla-piper", "initiatives", "new"], common,
               "EPC improvement"}, select, ["Yes", "No", "Unclear"], []},
             {form, 1296210633701830,
              {["dla-piper", "initiatives", "new"], common,
               "Description"}, textarea, none, []},
             {form, 1296210633395115,
              {["dla-piper", "initiatives", "new"], common,
               "Initiative Links"}, input, none, []},
             {form, 1296210634179936,
              {["dla-piper", "initiatives", "new"], common,
               "Internal Rate of Return"}, input, none, []},
             {form, 1296210633637687,
              {["dla-piper", "initiatives", "new"], common,
               "Set-up cost"}, input, none, []},
             {form, 1296210633421498,
              {["dla-piper", "initiatives", "new"], common,
               "Net saving"}, input, none, []},
             {form, 1296210633779948,
              {["dla-piper", "initiatives", "new"], common,
               "Running cost pa"}, input, none, []},
             {form, 1296210633806364,
              {["dla-piper", "initiatives", "new"], common,
               "Payback (yrs)"}, input, none, []},
             {form, 1296210633713314,
              {["dla-piper", "initiatives", "new"], common,
               "CRC impact"}, select, ["Yes", "No", "Unclear"], []},
             {form, 1296210633763942,
              {["dla-piper", "initiatives", "new"], common,
               "Energy saving"}, input, none, []},
             {form, 1296210633791892,
              {["dla-piper", "initiatives", "new"], common,
               "Approval history"}, input, none, []},
             {form, 1296210633820446,
              {["dla-piper", "initiatives", "new"], common,
               "Project Lead"}, input, none, []},
             {form, 1296210633852981,
              {["dla-piper", "initiatives", "new"], common,
               "Status"}, select, ["Idea", "Pending", "Approved"], []},
             {form, 1296210633867672,
              {["dla-piper", "initiatives", "new"], common,
               "_"}, button, none, [{"dest", "/dla-piper/initiatives/new/"}]},
             {form, 1296210634193954,
              {["dla-piper", "initiatives", "new"], common,
               "Target energy source"}, select,
              ["Electricity", "Gas", "Solid Fuels", "Liquid Fuels"], []},
             {form, 1296210633685166,
              {["dla-piper", "initiatives", "new"], common,
               "Business Impact"}, textarea, none,[]},
             {form, 1296210633731544,
              {["dla-piper", "initiatives", "new"], common,
               "Ref."}, input, none, []},
             {form, 1296654958405823,
              {["dla-piper", "initiatives", "new"], common,
               "kWh impact pa (%)"}, input, none, []},
             {form, 1296210633743265,
              {["dla-piper", "initiatives", "new"], common,
               "Approval docs"}, input, none, []},
             {form, 1296210633663970,
              {["dla-piper", "initiatives", "new"], common,
               "Initiative Lifespan"}, input, none, []},
             {form, 1296210633837885,
              {["dla-piper", "initiatives", "new"], common,
               "Other benefits"}, textarea, none, []},
             {form, 1296210634205581,
              {["dla-piper", "initiatives", "new"], common,
               "Location"}, select, ["st-pauls-place", "noble-street"], []}
            ]).
-define(G1, [
             {struct,[{"label", "Ref."},{"formula",[]}]},
             {struct,[{"label", "Status"},{"formula", "Idea"}]},
             {struct,[{"label", "Location"},{"formula", "st-pauls-place"}]},
             {struct,[{"label", "Description"},{"formula",[]}]},
             {struct,[{"label", "Business Impact"},{"formula",[]}]},
             {struct,[{"label", "Project Lead"},{"formula",[]}]},
             {struct,[{"label", "Target energy source"},
                      {"formula", "Electricity"}]},
             {struct,[{"label", "Initiative Lifespan"},{"formula",[]}]},
             {struct,[{"label", "Set-up cost"},{"formula",[]}]},
             {struct,[{"label", "Running cost pa"},{"formula",[]}]},
             {struct,[{"label", "Net saving"},{"formula",[]}]},
             {struct,[{"label", "Energy saving"},{"formula",[]}]},
             {struct,[{"label", "Net carbon saving"},{"formula",[]}]},
             {struct,[{"label", "kWh impact pa (%)"},{"formula",[]}]},
             {struct,[{"label", "EPC improvement"},{"formula", "Yes"}]},
             {struct,[{"label", "CRC impact"},{"formula", "Yes"}]},
             {struct,[{"label", "Other benefits"},{"formula",[]}]},
             {struct,[{"label", "Payback (yrs)"},{"formula",[]}]},
             {struct,[{"label", "Internal Rate of Return"},{"formula",[]}]},
             {struct,[{"label", "Approval history"},{"formula",[]}]},
             {struct,[{"label", "Approved date"},{"formula",[]}]},
             {struct,[{"label", "Start date"},{"formula",[]}]},
             {struct,[{"label", "Initiative Links"},{"formula",[]}]},
             {struct,[{"label", "Approval docs"},{"formula",[]}]}
            ]).

sec_test_() ->
    Expected = ?E1,
    Got = ?G1,
    [
     ?_assertEqual(validate_form(Expected, Got), true)
    ].

