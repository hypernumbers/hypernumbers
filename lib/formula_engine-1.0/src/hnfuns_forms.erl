%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Functions that pertain to forms
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers.com

-module(hnfuns_forms).

-export([
         input/1,
         textarea/1,
         button/1,
         radio/1,
         select/1
        ]).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-type trans() :: common | string().
-type html() :: string().

%-define(default_str_rules, [first_array, cast_numbers, cast_bools,
%                            cast_blanks, cast_dates ]).

input([])   -> input([""]);
input([V1]) ->
    Label = muin_collect:col([V1], [first_array, fetch, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    muin_util:run_or_err([Label], fun input_/1).

textarea([])   -> textarea([""]);
textarea([V1]) ->
    Label = muin_collect:col([V1], [first_array, fetch, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    muin_util:run_or_err([Label], fun textarea_/1).

button([])      -> button(["Submit Form"]);
button([Title]) -> button([Title, "Thanks for completing our form."]);
button([Title, Response]) -> button([Title, Response, "./replies/"]);
button([Title, Response, Results]) ->
    muin_collect:col([Title, Response, Results], [first_array, fetch, {cast,str}],
        [return_errors, {all, fun muin_collect:is_string/1}],
        fun([NTitle, NResponse, NResult]) ->
                button_(NTitle, NResponse, NResult)
        end).


select([])      -> select([""]);
select([Label]) -> select([Label, {array, [["option 1", "option 2"]]}]);
select([V1, V2]) ->
    [Label] = muin_collect:col([V1], [first_array, fetch, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    Opts = muin_collect:col([V2], [fetch, flatten, {ignore, blank}, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    muin_util:apply([Label, Opts], fun select_/2).

radio([])      -> radio([""]);
radio([Label]) -> radio([Label, {array, [["option 1", "option 2"]]}]);
radio([V1, V2]) ->
    [Label] = muin_collect:col([V1], [first_array, fetch, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    Opts = muin_collect:col([V2], [fetch, flatten, {ignore, blank}, {cast,str}],
                [return_errors, {all, fun muin_collect:is_string/1}]),
    muin_util:apply([Label, Opts], fun radio_/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec input_([string()], string(), trans()) -> {rawform, #form{}, html()}.
input_(Label) -> input_(Label, "", common).
%input_(Label, Default) -> input_(Label, Default, common).
input_([Label], _Default, Trans) ->
    Form = #form{id = {Trans, Label}, kind = input},
    Html = lists:flatten("<input type='input' class='hninput' " ++
                         "data-name='default' " ++
                         "data-label='"++Label++"' />"),
    {rawform, Form, Html}.

-spec textarea_([string()], string(), trans()) -> {rawform, #form{}, html()}.
textarea_(Label) -> textarea_(Label, "", common).
%textarea_(Label, Default) -> textarea_(Label, Default, common).
textarea_([Label], _Default, Trans) ->
    Form = #form{id = {Trans, Label}, kind = textarea},
    Html = lists:flatten("<textarea class='hntext' data-name='default' "
                         ++ "data-label='"++Label++"'></textarea>"),
    {rawform, Form, Html}.

-spec button_(string(), string(), string()) -> {rawform, #form{}, html()}.
button_(Value, Response, ResultsPath) ->
    Trans = common,
    Origin = hn_util:list_to_path(muin:context_setting(path)),
    Form = #form{id = {Trans, "_"}, 
                 kind = button, 
                 attrs = [{"dest", Origin}]},
    Html = lists:flatten("<input type='submit' class='hninput' value='"++Value++"'"
                         ++ " data-results='" ++ ResultsPath ++ "'"
                         ++ " data-origin='" ++ Origin ++ "?view=webpage'"
                         ++ " data-form-name='default' data-response='"
                         ++ Response ++ "' />"),
    {rawform, Form, Html}.

-spec select_(string(), [string()]) -> {rawform, #form{}, html()}.
select_(Label, Options) ->
    Trans = common,
    Form = #form{id = {Trans, Label},
                 kind = select,
                 restrictions = Options},
    Opts = [ "<option>" ++ Option ++ "</option>" || Option <- Options ],
    Html = lists:flatten("<select class='hninput' data-name='default' "++
                         "data-label='" ++ Label ++ "' >" ++ Opts ++ 
                         "</select>"),
    {rawform, Form, Html}.

-spec radio_(string(), [string()]) -> {rawform, #form{}, html()}.
radio_(Label, Options) ->
    Trans = common,
    Name = "tmp_" ++ muin_util:create_name(),
    Form = #form{id = {Trans, Label},
                 kind = radio,
                 restrictions = Options},
    [First | Rest] = Options,
    ID1 = "id_"++muin_util:create_name(),
    FirstOpt = "<div class='radio'><label for='"++ID1++"'>"
        ++ First ++ "</label><input id='"++ID1++"' type='radio' value='" ++
        First ++ "' name='" ++ Name ++ "' checked='true' /></div>",
    RestOpts = [ make_radio(Name, Opt) || Opt <- Rest],
    Opts = [FirstOpt | RestOpts],
    Html = lists:flatten("<div class='hninput' data-name='default' "++
                         "data-label='" ++ Label ++ "' >" ++ Opts ++ 
                         "</div>"),
    {rawform, Form, Html}.

make_radio(Name, Opt) ->
    ID = "id_"++muin_util:create_name(),
    "<div class='radio'><label for='"++ID++"'>" ++ Opt ++ "</label><input id='"++ID++
        "' type='radio' value='" ++ Opt ++ "' name='" ++ Name ++ "' /></div>".
