%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2007 - 2014, Hypernumbers.com
%%% @doc       System test library
%%% @end
%%% Created :  by gordon@hypernumbers.com
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-module(testsys).

-export([save/2, restore/2, restore/3, generate/0]).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(SYSTEST_DIR, "tests/system_test/").
-define(FIXTURE_DIR, "tests/system_test/fixtures/").

-define(pget(Key, List), proplists:get_value(Key, List, undefined)).
-define(rel(F), filename:absname(F, filename:dirname(
                                      filename:dirname(
                                        code:lib_dir(hypernumbers))))).


%% Persist the current page data located at Path as a json Fixture
save(Path, Fixture) ->
    CHeaders = [{"Accept", "application/json"}],
    case httpc:request(get, {Path, CHeaders}, [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Hds, JSON}} ->
            FN = filename:join(?FIXTURE_DIR, Fixture++".json"),
            file:write_file(FN, JSON);
        Other ->
            throw({bad_http_request_1, Other})
    end.


restore(Path, Fixture) ->
    restore(Path, Fixture, default).
restore(Path, Fixture, Profile) ->
    Type    = "application/json",
    Headers = [{"Accept", Type}],
    Body    = "{\"delete\":\"all\"}",
    case httpc:request(post, {Path, Headers, Type, Body}, [], [], Profile) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Hds, "\"success\""}} ->
            FN = filename:join(?rel(?FIXTURE_DIR), Fixture++".json"),
            hn_import:json_file(Path, FN);
        Other ->
            throw({bad_http_request_2, Other})
    end.

generate() ->
    Files      = filelib:wildcard(?FIXTURE_DIR++"*.json"),
    {ok, Tpls} = file:read_file(?SYSTEST_DIR++"test_SUITE.tpl"),
    Template   = binary_to_list(Tpls),
    [ gen_test(Template,X) || X <- Files ],
    ok.

gen_test(Template, Fixture) ->
    Name   = filename:basename(Fixture,".json"),
    Suite  = Name++"_SUITE",
    AFile  = ?SYSTEST_DIR++"actions/"++Name,

    Action = filename:absname(case filelib:is_file(AFile) of
                                  true  -> AFile;
                                  false -> ?SYSTEST_DIR++"actions/default"
                              end),

    {ok, JsonTxt} = file:read_file(Fixture),
    {struct, Json} = hn_util:js_to_utf8(mochijson:decode(JsonTxt)),
    {struct, Cells} = ?pget("cell", Json),
    {struct, HeadRow} = ?pget("1", Cells),
    {struct, A1} = ?pget("1", HeadRow),

    Count = case ?pget("value", A1) of
                "NOTESTS" -> 0;
                Range ->
                    {_,X1,_,X2} = util2:parse_range(Range),
                    X2-X1+2
            end,

    Ref = #refX{site="http://sys.hypernumbers.dev:9000", type = url, path=[Name]},
    Url = Ref#refX.site ++ hn_util:list_to_path(Ref#refX.path),

    Names = gen_names(Name, Count),
    Cases = gen_test_cases(Name, Name, Count),

    Test  = ?FORMAT(Template, [Suite, Action, Url, Name, Ref#refX.site,
                               Names, Cases]),

    file:write_file(?SYSTEST_DIR++Suite++".erl", Test).

gen_names(_Name, 0) ->
    [];
gen_names(Name, Count) ->
    Str = [ Name++"_A"++itol(X) || X <- lists:seq(2, Count) ],
    string:join(Str, ",").

gen_test_cases(_Name, _Path, 0) ->
    [];
gen_test_cases(Name, Path, N) ->
    Str = "~s(_Conf) -> ~n \"Success\" = get_val(#refX{path=[~p],type=url,obj="
        "{cell,{1,~p}}}).~n",

    [ ?FORMAT(Str,[Name++"_A"++itol(X), Path, X])
      || X <- lists:seq(2, N) ].

itol(X) ->
    integer_to_list(X).
