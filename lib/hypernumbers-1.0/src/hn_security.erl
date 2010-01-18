%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2009 by Gordon Guthrie
%%
%% this is how a form posts...
%% in ipost Array is [{struct,[{"ref","/u/gordon/blah/A:A"},
%%                             {"formula","Field 1"}]},
%%                    {struct,[{"ref","/u/gordon/blah/B:B"},
%%                             {"formula","Field 2"}]},
%%                    {struct,[{"ref","/u/gordon/blah/C:C"},
%%                             {"formula","Field 3"}]},
%%                    {struct,[{"ref","/u/gordon/blah/D:D"},
%%                             {"formula","Field 4"}]}]
%% Ref is {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]}
%%
%% this is how a cell posts
%% in ipost Array is [{struct,[{"ref","/u/gordon/blah/D:D"},{"formula","Test"}]}]
%% Ref is {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]}

%%%-------------------------------------------------------------------
-module(hn_security).

%% API
-export([
         make_security/2,
         is_valid/3
        ]).

%% debugging API
-export([run/0]).

-include_lib("eunit/include/eunit.hrl").
-include("spriki.hrl").
-include("security.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validate requests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_valid(security(), #refX{}, any()) -> true | false.
is_valid(Sec, Ref, Json) ->
    Approved = make_sec_approved(Ref, Sec),
    Candidate = make_candidate(Ref, Json),
    io:format("Approved is:~n~p~n", [Approved]),
    io:format("Candidate is:~n~p~n", [Candidate]),
    is_valid(Approved, Candidate).

-spec is_valid([[{string(), #binding{}}]],
               [{string(), string()}])
              -> true | false. 
is_valid([], _Candidate) -> false;
is_valid([Trans | Rest], Candidate) ->
    case screen_candidate(Trans, Candidate) of
        true -> true; 
        false -> is_valid(Rest, Candidate)
    end.

-spec screen_candidate([{string(), #binding{}}], 
                       [{string(), string()}]) 
                      -> true | false.
screen_candidate([], []) -> true;
screen_candidate([{To, #binding{type=T}} | RestApp], [{To, _V} | RestCan]) 
  when T =:= "input"; T =:= "textarea" ->
    screen_candidate(RestApp, RestCan);
screen_candidate([{To, #binding{from=F}} | RestApp], [{To, F} | RestCan]) ->
    screen_candidate(RestApp, RestCan);
screen_candidate(_, _) -> false.

-spec make_sec_approved(#refX{}, security()) -> [[{string(), #binding{}}]].
make_sec_approved(#refX{path=Base}, Security) ->
    [make_trans_approved(Base, S) || S <- Security].
    
-spec make_trans_approved(string(), transaction()) -> [{string(), #binding{}}].
make_trans_approved(Base, Trans) ->
    Trans2 = [{abs_path(Base, B#binding.to), B} || B <- Trans],
    lists:keysort(1, Trans2).

-spec make_candidate(#refX{}, any()) -> [{string(), string()}]. 
make_candidate(#refX{path=Base}, Json) ->
    PVs = [{abs_path(Base, P), V} || {struct,[{"ref", P},
                                              {"formula", V}]}
                                         <- Json],
    lists:keysort(1, PVs).

-spec abs_path([string()], string()) -> string(). 
abs_path(Path, [$. | _T] = Path2)  -> make_abs2(Path, Path2);
abs_path(_Path, [$/ | _T] = Path2) -> Path2;
abs_path(Path, Path2)              -> hn_util:list_to_path(Path) ++ Path2.

make_abs2(Path, Path2) ->
    R = lists:reverse(Path),
    P3 = string:tokens(Path2, "/"),
    {P4, [Ref]} = lists:split(length(P3) - 1, P3),
    NewPath = make_abs3(R, P4),
    NewPath ++ Ref.

make_abs3(Path, [])                -> hn_util:list_to_path(lists:reverse(Path));
make_abs3(Path, ["." | T2])        -> make_abs3(Path, T2);
make_abs3([_H1 | T1], [".." | T2]) -> make_abs3(T1, T2);
make_abs3(Path, [H2 | T2])         -> make_abs3([H2 | Path], T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Construct Security Objects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_security(#refX{}, string()) -> security().
make_security(Ref, File) ->
    % tpls aren't always well formed XML, but
    % wrapping them in a '<div></div>' pair makes 'em so
    File2 = "<div>" ++ File ++ "</div>",
    State = {Ref, []},
    Opts = [{event_fun, fun process/3},
            {event_state, State}],
    {ok, {_, List}, _} = xmerl_sax_parser:stream(File2, Opts),
    parse_transactions(lists:reverse(List)).

process({startElement, [], "div", {[], "div"}, List}, _Y, {Ref, Acc}) ->
    Acc2 = case is_hn(List) of
               true  -> add_rec(List, Ref, Acc);
               false -> Acc
           end,
    {Ref, Acc2};
process({startElement, [], "form", {[], "form"}, _List}, _Y, {Ref, Acc}) ->
    {Ref, [formstart | Acc]};
process({endElement, [], "form", {[], "form"}}, _Y, {Ref, Acc}) ->
    {Ref, [formend | Acc]};
process(_X, _Y, State) -> State.

is_hn(List) ->
    case lists:keyfind("data-type", 3, List) of
        false  -> false;
        _Tuple -> true
    end.

-spec add_rec(any(), #refX{}, [#binding{}]) -> [#binding{}]. 
add_rec(List, Ref, Acc) -> 
    case make_r1(List, Ref, #binding{}) of
        #binding{to = []} -> Acc;
        B                 -> [B | Acc]
    end.

-spec make_r1(any(), #refX{}, #binding{}) -> #binding{}. 
make_r1([], _R, A) ->
    A;
make_r1([{[], [], "class", _} | T], R, A) ->
    make_r1(T, R, A);
make_r1([{[], [], "data-type", Ty} | T], R, A) -> 
    make_r1(T, R, A#binding{type = Ty});
make_r1([{[], [], "data-binding-from", F} | T], R, A) -> 
    %% check that we can read this place.
    make_r1(T, R, A#binding{from = F});
make_r1([{[], [], "data-binding-to", To} | T], R, A)  -> 
    %% check that we can write here... 
    make_r1(T, R, A#binding{to = To}).

                                                 
-spec parse_transactions([formstart | formend | #binding{}]) -> security().
%% if the boolean is true we are in a form and we accumulate the bindings in A1
%% when the form ends we throw the contents of A1 into the main accumulator in A2
%% if the boolean is false, then each binding is a transaction in its own right
%% expects forms to terminate in the model or will crash
parse_transactions(List) -> make_t1(List, [], [], false).

make_t1([], [], A2, false)              -> A2;
make_t1([formstart | T], A1, A2, false) -> make_t1(T, A1, A2, true);
make_t1([formend | T], A1, A2, true)    -> make_t1(T, [], [A1 | A2], false);
make_t1([H | T], A1, A2, true)          -> make_t1(T, [H | A1], A2, true);
make_t1([H | T], _A1, A2, false)        -> make_t1(T, [], [[H] | A2], false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Debugging API                                                            %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run() ->
    File = "/opt/code/trunk/var/docroot/127.0.0.1&9000/_g/hypernumbers/home.tpl",
    Opts = [{event_fun, fun process/3}],

    {ok, List, _} = xmerl_sax_parser:file(File, Opts),
    parse_transactions(lists:reverse(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% EUnit Tests                                                              %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test1(RefX) ->
    Json = [{struct,[{"ref","/u/gordon/blah/A:A"},
                     {"formula","Field 1"}]},
            {struct,[{"ref","/u/gordon/blah/B:B"},
                     {"formula","Field 2"}]},
            {struct,[{"ref","/u/gordon/blah/C:C"},
                     {"formula","Field 3"}]},
            {struct,[{"ref","/u/gordon/blah/D:D"},
                     {"formula","Field 4"}]}],
    Form = "<div id='ventris'>"
        ++ "<link rel='stylesheet' type='text/css' href='/templates/ventris/default.css'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-binding-from='/A1' class='hn header' data-type='text'>"
        ++ "</div>"
        ++ "<div id='menu'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='contentwrapper'>"
        ++ "<div id='primarycontent'>"
        ++ "</div>"
        ++ "<div id='secondarycontent'>"
        ++ "<form class='hn' data-type='form'>"
        ++ "<label>"
        ++ "<span>"
        ++ "<div data-binding-from='A3' class='hn' data-type='text'>"
        ++ "</div>"
        ++ "</span>"
        ++ "</label>"
        ++ "<div class='hn' data-type='input' data-binding-to='A:A'>"
        ++ "</div>"
        ++ "<label>"
        ++ "<span>"
        ++ "<div data-binding-from='B3' class='hn' data-type='text'>"
        ++ "</div>"
        ++ "</span>"
        ++ "</label>"
        ++ "<div class='hn' data-type='input' data-binding-to='B:B'>"
        ++ "</div>"
        ++ "<label>"
        ++ "<span>"
        ++ "<div data-binding-from='C3' class='hn' data-type='text'>"
        ++ "</div>"
        ++ "</span>"
        ++ "</label>"
        ++ "<div class='hn' data-type='input' data-binding-to='C:C'>"
        ++ "</div>"
        ++ "<label>"
        ++ "<span>"
        ++ "<div data-binding-from='D3' class='hn' data-type='text'>"
        ++ "</div>"
        ++ "</span>"
        ++ "</label>"
        ++ "<div class='hn' data-type='input' data-binding-to='D:D'>"
        ++ "</div>"
        ++ "<input type='submit' value='submit'>"
        ++ "</input>"
        ++ "</form>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Security = make_security(RefX, Form),
    ?assert(is_valid(Security, RefX, Json)).
    
test2(RefX) ->
    Json = [{struct,[{"ref","/u/gordon/blah/D6"},{"formula","Test"}]}],
    Form  = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-type='text' class='hn header' data-binding-from='B2'>"
        ++ "</div>"
        ++ "<div id='menu'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='contentwrapper'>"
        ++ "<div id='primarycontent'>"
        ++ "</div>"
        ++ "<div id='secondarycontent'>"
        ++ "</div>"
        ++ "<div data-type='input' class='hn' data-binding-to='D6'>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Security = make_security(RefX, Form),
    ?assert(is_valid(Security, RefX, Json)).

%% Same path, but column target vs accepted cell target.
test3(RefX) ->
    Json = [{struct,[{"ref","/u/gordon/blah/D:D"},{"formula","Test"}]}],
    Form  = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-type='text' class='hn header' data-binding-from='B2'>"
        ++ "</div>"
        ++ "<div id='menu'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='contentwrapper'>"
        ++ "<div id='primarycontent'>"
        ++ "</div>"
        ++ "<div id='secondarycontent'>"
        ++ "</div>"
        ++ "<div data-type='input' class='hn' data-binding-to='D6'>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Security = make_security(RefX, Form),
    ?assertNot(is_valid(Security, RefX, Json)).

test4(RefX) ->
    Json = [{struct,[{"ref","/u/gordon/blah/D6"},{"formula","Test"}]}],
    Form  = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-type='text' class='hn header' data-binding-from='B2'>"
        ++ "</div>"
        ++ "<div id='menu'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='contentwrapper'>"
        ++ "<div id='primarycontent'>"
        ++ "</div>"
        ++ "<div id='secondarycontent'>"
        ++ "</div>"
        ++ "<div data-type='input' class='hn' data-binding-to='/D6'>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Security = make_security(RefX, Form),
    ?assertNot(is_valid(Security, RefX, Json)).

%% tests an absolute and a relative path binding
test5(RefX) ->
    Json = [{struct,[{"ref","/u/gordon/D6"},{"formula","Test"}]},
            {struct,[{"ref","/u/gordon/D7"},{"formula","Test"}]}],
    Form = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-type='text' class='hn header' data-binding-from='B2'>"
        ++ "</div>"
        ++ "<div id='menu'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='contentwrapper'>"
        ++ "<div id='primarycontent'>"
        ++ "</div>"
        ++ "<div id='secondarycontent'>"
        ++ "</div>"
        ++ "<form class='hn' data-type='form'>"
        ++ "<div data-type='input' class='hn' data-binding-to='../blah/../D6'>"
        ++ "</div>"
        ++ "<div data-type='input' class='hn' data-binding-to='/u/gordon/D7'>"
        ++ "</div>"
        ++ "</form>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Security = make_security(RefX, Form),
    ?assert(is_valid(Security, RefX, Json)).

testA() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "d1",
    Ret = abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ?assertEqual("/blah/bloh/bleh/d1", Ret).

testB() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "../d1",
    Ret = abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ?assertEqual("/blah/bloh/d1", Ret).

testC() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "./d1",
    Ret = abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ?assertEqual("/blah/bloh/bleh/d1", Ret).


unit_test_() -> 
    RefX = #refX{site= "http://127.0.0.1:9000",
                 path = ["u","gordon","blah"],
                 obj = {page,"/"},
                 auth = []},
    [fun testA/0,
     fun testB/0,
     fun testC/0,
     {with, RefX, [fun test1/1,
                   fun test2/1,
                   fun test3/1,
                   fun test4/1,
                   fun test5/1]}].
