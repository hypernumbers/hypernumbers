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
         make/3,
         is_valid/3
        ]).

%% debugging API
-export([run/0]).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("spriki.hrl").
-include("auth2.hrl").
-include("security.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validate requests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_valid(security(), #refX{}, any()) -> true | false.
is_valid(Sec, Ref, Json) ->
    Approved = make_sec_approved(Ref, Sec),
    Candidate = make_candidate(Ref, Json),
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

-spec make(string(), #refX{}, auth_req()) -> security().
make(Form, Ref, Auth) ->
    % tpls aren't always well formed XML, but
    % wrapping them in a '<div></div>' pair makes 'em so
    Form2 = "<div>" ++ Form ++ "</div>",
    State = {Ref, Auth, []},
    Opts = [{event_fun, fun process/3},
            {event_state, State}],
    case xmerl_sax_parser:stream(Form2, Opts) of 
        {_ErrTag, Reason} ->
            throw(Reason);
        {ok, {_, _, List}, _} ->
            parse_transactions(lists:reverse(List))
    end.

process({startElement, [], "div", {[], "div"}, List}, _Y, {Ref, Auth, Acc}) ->
    Acc2 = case is_hn(List) of
               true  -> add_rec(List, Ref, Auth, Acc);
               false -> Acc
           end,
    {Ref, Auth, Acc2};
process({startElement, [], "form", {[], "form"}, _List}, _Y, {Ref, Auth, Acc}) ->
    {Ref, Auth, [formstart | Acc]};
process({endElement, [], "form", {[], "form"}}, _Y, {Ref, Auth, Acc}) ->
    {Ref, Auth, [formend | Acc]};
process(_X, _Y, State) -> State.

is_hn(List) ->
    case lists:keyfind("data-type", 3, List) of
        false  -> false;
        _Tuple -> true
    end.

-spec add_rec(list(), #refX{}, auth_req(), [#binding{}]) -> [#binding{}]. 
add_rec(List, Ref, Auth, Acc) -> 
    case make_r1(List, Ref, Auth, #binding{}) of
        #binding{to = []} -> Acc;
        B                 -> [B | Acc]
    end.

-spec make_r1(any(), #refX{}, auth_req(), #binding{}) -> #binding{}. 
make_r1([], _R, _AR, A) ->
    A;
make_r1([{[], [], "class", _} | T], R, AR, A) ->
    make_r1(T, R, AR, A);
make_r1([{[], [], "data-type", Ty} | T], R, AR, A) -> 
    make_r1(T, R, AR, A#binding{type = Ty});
make_r1([{[], [], "data-binding-from", F} | T], R, AR, A) -> 
    Path = extract_path(abs_path(R#refX.path, F)),
    case auth_srv2:get_any_view(R#refX.site, Path, AR) of
        {view, _} ->
            make_r1(T, R, AR, A#binding{from = F});
        _Else ->
            throw({permission_denied, F})
    end;
make_r1([{[], [], "data-binding-to", To} | T], R, AR, A)  -> 
    S = "_g/core/spreadsheet",
    Path = extract_path(abs_path(R#refX.path, To)),
    case auth_srv2:check_particular_view(R#refX.site, Path, AR, S) of
        {view, S} ->
            make_r1(T, R, AR, A#binding{to = To});
        _Else ->
            throw({permission_denied, To})
    end.

extract_path([$/|_] = Url) ->
    case string:tokens(Url, "/") of
        [] -> []; 
        L -> drop_last(L)
    end;
extract_path(_Other) -> [].

drop_last([_]) -> []; 
drop_last([X | Rest]) -> [X | drop_last(Rest)].
        
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
    Json = [{struct,[{"ref","/u/testuser/blah/A:A"},
                     {"formula","Field 1"}]},
            {struct,[{"ref","/u/testuser/blah/B:B"},
                     {"formula","Field 2"}]},
            {struct,[{"ref","/u/testuser/blah/C:C"},
                     {"formula","Field 3"}]},
            {struct,[{"ref","/u/testuser/blah/D:D"},
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
    Security = make(Form, RefX, {"testuser", []}),
    ?assert(is_valid(Security, RefX, Json)).
    
test2(RefX) ->
    Json = [{struct,[{"ref","/u/testuser/blah/D6"},{"formula","Test"}]}],
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
    Security = make(Form, RefX, {"testuser", []}),
    ?assert(is_valid(Security, RefX, Json)).

%% Same path, but column target vs accepted cell target.
test3(RefX) ->
    Json = [{struct,[{"ref","/u/testuser/blah/D:D"},{"formula","Test"}]}],
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
    Security = make(Form, RefX, {"testuser", []}),
    ?assertNot(is_valid(Security, RefX, Json)).

test4(RefX) ->
    Json = [{struct,[{"ref","/u/testuser/blah/D6"},{"formula","Test"}]}],
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
    Security = make(Form, RefX, {"testuser", []}),
    ?assertNot(is_valid(Security, RefX, Json)).

%% tests an absolute and a relative path binding
test5(RefX) ->
    Json = [{struct,[{"ref","/u/testuser/D6"},{"formula","Test"}]},
            {struct,[{"ref","/u/testuser/D7"},{"formula","Test"}]}],
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
        ++ "<div data-type='input' class='hn' data-binding-to='/u/testuser/D7'>"
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
    Security = make(Form, RefX, {"testuser", []}),
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


path_test_() ->
    [fun testA/0,
     fun testB/0,
     fun testC/0].

secure_no_perms_test_() -> 
    Site = "http://unit_test:1234",
    auth_srv2:clear_all_perms_DEBUG(Site),
    auth_srv2:add_view(Site, [], [everyone], "_g/core/spreadsheet"),
    auth_srv2:add_view(Site, ["[**]"], [everyone], "_g/core/spreadsheet"),
    auth_srv2:set_champion(Site, [], "_g/core/spreadsheet"),
    auth_srv2:set_champion(Site, ["[**]"], "_g/core/spreadsheet"),

    RefX = #refX{site = Site,
                 path = ["u","testuser","blah"],
                 obj = {page,"/"},
                 auth = []},
    {with, RefX, [fun test1/1,
                  fun test2/1,
                  fun test3/1,
                  fun test4/1,
                  fun test5/1]}.
