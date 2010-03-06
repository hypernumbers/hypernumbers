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
         validate_get/3,
         validate_trans/3,
         abs_path/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("spriki.hrl").
-include("auth2.hrl").
-include("security.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validate requests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec validate_get(security(), string(), string()) -> true | false. 
validate_get({Gets, _}, BasePath, Target) ->
    AbsTarget = abs_path(BasePath, Target),
    is_valid_get(Gets, BasePath, AbsTarget).

-spec is_valid_get([string()], [string()], string()) -> true | false. 
is_valid_get([], _, _) -> false;
is_valid_get([GetRel | Rest], Base, AbsTarget) ->
    case abs_path(Base, GetRel) of
        AbsTarget -> true;
        _Else -> is_valid_get(Rest, Base, AbsTarget)
    end.
    
-spec validate_trans(security(), #refX{}, any()) -> true | false.
validate_trans({_, TransSet}, Ref, Json) ->
    Approved = make_approved(Ref, TransSet),
    Candidate = make_candidate(Ref, Json),
    is_valid_trans(Approved, Candidate).

-spec is_valid_trans([[{string(), #binding{}}]],
                     [{string(), string()}])
                    -> true | false. 
is_valid_trans([], _Candidate) -> false;
is_valid_trans([Trans | Rest], Candidate) ->
    case screen_candidate(Trans, Candidate) of
        true -> true; 
        false -> is_valid_trans(Rest, Candidate)
    end.

-spec screen_candidate([{string(), #binding{}}], 
                       [{string(), string()}]) 
                      -> true | false.
screen_candidate([], []) -> true;
screen_candidate([{To, #binding{type=T}} | RestApp], [{To, _V} | RestCan]) 
  when T =:= "input"; T =:= "textarea" ->
    screen_candidate(RestApp, RestCan);
screen_candidate([{To, _Binding} | RestApp], [{To, _V} | RestCan]) ->
    screen_candidate(RestApp, RestCan);
screen_candidate(_, _) -> false.

-spec make_approved(#refX{}, [transaction()]) 
                             -> [[{string(), #binding{}}]].
make_approved(#refX{path=Base}, TransSet) ->
    [make_trans_approved(Base, T) || T <- TransSet].
    
-spec make_trans_approved(string(), transaction()) -> [{string(), #binding{}}].
make_trans_approved(Base, Trans) ->
    Trans2 = [{abs_path(Base, B#binding.to), B} || B <- Trans],
    lists:keysort(1, Trans2).

-spec make_candidate(#refX{}, any()) -> [{string(), string()}]. 
make_candidate(Ref, [{"set", {struct, [{"list", {array, Trans}}]}}]) ->
    make_candidate_(Ref, Trans);
make_candidate(Ref, [{"set", Action}]) ->
    make_candidate_(Ref, [Action]).

make_candidate_(#refX{path=Base}, Trans) ->
    PVs = [{abs_path(Base, P), V} ||
              {struct,[{"ref", P}, {"formula", V}]} <- Trans],
    lists:keysort(1, PVs).

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
            parse_bindings(lists:reverse(List), Ref, Auth)
    end.

process({startElement, [], "div", {[], "div"}, List}, _Y, {Ref, AR, Acc}) ->
    Acc2 = case is_hn(List) of
               true  -> [bind(List, Ref, AR, #binding{}) | Acc];
               false -> Acc
           end,
    {Ref, AR, Acc2};
process({startElement, [], "form", {[], "form"}, _List}, _Y, {Ref, AR, Acc}) ->
    {Ref, AR, [formstart | Acc]};
process({endElement, [], "form", {[], "form"}}, _Y, {Ref, AR, Acc}) ->
    {Ref, AR, [formend | Acc]};
process(_X, _Y, State) -> State.

is_hn(List) ->
    case lists:keyfind("data-type", 3, List) of
        false  -> false;
        _Tuple -> true
    end.

-spec can_read(string(), string(), auth_req()) -> true | false.
can_read(Site, P, AR) ->
    Path = string:tokens(just_path(P), "/"),
    case auth_srv2:get_any_view(Site, Path, AR) of
        {view, _} -> true;
        _Else -> false
    end.

-spec can_write(string(), string(), auth_req()) -> true | false. 
can_write(Site, P, AR) ->
    S = "_g/core/spreadsheet",
    Path = string:tokens(just_path(P), "/"),
    case auth_srv2:check_particular_view(Site, Path, AR, S) of
        {view, S} -> true; 
        _Else -> false
    end.

-spec bind(list(), #refX{}, auth_req(), #binding{}) -> #binding{}. 
bind([], _R, _AR, B) ->
    B;
bind([{[], [], "data-type", Ty} | T], R, AR, B) -> 
    bind(T, R, AR, B#binding{type = Ty});
bind([{[], [], "data-binding-from", F} | T], R, AR, B) -> 
    case can_read(R#refX.site, abs_path(R#refX.path, F), AR) of
        true -> bind(T, R, AR, B#binding{from = F});
        false -> throw({permission_denied, F})
    end;
bind([{[], [], "data-binding-to", To} | T], R, AR, B) -> 
    case can_write(R#refX.site, abs_path(R#refX.path, To), AR) of
        true -> bind(T, R, AR, B#binding{to = To});
        false -> throw({permission_denied, To})
    end;
bind([{[], [], _, _} | T], R, AR, B) ->
    bind(T, R, AR, B).

-spec parse_bindings([formstart | formend | #binding{}], 
                     #refX{}, 
                     auth_req()) 
                    -> security().

parse_bindings(List, #refX{site=Site, path=Path}, AR) ->
    Gets = lists:usort([just_path(F)
                        || #binding{from = F} <- List,
                           F /= [],
                           can_read(Site, abs_path(Path, F), AR)]),
    Filter = fun(#binding{to = []}) -> false;
                (_) -> true 
             end,
    List2 = lists:filter(Filter, List),
    TransSet = gather(List2, [], [], false),
    {Gets, TransSet}.

%% if the boolean is true we are in a form and we accumulate the bindings in A1
%% when the form ends we throw the contents of A1 into the main accumulator in A2
%% if the boolean is false, then each binding is a transaction in its own right
%% expects forms to terminate in the model or will crash
gather([], [], A2, false)              -> A2;
gather([formstart | T], A1, A2, false) -> gather(T, A1, A2, true);
gather([formend | T], A1, A2, true)    -> gather(T, [], [A1 | A2], false);
gather([H | T], A1, A2, true)          -> gather(T, [H | A1], A2, true);
gather([H | T], _A1, A2, false)        -> gather(T, [], [[H] | A2], false).

-spec just_path(string()) -> string().
just_path([]) -> [];
just_path(Url) ->
    case lists:last(Url) of
        $/ -> Url;
        _Or ->
            Lead = if hd(Url) == $/ -> "/";
                      true -> [] end,
            Lead ++ case string:tokens(Url, "/") of
                        [] -> [];
                        [_S] -> [];
                        L -> string:join(drop_last(L), "/") ++ "/"
                    end
    end.

-spec drop_last(list()) -> list().
drop_last([_]) -> []; 
drop_last([X | Rest]) -> [X | drop_last(Rest)].

-spec abs_path([string()], string()) -> string().
abs_path(_, [$/ | _] = Path2) -> Path2;
abs_path(Path, Path2) -> 
    Path2Toks = string:tokens(Path2, "/"),
    PathR = lists:reverse(Path),
    "/" ++ string:join(abs_path2(PathR, Path2Toks), "/").

abs_path2(P1, P2) ->
    abs_path2(P1, P2, queue:new()).
abs_path2([], [], Q) ->
    queue:to_list(Q);
abs_path2(Left, ["." | RestR], Q) ->
    abs_path2(Left, RestR, Q);
abs_path2(Left, [".." | RestR], Q) ->
    case {queue:is_empty(Q), Left} of
        {false, _} -> abs_path2(Left, RestR, queue:drop_r(Q));
        {true, []} -> abs_path2([".."], RestR, Q); 
        {true, [".."|_]} -> abs_path2([".." | Left], RestR, Q);
        {true, [_|RestL]} -> abs_path2(RestL, RestR, Q)
    end;
abs_path2([], [X | RestR], Q) ->
    abs_path2([], RestR, queue:in(X, Q));
abs_path2([".." | RestL], [_ | RestR], Q) ->
    abs_path2(RestL, RestR, Q);
abs_path2([".." | _], [], _Q) ->
    "";
abs_path2([X | RestL], Right, Q) ->
    abs_path2(RestL, Right, queue:in_r(X, Q)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% EUnit Tests                                                              %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abspath_test_() ->
    Base = ["first", "second", "third"],
    [?_assertEqual("/already/absolute", abs_path(Base, "/already/absolute")),
     ?_assertEqual("/first/second/third/d1", abs_path(Base, "d1")),
     ?_assertEqual("/first/second/third/d1", abs_path(Base, "./d1")),
     ?_assertEqual("/first/second/d1", abs_path(Base, "../d1")),
     ?_assertEqual("/d1", 
                   abs_path(Base, "../third/../../ok/.././../././first//../d1")),
     ?_assertEqual("/first/second/d1", abs_path(Base, "../something/../d1"))
    ].

just_path_test_() ->
    [?_assertEqual("", just_path("")),
     ?_assertEqual("/", just_path("/")),
     ?_assertEqual("/", just_path("/blah")),
     ?_assertEqual("", just_path("..")),
     ?_assertEqual("../", just_path("../")),
     ?_assertEqual("../", just_path("../A1")),
     ?_assertEqual("./", just_path("./A1")),
     ?_assertEqual("", just_path("blah")),
     ?_assertEqual("/", just_path("/blah")),
     ?_assertEqual("/blah/", just_path("/blah/")),
     ?_assertEqual("/blah/", just_path("/blah/A:A")),
     ?_assertEqual("/blah/more/path/", just_path("/blah/more/path/A:A"))
    ].

trans_all_perms_test_() -> 
    Site = "http://example.com:1234",
    Setup = fun() -> 
                    auth_srv2:start_link(Site),
                    auth_srv2:add_view(Site, [], [everyone], "_g/core/spreadsheet"),
                    auth_srv2:add_view(Site, ["[**]"], [everyone], "_g/core/spreadsheet"),
                    auth_srv2:set_champion(Site, [], "_g/core/spreadsheet"),
                    auth_srv2:set_champion(Site, ["[**]"], "_g/core/spreadsheet"),
                    #refX{site = Site, path = ["u","testuser","blah"]}
            end,
    Cleanup = fun(_) -> auth_srv2:stop(Site) end,
    {setup, Setup, Cleanup, {with, [fun test1/1,
                                     fun test2/1,
                                     fun test3/1,
                                     fun test4/1,
                                     fun test5/1]}}.

test1(RefX) ->
    Json = [{"set",
             {struct,[{"list",
                       {array,[{struct,[{"ref","/u/testuser/blah/A:A"},
                                        {"formula","Field 1"}]},
                               {struct,[{"ref","/u/testuser/blah/B:B"},
                                        {"formula","Field 2"}]},
                               {struct,[{"ref","/u/testuser/blah/C:C"},
                                        {"formula","Field 3"}]},
                               {struct,[{"ref","/u/testuser/blah/D:D"},
                                        {"formula","Field 4"}]}]}}]}}],
    Form = "<div id='ventris'>"
        ++ "<link rel='stylesheet' type='text/css' href='/templates/ventris/default.css'>"
        ++ "</link>"
        ++ "<div id='outer'>"
        ++ "<div id='inner'>"
        ++ "<div class='clear'>"
        ++ "<div id='header'>"
        ++ "<div data-binding-from='/A1' foo='bar' class='hn header' data-type='text'>"
        ++ "text</div>"
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
    ?assert(validate_trans(Security, RefX, Json)).
    
test2(RefX) ->
    Json = [{"set",
             {struct,[{"ref","/u/testuser/blah/D6"},{"formula","Test"}]}}],
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
    ?assert(validate_trans(Security, RefX, Json)).

%% Same path, but column target vs accepted cell target.
test3(RefX) ->
    Json = [{"set", {struct,[{"ref","/u/testuser/blah/D:D"},{"formula","Test"}]}}],
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
    ?assertNot(validate_trans(Security, RefX, Json)).

test4(RefX) ->
    Json = [{"set", {struct,[{"ref","/u/testuser/blah/D6"},{"formula","Test"}]}}],
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
    ?assertNot(validate_trans(Security, RefX, Json)).

%% tests an absolute and a relative path binding
test5(RefX) ->
    Json = [{"set",
             {struct,[{"list",
                       {array, [{struct,[{"ref","/u/testuser/D6"},{"formula","Test"}]},
                                {struct,[{"ref","/u/testuser/D7"},{"formula","Test"}]}]
                       }}]}}],
    Form = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'>"
        "</link>"
        "<div id='outer'>"
        "<div id='inner'>"
        "<div class='clear'>"
        "<div id='header'>"
        "<div data-type='text' class='hn header' data-binding-from='B2'>"
        "</div>"
        "<div id='menu'>"
        "</div>"
        "</div>"
        "</div>"
        "<div id='plaincontent'>"
        "</div>"
        "<div id='contentwrapper'>"
        "<div id='primarycontent'><a href='http://google.com/?foo=bar&amp;fu=test'>testin</a>"
        "</div>"
        "<div id='secondarycontent'>"
        "</div>"
        "<form class='hn' data-type='form'>"
        "<div data-type='input' class='hn' data-binding-to='../blah/../D6'>"
        "</div>"
        "<div data-type='input' class='hn' data-binding-to='/u/testuser/D7'>"
        "</div>"
        "</form>"
        "</div>"
        "<div id='plaincontent'>"
        "</div>"
        "<div id='footer'>"
        "</div>"
        "</div>"
        "</div>"
        "</div>",
    Security = make(Form, RefX, {"testuser", []}),
    ?assert(validate_trans(Security, RefX, Json)).
