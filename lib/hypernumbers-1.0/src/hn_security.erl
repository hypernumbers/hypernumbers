%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_security).

%% API
-export([
         make_security/1,
         is_approved/3
        ]).


%% debugging API
-export([run/0]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("spriki.hrl").
-include("security.hrl").

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

is_approved(_RefX, _Json, [])    -> false;
is_approved(RefX, Json, [H | T]) ->
    case is_approved2(RefX, Json, H) of
        true  -> true;            
        false -> is_approved(RefX, Json, T)
    end.

is_approved2(RefX, Json, Transaction) ->
    Approved = make_approved(RefX, Transaction),
    Candidate = make_candidate(Json),
    is_app3(Candidate, Approved).

%% both lists must be the same length
is_app3([], [])                              -> true;
is_app3(_X, [])                              -> false;
is_app3([], _X)                              -> false;
is_app3([{P, _} | T1], [{P, anything} | T2]) -> is_app3(T1, T2);
is_app3([{P, V} | T1], [{P, L} | T2])        -> case lists:member(V, L) of
                                                    true  -> is_app3(T1, T2);
                                                    false -> false
                                                end;
is_app3([{_P1, _} | _T1], [{_P2, _} | _T2])  -> false. % testing _P1 =/= _P2

make_candidate(Json) -> lists:keysort(1, make_c2(Json, [])).

make_c2([], Acc) -> Acc;
make_c2([{struct, [{"ref", Path}, {"formula", Val}]} | T], Acc) ->
    make_c2(T, [{Path, Val} | Acc]).

make_approved(RefX, Transaction) -> lists:keysort(1, make_a2(RefX, Transaction, [])).

make_a2(_RefX, [], Acc)      -> Acc;
make_a2(RefX,  [H | T], Acc) -> make_a2(RefX, T, [make_a3(RefX, H) | Acc]).

make_a3(#refX{path = P}, #binding{type = Type, to = To}) ->
    P2 = make_abs_path(P, To),
    {P2, allowed_values(P2, Type)}.

allowed_values(_Path, "text")     -> "lookup text";
allowed_values(_Path, "input")    -> anything;
allowed_values(_Path, "textarea") -> anything;
allowed_values(_Path, "select")   -> "lookup select";
allowed_values(_Path, "radio")    -> "lookup radio";
allowed_values(_Path, "checkbox") -> "lookup checkbox".

make_abs_path(Path, [$. | _T] = Path2)  -> make_abs2(Path, Path2);
make_abs_path(_Path, [$/ | _T] = Path2) -> Path2;
make_abs_path(Path, Path2)              -> hn_util:list_to_path(Path) ++ Path2.

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

make_security(File) ->

    % tpls aren't always well formed XML, but
    % wrapping them in a '<div></div>' pair makes 'em so
    File2 = "<div>" ++ File ++ "</div>",
    Opts = [{event_fun, fun process/3}],

    {ok, List, _} = xmerl_sax_parser:stream(File2, Opts),
    make_transactions(lists:reverse(List)).

process(X, Y, undefined) -> process(X, Y, []);
process({startElement, [], "div", {[], "div"}, List}, _Y, Acc) ->
    case is_hn(List) of
        true  -> [make_rec(List) | Acc];
        false -> Acc
    end;
process({startElement, [], "form", {[], "form"}, _List}, _Y, Acc) ->
    [formstart | Acc];
process({endElement, [], "form", {[], "form"}}, _Y, Acc) ->
    [formend | Acc];
process(_X, _Y, Acc) -> Acc.

is_hn(List) ->
    case lists:keyfind("data-type", 3, List) of
        false  -> false;
        _Tuple -> true
    end.

make_rec(List) -> make_r1(List, #binding{}).

make_r1([], A)                                     -> A;
make_r1([{[], [], "class", _} | T], A)             -> make_r1(T, A);
make_r1([{[], [], "data-type", Ty} | T], A)        -> make_r1(T, A#binding{type = Ty});
make_r1([{[], [], "data-binding-from", F} | T], A) -> make_r1(T, A#binding{from = F});
make_r1([{[], [], "data-binding-to", To} | T], A)  -> make_r1(T, A#binding{to = To}).
                                                 
make_transactions(List) -> make_t1(List, [], [], false).

%% if the boolean is true we are in a form and we accumulate the bindings in A1
%% when the form ends we throw the contents of A1 into the main accumulator in A2
%% if the boolean is false, then each binding is a transaction in its own right
%% expects forms to terminate in the model or will crash
make_t1([], [], A2, false)              -> post_only(A2);
make_t1([formstart | T], A1, A2, false) -> make_t1(T, A1, A2, true);
make_t1([formend | T], A1, A2, true)    -> make_t1(T, [], [A1 | A2], false);
make_t1([H | T], A1, A2, true)          -> make_t1(T, [H | A1], A2, true);
make_t1([H | T], A1, A2, false)         -> make_t1(T, A1, [[H | []] | A2], false).

%% if the binding is 'to' and empy list then we chuck it away
%% a transaction can be a single binding tuple or a list of bindingd tuples
post_only(List) -> post_only2(List, []).

post_only2([], Acc)      -> Acc;
post_only2([H | T], Acc) -> case post_only3(H, []) of
                                []     -> post_only2(T, Acc);
                                NewAcc -> post_only2(T, [NewAcc | Acc])
                            end.

post_only3([], Acc)                      -> Acc;
post_only3([#binding{to = []} | T], Acc) -> post_only3(T, Acc);
post_only3([H | T], Acc)                 -> post_only3(T, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Debugging API                                                            %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run() ->
    File = "/opt/code/trunk/var/docroot/127.0.0.1&9000/_g/hypernumbers/home.tpl",
    Opts = [{event_fun, fun process/3}],

    {ok, List, _} = xmerl_sax_parser:file(File, Opts),
    make_transactions(lists:reverse(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% EUnit Tests                                                              %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test1() ->
    Json = [{struct,[{"ref","/u/gordon/blah/A:A"},
                     {"formula","Field 1"}]},
            {struct,[{"ref","/u/gordon/blah/B:B"},
                     {"formula","Field 2"}]},
            {struct,[{"ref","/u/gordon/blah/C:C"},
                     {"formula","Field 3"}]},
            {struct,[{"ref","/u/gordon/blah/D:D"},
                     {"formula","Field 4"}]}],
    RefX   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
    Form  = "<div id='ventris'>"
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
    Transactions = make_security(Form),
    io:format("Transactions is ~p~n", [Transactions]),
    (true == is_approved(RefX, Json, Transactions)).
    
test2() ->
    Json = [{struct,[{"ref","/u/gordon/blah/D6"},{"formula","Test"}]}],
    RefX   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
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
    Transactions = make_security(Form),
    io:format("Transactions is ~p~n", [Transactions]),
    is_approved(RefX, Json, Transactions).

test3() ->
    Json = [{struct,[{"ref","/u/gordon/blah/D:D"},{"formula","Test"}]}],
    RefX   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
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
    Transactions = make_security(Form),
    io:format("Transactions is ~p~n", [Transactions]),
    (false == is_approved(RefX, Json, Transactions)).

test4() ->
    Json = [{struct,[{"ref","/u/gordon/blah/D6"},{"formula","Test"}]}],
    RefX   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
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
    Transactions = make_security(Form),
    io:format("Transactions is ~p~n", [Transactions]),
    (false == is_approved(RefX, Json, Transactions)).

test5() ->
    Json = [{struct,[{"ref","/u/gordon/blah/D:D"},{"formula","Test"}]}],
    RefX   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
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
        ++ "<div data-type='input' class='hn' data-binding-to='../D6'>"
        ++ "</div>"
        ++ "</div>"
        ++ "<div id='plaincontent'>"
        ++ "</div>"
        ++ "<div id='footer'>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>"
        ++ "</div>",
    Transactions = make_security(Form),
    io:format("Transactions is ~p~n", [Transactions]),
    (false == is_approved(RefX, Json, Transactions)).

testA() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "d1",
    Ret = make_abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ("/blah/bloh/bleh/d1" == Ret).

testB() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "../d1",
    Ret = make_abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ("/blah/bloh/d1" == Ret).

testC() ->
    Path = ["blah", "bloh", "bleh"],
    Path2 = "./d1",
    Ret = make_abs_path(Path, Path2),
    io:format("Ret is ~p~n", [Ret]),
    ("/blah/bloh/bleh/d1" == Ret).


unit_test_() -> 
    [
     ?_assert(test1()),
     ?_assert(test2()),
     ?_assert(test3()),
     ?_assert(test4()),
     ?_assert(test5()),
     ?_assert(testA()),
     ?_assert(testB()),
     ?_assert(testC())
    ].
