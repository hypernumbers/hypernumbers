%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hn_security).

-export([run/0]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
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

run() ->
    File = "/opt/code/trunk/lib/hypernumbers-1.0/priv/docroot/"
        ++ "views/x.tiny.hn_9000/_u/anonymous/tiny.tpl",

    Opts = [{event_fun, fun process/3}],

    {ok, List, _} = xmerl_sax_parser:file(File, Opts),
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
make_t1([], [], A2, false)              -> lists:reverse(A2);
make_t1([formstart | T], A1, A2, false) -> make_t1(T, A1, A2, true);
make_t1([formend | T], A1, A2, true)    -> make_t1(T, [], [A1 | A2], false);
make_t1([H | T], A1, A2, true)          -> make_t1(T, [H | A1], A2, true);
make_t1([H | T], A1, A2, false)         -> make_t1(T, A1, [H | A2], false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% EUnit Tests                                                              %%%
%%%                                                                          %%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test1() ->
    _Input = [{struct,[{"ref","/u/gordon/blah/A:A"},
                      {"formula","Field 1"}]},
             {struct,[{"ref","/u/gordon/blah/B:B"},
                      {"formula","Field 2"}]},
             {struct,[{"ref","/u/gordon/blah/C:C"},
                      {"formula","Field 3"}]},
             {struct,[{"ref","/u/gordon/blah/D:D"},
                      {"formula","Field 4"}]}],
    _Ref   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
    _Form  = "<div id='ventris'><link rel='stylesheet' type='text/css' href='/templates/ventris/default.css'></link><div id='outer'><div id='inner'><div class='clear'><div id='header'><div data-binding-from='/A1' class='hn header' data-type='text'></div><div id='menu'></div></div></div><div id='contentwrapper'><div id='primarycontent'></div><div id='secondarycontent'><form class='hn' data-type='form'><label><span><div data-binding-from='A3' class='hn' data-type='text'></div></span></label><div class='hn' data-type='input' data-binding-to='A:A'></div><label><span><div data-binding-from='B3' class='hn' data-type='text'></div></span></label><div class='hn' data-type='input' data-binding-to='B:B'></div><label><span><div data-binding-from='C3' class='hn' data-type='text'></div></span></label><div class='hn' data-type='input' data-binding-to='C:C'></div><label><span><div data-binding-from='D3' class='hn' data-type='text'></div></span></label><div class='hn' data-type='input' data-binding-to='D:D'></div><input type='submit' value='submit'></input></form></div></div><div id='footer'></div></div></div></div>",
    false.

test2() ->
    _Input = [{struct,[{"ref","/u/gordon/blah/D:D"},{"formula","Test"}]}],
    _Ref   = {refX,"http://127.0.0.1:9000",["u","gordon","blah"],{page,"/"},[]},
    _Form  = "<link href='/templates/tiny/default.css' type='text/css' rel='stylesheet'></link><div id='outer'><div id='inner'><div class='clear'><div id='header'><div data-type='text' class='hn header' data-binding-from='B2'></div><div id='menu'></div></div></div><div id='plaincontent'></div><div id='contentwrapper'><div id='primarycontent'></div><div id='secondarycontent'></div><div data-type='input' class='hn' data-binding-to='D6'></div></div><div id='plaincontent'></div><div id='footer'></div></div></div></div>",
    false.

unit_test_() -> 
    [
     ?_assert(test1()),
     ?_assert(test2())
    ].
