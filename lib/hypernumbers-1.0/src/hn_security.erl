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

-include("security.hrl").

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
