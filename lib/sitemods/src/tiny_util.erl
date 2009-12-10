%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       Library for tiny.hn
%%%
%%% @end
%%% Created :  2 Dec 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(tiny_util).

%% API
-export([
         get_password/0,
         make_subs/0,
         get_unallocated_sub/0,
         get_tiny_site/0,
         is_valid_email/1
        ]).

-include("password.hrl").
-include("tiny.hrl").
-include("spriki.hrl").

-define(ALPHABET, [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j,
                   $k, $l, $m, $n, $o, $p, $q, $r, $s, $t,
                   $u, $v, $w, $x, $y, $z]).
-define(SUBS, tiny_sub).
-define(PERCENTAGE, $%). %.
        
        
get_tiny_site() ->

    Fun = fun() ->
                  mnesia:dirty_match_object(core_site, #core_site{_ = '_'})
          end,
    
    Sites =  mnesia:activity(transaction, Fun),
    get_tiny_site2(Sites).

get_tiny_site2([])                -> no_site;
get_tiny_site2([{_, S1, S2} | T]) -> case S2 of
                                         tiny_hn -> S1;
                                         _       -> get_tiny_site2(T)
                                     end.

get_password() -> 
    N1 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
    N2 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
    N3 = gen_server:call(random_srv, {random, int, 99}),
    {value, {N1, W1}} = lists:keysearch(N1, 1, ?WORDS),
    {value, {N2, W2}} = lists:keysearch(N2, 1, ?WORDS),
    W1 ++ "!" ++ W2 ++ integer_to_list(N3).

make_subs() ->
    % List = make_subs1(make_subs1(make_subs1([[]]))),
    List = gen_subs(1),
    Rand = make_random(List, []),
    NewList = lists:keysort(1, Rand),
    ok = create_sub_table(),
    write(NewList).

get_unallocated_sub() ->
    Fun = fun() ->
                  Head = #tiny_sub{sub = '$1', allocated=false},
                  {[H | _T], _Cont}
                      = mnesia:select(?SUBS, [{Head, [], ['$1']}], 1, write),
                  Rec = #tiny_sub{sub = H, allocated = true},
                  ok = mnesia:write(Rec),
                  H
          end,
    mnesia:activity(transaction, Fun).

%%
%% Internal Functions
%%

gen_subs(X) -> 
    gen_subs(X, [[]]).
gen_subs(0, Subs) -> Subs;
gen_subs(N, Subs) -> 
    Subs2 = [[X | Y] || X <- ?ALPHABET, Y <- Subs],
    gen_subs(N-1, Subs2).


make_random([], Acc) -> Acc;
make_random([H | T], Acc) ->
    R = random:uniform(26*26*26),
    make_random(T, [{R, H} | Acc]).

write([]) -> ok;
write([{_K, V} | T]) ->
    Fun = fun() ->
                  ok = mnesia:write(#tiny_sub{sub = V})
          end,
    mnesia:activity(transaction, Fun),
    write(T).

create_sub_table() ->
    Tables = mnesia:system_info(tables),
    case lists:member(?SUBS, Tables) of
        true  -> ok;
        false ->  Attrs = [{record_name, ?SUBS},
                           {attributes, record_info(fields, ?SUBS)},
                           {type, set},
                           {index, [allocated]},
                           {disc_only_copies, [node()]}],
                  {atomic, ok} = mnesia:create_table(?SUBS, Attrs),
                  ok
    end.

is_valid_email(Email) ->
    EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
        ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
        ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
        ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
        ++ "|biz|info|mobi|name|aero|jobs|museum)", %" for syntax highighting
    case re:run(Email, EMail_regex) of
        nomatch    -> not_email;
        {match, _} -> {email, Email}
    end.
