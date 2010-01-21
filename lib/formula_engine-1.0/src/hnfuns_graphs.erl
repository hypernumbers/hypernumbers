%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       Provides the markdown for graphs
%%%
%%% @end
%%% Created : 11 Jan 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(hnfuns_graphs).

-export([
         linegraph/1
        ]).

linegraph([{range, Data}]) ->
    Data2 = get_data(Data),
    Max = tconv:to_s(stdfuns_stats:max(Data2)),
    Min = tconv:to_s(stdfuns_stats:min(Data2)),
    "![graph](http://chart.apis.google.com/chart?chs=350x150&amp;chd="
        ++ "t:" ++ make_data(Data2) ++ "&amp;cht=lc&amp;"
        ++ "chds="++ Min ++ "," ++ Max ++ "&amp;"
        ++ "chxt=y&amp;chxr=0," ++ Min ++ "," ++ Max
        ++ ")".

get_data(List) -> get_d1(List, []).

get_d1([], Acc)        -> lists:reverse(Acc);
get_d1([[H] | T], Acc) -> get_d1(T, [H | Acc]).

make_data(List) -> make_d1(List, []).

make_d1([], Acc)      -> string:join(lists:reverse(Acc), ",");
make_d1([H | T], Acc) when is_integer(H) ->
    make_d1(T, [integer_to_list(H) | Acc]);
make_d1([H | T], Acc) when is_float(H) ->
    make_d1(T, [float_to_list(H) | Acc]).
