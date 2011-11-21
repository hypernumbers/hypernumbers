%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Sustainable Performance Ltd
%%% @doc       implements the mapping fun for pret
%%%            Temporary until user-defined fns
%%%            are added
%%%
%%% @end
%%% Created : 18 Nov 2011 by <gordon@hypernumbers.com>

-module(usermap).

-export([
         pret/2
         ]).

pret(MPAN, Date) ->
    io:format("MPAN is ~p~nDate is ~p~n", [MPAN, Date]),
    Date2 = lists:reverse(string:tokens(Date, "/")),
    Date3 = string:join([tconv:to_s(X) || X <- Date2], "/"),
    "/tmp/" ++ lookup(MPAN) ++ "/" ++ Date3 ++ "/".

lookup(MPAN) -> "erk".


