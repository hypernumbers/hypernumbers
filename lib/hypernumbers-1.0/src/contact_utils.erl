%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Some utilities for doing phone stuff
%%%
%%% @end
%%% Created : 13 Feb 2012 by gordon@hypernumbers.com

-module(twilio_utils).

-export([
         post_sms/1
         ]).

post_sms(Msg) ->
    URL = "..",
    Postreq = "",
    Return = httpc:request(post,
                           {URL, [{"Accept", "application/json"}],
                            "application/json", Postreq},
                           [{timeout, 5000}],
                           []),
    io:format("Return is ~p~n", [Return]),
    ok.
