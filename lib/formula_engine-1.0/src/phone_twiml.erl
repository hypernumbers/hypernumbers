%%% @author     Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       This function renders the TwiML
%%%            that the phone fns in hnfuns_phone.erl
%%%            need to operate
%%% @end
%%% Created : 13 Feb 2012 by gordon@hypernumbers.com

-module(phone_twiml).

-export([
         phone.out/1
         ]).

phone.out([]) ->
    ok.

