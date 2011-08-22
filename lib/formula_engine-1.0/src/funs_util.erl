%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       some utilities for hnfuns
%%%
%%% @end
%%% Created : 22 Aug 2011 by <gordon@hypernumbers.com>

-module(funs_util).

-export([
         check_size/2,
         check_size2/3
         ]).

-include("typechecks.hrl").

check_size(W, H) when W > 0 andalso W < 16 andalso H > 1 andalso H < 26 -> ok;
check_size(_W, _H) -> ?ERR_VAL.

check_size2(W, H, MinH)
  when W > 1 andalso W < 16
andalso H >= MinH andalso H < 26 -> ok;
check_size2(_W, _H, _MinH) -> ?ERR_VAL.

