%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       Utilities for bootstrap fns
%%%
%%% @end
%%% Created : 22 Oct 2012 by gordon@vixo.com

-module(bootstrap_utils).

-export([
         get_colour/1
        ]).

-include("errvals.hrl").

get_colour(0) -> "";
get_colour(1) -> "btn-primary";
get_colour(2) -> "btn-warning";
get_colour(3) -> "btn-danger";
get_colour(4) -> "btn-success";
get_colour(5) -> "btn-info";
get_colour(6) -> "btn-inverse";
get_colour(_) -> ?ERR_VAL.

