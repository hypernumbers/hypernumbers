%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, hypernumbers.com
%%% @doc       Builds edoc for the application
%%%
%%% @end
%%% Created : 25 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(make_edoc).

-export([build/0]).

build() ->
    Docs = [
            {'hypernumbers-1.0',   "../lib/hypernumbers-1.0",   [{todo, true}]},
            {'read_excel-1.0',     "../lib/read_excel-1.0",     [{todo, true}]},
            {'formula_engine-1.0', "../lib/formula_engine-1.0", [{todo, true}]},
            {'introspection-1.0',  "../lib/introspection-1.0",  [{todo, true}]},
            {'mnesia_logger-1.0',  "../lib/mnesia_logger-1.0",  [{todo, true}]}
           ],
    [ok = edoc:application(X, Y, Z) || {X, Y, Z} <- Docs ].
