%%% @doc Excel-compatible type conversions (mostly).
%%% <hasan@hypernumbers.com>

-module(xltconv).
-compile(export_all).
-include("typechecks.hrl").

%% X -> boolean.
%% might need a couple of variations of this.
to_b(true) ->
    true;
to_b(false) ->
    false;
to_b(1) ->
    true;
to_b(0) ->
    false;
to_b(_) ->
    ?ERR_VAL.
