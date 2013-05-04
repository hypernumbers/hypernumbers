%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2013 by Gordon Guthrie <gordon@hypernumbers.dev>
%%%-------------------------------------------------------------------
-module(pretty_print).

-export([
         json_file/1,
         prettify_dir/1
         ]).

json_file(File) -> os:cmd("cat " ++ File ++ " |  ./prettyjson.py").

prettify_dir(Dir) ->
    code:add_patha("../../lib/hypernumbers-1.0/ebin"),
    WC = filename:absname(Dir ++ "/*.json"),
    Files = filelib:wildcard(WC),
    Stamp = dh_date:format("Y_M_d_H_i_s"),
    [{ok, _} = file:copy(X, X ++ "." ++ Stamp ++ ".backup") || X <- Files],
    [ok = overwrite(X) || X <- Files],
    ok.

overwrite(File) ->
    NewFile = json_file(File),
    file:write_file(File, NewFile).
