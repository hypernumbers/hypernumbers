%%%%-----------------------------------------------------------------------------
%%% File        : bits.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : helpful bits and bobs
%%%
%%% Created     : 16 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(bits).

%% API
-export([clear_db/0, log/1]).

%%==============================================================================
%% API
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function:
%% Description:
%%------------------------------------------------------------------------------

%%==============================================================================
%% Internal functions
%%==============================================================================

clear_db()->
    hn_loaddb:create_db(disc_copies),
    ok.

log(String) ->
    File= case os:type() of
	      {win32,nt} -> "c:\\tmp\\hypernumbers_log.txt";
	      _          -> "/tmp/hypernumbers_log.txt"
	  end,
    Return=filelib:ensure_dir(File),
    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.

