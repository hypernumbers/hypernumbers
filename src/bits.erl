%%%%-----------------------------------------------------------------------------
%%% File        : bits.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : helpful bits and bobs
%%%
%%% Created     : 16 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(bits).

%% API
-export([clear_db/0]).

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
    application:stop(mnesia),
    application:stop(engine),
    load_db:create_db(persistent),
    application:start(engine),
    application:start(remoting),
    {ok, "db cleared down"}.
