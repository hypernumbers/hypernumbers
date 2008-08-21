%%%-----------------------------------------------------------------------------
%%% File        : engine.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : the calculation engine
%%%
%%% Created     : 15 Oct 2007 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(hypernumbers_app).
-behaviour(application).
-include("yaws.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================
%%------------------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%------------------------------------------------------------------------------
start(_Type, _Args) ->
 
    %% if clean startup of mnesia, create the db
    %% TODO : This is the wrong way to handly multiple
    %% nodes, but as we are removing mnesia, will do for now
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> 
            ok
    end,

    case mnesia:system_info(tables) of
        [schema] -> 
            hn_loaddb:create_db(disc_copies);
        _ ->
            Me = node(),
            case mnesia:table_info(schema,cookie) of
                {_,Me} -> ok;
                _      -> hn_loaddb:create_db(disc_copies)
            end
    end,

    application:set_env(yaws, embedded, true),
    application:start(yaws),

    DefaultGC = yaws_config:make_default_gconf(false, "id"),
    GC = DefaultGC#gconf{logdir="logs/yaws"},    
    SC = #sconf{port = 9000,
                appmods=[{"/",hn_yaws}],
                listen = {127,0,0,1},
                docroot = "include/docroot"},
    
    ok = yaws_api:setconf(GC, [[SC]]),
    
    case hypernumbers_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->     
            Error
    end.

%%------------------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%------------------------------------------------------------------------------
stop(_State) -> ok.
