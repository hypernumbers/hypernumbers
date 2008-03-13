%%%-------------------------------------------------------------------
%%% File        : read_excel.erl
%%% Author      : Gordon Guthrie 
%%% Description : 
%%%
%%% Created     : 25th July 2007 by  <gordonguthrie@backawinner.gg>
%%%-------------------------------------------------------------------
-module(read_excel).

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 stop/1,
	 die/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%--------------------------------------------------------------------

start(_Type, _StartArgs) ->

    {Status, Pid} = case read_excel_sup:start_link() of
                        {ok, Pid2} -> 
                            {{ok, Pid2},Pid2};
                        Error ->
                            {{error, {"read_excel_sup failed to start", Error}},null}
                    end,

    case Status of
        {ok, Pid} -> Status;
        Else -> 
            {error, {"the read_excel application has failed to start link", Else}}
    end.

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

die() ->
    exit("die in flames").

