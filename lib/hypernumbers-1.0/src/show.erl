%%%-----------------------------------------------------------------------------
%%% File        : show.erl
%%% Author      : Gordon Guthrie <gordonguthrie@vixo.com>
%%% Description : This function just throws html only pages (bodge)
%%%
%%% Created     : 10 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%% @private
%%%-----------------------------------------------------------------------------
-module(show).

-export([html/1,swf/1]).

%%%-----------------------------------------------------------------------------%%%
%%% Exported functions
%%%
%%%-----------------------------------------------------------------------------
swf(_Page)->
    ok.

html(Page)->
        case read_lines(Page) of
        {ok,String} -> String;
        error       -> error
    end.

%%%-----------------------------------------------------------------------------%%%
%%% Internal functions
%%%
%%%-----------------------------------------------------------------------------
get_all_lines(Device, Accum) ->
    case io:get_line(Device,"") of
	eof  ->  
	    file:close(Device),
	    Accum;
	Line ->
	    get_all_lines(Device,Accum ++ Line)
    end.

read_lines(FileName) ->
    case file:open(FileName,[read]) of
        {ok,Device} -> {ok,get_all_lines(Device,[])};
        _           -> error
    end.
 
