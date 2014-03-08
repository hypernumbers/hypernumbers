o%%%-----------------------------------------------------------------------------
%%% File        : show.erl
%%% Author      : Gordon Guthrie <gordonguthrie@vixo.com>
%%% @copyright  : (C) 2006 - 2014, Hypernumbers.com
%%% Description : This function just throws html only pages (bodge)
%%%
%%% Created     : 10 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%% @private
%%%-----------------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

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

