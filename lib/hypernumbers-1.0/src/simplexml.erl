%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd. 2007-2014
%%% @todo improve documentation

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

-module(simplexml).

-export([
    to_xml_string/1,
    from_xml_string/1,
    search/2 ]).

%% @doc Exports a xml encoded string
to_xml_string(SimpleXML) ->
    XML = xmerl_lib:expand_element(SimpleXML),
    lists:flatten(xmerl:export_simple([XML],xmerl_xml,[{prolog,[]}])).

%% @doc from an xml encoded string to simplexml tuple
from_xml_string(String) ->
    Opts        = [{space,normalize},{encoding,"utf-8"}],
    {Xmerl,_M}  = xmerl_scan:string(String,Opts),
    Xml         = xmerl_lib:simplify_element(Xmerl),
    rm_spaces(Xml).

rm_spaces({El,Attr,Children}) ->

    F = fun(" ") -> false;
           (_)   -> true
        end,
    NChild = lists:filter(F,Children),

    Ch = lists:map(fun rm_spaces/1,NChild),

    {El,Attr,Ch};

rm_spaces(String) ->
    String.

search(Xml,Name) ->
    lists:flatten(do_search(Xml,Name)).

do_search({Name,Attr,Children},Name) ->
    {Name,Attr,Children};
do_search({_DiffName,_Attr,Children},Name) ->
    do_search(Children,Name);
do_search([{Name,Attr,Children}|T],Name) ->
    [{Name,Attr,Children},do_search(T,Name)];
do_search([{_Name,_Attr,Children}|T],Name) ->
    [do_search(Children,Name),do_search(T,Name)];
do_search(_Else,_Name) ->
    [].


