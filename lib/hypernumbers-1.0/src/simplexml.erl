%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @todo improve documentation
-module(simplexml).

-export([ 
    to_json/1, 
    to_json_string/1, 
    from_json/1,
    from_json_string/1,
    to_xml_string/1,
    from_xml_string/1,
    search/2 ]).

%% @doc Exports a json encoded string taking "simple-form" XML content
to_json_string(SimpleXML) -> 
    JSON = to_json(SimpleXML),
    lists:flatten(json:encode(JSON)).

%% @doc Exports a json tuple representation
to_json({El,Attr,Child}) ->
    
    NChild = lists:map(fun to_json/1, Child),
    NAttr  = lists:map(
               fun({K,V}) -> {struct,[{K,V}]} end,
               Attr),
    
    {array,[atom_to_list(El),{array,NAttr ++ NChild}]};

to_json(String) -> 
    String.

%% @doc from an json encoded string to simplexml tuple
from_json_string(String) ->
    {ok,JSON} = json:decode_string(String),
    from_json(JSON).

%% @doc creates a simplexml object from a json tuple representation
from_json({array,[Name,{array,List}]}) ->
    
    {Children,Attributes} = 
        lists:partition(fun({struct,_}) -> false;
                           (_)          -> true
                        end,List),
    
    NChild = lists:map(fun from_json/1,Children),
    NAttr  = lists:map(fun({struct,[X]}) -> X end,Attributes),
    
    {list_to_atom(Name),NAttr,NChild};

from_json(String) -> 
    String.

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
    
    
