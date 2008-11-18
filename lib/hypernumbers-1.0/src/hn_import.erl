%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Import external spreadsheets into hypernumbers
-module(hn_import).
-include("hypernumbers.hrl").
-include("spriki.hrl").
-export([hn_xml/1]).

% -spec(hn_xml/1 :: (string()) -> ok).
%% @doc Yaws handler for all incoming HTTP requests
hn_xml(FileName) -> 

    {ok,Xml} = file:read_file(FileName),
    {root,[{domain,Domain}],Pages} 
        = simplexml:from_xml_string(binary_to_list(Xml)),

    [$/|Rest] = lists:reverse(Domain),
    NDomain   = lists:reverse(Rest),
 
    {ok,Ref} = hn_util:parse_url(Domain),
    NPages = hn_main:get_pages_under(Ref#ref.path),
    Del = fun(X) ->
                  hn_db:remove_item(Ref#ref{ref='_',path=X})
          end,
    lists:map(Del,NPages),
    
    F = fun({page,[{path,Path}],[Attr]}) ->
                misc_util:do_import(NDomain++Path,Attr)
        end,

    lists:map(F,Pages),

    {ok,NDomain}.
