%%% @doc Main Hypernumbers Internal API
%%% 
%%% Needs to be designed / cleaned up but needed it
%%% for the alpha
%%% @TODO needs to be documented properly and specs need to be done properly

-module(hn).
-author("Dale Harvey <dale@hypernumbers.com>").

-include("hypernumbers.hrl").
-include("spriki.hrl").

-export([ set/2, set/3 ]).
-export([ read/1, read/2 ]).
-export([ delete/1, delete/2 ]).

%%
%% EXPORTS
%%

% % @spec set(any(),any()) -> any()
%% @doc Set the value of a cell
set(Url, Value) when is_record(Url, ref) ->
    hn_main:set_attribute(Url, Value).

set(Url, Name, Value) ->
    {ok, Ref} = hn_util:parse_url(Url),
    set(Ref#ref{name=Name}, Value).

% % @spec read(any(),any()) -> any()
%% @doc Set the value of a cell
read(Url) when is_record(Url, ref) ->
    hn_db:get_item(Url).

read(Url, Name) ->
    {ok, Ref} = hn_util:parse_url(Url),
    read(Ref#ref{name=Name}).

% % @spec delete(any(),any()) -> any()
%% @doc Set the value of a cell
delete(Url) when is_record(Url, ref) ->
    hn_db:remove_item(Url).

delete(Url, Name) ->
    {ok, Ref} = hn_util:parse_url(Url),
    delete(Ref#ref{name=Name}).

