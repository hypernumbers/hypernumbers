%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers.com

-module(hnfuns_controls).

-export([
         'button.create'/1
        ]).

-include("spriki.hrl").

'button.create'(List) when is_list(List) ->
    Id = "id_" ++ muin_util:create_name(),
    [Title | Commands] = typechecks:std_strs(List),
    Origin = hn_util:list_to_path(muin:context_setting(path)),
    Payload = compile(Commands),
    Html = lists:flatten("<input id='" ++ Id ++ "' type='submit' "
                         ++ "class='hn-webcontrol' value='"++ Title
                         ++ "' data-payload='" ++ Payload ++ "' "
                         ++ "data-origin='" ++ Origin ++ "' />"),
    {webcontrol, {Payload, Title}, Html}.

compile(Commands) ->
    io:format("Commands are ~p~n", [Commands]),
    "42".
