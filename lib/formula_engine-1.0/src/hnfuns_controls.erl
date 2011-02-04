%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers.com

-module(hnfuns_controls).

-export([
         'create.button'/1
        ]).

-include("spriki.hrl").

'create.button'(List) when is_list(List) ->
    Id = "id_" ++ muin_util:create_name(),
    [Title | Commands] = typechecks:std_strs(List),
    Origin = hn_util:list_to_path(muin:context_setting(path)),
    Fun = fun(Expr) ->
                  L = webc_parser:compile(Expr),
                  [json_recs:rec_to_json(X) || X <- L]
          end,
    Payload = [{array, Fun(X)} || X <- Commands],
    Pay2 = {struct, [{create_pages_from_template, {array, Payload}}]},
    Json = mochijson:encode(Pay2),
    Html = lists:flatten("<input id='" ++ Id ++ "' type='submit' "
                         ++ "class='hn-webcontrol' value='"++ Title
                         ++ "' data-payload='" ++ Json ++ "' "
                         ++ "' data-action='create_from_templates'"
                         ++ "data-origin='" ++ Origin ++ "' />"),
    {webcontrol, {Payload, Title}, Html}.
