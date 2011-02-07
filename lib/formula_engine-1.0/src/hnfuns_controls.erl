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
-include("errvals.hrl").

'create.button'(List) when is_list(List) ->
    Id = "id_" ++ muin_util:create_name(),
    [Title | Commands] = typechecks:std_strs(List),
    case Commands of
        [] -> ?ERR_VAL;
        _  ->
            Origin = hn_util:list_to_path(muin:context_setting(path)),
            % first we create the list of commands records
            % this is the executable AST
            Fun1 = fun(Expr, {N, Acc}) ->
                           {N + 1, [{N, webc_parser:compile(Expr)} | Acc]}
                   end,
            io:format("Commands is ~p~n", [Commands]),
            {_, Commands2} = lists:foldl(Fun1, {1, []}, Commands),
            % no real reason to sort, just tidier!
            Commands3 = lists:sort(Commands2),
            % now we transform the commands in Json
            % this is the payload that is shipped to the button on the
            % browser
            Fun2 = fun({N, L}) ->
                           RecJson = [json_recs:rec_to_json(X) || X <- L],
                   io:format("RecJson is ~p~n", [RecJson]),
                           {struct, [{N, {array, RecJson}}]}
                   end,
            Payload = [Fun2(X) || X <- Commands3],
            Pay2 = {struct, [{postcreatepages, {array, Payload}}]},
            Json = mochijson:encode(Pay2),
            Html = lists:flatten("<input id='" ++ Id ++ "' type='submit' "
                                 ++ "class='hn-webcontrol' value='"++ Title
                                 ++ "' data-payload='" ++ Json ++ "' "
                         ++ "' data-action='postcreatepages'"
                                 ++ "data-origin='" ++ Origin ++ "' />"),
            % we stash the record set as the form definition and when the POST
            % comes in from the button we will translate it back into a record format
            % and then compare the records to see if the action is approved
            Form = #form{id = {'create-button', Title}, kind = "create-button",
                         attrs = Commands3},
            {webcontrol, {Form, Title}, Html}
    end.
