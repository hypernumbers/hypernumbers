%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc        Generates the help file
%%%
%%% @end
%%% Created :  1 Apr 2009 by <gordon@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(generate_help).

-export([run/0]).

-include("working_fns.hrl").
-include("french_fns.hrl").
-include("german_fns.hrl").
-include("italian_fns.hrl").
-include("portuguese_fns.hrl").
-include("russian_fns.hrl").
-include("spanish_fns.hrl").

-define(notes, [{"fr", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"},
                {"de", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"},
                {"it", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"},
                {"pt", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"},
                {"ru", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"},
                {"es", "No translation available. To help, " ++
                 "go <a href=\"http://example.com\">here</a>"}]).

run() ->

    % Replace with call to hypernumbers.com/dev/funs/
    Path = [code:priv_dir(hypernumbers), "/", "funs.json"],
    {ok, Bin} = file:read_file(Path),

    {struct, Json} = mochijson:decode(Bin),
    {"cell", {struct, Rows}} = lists:keyfind("cell", 1, Json),
    
    Val = fun(I, Attr) ->
                  {I, {struct, NAttr1}} = lists:keyfind(I, 1, Attr),
                  {"value", Val} = lists:keyfind("value", 1, NAttr1),
                  Val
          end,
    
    NJson = [ begin
                  {_Row, {struct, Row}} = Cell,                  
                  [Val("1", Row), Val("2", Row), Val("3", Row)]
              end || Cell <- Rows ],

    write(NJson, "en_gb"),
    
    ok.
    %% first do the English Fns
    %% Json = [json_util:jsonify(X) || X <- ?WORKING_FNS],
    %% Json2 = mochijson:encode({array, Json}),
    %% write(Json2, "en_gb"),
    %% %% now do Johnny Foreigner
    %% do_jf("fr", "french",     ?french_fns),
    %% do_jf("de", "german",     ?german_fns),
    %% do_jf("it", "italian",    ?italian_fns),
    %% do_jf("pt", "portuguese", ?portuguese_fns),
    %% do_jf("es", "spanish",    ?spanish_fns),
    %% do_jf("ru", "russian",    ?russian_fns).


do_jf(Code, _Lang, Fns) ->
    %% Find entry in ?notes for this language.
    {value, {Code, Notes}} = lists:keysearch(Code, 1, ?notes),
    
    Fun =
        fun(#help{name = N} = Help) ->
                NewH = case lists:keysearch(N, 2, Fns) of
                           {value, {_, _, NewN}} -> 
                               Help#help{name = NewN};
                           false ->
                               Help#help{notes = Notes}
                       end,
                json_util:jsonify(NewH)
        end,
    JsonStructs = [Fun(X) || X <- ?WORKING_FNS],
    JsonStrings =
        lists:foldr(fun(JsonStruct, Acc) ->
                            JsonStr = lists:flatten((mochijson:encoder([{input_encoding, utf8}]))(JsonStruct)),
                            [JsonStr|Acc]
                    end,
                    [],
                    JsonStructs),
    write(
      string:concat(
        string:concat("[", string:join(JsonStrings, ",")),
        "]"),
      Code).


write(Json, Lang) ->

    File = [code:priv_dir(hypernumbers), "/core_install/docroot/hypernumbers/",
            "funs_", Lang, ".json"],
    
    file:write_file(lists:flatten(File), io_lib:format("~p~n",[Json])).
        
