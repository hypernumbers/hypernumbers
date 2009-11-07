#!/usr/bin/env escript
%% -*- mode: erlang -*-

%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Tom McNulty
-define(DESTDIR, "../src/").
-define(FRONTENDS, ["russian", "french", "german", 
                    "italian", "spanish", "portuguese"]).


main(["force"]) -> generate(true);
main(_) -> generate(false).


generate(ForceCompile) ->
    Gen_lex = fun(X) -> gen_lex(X, ForceCompile) end,
    Gen_parse = fun(X) -> gen_parse(X, ForceCompile) end, 
    Gen_frontend = fun(X) -> gen_frontend(X, ForceCompile) end,

    Gen_lex(xfl_lexer),
    Gen_parse(xfl_parser),

    Gen_lex(num_format_lexer),
    Gen_parse(num_format_parser),

    Gen_lex(cond_lexer),
    Gen_parse(cond_parser),

    Gen_lex(url_query_lexer),
    Gen_parse(url_query_parser),

    Gen_lex(superlex),

    lists:foreach(fun(X) -> Gen_frontend(X) end, ?FRONTENDS),
    ok.


gen_lex(Name, Force) ->
    Source = filename:flatten([Name,".xrl"]),
    Dest = [?DESTDIR, Name, ".erl"],
    case Force or needs_update([Source], Dest) of
        true ->
            {ok, Lexer} = leex:file(Source),
            ok = file:rename(Lexer, Dest),
            io:format(" *** Generated Lexer ~s ***~n", [Name]),
            ok;
        false -> 
            ok
    end.

gen_parse(Name, Force) ->
    Source = filename:flatten([Name,".yrl"]),
    Dest = [?DESTDIR, Name, ".erl"],
    case Force or needs_update([Source], Dest) of
        true ->
            {ok, Lexer} = yecc:file(Source),
            ok = make_private(Lexer),
            ok = file:rename(Lexer, Dest),
            io:format(" *** Generated Parser ~s ***~n", [Name]),
            ok;
        false ->
            ok
    end.

gen_frontend(Lang, Force) ->
    Source = ["./maps/", Lang, ".txt"],
    Dest = [?DESTDIR, Lang, "_lexer.erl"],
    Template = "lexer_template.erb",
    Command = "generate_lexer.rb",
    case Force or needs_update([Source, Template, Command], Dest) of
        true ->
            os:cmd(io_lib:format("ruby ~s ~s", [Command, Lang])),
            Name = [Lang, "_lexer"],
            gen_lex(Name, true);
%            ok = file:delete([Name,".xrl"]);
        false ->
            ok
    end.

make_private(Fname) ->
    {ok,Bin} = file:read_file(Fname),
    ok = file:write_file(Fname, ["%%% @private\n", Bin]).

needs_update(Sources, Dest) ->
    MaxS = lists:max([filelib:last_modified(S) || S <- Sources]),
    MaxS > filelib:last_modified(Dest).



