%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


-module(generate).
-export([gen/0]).
-import(file, [delete/1, rename/2]).
-define(P(X), io:format("*****" ++ X ++ "~n")).

gen() ->
    DestDir = "../../lib/formula_engine-1.0/src/",

    %% Compile Leex.
    {ok, leex} = compile:file("leex.erl"),
    ?P("Compiled Leex"),

    %% Generate main lexers & parser.
    leex:gen(lexer_desc, muin_lexer),
    ?P("Compiled main lexer."),
    yecc:yecc(parser_desc, muin_parser),
    ?P("Compiled main parser."),

    %% Generate other stuff.
    leex:gen(supd_lexer, muin_supd_lexer),
    ?P("Compiled structural updates lexer."),
    leex:gen(ru_fe, ru_fe),
    ?P("Compiled Russian front-end"),

    %% Move the generated files to the right directory.
    delete(DestDir ++ "muin_lexer.erl"),
    delete(DestDir ++ "muin_parser.erl"),
    delete(DestDir ++ "muin_supd_lexer.erl"),
    rename("muin_lexer.erl", DestDir ++ "muin_lexer.erl"),
    rename("muin_parser.erl", DestDir ++ "muin_parser.erl"),
    rename("muin_supd_lexer.erl", DestDir ++ "muin_supd_lexer.erl"),
    rename("ru_fe.erl", DestDir ++ "ru_fe.erl"),
    
    ?P("OK"). % Cheeky, cos one or more commands above could've failed.
