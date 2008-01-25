%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


-module(generate).
-export([gen/0]).
-import(io, [format/1]).
-import(file, [delete/1, rename/2]).

gen() ->
    DestDir = "../../lib/formula_engine-1.0/src/",

    %% Compile Leex.
    {ok, leex} = compile:file("leex.erl"),
    format("Compiled Leex~n"),

    %% Generate main lexers & parser.
    leex:gen(lexer_desc, muin_lexer),
    format("Compiled main lexer.~n"),
    yecc:yecc(parser_desc, muin_parser),
    format("Compiled main parser.~n"),
    leex:gen(supd_lexer, muin_supd_lexer),
    format("Compiled structural updates lexer.~n"),

    %% Move the generated files to the right directory.
    delete(DestDir ++ "muin_lexer.erl"),
    delete(DestDir ++ "muin_parser.erl"),
    delete(DestDir ++ "muin_supd_lexer.erl"),
    rename("muin_lexer.erl", DestDir ++ "muin_lexer.erl"),
    rename("muin_parser.erl", DestDir ++ "muin_parser.erl"),
    rename("muin_supd_lexer.erl", DestDir ++ "muin_supd_lexer.erl"),

    io:format("OK~n"). % Cheeky, cos one or more commands above could've failed.
