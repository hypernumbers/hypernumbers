%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


-module(generate).
-export([gen/0]).
-import(fileutil, [cp/2, mv/2, rm/1]).
-import(io, [format/1]).

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
    rm(DestDir ++ "muin_lexer.erl"),
    rm(DestDir ++ "muin_parser.erl"),
    rm(DestDir ++ "muin_supd_lexer.erl"),
    mv("muin_lexer.erl", DestDir),
    mv("muin_parser.erl", DestDir),
    mv("muin_supd_lexer.erl", DestDir),

    io:format("OK~n"). % Cheeky, cos one or more commands above could've failed.
