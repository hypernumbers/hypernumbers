%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


-module(generate).
-export([gen/0]).
-import(file, [copy/2, delete/1, get_cwd/0, rename/2]).
-define(P(X), io:format("*****" ++ X ++ "~n")).

gen() ->
    DestDir = "../../lib/formula_engine-1.0/src/",

    %% Compile Leex.
    {ok, leex} = compile:file("leex.erl"),
    ?P("Compiled Leex"),

    %% Generate main lexers & parser.
    leex:gen(lexer_desc, muin_lexer),
    ?P("Generated main lexer."),
    yecc:yecc(parser_desc, muin_parser),
    ?P("Generated main parser."),

    %% Generate Russian front-end.
    {ok, CurrDir} = get_cwd(),
    copy(CurrDir ++ "/language_frontends/russian_lexer.xrl",
         CurrDir ++ "/russian_lexer.xrl"),
    leex:gen(russian_lexer, russian_lexer),
    ?P("Generated Russian front-end"),

    %% Move the generated files to the right directory.
    delete(DestDir ++ "muin_lexer.erl"),
    delete(DestDir ++ "muin_parser.erl"),
    delete(DestDir ++ "muin_supd_lexer.erl"),
    delete(DestDir ++ "russian_lexer.erl"),
    rename("muin_lexer.erl", DestDir ++ "muin_lexer.erl"),
    rename("muin_parser.erl", DestDir ++ "muin_parser.erl"),
    rename("muin_supd_lexer.erl", DestDir ++ "muin_supd_lexer.erl"),
    rename("russian_lexer.erl", DestDir ++ "russian_lexer.erl"),

    delete("russian_lexer.erl"),
    
    ?P("OK").
