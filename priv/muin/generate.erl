%%% @doc    Generates and compiles Muin's lexer and parser from grammar
%%%         files under SVNROOT/priv/muin/
%%% @author Hasan Veldstra <hasan@hypernumbers.com>


-module(generate).
-export([gen/0]).
-import(file, [copy/2, delete/1, get_cwd/0, rename/2]).
-define(P(X), io:format(" *** " ++ X ++ " ***~n")).
-define(FRONTENDS,
        ["russian", "french", "german", "italian", "spanish", "portuguese"]).

gen() ->
    DestDir = "../../lib/formula_engine-1.0/src/",

    %% Compile Leex.
    {ok, leex} = compile:file("leex.erl"),
    ?P("Compiled Leex"),

    %% Generate main lexers & parser.
    leex:gen(xfl_lexer, xfl_lexer),
    ?P("Generated main lexer."),
    yecc:yecc(xfl_parser, xfl_parser),
    ?P("Generated main parser."),
    leex:gen(num_format_lex, num_format_lexer),
    ?P("Generated number format lexer."),
    yecc:yecc(num_format, num_format_parser),
    ?P("Generated number format parser."),
    leex:gen(cond_lex, cond_lexer),
    ?P("Generated conditional lexer."),
    yecc:yecc(cond_parser, cond_parser),
    ?P("Generated conditinoal parser."),

    %% Generate super lexer.
    leex:gen(superlex, superlex),
    ?P("Generated super lexer!"),

    %% Generate frontends.
    lists:foreach(fun(X) -> gen_frontend(X) end,
                  ?FRONTENDS),

    %% Move the generated files to the right directory.
    delete(DestDir ++ "xfl_lexer.erl"),
    delete(DestDir ++ "xfl_parser.erl"),
    delete(DestDir ++ "superlex.erl"),
    lists:foreach(fun(X) -> delete(DestDir ++ X ++ "_lexer.erl") end,
                  ?FRONTENDS),
        
    delete(DestDir ++ "num_format_lexer.erl"),
    delete(DestDir ++ "num_format_parser.erl"),
    rename("xfl_lexer.erl", DestDir ++ "xfl_lexer.erl"),
    rename("xfl_parser.erl", DestDir ++ "xfl_parser.erl"),
    rename("superlex.erl", DestDir ++ "superlex.erl"),
    lists:foreach(fun(X) -> rename(X ++ "_lexer.erl", DestDir ++ X ++ "_lexer.erl") end,
                  ?FRONTENDS),
    rename("num_format_lexer.erl", DestDir ++ "num_format_lexer.erl"),
    rename("num_format_parser.erl", DestDir ++ "num_format_parser.erl"),

    rename("cond_lexer.erl", DestDir ++ "cond_lexer.erl"),
    rename("cond_parser.erl", DestDir ++ "cond_parser.erl"),

    lists:foreach(fun(X) -> delete(X ++ "_lexer.erl") end,
                  ?FRONTENDS),
    
    ?P("OK").

gen_frontend(Lang) ->
    {ok, Currdir} = get_cwd(),
    copy(Currdir ++ "/language_frontends/" ++ Lang ++ "_lexer.xrl",
         Currdir ++ "/" ++ Lang ++ "_lexer.xrl"),
    leex:gen(list_to_atom(Lang ++ "_lexer"), list_to_atom(Lang ++ "_lexer")),
    ?P("Generated " ++ string:to_upper([hd(Lang)]) ++ tl(Lang) ++ " frontend").
