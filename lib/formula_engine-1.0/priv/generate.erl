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
    DestDir = "../src/",

    %% Compile Leex.
    {ok, leeex} = compile:file("leeex.erl"),
    ?P("Compiled Leex"),

    %% Generate main lexers & parser.
    leeex:gen(xfl_lexer, xfl_lexer),
    ?P("Generated main lexer."),
    yecc:yecc(xfl_parser, xfl_parser),
    make_private(xfl_parser),
    ?P("Generated main parser."),
    leeex:gen(num_format_lex, num_format_lexer),
    ?P("Generated number format lexer."),
    yecc:yecc(num_format, num_format_parser),
    make_private(num_format_parser),
    ?P("Generated number format parser."),
    leeex:gen(cond_lex, cond_lexer),
    ?P("Generated conditional lexer."),
    yecc:yecc(cond_parser, cond_parser),
    make_private(cond_parser),
    ?P("Generated conditional parser."),
    leeex:gen(url_query_lex, url_query_lexer),
    ?P("Generated url query lexer."),
    yecc:yecc(url_query, url_query_parser),
    make_private(url_query_parser),
    ?P("Generated url query parser."),
	
    %% Generate super lexer.
    leeex:gen(superlex, superlex),
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
    rename("xfl_lexer.erl",  DestDir ++ "xfl_lexer.erl"),
    rename("xfl_parser.erl", DestDir ++ "xfl_parser.erl"),
    rename("superlex.erl",   DestDir ++ "superlex.erl"),
    lists:foreach(fun(X) -> rename(X ++ "_lexer.erl", DestDir ++ X ++ "_lexer.erl") end,
                  ?FRONTENDS),
    rename("num_format_lexer.erl",  DestDir ++ "num_format_lexer.erl"),
    rename("num_format_parser.erl", DestDir ++ "num_format_parser.erl"),

    rename("cond_lexer.erl",  DestDir ++ "cond_lexer.erl"),
    rename("cond_parser.erl", DestDir ++ "cond_parser.erl"),

    rename("url_query_lexer.erl",  DestDir ++ "url_query_lexer.erl"),
    rename("url_query_parser.erl", DestDir ++ "url_query_parser.erl"),
	
    lists:foreach(fun(X) -> delete(X ++ "_lexer.erl") end,
                  ?FRONTENDS),
    
    ?P("OK").

gen_frontend(Lang) ->
    {ok, Currdir} = get_cwd(),
    copy(Currdir ++ "/language_frontends/" ++ Lang ++ "_lexer.xrl",
         Currdir ++ "/" ++ Lang ++ "_lexer.xrl"),
    leeex:gen(list_to_atom(Lang ++ "_lexer"), list_to_atom(Lang ++ "_lexer")),
    ?P("Generated " ++ string:to_upper([hd(Lang)]) ++ tl(Lang) ++ " frontend").

make_private(FileName) ->
    FileName2 = atom_to_list(FileName)++".erl",
    {ok,Bin} = file:read_file(FileName2),
    FileContents = binary_to_list(Bin),
    NewFile = list_to_binary(lists:append(["%%% @private\n", FileContents])),
    ok = file:write_file(FileName2, NewFile).
    
