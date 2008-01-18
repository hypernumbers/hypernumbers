%%%-----------------------------------------------------------------------------
%%% @doc    Generates and compiles Muin's lexer and parser from description
%%%         files under SVNROOT/priv/muin/
%%%
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @end
%%%-----------------------------------------------------------------------------

-module(generate).
-export([gen/0]).

gen() ->
    %% ===== Compile Leex.
    {ok, leex} = compile:file("leex.erl"),

    %% ===== Generate main lexer and parser.
    leex:gen(lexer_desc, muin_lexer),
    leex:gen(num_format_lex,num_format_lexer),
    yecc:yecc(parser_desc, muin_parser),

    %% ===== Generate Lisp frontend.
    %% When extending this to solaris os:type will return {unix,sunos}
    %% but the linux commands should work just fine.
    OS = case os:type() of
             {unix, linux}  -> linux;
             {unix, darwin} -> linux;
             {win32, _} -> windows
         end,
    %% First, copy Leex  and Yecc files to current directory.
    copy("./lisp_frontend/lisp_lexer_desc.xrl",".",OS),
    copy("./lisp_frontend/lisp_parser_desc.yrl",".",OS),
    %% Second, generate the lexer and the parser.
    leex:gen(lisp_lexer_desc, lisp_lexer),
    yecc:yecc(lisp_parser_desc, lisp_parser),
    %% Finally, delete copied files.
    delete("lisp_lexer_desc.xrl",OS),
    delete("lisp_parser_desc.yrl",OS),

    %% ===== Move the generated files to the right directory.
    %% The main lexer and parser first.
    delete("../../lib/parser-1.0/src/muin_lexer.erl",OS),
    delete("../../lib/parser-1.0/src/num_format_lexer.erl",OS),
    delete("../../lib/parser-1.0/src/muin_parser.erl",OS),
    move("muin_lexer.erl","../../lib/parser-1.0/src/",OS),
    move("num_format_lexer.erl","../../lib/parser-1.0/src/",OS),
    move("muin_parser.erl","../../lib/parser-1.0/src/",OS),

    %% Create directory for frontend code.
    os:cmd("mkdir ../../lib/parser-1.0/src/frontends"),

    %% Lisp lexer and parser.
    delete("../../lib/parser-1.0/src/lisp_lexer.erl",OS),
    delete("../../lib/parser-1.0/src/lisp_parser.erl",OS),
    move("lisp_lexer.erl","../../lib/parser-1.0/src/",OS),
    move("lisp_parser.erl","../../lib/parser-1.0/src/",OS),

    io:format("OK~n"). % Cheeky, cos one or more commands above could've failed.

copy(From,To,OS)->
   case OS of
    linux      -> os:cmd("cp "++From++" "++To);
    windows -> From2=filename:nativename(From),
                    To2=filename:nativename(To),
                    os:cmd("copy "++(From2)++" "++To2)
  end.
  
move(From,To,OS)->
   case OS of
    linux      -> os:cmd("mv "++From++" "++To);
    windows ->  From2=filename:nativename(From),
                    To2=filename:nativename(To),
                    os:cmd("move "++From2++" "++To2)
  end.
  
delete(File,OS)->
   case OS of
    linux      -> os:cmd("rm "++File);
    windows ->  File2=filename:nativename(File),
                    os:cmd("del "++File2)
  end.
