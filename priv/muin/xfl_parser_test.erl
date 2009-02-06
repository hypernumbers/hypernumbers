
%%% @doc Tests for the XFL parser.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% NB: This file is included into xfl_parser.yrl

%% @doc Parsing function for tests.

parse_test(Str) ->
    Coord = {10, 20},
    {ok, Toks} = xfl_lexer:lex(Str, Coord),
    {ok, Ast} = xfl_parser:parse(Toks),
    Ast.

-define(P(Str, ExpectedAst), ?_assert(parse_test(Str) == ExpectedAst)).

%%% intersections (range overlap really)

intersection_test_() ->
    [
     ?P("A1:B10 ^^ A1:A5",
        ['^^',
         [':', {ref,{col,-9},{row,-9},"./","A1"}, {ref,{col,-8},{row,0},"./","B10"}],
         [':', {ref,{col,-9},{row,-9},"./","A1"}, {ref,{col,-9},{row,-5},"./","A5"}]]),

     ?P("@YEAR ^^ @SALESMAN",
        ['^^',[name,"year","./"],[name,"salesperson","./"]]),

     ?P("/bla/foo/bar/A1:B10 ^^ A1:A5",
        ['^^',
         [':', {ref,{col,-9},{row,-9},"/bla/foo/bar/","A1"}, {ref,{col,-8},{row,0},"./","B10"}],
         [':', {ref,{col,-9},{row,-9},"./","A1"}, {ref,{col,-9},{row,-5},"./","A5"}]]),
     
     ?P("/salesdata/@YEAR ^^ @SALESMAN",
        ['^^',
         [name,"@YEAR","/salesdata/"],
         [name,"salesman","./"]])        
    ].

%% -define(CURRENT_CELL, {10, 10}).
%% -define(TESTS,
%%         [
%%          { "1/2",  ['/',1,2]},
%%          { "ABC123 / 42", ['/',[ref,{col,721},{row,113},"./"],42] },
%%          { "/some/pretty/long/path/B75 / 100", % significant whitespace
%%            ['/', [ref,{col,-8},{row,65},"/some/pretty/long/path/"],100]},
%%          { "(/some/pretty/long/path/B75)/100", % disambiguate with parenthesis
%%            ['/',[ref,{col,-8},{row,65},"/some/pretty/long/path/"],100]}
%%         ]).

%% test() ->
%%     Parse = fun(Str) ->
%%                     {ok, Toks} = xfl_lexer:lex(Str, ?CURRENT_CELL),
%%                     {ok, Ast} = xfl_parser:parse(Toks),
%%                     Ast
%%             end,
    
%%     foreach(fun({Str, ExpectedAst}) ->
%%                     case Parse(Str) of
%%                         ExpectedAst -> ok;
%%                         _Other      -> io:format("FAIL: mismatch for: ~s~n", [Str])
%%                     end
%%             end,
%%             ?TESTS).
