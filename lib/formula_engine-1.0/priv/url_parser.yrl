%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
Segs
Seg
Path
Cond
Clause
Ref
Comps
Comp
File
Partfile.

Terminals

open
close
slash
path
cellref
row
col
range
fullstop
.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> Segs slash Ref : tidy(flatpack('$1'),    type('$3')).
Expr -> Segs slash     : tidy(flatpack('$1'),   {page, "/"}).
Expr -> Seg slash Ref  : tidy(flatpack(['$1']),  type('$3')).
Expr -> Seg slash      : tidy(flatpack(['$1']), {page, "/"}). % make the seg a list!
Expr -> slash Ref      : tidy({url, []},         type('$2')).
Expr -> slash          : tidy({url, []},        {page, "/"}).

Segs -> Segs Seg : join('$1', '$2').
Segs -> Seg Seg  : join('$1', '$2').

Seg -> Path : '$1'.
Seg -> Cond : '$1'.     

Path -> slash path    : seg('$2').
Path -> slash cellref : seg('$2').

Cond -> slash Clause : '$2'.

Clause -> open Comps close : zseg('$2').

Comps -> Comp      : '$1'.
Comps -> Comp Comp : join('$1', '$2').

Comp -> path  : '$1'.
Comp -> Ref   : '$1'.
Comp -> slash : '$1'.

Ref -> File    : '$1'.

Ref -> cellref : '$1'.
Ref -> row     : '$1'.
Ref -> col     : '$1'.
Ref -> range   : '$1'.

Partfile -> path    : '$1'.
Partfile -> cellref : '$1'.

File -> File fullstop Partfile     : make_filename('$1', '$3').
File -> Partfile fullstop Partfile : make_filename('$1', '$3').

Erlang code.
%% Erlang code follows here
-export([
         make_refX/1
        ]).

-export([
         p_TEST/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

%%% Functions used in parsing

%% sometimes it can be {filname, _}, {path, _}
make_filename({_, Root}, {path, Ext}) -> {filename, Root ++ "." ++ Ext}.

tidy({Type,  Path},  {page, "/"}   = O) -> {Type, Path, O};
tidy({Type,  Path},  {cell, _}     = O) -> {Type, Path, O};
tidy({Type,  Path},  {range, _}    = O) -> {Type, Path, O};
tidy({_Type, Path},  {row, _}      = O) -> {gurl, Path, O};
tidy({_Type, Path},  {column, _}   = O) -> {gurl, Path, O};
tidy({gurl,  _Path}, {filename, _})     -> ?ERR_VAL; % can never have a filename gurl
tidy({url,   Path},  {filename, _} = O) -> {url, Path, O}.

flatpack(List) -> fp(List, url, []).

fp([], Type, Acc)                 -> {Type, lists:reverse(Acc)};
fp([{seg,  Seg}  | T], Type, Acc) -> fp(T, Type, [Seg | Acc]);
fp([{zseg, Zseg} | T], _, Acc)    -> fp(T, gurl, [Zseg | Acc]).

join(A, B) when is_list(A) ->
    io:format("Joining (2) ~p and ~p~n", [A, B]),
    lists:concat([A, [B]]);
join(A, B) -> io:format("Joining ~p and ~p~n", [A, B]),
              [A, B].

flit(A) -> io:format("Flitting ~p~n", [A]),
           A.

seg({path, Chars})    -> io:format("Seg is a path ~p~n", [Chars]),
                         {seg, Chars};
seg({cellref, Chars}) -> io:format("Seg is a cellref ~p~n", [Chars]),
                         {seg, Chars}.

type({_, Chars}) -> hn_util:parse_ref(Chars).

zseg({_, Txt})     -> {zseg, "[" ++ Txt ++ "]"};
zseg(List) when is_list(List) -> z(List, []).

z([], Acc)             -> ["[" | lists:reverse(["]" | Acc])];
z([{_, Txt} | T], Acc) -> z(T, [Txt | Acc]).

%% API

make_refX("http://"++URL) ->
    {Site, PathAndRef} = lists:split(string:chr(URL, $/) - 1, URL),
    {ok, Toks, 1} = url_lexer:lex(PathAndRef),
        Ret = parse(Toks),
    case Ret of
        {ok, {Type, Path, Ref}} -> 
            #refX{site = "http://" ++ Site, type = Type, path = Path, obj = Ref};
        {error, Err} -> error_logger:error_msg("url parsing of ~p~n~p~n"
                                               ++ "failed with ~p~n",
                                               [URL, Toks, Err]),
                        exit(wiggo)
    end.

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

p_TEST(String) ->
    io:format("String is ~p~n", [String]),
    {ok, Toks, 1} = url_lexer:lex(String),
    io:format("Toks is ~p~n", [Toks]),
    {ok, {Type, Path, Ref}} = parse(Toks),
    {Type, Path, Ref}.

seg_test_() ->
    [
     ?_assert(p_TEST("/") == {url, [ ],
                              {page, "/"}}),

     ?_assert(p_TEST("/blah/") == {url, [
                                         "blah"
                                        ],
                                   {page, "/"}}),

     ?_assert(p_TEST("/blah2/") == {url, [
                                          "blah2"
                                         ],
                                   {page, "/"}}),
     
     ?_assert(p_TEST("/blah/blah/") == {url,[
                                             "blah",
                                             "blah"
                                            ],
                                        {page, "/"}}),

     ?_assert(p_TEST("/Blah/bLah/") == {url, [
                                              "blah",
                                              "blah"
                                             ],
                                        {page, "/"}}),

     ?_assert(p_TEST("/[blah]/") == {gurl, [
                                            "[blah]"
                                           ],
                                     {page, "/"}}),

     ?_assert(p_TEST("/[bLAh]/") == {gurl, [
                                            "[bLAh]"
                                           ],
                                     {page, "/"}}),

     ?_assert(p_TEST("/blah/[blah]/") == {gurl, [
                                                 "blah",
                                                 "[blah]"
                                                ],
                                          {page, "/"}})
    ].

ref_test_() ->
    [
     ?_assert(p_TEST("/a1") == {url, [ ],
                                {cell, {1, 1}}}),

     ?_assert(p_TEST("/blah/$a$2") == {url, [
                                             "blah"
                                            ],
                                       {cell, {1, 2}}}),

     ?_assert(p_TEST("/blah/blah/b:c") == {gurl, [
                                                  "blah",
                                                  "blah"
                                                 ],
                                           {column, {range, {2, zero,  3, inf}}}}),

     ?_assert(p_TEST("/Blah/bLah/$b:c") == {gurl, [
                                                   "blah",
                                                   "blah"
                                                  ],
                                            {column, {range, {2, zero, 3, inf}}}}),

     ?_assert(p_TEST("/[blah]/a1:B2") == {gurl, [
                                                 "[blah]"
                                                ],
                                          {range,{1, 1, 2, 2}}}),

     ?_assert(p_TEST("/[bLAh]/2:3") == {gurl, [
                                               "[bLAh]"
                                              ],
                                        {row, {range, {zero, 2, inf, 3}}}}),

     ?_assert(p_TEST("/blah/[blah]/1:$1") == {gurl, [
                                                     "blah",
                                                     "[blah]"
                                                    ],
                                              {row, {range, {zero, 1, inf, 1}}}})
    ].

filename_test_() ->
    [
     ?_assert(p_TEST("/bingo.xls")     == {url, [ ], {filename, "bingo.xls"}}),
     ?_assert(p_TEST("/blah/b.xls")    == {url, ["blah"], {filename, "b.xls"}}),
     ?_assert(p_TEST("/hn.jquery.js")  == {url, [ ], {filename, "hn.jquery.js"}}),
     ?_assert(p_TEST("/blah/hn.jq.js") == {url, ["blah"], {filename, "hn.jq.js"}})
    ].

prod_test_() ->
    [
     ?_assert(make_refX("http://tests.hypernumbers.dev:9000/a_quis_custodiet_custodiens/sheet1/A13") == {refX, "http://tests.hypernumbers.dev:9000", url, ["a_quis_custodiet_custodiens", "sheet1"], {cell, {1, 13}}}),
               ?_assert(make_refX("http://tests.hypernumbers.dev:9000/a_quis_custodiet_custodiens/a_quis_custodiet_custodiens/sheet1/") == {refX, "http://tests.hypernumbers.dev:9000", url, ["a_quis_custodiet_custodiens", "a_quis_custodiet_custodiens", "sheet1"], {page, "/"}})
     ].
