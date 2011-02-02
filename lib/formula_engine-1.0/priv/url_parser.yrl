%%% -*- mode: erlang -*-
%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
SubExpr
Segs
Seg
Path
Refs
Ref
Fullstops
.

Terminals

zseg
slash
path
cellref
row
col
range
excel_expr
fullstop
.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> fullstop fullstop SubExpr : fix_up("..", '$3').
Expr -> fullstop          SubExpr : fix_up(".",  '$2').
Expr ->                   SubExpr : fix_up("",   '$1').

SubExpr -> Segs  slash Refs : tidy(flatpack('$1'),    type('$3')).
SubExpr -> Segs  slash      : tidy(flatpack('$1'),   {page, "/"}).
SubExpr -> Seg   slash Refs : tidy(flatpack(['$1']),  type('$3')).
SubExpr -> Seg   slash      : tidy(flatpack(['$1']), {page, "/"}).
SubExpr -> slash Refs       : tidy({url, []},         type('$2')).
SubExpr -> slash            : tidy({url, []},        {page, "/"}).

Segs -> Segs Seg : join('$1', '$2').
Segs -> Seg Seg  : join('$1', '$2').

Seg -> Path       : '$1'.
Seg -> slash zseg : zseg('$2').

Fullstops -> fullstop          : '$1'.
Fullstops -> fullstop fullstop : '$1' ++ '$2'.

Path -> slash path      : seg('$2').
Path -> slash cellref   : seg('$2').
Path -> slash Fullstops : seg('$2').

Refs -> Refs Ref : join('$1', '$2').
Refs -> Ref      : '$1'.

Ref -> fullstop   : '$1'.
Ref -> path       : '$1'.
Ref -> cellref    : '$1'.
Ref -> row        : '$1'.
Ref -> col        : '$1'.
Ref -> range      : '$1'.
Ref -> excel_expr : '$1'.

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

fix_up(_A, B) -> B.

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

join(A, B) when is_list(A) -> lists:concat([A, [B]]);
join(A, B)                 -> [A, B].

seg({fullstop, Chars}) -> {seg, Chars};
seg({path, Chars})     -> {seg, Chars};
seg({cellref, Chars})  -> {seg, Chars}.

type(List) when is_list(List) -> hn_util:parse_ref(t2(List, []));
type({_, Chars})              -> hn_util:parse_ref(Chars).

t2([], Acc)             -> lists:flatten([lists:reverse(Acc)]);
t2([{_, Txt} | T], Acc) -> t2(T, [Txt | Acc]).

zseg({_, Txt}) -> {zseg, "[" ++ Txt ++ "]"}.

%% API

make_refX("http://"++URL) ->
    {Site, PathAndRef} = lists:split(string:chr(URL, $/) - 1, URL),
    {ok, Toks} = url_lexer:lex(PathAndRef),
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
    {ok, Toks} = url_lexer:lex(String),
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

     %% ?_assert(p_TEST("/Blah/bLah/") == {url, [
     %%                                          "blah",
     %%                                          "blah"
     %%                                         ],
     %%                                    {page, "/"}}),

     ?_assert(p_TEST("/[blah]/") == {gurl, [
                                            "[blah]"
                                           ],
                                     {page, "/"}}),

     %% ?_assert(p_TEST("/[bLAh]/") == {gurl, [
     %%                                        "[bLAh]"
     %%                                       ],
     %%                                 {page, "/"}}),

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

     %% ?_assert(p_TEST("/Blah/bLah/$b:c") == {gurl, [
     %%                                               "blah",
     %%                                               "blah"
     %%                                              ],
     %%                                        {column, {range, {2, zero, 3, inf}}}}),

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
     
               ?_assert(make_refX("http://tests.hypernumbers.dev:9000/a_quis_custodiet_custodiens/a_quis_custodiet_custodiens/sheet1/") == {refX, "http://tests.hypernumbers.dev:9000", url, ["a_quis_custodiet_custodiens", "a_quis_custodiet_custodiens", "sheet1"], {page, "/"}}),
     
               ?_assert(make_refX("http://tests.hypernumbers.dev:9000/a_quis_custodiet_custodiens/a_quis_custodiet_custodiens/sheet1/A13") == {refX, "http://tests.hypernumbers.dev:9000", url, ["a_quis_custodiet_custodiens", "a_quis_custodiet_custodiens", "sheet1"], {cell, {1, 13}}}),

                    ?_assert(make_refX("http://hypernumbers.dev:9000/_sync/tell/anonymous|_2a087da0d62cd0840f9d1a5667ed6646|never|51cef8208e98daca95be750d8c26faa6/") == {refX, "http://hypernumbers.dev:9000", url, ["_sync","tell","anonymous|_2a087da0d62cd0840f9d1a5667ed6646|never|51cef8208e98daca95be750d8c26faa6"], {page, "/"}}),

     ?_assert(make_refX("http://hypernumbers.dev:9000/[(or(a1 > 1, a2 > 2))]/a3") == {refX, "http://hypernumbers.dev:9000", gurl, ["[(or(a1>1,a2>2))]"], {cell, {1, 3}}}),
     
     ?_assert(make_refX("http://hypernumbers.dev:9000/./[(or(a1 > 1, a2 > 2))]/a3") == {refX, "http://hypernumbers.dev:9000", gurl, [".","[(or(a1>1,a2>2))]"], {cell, {1, 3}}}),
     
     ?_assert(make_refX("http://hypernumbers.dev:9000/[(or(./a1 > 1, ./a2 > 2))]/a3") == {refX, "http://hypernumbers.dev:9000", gurl, ["[(or(./a1>1,./a2>2))]"], {cell, {1, 3}}}),

     ?_assert(make_refX("http://hypernumbers.dev:9000/./[(or(./a1 > 1, ./a2 > 2))]/a3") == {refX, "http://hypernumbers.dev:9000", gurl, [".","[(or(./a1>1,./a2>2))]"], {cell, {1, 3}}}),

     ?_assert(make_refX("http://hypernumbers.com/_sync/seek/?return=http%3A%2F%2Fbi27.tiny.hn%2F%3Fview%3Ddemopage") == {refX, "http://hypernumbers.com", url, ["_sync", "seek"], {page, "/"}}),

     ?_assert(make_refX("http://hypernumbers.com/_ping/?spoor=3b268089bf028bf5&return=http://support.hypernumbers.com/helpoverview/overview/") == {refX, "http://hypernumbers.com", url, ["_ping"], {page, "/"}})
     
     ].
