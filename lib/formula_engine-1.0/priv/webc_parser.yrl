%%% @doc cond parser parses condition clauses
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Expr
RelPath
SubExpr
Segs
Seg
Control
Clause
SubClauses2
SubClause1
SubClause2
.

Terminals

fullstop
open
close
comma
dollar
semicolon
slash
plainpath
.

Rootsymbol Expr.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators

%% ----- Grammar definition.

Expr -> fullstop fullstop SubExpr : [wrap_plain({plainpath, ".."}) | '$3'].
Expr -> fullstop SubExpr          : [wrap_plain({plainpath, "."})  | '$2'].
Expr -> SubExpr                   : '$1'.

SubExpr -> Seg  slash : ['$1'] .
SubExpr -> Segs slash : lists:flatten('$1').

Segs -> Segs Seg : join('$1', '$2').
Segs -> Seg  Seg : join('$1', '$2').

Seg -> Control : '$1'.

Seg -> slash plainpath : wrap_plain('$2').
Seg -> slash RelPath   : '$2'.

RelPath -> RelPath fullstop : wrap_plain({plainpath, ".."}).
RelPath -> fullstop         : wrap_plain({plainpath, "."}).

Control -> Clause close : '$1'.

Clause -> SubClause1             : wrap_plain(clause1(lists:flatten('$1'))).
Clause -> SubClause1 SubClauses2 : clauses(lists:flatten('$1'), '$2').

SubClauses2 -> SubClauses2 SubClause2 : join_subclause2(lists:flatten('$1'), {'$2'}).
SubClauses2 -> SubClause2             : [{'$1'}].

SubClause1 -> SubClause1 comma plainpath        : join('$1', '$3').
SubClause1 -> slash open plainpath              : ['$3'].

SubClause2 -> semicolon plainpath        : ['$2'].
SubClause2 -> SubClause2 comma plainpath : lists:flatten(join('$1', '$3')).
SubClause2 -> SubClause2 comma dollar plainpath : lists:flatten(join('$1', make_user('$3', '$4'))).

Erlang code.

-export([
         compile/1,
         p_TEST/1
        ]).

-include("spriki.hrl").
-include("errvals.hrl").

%%% Api
compile(String) ->
    {ok, Toks, 1} = webc_lexer:lex(String),
    {ok, AST} = parse(Toks),
    AST.

%%%% Parser code

wrap_plain(A) -> #segment{page = A}.

clauses(A, B) -> c2(clause1(A), B, #segment{}).

c2(A, [], Control) ->
    Control#segment{page = A};
c2(A, [{[{plainpath, "go"}, {plainpath, Type}]} | T], Control)
  when Type == "table" orelse Type == "spreadsheet"
       orelse Type == "webpage" orelse Type == "wikipage" ->
    c2(A, T, Control#segment{redirect = #destination{type = Type}});
c2(A, [{[{plainpath, "go"}]} | T], Control) ->
    c2(A, T, Control#segment{redirect = #destination{type = "default"}});
c2(A, [{H} | T], C) ->
    [_ | T2] = H,
    {_, NewGroups} = lists:unzip(T2),
    NewC = case hd(H) of
               {plainpath, "table"} ->
                   make_controls(NewGroups, C, table);
               {plainpath, "spreadsheet"} ->
                   make_controls(NewGroups, C, spreadsheet);
               {plainpath, "webpage"} ->
                   make_controls(NewGroups, C, webpage);
               {plainpath, "wikipage"} ->
                   make_controls(NewGroups, C, wikpage);
               _Other ->
                   exit("no such view")
           end,
    c2(A, T, NewC).

make_user({dollar, [$$]}, {plainpath, "user"}) -> {plainpath, "$user"};
make_user(A, B) -> [A, B].

make_controls(Groups, C, table) ->
    OldG = C#segment.addtablegroups,
    NewG = lists:merge(Groups, OldG),
    C#segment{addtablegroups = NewG};
make_controls(Groups, C, spreadsheet) ->
    OldG = C#segment.addspreadsheetgroups,
    NewG = lists:merge(Groups, OldG),
    C#segment{addspreadsheetgroups = NewG};
make_controls(Groups, C, webpage) ->
    OldG = C#segment.addwebpagegroups,
    NewG = lists:merge(Groups, OldG),
    C#segment{addwebpagegroups = NewG};
make_controls(Groups, C, wikipage) ->
    OldG = C#segment.addwikipagegroups,
    NewG = lists:merge(Groups, OldG),
    C#segment{addwikipagegroups = NewG}.

%% 1 arg
clause1([{plainpath, A}]) ->
    {plainpath, A};

%% 2 args
clause1([{plainpath, A}, {plainpath, B}]) ->
    #namedpage{template = A, name = B};
%% 3 args
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "incr"}]) ->
    #numberedpage{template = A, type = "increment", prefix = ""};
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "increment"}]) ->
    #numberedpage{template = A, type = "increment", prefix = ""};
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "random"}]) ->
    #numberedpage{template = A, type = "random", prefix = ""};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "yy"}]) ->
    #datedpage{template = A, format = "yy"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "yyyy"}]) ->
    #datedpage{template = A, format = "yyyy"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "m"}]) ->
    #datedpage{template = A, format = "m"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "mm"}]) ->
    #datedpage{template = A, format = "mm"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "mmm"}]) ->
    #datedpage{template = A, format = "mmm"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "mmmm"}]) ->
    #datedpage{template = A, format = "mmmm"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "d"}]) ->
    #datedpage{template = A, format = "d"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "dd"}]) ->
    #datedpage{template = A, format = "dd"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "ddd"}]) ->
    #datedpage{template = A, format = "ddd"};
clause1([{plainpath, A}, {plainpath, "date"}, {plainpath, "dddd"}]) ->
    #datedpage{template = A, format = "dddd"};
% 4 args
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "incr"},
         {plainpath, B}]) ->
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = "increment", prefix = B};
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "increment"},
         {plainpath, B}]) ->
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = "increment", prefix = B};
clause1([{plainpath, A}, {plainpath, "auto"}, {plainpath, "random"},
         {plainpath, B}]) ->
    ok = check_not_num_prefix(B),
    #numberedpage{template = A, type = "random", prefix = B}.

join_subclause2(A, B) -> lists:flatten([A, B]).

join(A, B) -> [A, B].

check_not_num_prefix(A) ->
    case tconv:to_num(A) of
        {error, nan} -> ok;
        _            -> ?ERR_VAL
    end.

%%% Tests:
-include_lib("eunit/include/eunit.hrl").

p_TEST(String) ->
    io:format("String is ~p~n", [String]),
    {ok, Toks, 1} = webc_lexer:lex(String),
    io:format("Toks is ~p~n", [Toks]),
    {ok, AST} = parse(Toks),
    io:format("AST is ~p~n", [AST]),
    AST.

seg_test_() ->
    [
     ?_assert(p_TEST("/blah/") ==
              [
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("./blah/") ==
              [
               {segment, {plainpath, "."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("./blah/") ==
              [
               {segment, {plainpath, "."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("../blah/") ==
              [
               {segment, {plainpath, ".."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("../blah/.././handy/") ==
              [
               {segment, {plainpath, ".."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath, ".."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "."},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "handy"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, Name]/") ==
              [
               {segment, {namedpage, "template", "name"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, auto, increment]/") ==
              [
               {segment, {numberedpage, "template", "increment", ""},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, auto, increment, Yeah]/") ==
              [
               {segment, {numberedpage, "template", "increment", "yeah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, auto, random]/") ==
              [
               {segment, {numberedpage, "template", "random", ""},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, auto, random, Yeah]/") ==
              [
               {segment, {numberedpage, "template", "random", "yeah"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, yy]/") ==
              [
               {segment, {datedpage, "template", "yy"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, yyyy]/") ==
              [
               {segment, {datedpage, "template", "yyyy"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, m]/") ==
              [
               {segment, {datedpage, "template", "m"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, mm]/") ==
              [
               {segment, {datedpage, "template", "mm"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, mmm]/") ==
              [
               {segment, {datedpage, "template", "mmm"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, mmmm]/") ==
              [
               {segment, {datedpage, "template", "mmmm"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, d]/") ==
              [
               {segment, {datedpage, "template", "d"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, dd]/") ==
              [
               {segment, {datedpage, "template", "dd"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, ddd]/") ==
              [
               {segment, {datedpage, "template", "ddd"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[Template, date, dddd]/") ==
              [
               {segment, {datedpage, "template", "dddd"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/blah/[Template, Name]/") ==
              [
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []},
               {segment, {namedpage, "template", "name"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/blah/bleh/[Template, Name]/bloh/") ==
              [
               {segment, {plainpath, "blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "bleh"},
                #destination{}, [], [], [], []},
               {segment, {namedpage, "template", "name"},
                #destination{}, [], [], [], []},
               {segment, {plainpath, "bloh"},
                #destination{}, [], [], [], []}
              ])

    ].

rel_test_() ->
    [
     ?_assert(p_TEST("./blah/bleh/[Template, Name]/bloh/") ==
              [
               {segment, {plainpath,"."},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bleh"},
                #destination{}, [], [], [], []},
               {segment, {namedpage,"template", "name"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bloh"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("../blah/bleh/[Template, Name]/bloh/") ==
              [
               {segment, {plainpath,".."},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bleh"},
                #destination{}, [], [], [], []},
               {segment, {namedpage,"template", "name"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bloh"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/blah/./bleh/[Template, Name]/bloh/") ==
              [
               {segment, {plainpath,"blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"."},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bleh"},
                #destination{}, [], [], [], []},
               {segment, {namedpage,"template", "name"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bloh"},
                #destination{}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/blah/../bleh/[Template, Name]/bloh/") ==
              [
               {segment, {plainpath,"blah"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,".."},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bleh"},
                #destination{}, [], [], [], []},
               {segment, {namedpage,"template", "name"},
                #destination{}, [], [], [], []},
               {segment, {plainpath,"bloh"},
                #destination{}, [], [], [], []}
              ])
    ].

prod_test_() ->
    [

     ?_assert(p_TEST("/[jingo, bobbie]/") ==
              [
               {segment, {namedpage,"jingo", "bobbie"},
                #destination{}, [], [], [], []}
              ])
    ].

adv_test_() ->
    [

     ?_assert(p_TEST("/[jingo, bobbie; go]/") ==
              [
               {segment, {namedpage,"jingo", "bobbie"},
                #destination{type = "default"}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[jingo, bobbie; table, group1]/") ==
              [
               {segment, {namedpage,"jingo", "bobbie"},
                #destination{type = false}, [], [], [], ["group1"]}
              ]),

     ?_assert(p_TEST("/[jingo, bobbie; go, table]/") ==
              [
               {segment, {namedpage,"jingo", "bobbie"},
                #destination{type = "table"}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[bobbie; go]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = "default"}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[bobbie; go, table]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = "table"}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[bobbie;table, group1, group2, group3]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = false}, [], [], [], ["group1", "group2",
                                                         "group3"]}
              ]),

     ?_assert(p_TEST("/[bobbie;table, $user, group2, group3]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = false}, [], [], [], ["$user", "group2",
                                                         "group3"]}
              ]),

     ?_assert(p_TEST("/[bobbie; go, table; table, group1, group2, group3]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = "table"}, [], [], [], ["group1", "group2",
                                                           "group3"]}
              ]),

     ?_assert(p_TEST("/[bobbie; go, table; table, group1, group2, group3; " ++
                     "table, group4, group5]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = "table"}, [], [], [], ["group1", "group2",
                                                           "group3",
                                                           "group4", "group5"]}
              ]),

     ?_assert(p_TEST("/[bobbie; go, table; table, group1, group2, group3; " ++
                     "table, group4, group5]/bleh/[banjo, auto, incr;go;" ++
                     "table, group1; spreadsheet, group2]/") ==
              [
               {segment, {plainpath,"bobbie"},
                #destination{type = "table"}, [], [], [], ["group1", "group2",
                                                           "group3",
                                                           "group4", "group5"]},
               {segment, {plainpath,"bleh"},
                #destination{type = false}, [], [], [], []},
               {segment, {numberedpage,"banjo", "increment", ""},
                #destination{type = "default"}, ["group2"], [], [],
                ["group1"]}

              ])
    ].

stevies_test_() ->
    [
     ?_assert(p_TEST("/[jingo, 1]/") ==
              [
               {segment, {namedpage,"jingo", "1"},
                #destination{type = false}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[jingo, 1-2]/") ==
              [
               {segment, {namedpage,"jingo", "1-2"},
                #destination{type = false}, [], [], [], []}
              ]),

     ?_assert(p_TEST("/[jingo, 12aA_-~]/") ==
              [
               {segment, {namedpage,"jingo", "12aa_-~"},
                #destination{type = false}, [], [], [], []}
              ])
    ].
