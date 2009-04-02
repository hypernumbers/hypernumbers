%%% @doc    Lexer for XFL.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% TODO: Double "" as a way to include a quote character in a string.
%%%       This works in Ruby: rx = /\"((\"\")|(.)|(\n))*\"/
%%% TODO: Good unit test coverage.

Definitions.

%%% Constants: integers, floats (in decmial and scientific notations),
%%% booleans, strings, error constants.

INT = ([0-9]+)
FLOATDEC = ([0-9]+\.[0-9]+)
FLOATSCI = ([0-9]+\.[0-9]+((E|e))(\+|\-)?[0-9]+)

TRUE = ((T|t)(R|r)(U|u)(E|e))
FALSE = ((F|f)(A|a)(L|l)(S|s)(E|e))
BOOL = ({TRUE}|{FALSE})

STR = (\"[^"]*\")
%%" % erlang-mode fix

ERRVAL = \#NULL\!|\#DIV\/0\!|\#VALUE\!|\#REF\!|\#NAME\?|\#NUM\!|\#N\/A

%%% References: cell references (A1 & RC), range references (finite, column, row;
%%% both styles), name references.

%%% 1. Cell references.

%%% References may start with / or ./ or ../
%%% Bangs are also supported in place of slashes.
%%% Path components are allowed to contain any combination of English
%%% letters, digits, underscores and tildes.

START_OF_SSREF = ((\/|\!)|(\.\.(\/|\!))+|\.(\/|\!))
PATH_COMP = ([a-zA-Z0-9_\-~]+)
MAYBE_PATH = (((({PATH_COMP})|(\.)|(\.\.))(\/|\!))*)

URL_PREFIX = {START_OF_SSREF}{MAYBE_PATH}
MAYBE_URL_PREFIX = (({URL_PREFIX})?)

WORD = ([a-zA-Z]+)

%%% For RC-style refs, the offset is an integer in brackets with an optional
%%% leading + or -.

OFFSET_RC = (\[(\+|\-)?({INT})\])
MAYBE_OFFSET_RC = ({OFFSET_RC}?)

A1_REF_REL  = ({MAYBE_URL_PREFIX})({WORD})({INT})
A1_REF_FIX  = ({MAYBE_URL_PREFIX})(\$)({WORD})(\$)({INT})
A1_REF_MIX1 = ({MAYBE_URL_PREFIX})(\$)({WORD})({INT})
A1_REF_MIX2 = ({MAYBE_URL_PREFIX})({WORD})(\$)({INT})

RC_REF_REL  = ({MAYBE_URL_PREFIX})(({MAYBE_OFFSET_RC})(R|r)({MAYBE_OFFSET_RC})(C|c))
RC_REF_FIX  = ({MAYBE_URL_PREFIX})(({INT})(R|r)({INT})(C|c))
RC_REF_MIX1 = ({MAYBE_URL_PREFIX})(({MAYBE_OFFSET_RC})(R|r)({INT})(C|c))
RC_REF_MIX2 = ({MAYBE_URL_PREFIX})(({INT})(R|r)({MAYBE_OFFSET_RC})(C|c))

%%% Helper classes:

A1_REF = ({A1_REF_REL}|{A1_REF_FIX}|{A1_REF_MIX1}|{A1_REF_MIX2})
RC_REF = ({RC_REF_REL}|{RC_REF_FIX}|{RC_REF_MIX1}|{RC_REF_MIX2})

%%% 2. Finite ranges.

FINITE_RANGE_A1 = ({MAYBE_URL_PREFIX})({A1_REF})(\:)({A1_REF})
FINITE_RANGE_RC = ({MAYBE_URL_PREFIX})({RC_REF})(\:)({RC_REF})

%%% 3. Row & column ranges.

%%% A1:

A1_COL_REF_REL  = (({MAYBE_URL_PREFIX})({WORD})(\:)({WORD}))
A1_COL_REF_FIX  = (({MAYBE_URL_PREFIX})(\$)({WORD})(\:)(\$)({WORD}))
A1_COL_REF_MIX1 = (({MAYBE_URL_PREFIX})(\$)({WORD})(\:)({WORD}))
A1_COL_REF_MIX2 = (({MAYBE_URL_PREFIX})({WORD})(\:)(\$)({WORD}))

A1_ROW_REF_REL  = (({MAYBE_URL_PREFIX})({INT})(\:)({INT}))
A1_ROW_REF_FIX  = (({MAYBE_URL_PREFIX})(\$)({INT})(\:)(\$)({INT}))
A1_ROW_REF_MIX1 = (({MAYBE_URL_PREFIX})(\$)({INT})(\:)({INT}))
A1_ROW_REF_MIX2 = (({MAYBE_URL_PREFIX})({INT})(\:)(\$)({INT}))

%% RC:

RC_COL_REF_REL  = (({MAYBE_URL_PREFIX})({MAYBE_OFFSET_RC})(C|c)(\:)({MAYBE_OFFSET_RC})(C|c))
RC_COL_REF_FIX  = (({MAYBE_URL_PREFIX})({INT})(C|c)(\:)({INT})(C|c))
RC_COL_REF_MIX1 = (({MAYBE_URL_PREFIX})({INT})(C|c)(\:)({MAYBE_OFFSET_RC})(C|c))
RC_COL_REF_MIX2 = (({MAYBE_URL_PREFIX})({MAYBE_OFFSET_RC})(C|c)(\:)({INT})(C|c))

RC_ROW_REF_REL  = (({MAYBE_URL_PREFIX})({MAYBE_OFFSET_RC})(R|r)(\:)({MAYBE_OFFSET_RC})(R|r))
RC_ROW_REF_FIX  = (({MAYBE_URL_PREFIX})({INT})(R|r)(\:)({INT})(R|r))
RC_ROW_REF_MIX1 = (({MAYBE_URL_PREFIX})({INT})(R|r)(\:)({MAYBE_OFFSET_RC})(R|r))
RC_ROW_REF_MIX2 = (({MAYBE_URL_PREFIX})({MAYBE_OFFSET_RC})(R|r)(\:)({INT})(R|r))

%% Helper classes:

A1_COL_RANGE = ({A1_COL_REF_REL}|{A1_COL_REF_FIX}|{A1_COL_REF_MIX1}|{A1_COL_REF_MIX2})
A1_ROW_RANGE = ({A1_ROW_REF_REL}|{A1_ROW_REF_FIX}|{A1_ROW_REF_MIX1}|{A1_ROW_REF_MIX2})

RC_COL_RANGE = ({RC_COL_REF_REL}|{RC_COL_REF_FIX}|{RC_COL_REF_MIX1}|{RC_COL_REF_MIX2})
RC_ROW_RANGE = ({RC_ROW_REF_REL}|{RC_ROW_REF_FIX}|{RC_ROW_REF_MIX1}|{RC_ROW_REF_MIX2})

%%% Named expression:

%%% Named expression or function name:
NAME = ([a-zA-Z][a-zA-Z0-9_\.]*)
NAME_REF = ({MAYBE_URL_PREFIX}{NAME})

%%% Whitespace:

WHITESPACE = ([\000-\s]*)


Rules.

%%% 1. Cell references:

{A1_REF} : {token, to_cellref(YYtext, a1)}.
{RC_REF} : {token, to_cellref(YYtext, rc)}.

%%% 2. Finite ranges:

{FINITE_RANGE_A1} : {token, finite_range(YYtext, a1)}.
{FINITE_RANGE_RC} : {token, finite_range(YYtext, rc)}.

%%% 3. Row & column ranges:

{A1_COL_RANGE} : {token, a1_col_range(YYtext)}.
{A1_ROW_RANGE} : {token, a1_row_range(YYtext)}.
{RC_COL_RANGE} : {token, rc_col_range(YYtext)}.
{RC_ROW_RANGE} : {token, rc_row_range(YYtext)}.

%%% 4. Named expressions:

{NAME_REF}     : {token, name(YYtext)}.

%% Basic data types.
{INT}      : {token, {int, tconv:to_i(YYtext)}}.
{FLOATDEC} : {token, {float, tconv:to_f(YYtext)}}.
{FLOATSCI} : {token, {float, tconv:to_f(YYtext)}}.
{BOOL}     : {token, {bool, YYtext == "TRUE"}}.
{STR}      : {token, {str, hslists:mid(YYtext)}}.

{ERRVAL} : {token, {errval, list_to_atom(YYtext)}}.

%% Discard whitespace:
{WHITESPACE} : .

%% Punctuation & operators:

=  : {token, {'='}}.
,  : {token, {','}}.
\( : {token, {'('}}.
\) : {token, {')'}}.
\{ : {token, {'{'}}.
\} : {token, {'}'}}.
\; : {token, {';'}}.

%% Operators.
%% Arithmetic.
\+ : {token, {'+'}}.
\- : {token, {'-'}}.
\* : {token, {'*'}}.
\/ : {token, {'/'}}.
\^ : {token, {'^'}}.

%% Logical.
>  : {token, {'>'}}.
<  : {token, {'<'}}.
>= : {token, {'>='}}.
<= : {token, {'<='}}.
<> : {token, {'<>'}}.

%% Other.
&  : {token, {'&'}}.
\% : {token, {'%'}}.
\: : {token, {':'}}.
\^\^ : {token, {'^^'}}.

%% Newline signifies end of input.
\n : {end_token, {'$end'}}.
%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, YYtext}}.

Erlang code.

%%% TODO: Normalize range references so that it's always left to right.

-export([lex/2, debang/1]).
-include("handy_macros.hrl").
-include("muin_records.hrl").

%% These are read-only.
-define(mx, get(mx)).
-define(my, get(my)).
-define(pivot, {?mx, ?my}).

%%% @type coord() = {Row :: pos_integer(), Column :: pos_integer()}
%%% @type tokens() = [{atom(), any()}]

%% @spec lex(Input :: string(), Cell :: coord()) -> tokens()
%% @doc  Lex a string relative to some cell.
lex(Input, {Mx, My}) ->
    put(mx, Mx),
    put(my, My),
    case string(Input) of
        {ok, Toks, _} -> {ok, Toks};
        _             -> lexer_error
    end.

%% @doc Replace all !s with /s in a string. (Used to normalize references.)
debang(Ssref) ->
    {ok, Newssref, _} = regexp:gsub(Ssref, "!", "/"),
    Newssref.

%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

name(YYtext) when hd(YYtext) == $.; hd(YYtext) == $/ ->
    {Path, Name} = split_ssref(YYtext),
    #namedexpr{path = Path, text = Name};
name(YYtext) ->
    {name, YYtext}.

to_cellref(YYtext, Type) ->
    {Path, Cell} = split_ssref(YYtext),
    {ColCoord, RowCoord} = extract_coords(Cell, Type, ?pivot),
    #cellref{col = ColCoord, row = RowCoord, path = Path, text = YYtext}.

%%% @doc Return coordinates of a given **local** cell reference.

extract_coords(Ref, rc, {PivotCol, PivotRow}) ->
    case string:tokens(string:to_upper(Ref), "R") of
        ["C"] -> % no row or column offset = same cell/0 offset
            {{offset, 0}, {offset, 0}};
        [ColStr] -> % row offset not specified = same row
            ColCoord = rc_col_to_coord(ColStr),
            {ColCoord, {offset, 0}};
        [RowStr, "C"] -> % col offset not specified
            RowCoord = rc_row_to_coord(RowStr),
            {{offset, 0}, RowCoord};
        [RowStr, ColStr] -> % both row & column specified
            ColCoord = rc_col_to_coord(ColStr),
            RowCoord = rc_row_to_coord(RowStr),
            {ColCoord, RowCoord}
    end;
extract_coords(Ref, a1, {PivotCol, PivotRow}) ->
    {ok, Ref2, _} = regexp:gsub(Ref, "\\$", ""),
    ColStr = takewhile(fun is_alpha/1, string:to_lower(Ref2)),
    Col = tconv:to_i(ColStr),
    Row = tconv:to_i(lists:nthtail(length(ColStr), Ref2)),
    IsColFixed = (hd(Ref) == $$),
    IsRowFixed = member($$, tl(Ref)),
    {?COND(IsColFixed, Col, {offset, Col - PivotCol}),
     ?COND(IsRowFixed, Row, {offset, Row - PivotRow})}.
             
%% @doc Construct a range object from matched token text.

finite_range(YYtext, Kind) ->
    {Path, LhsArg, RhsArg} = split_range(YYtext),
    {Tl, Br} = find_proper_bounds(LhsArg, RhsArg, Kind),
    #rangeref{type = finite, path = Path, tl = Tl, br = Br, text = YYtext}.

%% ColId is something like "A" or "$XYZ"
a1_col_to_coord(ColId) ->
    case string:substr(ColId, 1, 1) of
        "$" -> tconv:to_i(string:substr(ColId, 2));
        _   -> {offset, tconv:to_i(ColId) - ?my}
    end.

%% RowId is something like "1" or "$123"
a1_row_to_coord(RowId) ->
    case string:substr(RowId, 1, 1) of
        "$" -> tconv:to_i(string:substr(RowId, 2));
        _   -> {offset, tconv:to_i(RowId) - ?mx}
    end.

%% Return index from half of an RC ref (i.e. just an R/C part, e.g. "[10]R") and whether
%% it's fixed or not.
read_offset_r_or_c(Str) ->
    Rx = "(((\\+|\\-)?[0-9]+))",
    {match, [{St, Len}]} = regexp:matches(Str, Rx),
    Num = tconv:to_i(string:substr(Str, St, Len)),
    IsFixed = (string:str(Str, "[") == 0),
    {Num, IsFixed}.

rc_col_to_coord(ColId) ->
    case read_offset_r_or_c(ColId) of
        {Num, true}  -> Num;
        {Num, false} -> {offset, Num}
    end.

rc_row_to_coord(RowId) ->
    rc_col_to_coord(RowId).

a1_col_range(YYtext) ->
    {Path, LhsCol, RhsCol} = split_range(YYtext),
    #rangeref{type = col,
              path = Path,
              tl = {col, a1_col_to_coord(LhsCol)},
              br = {col, a1_col_to_coord(RhsCol)},
              text = YYtext}.

a1_row_range(YYtext) ->
    {Path, LhsRow, RhsRow} = split_range(YYtext),
    #rangeref{type = row,
              path = Path,
              tl = {row, a1_row_to_coord(LhsRow)},
              br = {row, a1_row_to_coord(RhsRow)},
              text = YYtext}.

rc_col_range(YYtext) ->
    {Path, LhsCol, RhsCol} = split_range(YYtext),
    #rangeref{type = col,
              path = Path,
              tl = {col, rc_col_to_coord(LhsCol)},
              br = {col, rc_col_to_coord(RhsCol)},
              text = YYtext}.

rc_row_range(YYtext) ->
    {Path, LhsRow, RhsRow} = split_range(YYtext),
    #rangeref{type = row,
              path = Path,
              tl = {row, rc_row_to_coord(LhsRow)},
              br = {row, rc_row_to_coord(RhsRow)},
              text = YYtext}.

%% @doc Takes coords of two cells defining bounds of some range, and returns
%% coords for top-left and bottom-right cells of that range.

find_proper_bounds(LhsArg, RhsArg, Kind) ->
    LhsCoord = extract_coords(LhsArg, Kind, ?pivot),
    RhsCoord = extract_coords(RhsArg, Kind, ?pivot),
    {LhsCoord, RhsCoord}.

%% @doc Given a cell reference, returns a tuple of {Path, Cell}
%% TODO: rename this.
%% TODO: duplicated in muin_util?                      
split_ssref(Ref) ->
    I = string:rstr(Ref, "/"),
    Path = lists:sublist(Ref, I),
    Cell = lists:nthtail(I, Ref),
    case Path of
        [] -> {"./", Cell};
        _  -> {Path, Cell}
    end.

split_range(Ref) ->
    {Path, Range} = split_ssref(Ref),
    [LhsArg, RhsArg] = string:tokens(Range, ":"),
    {Path, LhsArg, RhsArg}.

is_alpha(Char) when is_integer(Char) ->
    (member(Char, seq($A, $Z)) orelse
     member(Char, seq($a, $z)));
is_alpha([Char]) ->
    is_alpha(Char).

%% Return the smaller of two numbers.

min(X, X) -> X;
min(X, Y) when X > Y -> Y;
min(X, Y) when Y > X -> X.

%% Return the larger of two numbers.

max(X, X) -> X;
max(X, Y) when X > Y -> X;
max(X, Y) when Y > X -> Y.

%% Try to convert a string to an integer.
%% Not using tconv:to_i/1 because it will implicitly try b26-string -> integer
%% conversion too, which isn't what I want here.

to_i(Str) ->
    case muin_util:attempt(fun() -> list_to_integer(Str) end) of
        {ok, Num}  -> Num;
        {error, X} -> {error, X}
    end.

%%% Tests:

-ifdef(debug).
-include_lib("eunit/include/eunit.hrl").
-include("xfl_lexer_test.erl").
-endif.
