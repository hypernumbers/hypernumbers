%%% -*- mode: erlang -*-
%%% @doc    Lexer for XFL.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%%% TODO: Double "" as a way to include a quote character in a string.
%%%       This works in Ruby: rx = /\"((\"\")|(.)|(\n))*\"/
%%% TODO: Good unit test coverage.

Definitions.

%%% Constants: integers, floats (in decmial and scientific notations),
%%% booleans, strings, error constants.

INT = ([0-9]+)
FLOATDEC = (([0-9]+)?\.[0-9]+)
FLOATSCI = (([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

TRUE = ((T|t)(R|r)(U|u)(E|e))
FALSE = ((F|f)(A|a)(L|l)(S|s)(E|e))
BOOL = ({TRUE}|{FALSE})

STR = (\"[^"]*\")
%%" % erlang-mode fix

ERRVAL = \#NULL\!|\#AUTH\!|\#DIV\/0\!|\#VALUE\!|\#REF\!|\#NAME\?|\#NUM\!|\#N\/A

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
NONBREAKINGSPACE = ((\x{c2})(\x{a0}))

Rules.

%% Basic data types.
{FLOATDEC} : {token, {float, TokenLine, make_float(TokenChars)}}.
{FLOATSCI} : {token, {float, TokenLine, make_float(TokenChars)}}.
{INT}      : {token, {int, TokenLine, tconv:to_i(TokenChars)}}.
{BOOL}     : {token, {bool, TokenLine, string:to_upper(TokenChars) == "TRUE"}}.
{STR}      : {token, {str, TokenLine, hslists:mid(TokenChars)}}.
{ERRVAL}   : {token, {errval, TokenLine, list_to_atom(TokenChars)}}.

%%% 1. Cell references:

{A1_REF} : {token, to_cellref(TokenChars, TokenLine, a1)}.
{RC_REF} : {token, to_cellref(TokenChars, TokenLine, rc)}.

%%% 2. Finite ranges:g

{FINITE_RANGE_A1} : {token, finite_range(TokenChars, TokenLine, a1)}.
{FINITE_RANGE_RC} : {token, finite_range(TokenChars, TokenLine, rc)}.

%%% 3. Row & column ranges:

{A1_COL_RANGE} : {token, a1_col_range(TokenChars, TokenLine)}.
{A1_ROW_RANGE} : {token, a1_row_range(TokenChars, TokenLine)}.
{RC_COL_RANGE} : {token, rc_col_range(TokenChars, TokenLine)}.
{RC_ROW_RANGE} : {token, rc_row_range(TokenChars, TokenLine)}.

%%% 4. Named expressions:

{NAME_REF}     : {token, name(TokenChars, TokenLine)}.

%% Discard whitespace:
{WHITESPACE}       : skip_token.
{NONBREAKINGSPACE} : skip_token.

%% Punctuation & operators:

=  : {token, {'=', TokenLine}}.
,  : {token, {',', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\; : {token, {';', TokenLine}}.

%% Operators.
%% Arithmetic.
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.
\* : {token, {'*', TokenLine}}.
\/ : {token, {'/', TokenLine}}.
\^ : {token, {'^', TokenLine}}.

%% Logical.
>  : {token, {'>', TokenLine}}.
<  : {token, {'<', TokenLine}}.
>= : {token, {'>=', TokenLine}}.
<= : {token, {'<=', TokenLine}}.
<> : {token, {'<>', TokenLine}}.

%% Other.
&    : {token, {'&', TokenLine}}.
\%   : {token, {'%', TokenLine}}.
\:   : {token, {':', TokenLine}}.
\^\^ : {token, {'^^', TokenLine}}.

\n : skip_token.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenLine, TokenChars}}.

Erlang code.

%%% TODO: Normalize range references so that it's always left to right.

-export([lex/2, debang/1]).
-include("handy_macros.hrl").
-include("muin_records.hrl").
-include("typechecks.hrl").

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
        {ok, Toks, _} ->
            case no_strings_above_limit(Toks) of
                true ->  {ok, Toks};
                false -> lexer_error
            end;
        _             ->
            lexer_error
    end.


%% @doc Replace all !s with /s in a string. (Used to normalize references.)
debang(Ssref) ->
    re:replace(Ssref, "!", "/", [global,{return,list}]).


%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%% @doc Check there are no string constants that are too large.
no_strings_above_limit(Toks) ->
    lists:all(fun({str, _, Str}) ->
                case string:len(Str) of
                    Ok when Ok < ?STRING_SIZE_LIMIT -> true;
                    _Else                           -> false
                end;
           (_) -> true
        end,
        Toks).


name(TokenChars, TokenLine) when hd(TokenChars) == $.; hd(TokenChars) == $/ ->
    {Path, Name} = split_ssref(TokenChars),
    {namedexpr, TokenLine, #namedexpr{path = Path, text = Name}};
name(TokenChars, TokenLine) ->
    {name, TokenLine, TokenChars}.

to_cellref(TokenChars, TokenLine, Type) ->
    {Path, Cell} = split_ssref(TokenChars),
    {ColCoord, RowCoord} = extract_coords(Cell, Type, ?pivot),
    {cellref, TokenLine, #cellref{col = ColCoord, row = RowCoord, 
                                  path = Path, text = TokenChars}}.

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
    Ref2 = re:replace(Ref, "\\$", "", [global,{return,list}]), %"
    ColStr = lists:takewhile(fun is_alpha/1, string:to_lower(Ref2)),
    Col = tconv:to_i(ColStr),
    Row = tconv:to_i(lists:nthtail(length(ColStr), Ref2)),
    IsColFixed = (hd(Ref) == $$),
    IsRowFixed = lists: member($$, tl(Ref)),
    NewCol = case IsColFixed of
          true  -> Col;
          false -> {offset, Col - PivotCol}
     end,
     NewRow = case IsRowFixed of
          true  -> Row;
          false -> {offset, Row - PivotRow}
     end,
     {NewCol, NewRow}.

%% @doc Construct a range object from matched token text.

finite_range(TokenChars, TokenLine, Kind) ->
    {Path, LhsArg, RhsArg} = split_range(TokenChars),
    {Tl, Br} = find_proper_bounds(LhsArg, RhsArg, Kind),
    {W, H} = finite_range_dimensions(Tl, Br),
    {rangeref, TokenLine, #rangeref{type = finite, path = Path, tl = Tl, br = Br, 
                                    width = W, height = H, 
                                    text = TokenChars}}.

%% args are normalized/offset.

finite_range_dimensions({TlCol, TlRow}, {BrCol, BrRow}) ->
    TlColIndex = col_index(TlCol),
    TlRowIndex = row_index(TlRow),
    BrColIndex = col_index(BrCol),
    BrRowIndex = row_index(BrRow),
    {BrColIndex - TlColIndex + 1, BrRowIndex - TlRowIndex + 1}.

row_index(N) when is_integer(N) -> N;
row_index({offset, N}) -> ?my + N.

col_index(N) when is_integer(N) -> N;
col_index({offset, N}) -> ?mx + N.

    
%% ColId is something like "A" or "$XYZ"
a1_col_to_coord(ColId) ->
    case string:substr(ColId, 1, 1) of
        "$" -> tconv:to_i(string:substr(ColId, 2)); %"
        _   -> {offset, tconv:to_i(ColId) - ?my}
    end.

%% RowId is something like "1" or "$123"
a1_row_to_coord(RowId) ->
    case string:substr(RowId, 1, 1) of 
        "$" -> tconv:to_i(string:substr(RowId, 2)); %"
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

a1_col_range(TokenChars, TokenLine) ->
    {Path, LhsCol, RhsCol} = split_range(TokenChars),
    TlCoord = a1_col_to_coord(LhsCol),
    BrCoord = a1_col_to_coord(RhsCol),
    W = col_index(BrCoord) - col_index(TlCoord) + 1,
    {rangeref, TokenLine, #rangeref{type = col,
                                    path = Path,
                                    tl = {col, TlCoord},
                                    br = {col, BrCoord},
                                    width = W,
                                    height = na,
                                    text = TokenChars}}.

a1_row_range(TokenChars, TokenLine) ->
    {Path, LhsRow, RhsRow} = split_range(TokenChars),
    TlCoord = a1_row_to_coord(LhsRow),
    BrCoord = a1_row_to_coord(RhsRow),
    H = row_index(BrCoord) - row_index(TlCoord) + 1,
    {rangeref, TokenLine, #rangeref{type = row,
                                    path = Path,
                                    tl = {row, TlCoord},
                                    br = {row, BrCoord},
                                    width = na,
                                    height = H,
                                    text = TokenChars}}.

rc_col_range(TokenChars, TokenLine) ->
    {Path, LhsCol, RhsCol} = split_range(TokenChars),
    TlCoord = rc_col_to_coord(LhsCol),
    BrCoord = rc_col_to_coord(RhsCol),
    W = col_index(BrCoord) - col_index(TlCoord) + 1,
    {rangeref, TokenLine, #rangeref{type = col,
                                    path = Path,
                                    tl = {col, rc_col_to_coord(LhsCol)},
                                    br = {col, rc_col_to_coord(RhsCol)},
                                    width = W,
                                    height = na,
                                    text = TokenChars}}.

rc_row_range(TokenChars, TokenLine) ->
    {Path, LhsRow, RhsRow} = split_range(TokenChars),
    {rangeref, TokenLine, #rangeref{type = row,
                                    path = Path,
                                    tl = {row, rc_row_to_coord(LhsRow)},
                                    br = {row, rc_row_to_coord(RhsRow)},
                                    text = TokenChars}}.

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
    (lists:member(Char, lists:seq($A, $Z)) orelse
     lists:member(Char, lists:seq($a, $z)));
is_alpha([Char]) ->
    is_alpha(Char).


%% Turn .1/0.1/.1e+10/0.1e+10 into a float.
%% We include the original string for reverse assembling the formula in
%% normalization step.  That then gets thrown away and only the float
%% makes it into the final AST.

make_float(TokenChars) when hd(TokenChars) == $. ->
    FloatAsStr = [$0 | TokenChars],
    {tconv:to_f(FloatAsStr), FloatAsStr};
make_float(TokenChars) ->
    FloatAsStr = case lists:member($., TokenChars) of
                     true -> TokenChars;
                     false -> add_decimal(TokenChars)
                 end,
    {tconv:to_f(FloatAsStr), FloatAsStr}.

add_decimal([]) -> ".0";
add_decimal([E | Tail]) when E == $e; E == $E -> ".0e" ++ Tail;
add_decimal([X | Tail]) -> [X | add_decimal(Tail)]. 

%%% Tests:

-ifdef(debug).
-include_lib("eunit/include/eunit.hrl").
-include("xfl_lexer_test.erl").
-endif.
