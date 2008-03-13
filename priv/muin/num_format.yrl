%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
Format
Col
NumFormat
NumTokens
DateFormat
NumTokPost
Fix
Escaped
.

Terminals

%% numbers
number

%% format stuff
colour

%% conditions
condition

%% operators
gt lt eq minus plus

%% format characters
space
zero
hash
question
dot
percent
comma
exponent
underscore
dollar
forwardslash
open_bra
close_ket
colon
esc
at
year
mon_min
mon
day
hour
sec
ampm

%% Other tokens
char

.

Rootsymbol Final.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators
Unary 50 esc.
Unary 100 minus plus.
Unary 150 gt lt eq.


%% ----- Grammar definition.

%% Set up the escaped character formats
Escaped -> esc space        : esc('$2').
Escaped -> esc zero         : esc('$2').
Escaped -> esc hash         : esc('$2').
Escaped -> esc question     : esc('$2').
Escaped -> esc dot          : esc('$2').
Escaped -> esc percent      : esc('$2').
Escaped -> esc comma        : esc('$2').
Escaped -> esc exponent     : esc('$2').
Escaped -> esc underscore   : esc('$2').
Escaped -> esc dollar       : esc('$2').
Escaped -> esc forwardslash : esc('$2').
Escaped -> esc open_bra     : esc('$2').
Escaped -> esc close_ket    : esc('$2').
Escaped -> esc colon        : esc('$2').
Escaped -> esc esc          : esc('$2').
Escaped -> esc at           : esc('$2').
Escaped -> esc year         : esc('$2').
Escaped -> esc mon_min      : esc('$2').
Escaped -> esc mon          : esc('$2').
Escaped -> esc day          : esc('$2').
Escaped -> esc hour         : esc('$2').
Escaped -> esc sec          : esc('$2').
Escaped -> esc ampm         : esc('$2').
Escaped -> esc char         : esc('$2').

%% you can skip either or both of Condition or Col with a format
Final -> condition Format     : concat2('$1','$2').
Final -> Format               : '$1'.

Format -> NumFormat  : '$1'.
Format -> DateFormat : '$1'.
Format -> Col Format : concat2('$1','$2').

Fix -> Escaped     : '$1'.
Fix -> char        : '$1'.
Fix -> Escaped Fix : concat('$1','$2').

%% Set up the numerical formats
NumTokens -> space    : '$1'.
NumTokens -> number   : '$1'.
NumTokens -> hash     : '$1'.
NumTokens -> dot      : '$1'.
NumTokens -> question : '$1'.

NumTokPost -> comma                 : '$1'.
NumTokPost -> NumTokPost NumTokPost : concat('$1','$2').

NumTokens -> NumTokens NumTokens  : concat('$1','$2').
NumTokens -> NumTokens NumTokPost : concat('$1','$2').
NumTokens -> NumTokens Fix        : concat('$1','$2').
NumTokens -> Fix NumTokens        : concat('$1','$2').

NumFormat -> NumTokens : make_format('$1').

%% Set up the date formats
DateFormat -> year    : '$1'.
DateFormat -> mon_min : '$1'.
DateFormat -> mon     : '$1'.
DateFormat -> day     : '$1'.
DateFormat -> hour    : '$1'.
DateFormat -> sec     : '$1'.
DateFormat -> ampm    : '$1'.

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

concat({_,A},{_,B}) -> [A,B];
concat(A,B)        -> [A,B].

concat2(A,B) -> [A,B].

esc(A) -> {char, A}.

make_format(A) -> 
        B=lists:flatten(A),
        {ok,ok}=check_nos(B),
        B.

%% If any of the numbers turns out not be zero wig out
check_nos([]) -> {ok,ok};
check_nos([{number,No}|T])->
        I=list_to_integer(No),
        case I of
                0 -> true;
                _ -> exit("bad format")
            end,
        check_nos(T);
check_nos([_H|T]) -> check_nos(T).
