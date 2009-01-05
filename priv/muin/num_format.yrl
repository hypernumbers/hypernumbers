%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
FinalFormat
ColFormat
Col
Format
FullFormat
Tokens
Token
Escaped
Escaped1
Escaped2
Cond
Semicolon
.

Terminals

%% general format
general

%% format stuff
colour

%% conditions
condition

%% format characters
dollar
minus
plus
fwdslash
open_bra
close_ket
quote
colon
space
underscore
percent
esc
at
year
mon_min
min
mon
day
hour
sec
ampm
string
format
fraction
semicolon

%% Other tokens
char

.

Rootsymbol Final.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators
Unary 200 Escaped1.
Unary 100 Escaped2.

%% ----- Grammar definition.

Final -> FinalFormat    : make_src('$1').
Final -> general        : get_general().
Final -> general Tokens : get_general(). %% Bad boy - dropping terminal tokens for a general format...

Semicolon -> semicolon           : '$1'.
Semicolon -> Semicolon semicolon : concat('$1','$2').

FinalFormat -> ColFormat Semicolon FinalFormat : concat(verify('$1'),'$2','$3').

FinalFormat -> ColFormat Semicolon  : concat(verify('$1'),'$2').
FinalFormat -> ColFormat            : verify('$1').

%% Set up the escaped character formats
Escaped -> Escaped1        : '$1'.
Escaped -> Escaped2        : '$1'.
Escaped1 -> esc Token      : esc('$2').
Escaped2 -> esc esc        : esc('$2').

%% you can skip either or both of Condition or Col with a format
ColFormat -> Col FullFormat : concat('$1','$2').
ColFormat -> FullFormat     : '$1'.

Format -> Tokens          : '$1'.
Format -> fraction        : '$1'.

FullFormat -> Format          : '$1'.
FullFormat -> Cond FullFormat : concat('$1','$2').

Cond -> condition : make_cond('$1').

%% Set up the numerical formats
Token -> format     : '$1'.
Token -> space      : '$1'.
Token -> fwdslash   : '$1'.
Token -> minus      : '$1'.
Token -> underscore : '$1'.
Token -> colon      : '$1'.
Token -> string     : strip('$1').
Token -> char       : '$1'.
Token -> dollar     : '$1'.
Token -> plus       : '$1'.
Token -> open_bra   : '$1'.
Token -> close_ket  : '$1'.
Token -> quote      : '$1'.
Token -> percent    : '$1'.

%% And Date Formats
Token -> year       : '$1'.
Token -> mon_min    : '$1'.
Token -> mon        : '$1'.
Token -> min        : '$1'.
Token -> day        : '$1'.
Token -> hour       : '$1'.
Token -> sec        : '$1'.
Token -> ampm       : '$1'.

%% And text token
Token -> at         : '$1'.

Tokens -> Tokens Token   : concat('$1','$2').
Tokens -> Token          : '$1'. 
Tokens -> Tokens Escaped : concat('$1','$2').
Tokens -> Escaped        : '$1'.

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

-import(num_format,
    [make_src/1,
     get_general/0,
     concat/2,
     concat/3,
     verify/1,
     esc/1,
     make_cond/1,
     strip/1]).