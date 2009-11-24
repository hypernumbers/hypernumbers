%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

Final
FinalFormat
ColFormat
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
markdown

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

Final -> FinalFormat     : num_format:make_src('$1').
Final -> general         : num_format:get_general().
Final -> general Tokens  : num_format:get_general(). %% Bad boy - dropping terminal tokens for a general format...
Final -> markdown        : num_format:get_markdown().
Final -> markdown Tokens : num_format:get_markdown(). %% Bad boy - dropping terminal tokens for a markdown format...

Semicolon -> semicolon : [lit('$1')].
Semicolon -> semicolon Semicolon : [lit('$1') | '$2'].

FinalFormat -> ColFormat Semicolon FinalFormat : [num_format:verify('$1'),'$2','$3'].

FinalFormat -> ColFormat Semicolon  : [num_format:verify('$1'),'$2'].
FinalFormat -> ColFormat            : num_format:verify('$1').

%% Set up the escaped character formats
Escaped -> Escaped1        : '$1'.
Escaped -> Escaped2        : '$1'.
Escaped1 -> esc Token      : num_format:esc('$2').
Escaped2 -> esc esc        : num_format:esc(lit('$2')).

%% you can skip either or both of Condition or Col with a format
ColFormat -> colour FullFormat : [lit('$1'), '$2'].
ColFormat -> FullFormat     : '$1'.

Format -> Tokens          : '$1'.
Format -> fraction        : lit('$1').

FullFormat -> Format          : '$1'.
FullFormat -> Cond FullFormat : ['$1', '$2'].

Cond -> condition : num_format:make_cond('$1').

%% Set up the numerical formats
Token -> format     : lit('$1').
Token -> space      : lit('$1').
Token -> fwdslash   : lit('$1').
Token -> minus      : lit('$1').
Token -> underscore : lit('$1').
Token -> colon      : lit('$1').
Token -> string     : num_format:strip(lit('$1')).
Token -> char       : lit('$1').
Token -> dollar     : lit('$1').
Token -> plus       : lit('$1').
Token -> open_bra   : lit('$1').
Token -> close_ket  : lit('$1').
Token -> quote      : lit('$1').
Token -> percent    : lit('$1').

%% And Date Formats
Token -> year       : lit('$1').
Token -> mon_min    : lit('$1').
Token -> mon        : lit('$1').
Token -> min        : lit('$1').
Token -> day        : lit('$1').
Token -> hour       : lit('$1').
Token -> sec        : lit('$1').
Token -> ampm       : lit('$1').

%% And text token
Token -> at         : lit('$1').


Tokens -> Escaped        : '$1'.
Tokens -> Tokens Token   : ['$1','$2'].
Tokens -> Token          : '$1'. 

Erlang code.


lit({Cat, _LineNumber, Sym}) ->
    {Cat, Sym}.
    
