%%% @doc num_format parser compiles number formats
%%% @author Gordon Guthrie <gordon@hypernumbers.com>

Nonterminals

FinalFormat
CondFormat
Col
Format
DateFormat
FullFormat
Tokens
Escaped
Cond
Bits
At
.

Terminals

%% format stuff
colour

%% conditions
condition


%% format characters
dollar
minus
plus
forwardslash
open_bra
close_ket
colon
space
underscore
percent
esc
at
year
mon_min
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

Rootsymbol FinalFormat.
Endsymbol  '$end'.

%% Associativity and precedence rules for operators
%%Unary 50 Bits.
Unary 100 Escaped.

%% ----- Grammar definition.

FinalFormat -> CondFormat semicolon FinalFormat : concat('$1','$2','$3').
FinalFormat -> CondFormat                       : '$1'.

Bits -> Escaped : mark('$1').
Bits -> Tokens  : mark('$1').

%% Set up the escaped character formats
Escaped -> esc Tokens  : esc('$2').
Escaped -> esc esc     : esc('$2').

%% you can skip either or both of Condition or Col with a format
CondFormat -> Cond FullFormat : concat2('$1','$2').
CondFormat -> FullFormat      : '$1'.

Cond -> condition : make_cond('$1').

Format -> format       : '$1'.
Format -> fraction     : '$1'.
Format -> Bits         : '$1'.
Format -> Format Bits  : concat2('$1','$2').

FullFormat -> Format          : '$1'.
FullFormat -> Format percent  : concat2('$1','$2').
FullFormat -> Bits Format     : concat2('$1','$2').
FullFormat -> Col FullFormat  : concat2('$1','$2').
FullFormat -> DateFormat      : '$1'.
FullFormat -> Bits DateFormat : concat2('$1','$2').
FullFormat -> At              : '$1'.
FullFormat -> Bits At         : concat2('$1','$2').

%% Set up the numerical formats
Tokens -> space        : '$1'.
Tokens -> forwardslash : '$1'.
Tokens -> minus        : '$1'.
Tokens -> underscore   : '$1'.
Tokens -> colon        : '$1'.
Tokens -> string       : strip('$1').
Tokens -> char         : '$1'.
Tokens -> dollar       : '$1'.
Tokens -> plus         : '$1'.
Tokens -> open_bra     : '$1'.
Tokens -> close_ket    : '$1'.

%% Date Formats
DateFormat -> year         : '$1'.
DateFormat -> mon_min      : '$1'.
DateFormat -> mon          : '$1'.
DateFormat -> day          : '$1'.
DateFormat -> hour         : '$1'.
DateFormat -> sec          : '$1'.
DateFormat -> ampm         : '$1'.

DateFormat -> DateFormat DateFormat : concat2('$1','$2').
DateFormat -> DateFormat Bits       : concat2('$1','$2').

Tokens -> Tokens Tokens  : concat2('$1','$2').

At -> at      : '$1'.
At -> at Bits : mark2('$1','$2').

Col -> colour : '$1'.

%% ----- Helper functions.
%% TODO: Some of the code below could be cleaned up a bit.

Erlang code.

mark(A) -> {mark,A}.

mark2(A,B) -> {mark2,[A|B]}.

concat({_,A},{_,B}) -> [A,B];
concat(A,B)         -> [A,B].

concat2(A,B) -> [A,B].

concat(A,B,C) -> [A,B|C].

esc(A) -> {char, A}.

make_format(A) when is_list(A)-> 
        B=lists:flatten(A),
        {ok,ok}=check_nos(B),
        B;
make_format(A)->[A].

%% If any of the numbers turns out not be zero wig out
check_nos([]) -> {ok,ok};
check_nos([{number,No}|T])->
        I=make_no(No),
        case I of
                0   -> true;
                0.0 -> true;
                _   -> exit("bad format")
            end,
        check_nos(T);
check_nos([_H|T]) -> check_nos(T).

make_cond({condition,String})->
        Str2=string:strip(String,left,$[),
        Str3=string:strip(Str2,right,$]),
        {condition,Str3}.

make_no(No)->
    try
	list_to_integer(No)
    catch
	exit:  _Reason  -> list_to_float(No);
        error: _Message -> list_to_float(No);
	throw: _Term    -> list_to_float(No)
    end.

get_at(A) -> A.

strip({string,A}) -> A1 = string:strip(A,both,$"),%" syntax highlighting fix
        {string2,A1}. 