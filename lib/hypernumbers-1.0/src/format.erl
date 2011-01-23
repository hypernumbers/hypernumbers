%%% @doc    This module handles the number/text formatting for a cell.
%%% 
%%%         It does 2 things:
%%%         <ul>
%%%         <li>takes an Excel compatible format description and compiles 
%%%         it to Erlang source code capable of being eval'ed</li>
%%%         <li>executes the formatting source code against a value</li>
%%%         </ul>
%%% 
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

%% Standard Interfaces
-export([
         get_src/1,
         run_format/2
        ]).

%% -import(format_util,
%%         [clock_12/1,
%%          pad_year/1,
%%          pad_calendar/1,
%%          get_short_day/1,
%%          get_day/1,
%%          get_short_month/1,
%%          get_month/1,
%%          get_last_two/1
%%         ]).

%% Run-time Interface
%% The module format is called from within generated code only
%% It is *NOT* an interface that should be programmed against
-export([
         get_general/1,
         format/2,
         conditional/3
        ]).

-include("ascii.hrl").

%%%
%%% Public Interfaces
%%% 

%%% @doc takes a format and returns the compiled code that will format input to that format
%%% @end
%%% @spec get_src(Format::string()) -> {erlang,ErlangFun::string()} | {error, InvMsg::atom()}
get_src(Format)->
    try get_src2(Format)
    catch
        error:_Err -> {error,error_in_format};
        exit:_Exit -> {error,error_in_format}
    end.

%%% @doc takes a value and the src code for a format and returns the
%%% value formatted by the source code
%%% @end
%%% @spec run_format(Value::string(),Src::string()) -> {ok, Output::string()}
run_format(X, Src)->
    {ok,ErlTokens,_} = erl_scan:string(Src),
    {ok,ErlAbsForm}  = erl_parse:parse_exprs(ErlTokens),
    {value,Fun,_}    = erl_eval:exprs(ErlAbsForm,[]),
    {ok,Fun(X)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                           %%%
%%% Run-time interfaces - not to be programmed against!                       %%%
%%%                                                                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
%%% @doc Run-time interface not an API - this function should *NOT*
%%% be programmed against
%%% @end
format(X = {datetime, _, _},{date,Format}) -> {_, Date, Time} = X,
                                              Y = {Date, Time},
                                              format_date(Y,Format);
format(X,{number,Format})                  -> format_num(X,Format);
format(X,{text,Format})                    -> format(X,Format,[]).

%%% @hidden
%%% @doc run-time interface not an API - this function should *NOT*
%%% be programmed against
%%% @end
conditional({datetime, _D, _T}, ">", _B) -> true;
conditional({datetime, _D, _T}, _C, _B)  -> false;
conditional(A, ">", B)                   -> (A > B);
conditional(A, ">=", B)                  -> (A >= B);
conditional(A, "==", B)                  -> (A == B);
conditional(A, "=<", B)                  -> (A =< B);
conditional(A, "<", B)                   -> (A < B).

%%% @hidden
%%% @doc Callback from parser - dont know at compile time which general format
%%% to give the number X - is it a float? or an integer?
%%% @end
get_general(X) when is_integer(X) -> {format,"0"};
get_general(X) when is_float(X)   -> {format, "0.00"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                         %%%
%%% All these functions are called from format and are internal             %%%
%%%                                                                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_src2(Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    {ok,Output}=num_format_parser:parse(Tokens),
    {erlang,Output}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format numbers                                                            %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_num(X,Format)->
    {NewFormat,InsertList}=make_num_format(Format),
    X2=case is_percentage(Format) of
           true  -> X * 100;
           false -> X
       end,
    % we need to know the length of the format incluse of the decimal place 
    % (if any) but exclusive of any commas. The reason for this is you
    % might have a number like '1234' and a format like 'x0.00'
    % This will give a format of '0.00' and an insert point of '{4,x}'
    % the number 1234 busts out of the measure '0.00' to the left and 
    % the insert point needs to realise that the x should be placed at
    % the beginning to give 'x1234.00' and not at the insert point of 4
    % because '123x4.00' is not right
    Number=lists:flatten(print_num(X2,NewFormat)),
    NewFmtLen=string:len(strip_commas(NewFormat)),
    NumLen=string:len(Number),
    NewInsertList=if
                      (NumLen > NewFmtLen) -> fix_insert_list(InsertList,NumLen,NewFmtLen);
                      true                 -> InsertList
                  end,
    % insert needs to know if there are commas in the output
    StrippedNumber=strip_commas(Number),
    case StrippedNumber of
        Number -> insert(Number,NewInsertList,no_commas);
        _Other -> insert(Number,NewInsertList,commas)
    end.

fix_insert_list(InsertList,NumLen,FmtLen) -> fix_insert_list(InsertList,NumLen,FmtLen,[]).

fix_insert_list([],_NumLen,_FmtLen,Acc)         -> lists:reverse(Acc);
fix_insert_list([{N,Bits}|T],NumLen,FmtLen,Acc) ->
    NewAcc = if
                 N >= FmtLen -> {NumLen+1,Bits};
                 true        -> {N,Bits}
             end,
    fix_insert_list(T,NumLen,FmtLen,[NewAcc|Acc]).

% this function scans a format and returns true if there is
% a percentage sign *anywhere* in the format
is_percentage([])                 -> false;
is_percentage([{percent,"%"}|_T]) -> true;
is_percentage([_H|T])             -> is_percentage(T).

% this function takes a formatted number and interpolates any strings
% at the appropriate point if required
insert(Number,InsertList,Type) ->
    DecPos=get_dec_pos(Number),
    insert(Number,InsertList,Type,DecPos,0).

insert(Number,[],_Type,_DecPos,_Offset)           -> Number;
insert(Number,[{Int,Insert}|T],Type,DecPos,Offset) ->
    Len=length(Number),
    {NewInt,_NoOfCommas}=new_ins_pnt(Int,Offset,DecPos,Type),
    InsertStr=get_string(Insert),
    NewOffset=Offset+length(InsertStr),
    {LeftStr,RightStr}=if
                           % if the new insert point busts the measure append it
                           (Len-NewInt) =< 0  -> {"",Number};
                           % otherwise just split the string
                           true               -> {string:substr(Number,1,Len-NewInt),
                                                  string:substr(Number,Len-NewInt+1,Len)}
                       end,
    NewNumber=lists:concat([LeftStr,InsertStr,RightStr]),
    insert(NewNumber,T,Type,DecPos,NewOffset).

get_dec_pos(Number) ->
    case re:run(Number,"\\.", [{capture, first}]) of
        {match, [{Start,_Len}]} -> length(Number)-Start+1;
        nomatch                 -> 0
    end.

% The new insert point has to be shifted by a comma point if there are commas and the
% current insert point is past the decimal point only - otherwise it just gets
% the cumulative offset added to it (clear as mud)...
new_ins_pnt(Int,Off,DP,commas) when (Int > DP) ->
    CommaOffset=erlang:trunc((Int-DP)/3),
    % if there is to be a comma and a
    % text insert at the same point
    % the comma goes to the left
    % the case statement deals with it
    Correction=case (((Int-DP)/3)-CommaOffset) of
                   0.0 -> -1;
                   _   -> 0
               end,
    {Int+Off+CommaOffset+Correction,CommaOffset};
new_ins_pnt(Int,Off,_DP,_Type) -> {Int+Off,0}.

get_string({_Type,String}) -> String.

% this function takes the parsed format string and strips out the decoration (eg strings
% and other stuff that must be interpolated the numberical/date or text value and returns
% the actual format (which will be applied to the value) and an interpolations structure
% which will be stripped through it
%
% eg a format like "0\"a\".0" with the value "4"
%
% this will return the format "0.0" and the interpolation "\"a\"" with the insert point of
% 2 (I think) ie stick an a at the second point from the right...
% the value "4" would be formatted to "4.0" and then the string would be yoked in to
% give "4a.0"
%
% We do lists:reverse cos we bust out the measure to the left 'cos of Arabic numerals...
% ie all insert point indexes go right to left on numerals so by reversing we can
% count left to right
make_num_format(Format) -> make_num_format(lists:reverse(Format),[],[]).

make_num_format([],NewF,AccIL) ->
    {NewF,lists:reverse(AccIL)};
make_num_format([{format,Format}|Rest],NewF,AccIL) ->
    make_num_format(Rest,lists:concat([Format,NewF]),AccIL);
make_num_format([Other|Rest],NewF,AccIL) ->
    % It's important that the length is the length of the stripped comma
    % format here... (trust me!)
    make_num_format(Rest,NewF,[{length(strip_commas(NewF)),Other}|AccIL]).

print_num(_Output,[])    -> [];
print_num(Output,Format) ->
    % First up sort out the format
    Split=string:tokens(Format,"."),
    {IntFormat,DecFormat} = case Split of
                                [A1,B1] -> {A1,B1};
                                [A1]    -> {A1,null}
                            end,
    % Now prepare the output
    % Now round the output before formatting it
    Output2=round(Output,get_len(DecFormat)),
    Output3=make_list(Output2),
    Split2=string:tokens(Output3,"."),
    {Integers,Decimals} = case Split2 of
                              [A2,B2] -> {A2,B2};
                              [A2]    -> {A2,null}
                          end,
    get_num_output(Integers,IntFormat,Decimals,DecFormat).

% this function gets the actual formatted number PRIOR to interpolation
% of any additional strings stuff or other components...
get_num_output(Integers,IntFormat,Decimals,DecFormat)->
    {ok,Has_Exp}=has(exponent,DecFormat),
    case Has_Exp of
        false -> A=get_num_output1(Integers,IntFormat),
                 B=get_num_output2(Decimals,DecFormat),
                 A++B;
        true  -> get_num_exp(Integers,IntFormat,Decimals,DecFormat)
    end.

get_num_output1(Integers,IntFormat)->
    {ok,Has_Commas}=has(commas,IntFormat),
    IntFormat2=case Has_Commas of
                   true  -> strip_commas(IntFormat);
                   false -> IntFormat
               end,
    Output=format_int(Integers,IntFormat2),
    Return=case Has_Commas of
               true  -> add_commas(Output);
               false -> Output
           end,
    Return.

%% if the decimal format is null display no decimals
get_num_output2(_Decimals,null) -> "";
%% if there are no decimals make it a zero
get_num_output2(null,DecFormat) ->
    get_num_output2("0",DecFormat);
%% if there are both decimals and a decimal output then format them
get_num_output2(Decimals,DecFormat)->
    DecLen=string:len(Decimals),
    FormatLen=string:len(DecFormat),
    Return=case (DecLen >= FormatLen) of
               true  -> trunc(Decimals,DecLen-FormatLen);
               false -> Decimals
           end,
    % Decimals are always truncated so nobust is used
    Return2=app_fmt(Return,DecFormat,nobust),
    bodge2(Return2,FormatLen).

%% the fmt can be applied in either busting or not busting the measure
app_fmt(Number,Format,Type) ->
    Number2=string:strip(Number,right,?ASC_ZERO),
    app_fmt(Number2,Format,Type,[]).

%% the first line lets the measure be bust
app_fmt([],[],bust,A)                         -> lists:reverse(A);
%% the second line truncates the return and doesn't bust the measure
app_fmt(_Number,[],nobust,A)                    -> lists:reverse(A);
app_fmt([N|T],[],bust,A)                        -> app_fmt(T,[],bust,[N|A]);
app_fmt([?ASC_ZERO|T1],[?ASC_ZERO|T2],Type,A)   -> app_fmt(T1,T2,Type,[?ASC_ZERO|A]);
app_fmt([?ASC_ZERO|T1],[?ASC_HASH|T2],nobust,A) -> app_fmt(T1,T2,nobust,A);
app_fmt([?ASC_ZERO|T1],[?ASC_HASH|T2],bust,A)   -> app_fmt(T1,T2,bust,[?ASC_ZERO|A]);
app_fmt([?ASC_ZERO|T1],[?ASC_Q|T2],Type,A)      -> app_fmt(T1,T2,Type,[?ASC_SPACE|A]);
app_fmt([N|T1],[?ASC_ZERO|T2],Type,A)           -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([N|T1],[?ASC_HASH|T2],Type,A)           -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([N|T1],[?ASC_Q|T2],Type,A)              -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([],[?ASC_ZERO|T2],Type,A)               -> app_fmt([],T2,Type,[?ASC_ZERO|A]);
app_fmt([],[?ASC_HASH|T2],Type,A)               -> app_fmt([],T2,Type,A);
app_fmt([],[?ASC_Q|T2],Type,A)                  -> app_fmt([],T2,Type,[?ASC_SPACE|A]).

%% just rounds off a number to the appropriate number of decimals
round(X,N) when is_list(X)          -> X2=tconv:to_i(X),
                                       round(X2,N);
round(X,0)                           -> round(X);
round(X,N) when N > 0, is_integer(N) ->
    X2=tconv:to_l(X),
    case (no_of_decimals(X2) > N) of
        true  -> round(X*math:pow(10,N))/math:pow(10,N);
        false -> X
    end.

%% splits a number (expressed as a string) and returns the number
%% of decimal places
no_of_decimals(Number) when is_list(Number) ->
    case string:tokens(Number,".") of
        [_A,B] -> length(B);
        [_A]   -> 0
    end.

%% get the number as an exponent
get_num_exp(Integers,IntFormat,Decimals,DecFormat) ->
    [DecFormat2,Exp]=string:tokens(string:to_upper(DecFormat),"E"),
    Offset=format_exp(Integers,Decimals,list_to_integer(Exp)),
    % if the number is bigger than 10 you offset it by a negative
    % number and 'make it up' in the exponent
    % if the number is less than 1 you offset it by a positive number
    % and reduce it in the exponent
    % which is why we use the negative of the offset in the next line
    NewNum=make_number(Integers,Decimals)*math:pow(10,-Offset),
    Format=IntFormat++"."++DecFormat2,
    Return=print_num(NewNum,Format),
    if
        Offset < 0  -> Return++"e"++integer_to_list(Offset);
        Offset >= 0 -> Return++"e+"++integer_to_list(Offset)
    end.

%% if the exponent is zero then calculate an offset that makes the number
%% 123.456 appear like 1.23456
format_exp("0",Decimals,0)            -> get_neg_offset(Decimals);
format_exp([?ASC_MINUS|Integer],_,0)  -> length(Integer)-1;
format_exp(Integer,_,0)               -> length(Integer)-1;
%% for other numbers just return the offset
format_exp(_Integer,_Decimals,Offset) -> Offset.

%% the negative offset is got by counting leading zeros
get_neg_offset(Decimals) -> get_neg_offset(Decimals,1).

get_neg_offset([?ASC_ZERO|T],N) -> get_neg_offset(T,N+1);
get_neg_offset(_,N)             -> -N.

make_number(Integers,null)     -> list_to_integer(Integers);
make_number(Integers,Decimals) -> list_to_float(Integers++"."++Decimals).

%% truncates a decimal value to a fixed length
trunc(Decimals,Len) ->
    Divisor=math:pow(10,Len),
    RegExp="(e\\+000)",
    Decimals2 = re:replace(Decimals, RegExp, "", [{return, list}, global]),
    Trunc=round(list_to_integer(Decimals2)/Divisor),
    Trunc2=integer_to_list(Trunc),
    OldLen = string:len(Decimals)-Len,
    NewLen = string:len(Trunc2),
    % if the truncated string is now too small pad it with zero's to the left
    Trunc3 = case (OldLen > NewLen) of
                 true -> get_pad(OldLen - NewLen, ?ASC_ZERO)++Trunc2;
                 false -> Trunc2
               end,
    % we now strip off any trailing zeros
    % because if the format is something like "00.0?" that trailing zero is
    % to be replaced by a space...
    string:strip(Trunc3,right,?ASC_ZERO).

%% formats integers that are passed in as strings
format_int([?ASC_MINUS|Integers],Format)->
    bodge([?ASC_MINUS]++format_int(Integers,Format));
format_int(Integers,Format)->
    % we  have to feed in the integer and the format reversed...
    % then obviously re-reverse the answer...
    % this side always busts the measure so pass in bust
    Padded=lists:reverse(app_fmt(lists:reverse(Integers),lists:reverse(Format),bust)),
    bodge(Padded).

%% bodge for when a negative zero is returned...
bodge("-0")   -> "0";
bodge(Number) -> Number.

%% bodge for when a zero is returned that must be padded
bodge2("0",FormatLen)                      -> "."++string:chars(?ASC_ZERO,FormatLen);
bodge2(Num,_FormatLen) when is_list(Num) -> "."++Num.

%% takes a number represented as a string and adds in commas as thousand
%% delimiters
add_commas(A)->
    B=lists:reverse(A),
    add_commas(B,3,[]).

add_commas([],_,[?ASC_COMMA|Return])             -> Return;
add_commas([],_,[?ASC_MINUS,?ASC_COMMA|Return]) -> [?ASC_MINUS|Return];
add_commas([],_,Acc)                               -> Acc;
add_commas([H|T],1,Acc)                            -> add_commas(T,3,[?ASC_COMMA,H|Acc]);
add_commas([H|T],N,Acc)                            -> add_commas(T,N-1,[H|Acc]).

get_pad(N,_)    when N =< 0 -> "";
get_pad(N,Char) when N  > 0 -> get_pad(N,Char,[]).

get_pad(0,_Char,Acc) -> Acc;
get_pad(N,Char,Acc)  -> get_pad(N-1,Char,[Char|Acc]).

has(_Type, null) -> {ok,false};
has(Type, Format) ->
    RegExp = case Type of
                 commas        -> ",";
                 % hashes        -> "#";  % not used at the mo
                 % questionmarks -> "\?";
                 % fractions     -> "/";
                 exponent      -> "[E|e]"
             end,
    Format2 = re:replace(Format, RegExp, "", [{return, list}, global]),
    case Format of
        Format2 -> {ok,false};
        _       -> {ok,true}
    end.

make_list(Number) when is_list(Number)    -> Number;
make_list(Number) when is_float(Number)   -> Float=float_to_list(Number),
                                             rejig(Float);
make_list(Number) when is_integer(Number) -> integer_to_list(Number).

rejig(Float)->
    [Number,[Sign|Exp]]=string:tokens(Float,"e"),
    [Sign2|Rest]=Number,
    % The number might start with a minus sign - so fix that...
    case Sign2 of
        ?ASC_MINUS -> [?ASC_MINUS]++shift(Rest,Sign,Exp);
        _          -> shift(Number,Sign,Exp)
    end.

shift(Number,?ASC_PLUS,Exp) ->
    Exp2=list_to_integer(Exp),
    [Int,Dec]=string:tokens(Number,"."),
    X=(Exp2-length(Dec)),
    Dec2=if X > 0 -> Int++Dec++get_pad(X,?ASC_ZERO)++".";
            X == 0 -> Int++".";
            true   -> Len=length(Dec),
                      Front=string:substr(Dec,1,Exp2),
                      Back=string:substr(Dec,1+Exp2,Len),
                      Front++"."++Back
         end,
    Int++Dec2;
shift(Number,?ASC_MINUS,Exp) ->
    Exp2=list_to_integer(Exp),
    [Int,Dec]=string:tokens(Number,"."),
    X=(Exp2-length(Int)),
    Int2=if X > 0  -> "0."++get_pad(X,?ASC_ZERO)++Int;
            X == 0 -> "0."++Int;
            true   -> Len=length(Dec),
                      Front=string:substr(Int,1,Exp2),
                      Back=string:substr(Int,1+Exp2,Len-Exp2),
                      Front++"."++Back
         end,
    Int2++Dec.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format dates                                                              %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_date(X, Format) ->
    Has_AMPM=has_AMPM(Format),
    format_date(X,Format,Has_AMPM,[]).

format_date(_X,[],_Has_AMPM,Acc)  -> lists:flatten(lists:reverse(Acc));
format_date(X,[H|T],Has_AMPM,Acc) -> format_date(X,T,Has_AMPM,[execute(X,H,Has_AMPM)|Acc]).

has_AMPM([])                -> false;
has_AMPM([{ampm,_F}|_Rest]) -> true;
has_AMPM([_H|T])            -> has_AMPM(T).

execute(X,{year,two_digit},_) ->
    {{Year,_M,_D},_T}=X,
    format_util:get_last_two(format_util:pad_year(integer_to_list(Year)));
execute(X,{year,four_digit},_) ->
    {{Year,_M,_D},_T}=X,
    format_util:pad_year(integer_to_list(Year));
execute(X,{mon,no_zero},_) ->
    {{_Y,Mon,_D},_T}=X,
    integer_to_list(Mon);
execute(X,{mon,zero},_) ->
    {{_Y,Mon,_D},_Time}=X,
    MM=integer_to_list(Mon),
    format_util:pad_calendar(MM);
execute(X,{mon,abbr},_) ->
    {{_Y,Month,_D},_T}=X,
    format_util:get_short_month(Month);
execute(X,{mon,full},_) ->
    {{_Y,Month,_D},_T}=X,
    format_util:get_month(Month);
execute(X,{min,elapsed},_) ->
    Secs=calendar:datetime_to_gregorian_seconds(X),
    integer_to_list(round(Secs/60));
execute(X,{day,no_zero},_) ->
    {{_Y,_M,Day},_T}=X,
    integer_to_list(Day);
execute(X,{day,zero},_) ->
    {{_Y,_M,Day},_T}=X,
    DD=integer_to_list(Day),
    format_util:pad_calendar(DD);
execute(X,{day,abbr},_) ->
    {Date,_T}=X,
    Day = calendar:day_of_the_week(Date),
    format_util:get_short_day(Day);
execute(X,{day,full},_) ->
    {Date,_T}=X,
    Day = calendar:day_of_the_week(Date),
    format_util:get_day(Day);
execute(X,{hour,no_zero},true) ->
    {_D,{Hour,_M,_S}}=X,
    integer_to_list(format_util:clock_12(Hour));
execute(X,{hour,no_zero},false) ->
    {_D,{Hour,_M,_S}}=X,
    integer_to_list(Hour);
execute(X,{hour,zero},true) ->
    {_D,{Hour,_M,_S}}=X,
    format_util:pad_calendar(integer_to_list(format_util:clock_12(Hour)));
execute(X,{hour,zero},false) ->
    {_D,{Hour,_M,_S}}=X,
    format_util:pad_calendar(integer_to_list(format_util:clock_12(Hour)));
execute(X,{hour,elapsed},_) ->
    Secs=calendar:datetime_to_gregorian_seconds(X),
    integer_to_list(round(Secs/3600));
execute(X,{min,no_zero},_) ->
    {_D,{_H,Min,_S}}=X,
    integer_to_list(Min);
execute(X,{min,zero},_) ->
    {_D,{_H,Min,_S}}=X,
    MM=integer_to_list(Min),
    format_util:pad_calendar(MM);
execute(X,{sec,no_zero},_) ->
    {_D,{_H,_M,Sec}}=X,
    integer_to_list(Sec);
execute(X,{sec,zero},_) ->
    {_D,{_H,_M,Sec}}=X,
    format_util:pad_calendar(integer_to_list(Sec));
execute(X,{sec,elapsed},_) ->
    Secs=calendar:datetime_to_gregorian_seconds(X),
    integer_to_list(round(Secs));
execute(X,{ampm,full_caps},_) ->
    {_D,{Hour,_M,_S}}=X,
    case (Hour > 12) of
        true  -> "PM";
        false -> "AM"
    end;
execute(X,{ampm,full_lowercase},_) ->
    {_D,{Hour,_M,_S}}=X,
    case (Hour > 12) of
        true  -> "pm";
        false -> "am"
    end;
execute(X,{ampm,abbr_caps},_) ->
    {_D,{Hour,_M,_S}}=X,
    case (Hour > 12) of
        true  -> "P";
        false -> "A"
    end;
execute(X,{ampm,abbr_lowercase},_) ->
    {_D,{Hour,_M,_S}}=X,
    case (Hour > 12) of
        true  -> "p";
        false -> "a"
    end;
execute(_X,{_Other,String},_) ->
    String.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format other things (bit shite!)                                          %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format(_X,[],Acc)             -> lists:flatten(lists:reverse(Acc));
format(X,[{at,"@"}|T],Acc)    -> format(X,T,[X|Acc]);
format(X,[{_Type,Bit}|T],Acc) -> format(X,T,[Bit|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Utility functions                                                         %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gets lenghts where an object can be null
get_len(null)                    -> 0;
get_len(List) when is_list(List) -> length(List).

%% strips commas out of a string
strip_commas(A) ->
    re:replace(A, ",", "", [{return, list}, global]).
