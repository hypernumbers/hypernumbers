%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

%% Standard Interfaces
-export([
          get_src/1,
          run_format/2
          ]).

%% Run-time Interface
%% The module format is called from within generated code only
%% It is *NOT* an interface that should be programmed against
-export([
          format/2
        ]).

-include("ascii.hrl").

%%% @doc takes a format and returns the compiled code that will format input to that format
%%% @spec format(Format) -> {erlang,ErlangFun} | {error, InvMsg}
%%% @type Format = string
%%% @type InvMsg = string
get_src(Format)->
  try get_src2(Format)
  catch
    exit:_ ->
      {error,error_in_format}
  end.

get_src2(Format)->
    io:format("in format:compile_format Format is ~p~n",[Format]),
    {ok,Tokens,_}=num_format_lexer:string(Format),
    io:format("in format:compile_format Tokens are ~p~n",[Tokens]),
    {ok,Output}=num_format_parser:parse(Tokens),
    io:format("in format:compile_format Output is ~p~n",[Output]),
    {erlang,Output}.

%%% @doc takes a value and the src code for a format and returns the
%%% value formatted by the source code
%%% @spec format(Value,Src) -> string
%%% @type Format = string
%%% @type Src = string
run_format(X,Src)->
  io:format("in format:run_format Src is ~p~n",[Src]),
  {ok,ErlTokens,_}=erl_scan:string(Src),
  {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),
  {value,Fun,_}=erl_eval:exprs(ErlAbsForm,[]),
  Fun(X).

%%% @doc Run-time interface not an API - this function should *NOT* be programmed against
format(X,{date,Format})   -> format_date(calendar:gregorian_seconds_to_datetime(X),Format);
format(X,{number,Format}) -> format_num(X,Format);
format(X,{text,Format})   -> format(X,Format,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% All these functions are called from format and are internal               %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Format a number
%%
format_num(X,Format)->
  io:format("in format:format_num Format is ~p~n",[Format]),
  {NewFormat,InsertList}=make_format(Format),
  X2=case is_percentage(Format) of
    yes -> X * 100;
    no  -> X
  end,
  io:format("in format:format_num X2 is ~p NewFormat is ~p InsertList is ~p~n",
    [X2,NewFormat,InsertList]),
  Number=lists:flatten(print(X2,NewFormat)),
  io:format("in format:format_num Number is ~p~n",[Number]),
  %% insert needs to know if there are commas in the output
  StrippedNumber=strip_commas(Number),
  io:format("in format:format_num StrippedNumber is ~p~n",[StrippedNumber]),
  NewNumber = case StrippedNumber of
    Number -> insert(Number,InsertList,no_commas);
    Other  -> insert(Number,InsertList,commas)
  end,
  io:format("in format:format_num NewNumber is ~p~n",[NewNumber]),
  NewNumber.

is_percentage([])                 -> no;
is_percentage([{percent,"%"}|_T]) -> yes;
is_percentage([H|T])              -> is_percentage(T).

insert(Number,InsertList,Type) -> insert(Number,InsertList,Type,0).

insert(Number,[],_Type,_Offset) -> Number;
insert(Number,[{Int,Insert}|T],Type,Offset) ->
  io:format("in format:insert (1) Number is ~p Int is ~p Insert is ~p T is ~p Type is ~p "++
      "Offset is ~p~n",[Number,Int,Insert,T,Type,Offset]),
  Len=length(Number),
  DecPos=get_dec_pos(Number),
  NewInt=new_ins_pnt(Int,Offset,DecPos,Type),
  io:format("in format:insert (1) Len is ~p DecPos is ~p NewInt is ~p~n",[Len,DecPos,NewInt]),
  InsertString=get_string(Insert),
  NewOffset=Offset+length(InsertString),
  io:format("in format:insert (1) InsertString is ~p NewOffset is ~p~n",[InsertString,NewOffset]),
  LeftString=string:substr(Number,1,Len-NewInt),
  RightString=string:substr(Number,Len-NewInt+1,Len),
  NewNumber=lists:concat([LeftString,InsertString,RightString]),
  io:format("in format:insert (1) RightString is ~p LeftString is ~p NewNumber is ~p~n",
    [RightString,LeftString,NewNumber]),
  insert(NewNumber,T,Type,NewOffset).

get_dec_pos(Number)->
  case regexp:match(Number,"\\.") of
    {match,Start,_Len} -> length(Number)-Start+1;
    nomatch             -> 0
  end.

%% The new insert point has to be shifted by a comma point if there are commas and the
%% current insert point is past the decimal point only - otherwise it just gets the cumulative
%% offset added to it (clear as mud)...
new_ins_pnt(Int,Off,DP,commas) when (Int > DP) -> Int+Off+erlang:trunc((Int-DP)/3);
new_ins_pnt(Int,Off,DP,_Type)                   -> Int+Off.

get_string({Type,String}) ->
  io:format("in format:get_string Type is ~p String is ~p~n",[Type,String]),
  String.

%% We do lists:reverse cos we bust out the measure to the left 'cos of Arabic numerals...
make_format(Format) -> make_format(lists:reverse(Format),[],[]).

make_format([],NewF,AccIL) ->
  io:format("in format:make_format (1) NewF is ~p AccIL is ~p~n",[NewF,AccIL]),
  {NewF,lists:reverse(AccIL)};
make_format([{format,Format}|Rest],NewF,AccIL) -> 
  io:format("in format:make_format (2) Format is ~p Rest is ~p NewF is ~p AccIL is ~p~n",
    [Format,Rest,NewF,AccIL]),  
  make_format(Rest,lists:concat([Format,NewF]),AccIL);
make_format([Other|Rest],NewF,AccIL) -> 
  io:format("in format:make_format (3) Other is ~p Rest is ~p NewF is ~p AccIL is ~p~n",
    [Other,Rest,NewF,AccIL]),
  %% It's important that the length is the length of the stripped comma
  %% format here... (trust me!)
  make_format(Rest,NewF,[{length(strip_commas(NewF)),Other}|AccIL]).

print(Output,Format)->
  io:format("in format:print Output is ~p Format is ~p~n",[Output,Format]),
  %% First up sort out the format
  Split=string:tokens(Format,"."),
  io:format("in format:print Split is ~p~n",[Split]),
  {IntFormat,DecFormat} = case Split of
      [A1,B1] -> {A1,B1};
      [A1]    -> {A1,null}
  end,
  io:format("in format:print IntFormat is ~p DecFormat is ~p~n",[IntFormat,DecFormat]),
  %% Now prepare the output
  %% Now round the output before formatting it
  Output2=round(Output,get_len(DecFormat)),
  io:format("in format:print Output2 is ~p~n",[Output2]),
  Output3=make_list(Output2),
  io:format("in format:print Output3 is ~p~n",[Output3]),
  Split2=string:tokens(Output3,"."),
  io:format("in format:print Split2 is ~p~n",[Split2]),
  {Integers,Decimals} = case Split2 of
      [A2,B2] -> {A2,B2};
      [A2]    -> {A2,null}
  end,
  io:format("in format:print Integers is ~p IntFormat is ~p Decimals is ~p DecFormat is ~p~n",
    [Integers,IntFormat,Decimals,DecFormat]),
  Return=get_output(Integers,IntFormat,Decimals,DecFormat),
  io:format("in format:print Return is ~p~n",[Return]),
  Return.

get_output(Integers,IntFormat,Decimals,DecFormat)->
  {ok,Has_Exp}=has(exponent,DecFormat),
  io:format("in format:get_output Integers is ~p IntFormat is ~p Decimals is ~p DecFormat is ~p "++     "Has_Exp is ~p~n",[Integers,IntFormat,Decimals,DecFormat,Has_Exp]),
  Return=case Has_Exp of
    no  -> get_output1(Integers,IntFormat)++get_output2(Decimals,DecFormat);
    yes -> get_exp(Integers,IntFormat,Decimals,DecFormat)
  end,
  io:format("in format:get_output Return is ~p~n",[Return]),
  Return.

get_len(null)                     -> 0;
get_len(List) when is_list(List) -> length(List).

round(X,0)                            -> round(X);
round(X,N) when N > 0, is_integer(N) ->
  X2=tconv:to_l(X),
  io:format("in format:round X is ~p X2 is ~p and N is ~p~n",[X,X2,N]),
  case (no_of_decimals(X2) > N) of
    true  -> round(X*math:pow(10,N))/math:pow(10,N);
    false -> X
  end.
  
no_of_decimals(Number) when is_list(Number) ->
  case string:tokens(Number,".") of
    [A,B] -> length(B);
    [A]   -> 0
  end.

get_exp(Integers,IntFormat,Decimals,DecFormat) ->
  io:format("in format:get_exp Integers are ~p IntFormat is ~p Decimals are ~p DecFormat is ~p~n",
    [Integers,IntFormat,Decimals,DecFormat]),
  io:format("in format:get_exp *****Exponents not being processed yet******~n"),
  Integers++Decimals.
  
get_output1(Integers,IntFormat)->
  io:format("in format:get_output1 got to 1~n"),
  {ok,Has_Commas}=has(commas,IntFormat),
  io:format("in format:get_output1 got to 2~n"),
  IntFormat2=case Has_Commas of
    yes -> strip_commas(IntFormat);
    no  -> IntFormat
  end,
  io:format("in format:get_output1 Has_Commas is ~p~n",[Has_Commas]),
  Output=format_int(Integers,IntFormat2),
  io:format("in format:get_output1 Output is ~p~n",[Output]),
  Return=case Has_Commas of
    yes -> add_commas(Output);
    no  -> Output
  end,
  io:format("In format:get_output1 Return is ~p~n",[Return]),
  Return.

strip_commas(A) ->
  {ok,Return,_}=regexp:gsub(A,",",""),
  Return.

get_output2(_Decimals,null) -> "";
get_output2(null,DecFormat) -> 
  io:format("in format:get_output2 (1) DecFormat is ~p~n",[DecFormat]),
  Len=length(DecFormat),
  Decimals=get_pad(Len,?ASC_ZERO),
  io:format("in format:get_output2 (1) Len is ~p and Decimals is ~p~n",[Len,Decimals]),
  %% Now that we have recreated the decimals just run get_output2 again
  get_output2(Decimals,DecFormat);
get_output2(Decimals,DecFormat)->
  io:format("In format:get_output2 (2) Decimals is ~p and DecFormat is ~p~n",[Decimals,DecFormat]),
  DecLen=string:len(Decimals),
  FormatLen=string:len(DecFormat),
  io:format("in format:get_output2 (2) Decimals is ~p DecFormat is ~p DecLen is ~p FormatLen is ~p~n",
    [Decimals,DecFormat,DecLen,FormatLen]),
  Return=case (DecLen >= FormatLen) of
    true  -> trunc(Decimals,DecLen-FormatLen);
    false -> display(reverse_pad(Decimals,FormatLen-DecLen),DecFormat)
  end,
  io:format("in format:get_output2 (2) Return is ~p~n",[Return]),
  bodge2(Return,FormatLen).

trunc(Decimals,Len) ->
  io:format("in format:trunc Decimals is ~p Len is ~p~n",[Decimals,Len]),
  Divisor=math:pow(10,Len),
  io:format("in format:trunc Divisor is ~p~n",[Divisor]),
  round(list_to_integer(Decimals)/Divisor).

format_int([?ASC_MINUS|Integers],Format)->
  bodge([?ASC_MINUS]++format_int(Integers,Format));
format_int(Integers,Format)->
  io:format("in format:format_int Integers  is ~p and Format is ~p~n",[Integers,Format]),
  IntLen=string:len(Integers),
  FormatLen=string:len(Format),
  io:format("in format:format_int IntLen is ~p and FormatLen is ~p~n",[IntLen,FormatLen]),
  Return=case (IntLen >= FormatLen) of
    true  -> Integers;
    false -> display(pad(Integers,FormatLen-IntLen),Format)
  end,
  bodge(Return).

%% bodge for when a zero is returned that must be padded
bodge2(0,FormatLen)   -> "."++string:chars($0,FormatLen);
bodge2(Num,FormatLen) -> "."++integer_to_list(Num).

%% bodge for when a negative zero is returned...
bodge("-0")   -> "0";
  bodge(Number) -> Number.
  
add_commas(A)->
  B=lists:reverse(A),
  add_commas(B,3,[]).
  
add_commas([],_,[?ASC_COMMA|Return])             -> Return;
add_commas([],_,[?ASC_MINUS,?ASC_COMMA|Return]) -> [?ASC_MINUS|Return];
add_commas([],_,Acc)                               -> Acc;
add_commas([H|T],1,Acc)                            -> add_commas(T,3,[?ASC_COMMA,H|Acc]);
add_commas([H|T],N,Acc)                            -> add_commas(T,N-1,[H|Acc]).
  
display(A,B)->
  io:format("in format:display/2~n-A is ~p~n-B is ~p~n",[A,B]),
  display(A,B,[],no).

display([null|Rest1],_,Acc,yes) ->
  NewAcc=[?ASC_ZERO|Acc],
  %% We no longer care about the format
  display(Rest1,[],NewAcc,yes);
display(End,_,Acc,yes)-> 
  Return=lists:concat([lists:reverse(Acc),End]),
  io:format("in format:display/4 Return is ~p~n",[Return]),
  Return;
display([null|Rest1],[H|Rest2],Acc,no)->
  io:format("in format:display/4 (2) Acc is ~p~n",[Acc]),
  NewAcc=[get_display(H)|Acc],
  display(Rest1,Rest2,NewAcc,no);
display(Number,Format,Acc,no)->
  io:format("in format:display/4 (2)~n-Number is ~p~n-Format is ~p~n-Acc is ~p~n",
    [Number,Format,Acc]),
  %% toggle yes to throw away the rest of the format
  display(Number,Format,Acc,yes).
  
get_display(?ASC_ZERO) -> ?ASC_ZERO;
get_display(?ASC_HASH) -> [];
get_display(?ASC_QU)   -> ?ASC_SPACE.

pad(Integers,Length)->
  Pad=get_pad(Length,null),
  lists:concat([Pad,Integers]).

reverse_pad(Decimals,Length)->
  Pad=get_pad(Length,null),
  lists:concat([Decimals,Pad]).

get_pad(N,Char) when N > 0 ->
  get_pad(N,Char,[]).
  
get_pad(0,Char,Acc) -> 
  %%io:format("in format:get_pad/3 Char is ~p Acc is ~p~n",[Char,Acc]),
  Acc;
get_pad(N,Char,Acc) ->
  %%io:format("in format:get_pad/3 N is ~p Char is ~p Acc is ~p~n",[N,Char,Acc]),
  get_pad(N-1,Char,[Char|Acc]).

has(_Type,null) -> {ok,no};
has(Type,Format) ->
  io:format("in format:has Type is ~p and Format is ~p~n",[Type,Format]),
  RegExp=case Type of
    commas        -> ",";
    hashes        -> "#";
    questionmarks -> "\?";
    exponent      -> "[E|e]"
  end,
  {ok,Format2,_}=regexp:gsub(Format,RegExp,""),
  case Format of
    Format2 -> {ok,no};
    _       -> {ok,yes}
  end.
  
get_decs(Format)-> {string:len(Format),no}.

make_list(Number) when is_float(Number) -> 
  Float=float_to_list(Number),
  rejig(Float);
make_list(Number) when is_integer(Number) -> 
  integer_to_list(Number).

rejig(Float)->
  [Number,[Sign|Exp]]=string:tokens(Float,"e"),
  io:format("in format:rejig Number is ~p Sign is ~p and Exp is ~p~n",[Number,Sign,Exp]),
  [Sign2|Rest]=Number,
  %% The number might start with a minus sign - so fix that...
  case Sign2 of
    ?ASC_MINUS -> [?ASC_MINUS]++shift(Rest,Sign,Exp);
    _           -> shift(Number,Sign,Exp)
  end.

shift(Number,?ASC_PLUS,Exp) ->
  Exp2=list_to_integer(Exp),
  io:format("in format:shift (+) Number is ~p Exp is ~p Exp2 is ~p~n",[Number,Exp,Exp2]),
  [Int,Dec]=string:tokens(Number,"."),
  io:format("in format:shift (+) Int is ~p Dec is ~p~n",[Int,Dec]),
  X=(Exp2-length(Dec)),
  io:format("in format:shift (+) X is ~p~n",[X]),
  Dec2=if X > 0 -> Int++Dec++get_pad(X,$0)++".";
            X == 0 -> Int++".";
            true   -> Len=length(Dec),
                     Front=string:substr(Dec,1,Exp2),
                     Back=string:substr(Dec,1+Exp2,Len),
                     io:format("in format:shift (+) Len is ~p Front is ~p Back is ~p~n",
                        [Len,Front,Back]),
                     Front++"."++Back
          end,
  io:format("in shift (+) Int is ~p Dec2 is ~p~n",[Int,Dec2]),
  Int++Dec2;
shift(Number,?ASC_MINUS,Exp) ->
  Exp2=list_to_integer(Exp),
  io:format("in shift (-) Number is ~p Exp is ~p Exp2 is ~p~n",[Number,Exp,Exp2]),
  [Int,Dec]=string:tokens(Number,"."),
  io:format("in shift (-) Int is ~p Dec is ~p~n",[Int,Dec]),
  X=(Exp2-length(Int)),
  io:format("in format:shift X is ~p~n",[X]),
  Int2=if X > 0 -> "0."++get_pad(X,$0)++Int;
            X == 0 -> Dec;
            true   -> Len=length(Dec),
                     Front=string:substr(Int,1,Exp2),
                     Back=string:substr(Int,1+Exp2,Len-Exp2),
                     io:format("in format:shift (-) Len is ~p Front is ~p Back is ~p~n",
                        [Len,Front,Back]),
                     Front++"."++Back
          end,
  io:format("in shift (-) Int2 is ~p Dec is ~p~n",[Int2,Dec]),
  Int2++Dec.

%%
%% Format Dates
%%
format_date(X,Format)->
  Has_AMPM=has_AMPM(Format),
  format_date(X,Format,Has_AMPM,[]).

format_date(X,[],_Has_AMPM,Acc)   -> lists:flatten(lists:reverse(Acc));
format_date(X,[H|T],Has_AMPM,Acc) -> io:format("in format:format (1) X is ~p H is ~p T is ~p "++
                                    "and Acc is ~p~n",[X,H,T,Acc]),
                                    format_date(X,T,Has_AMPM,[execute(X,H,Has_AMPM)|Acc]).

has_AMPM([])                -> no;
has_AMPM([{ampm,_F}|_Rest]) -> yes;
has_AMPM([_H|T])            -> has_AMPM(T).

execute(X,{year,two_digit},_)      -> {{Year,_M,_D},_T}=X,
                                       io:format("in format:execute Year is ~p~n",[Year]),
                                       get_last_two(pad_year(integer_to_list(Year)));
execute(X,{year,four_digit},_)     -> {{Year,_M,_D},_T}=X,
                                       io:format("in format:execute Year is ~p~n",[Year]),
                                       pad_year(integer_to_list(Year));
execute(X,{mon_min,no_zero},_)     -> {_D,{_H,Min,_S}}=X,
                                       io:format("in format:execute Min is ~p~n",[Min]),
                                       integer_to_list(Min);
execute(X,{mon_min,zero},_)        -> {{_Y,Mon,_D},_Time}=X,
                                       io:format("in format:execute Mon is ~p~n",[Mon]),
                                       MM=integer_to_list(Mon),
                                       io:format("in format:execute MM is ~p~n",[MM]),
                                       pad_calendar(MM);
execute(X,{mon,abbr},_)            -> {{_Y,Month,_D},_T}=X,
                                       io:format("in format:execute Month is ~p~n",[Month]),
                                       get_short_month(Month);
execute(X,{mon,full},_)            -> {{_Y,Month,_D},_T}=X,
                                       io:format("in format:execute Month is ~p~n",[Month]),
                                       get_month(Month);
execute(X,{day,no_zero},_)         -> {{_Y,_M,Day},_T}=X,
                                       io:format("in format:execute Day is ~p~n",[Day]),
                                       integer_to_list(Day);
execute(X,{day,zero},_)            -> {{_Y,_M,Day},_T}=X,
                                       io:format("in format:execute Day is ~p~n",[Day]),
                                       DD=integer_to_list(Day),
                                       io:format("in format:execute DD is ~p~n",[DD]),
                                       pad_calendar(DD);
execute(X,{day,abbr},_)            -> {{_Y,_M,Day},_T}=X,
                                       io:format("in format:execute Day is ~p~n",[Day]),
                                       get_short_day(Day);
execute(X,{day,full},_)            -> {{_Y,_M,Day},_T}=X,
                                       io:format("in format:execute Day is ~p~n",[Day]),
                                       get_day(Day);
execute(X,{hour,no_zero},yes)      -> {_D,{Hour,_M,_S}}=X,
                                        io:format("in format:execute Hour is ~p~n",[Hour]),
                                        integer_to_list(clock_12(Hour));
execute(X,{hour,no_zero},no)       -> {_D,{Hour,_M,_S}}=X,
                                        io:format("in format:execute Hour is ~p~n",[Hour]),
                                        integer_to_list(Hour);
execute(X,{hour,zero},yes)         -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       pad_calendar(integer_to_list(clock_12(Hour)));
execute(X,{hour,zero},no)          -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       pad_calendar(integer_to_list(Hour));
execute(X,{sec,no_zero},_)         -> {_D,{_H,_M,Sec}}=X,
                                       io:format("in format:execute Sec is ~p~n",[Sec]),
                                       integer_to_list(Sec);
execute(X,{sec,zero},_)            -> {_D,{_H,_M,Sec}}=X,
                                       io:format("in format:execute Sec is ~p~n",[Sec]),
                                       pad_calendar(integer_to_list(Sec));
execute(X,{ampm,full_caps},_)      -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       case (Hour > 12) of
                                         true  -> "PM";
                                         false -> "AM"
                                       end;
execute(X,{ampm,full_lowercase},_) -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       case (Hour > 12) of
                                         true  -> "pm";
                                         false -> "am"
                                       end;
execute(X,{ampm,abbr_caps},_)      -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       case (Hour > 12) of
                                         true  -> "P";
                                         false -> "A"
                                       end;
execute(X,{ampm,abbr_lowercase},_) -> {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       case (Hour > 12) of
                                         true  -> "p";
                                         false -> "a"
                                       end;
execute(X,{Other,String},_)         -> String.

clock_12(Hour) when (Hour > 12) -> Hour-12;
clock_12(Hour)                   -> Hour.

pad_year(A) ->
  case length(A) of
    1 -> "000"++A;
    2 -> "00"++A;
    3 -> "0"++A;
    4 -> A
  end.

pad_calendar(A) ->
  case length(A) of 
  1 -> "0"++A;
  2 -> A
end.

get_short_day(1) -> "Mon";
get_short_day(2) -> "Tues";
get_short_day(3) -> "Wed";
get_short_day(4) -> "Thur";
get_short_day(5) -> "Fri";
get_short_day(6) -> "Sat";
get_short_day(7) -> "Sun".

get_day(1) -> "Monday";
get_day(2) -> "Tuesday";
get_day(3) -> "Wednesday";
get_day(4) -> "Thursday";
get_day(5) -> "Friday";
get_day(6) -> "Saturday";
get_day(7) -> "Sunday".

get_short_month(1) -> "Jan";
get_short_month(2) -> "Feb";
get_short_month(3) -> "Mar";
get_short_month(4) -> "Apr";
get_short_month(5) -> "May";
get_short_month(6) -> "Jun";
get_short_month(7) -> "Jul";
get_short_month(8) -> "Aug";
get_short_month(9) -> "Sept";
get_short_month(10) -> "Oct";
get_short_month(11) -> "Nov";
get_short_month(12) -> "Dec".

get_month(1) -> "January";
get_month(2) -> "February";
get_month(3) -> "March";
get_month(4) -> "April";
get_month(5) -> "May";
get_month(6) -> "June";
get_month(7) -> "July";
get_month(8) -> "August";
get_month(9) -> "September";
get_month(10) -> "October";
get_month(11) -> "November";
get_month(12) -> "December".

get_last_two(String) ->
  Return=string:substr(String,length(String)-1,2),
  io:format("In format:get_last_two Return is ~p~n",[Return]),
  Return.

%%
%% Format other things (bit shite!)
%%
format(X,[],Acc)    -> lists:flatten(lists:reverse(Acc));
format(X,[H|T],Acc) -> io:format("in format:format (1) X is ~p H is ~p T is ~p and Acc is ~p~n",
                          [X,H,T,Acc]),
                        format(X,T,[{X,H}|Acc]).
