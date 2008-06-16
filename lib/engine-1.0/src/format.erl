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
%%% @end
%%% @spec get_src(Format::string()) -> {erlang,ErlangFun::string()} | {error, InvMsg::atom()}
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
%%% @end
%%% @spec run_format(Value::string(),Src::string()) -> {ok, Output::string()}
run_format(X,Src)->
    io:format("in format:run_format X is ~p and Src is ~p~n",[X,Src]),
    {ok,ErlTokens,_}=erl_scan:string(Src),
    %%io:format("in format:run_format got to 1 ErlTokens are ~p~n",[ErlTokens]),
    {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),
    io:format("in format:run_format got to 2~n"),
    {value,Fun,_}=erl_eval:exprs(ErlAbsForm,[]),
    io:format("in format:run_format got to 3~n"),
    {ok,Fun(X)}.

%%% @doc Run-time interface not an API - this function should *NOT* be programmed against
%%% @end
format(X,{date,Format})   -> format_date(calendar:gregorian_seconds_to_datetime(X),Format);
format(X,{number,Format}) -> format_num(X,Format);
format(X,{text,Format})   -> format(X,Format,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% All these functions are called from format and are internal               %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format numbers                                                            %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_num(X,Format)->
    io:format("in format:format_num Format is ~p~n",[Format]),
    {NewFormat,InsertList}=make_num_format(Format),
    X2=case is_percentage(Format) of
	   true  -> X * 100;
	   false -> X
       end,
    io:format("in format:format_num X2 is ~p NewFormat is ~p InsertList is ~p~n",
	      [X2,NewFormat,InsertList]),
    Number=lists:flatten(print_num(X2,NewFormat)),
    io:format("in format:format_num Number is ~p~n",[Number]),
    %% insert needs to know if there are commas in the output
    StrippedNumber=strip_commas(Number),
    io:format("in format:format_num StrippedNumber is ~p~n",[StrippedNumber]),
    NewNumber = case StrippedNumber of
		    Number -> insert(Number,InsertList,no_commas);
		    _Other -> insert(Number,InsertList,commas)
		end,
    io:format("in format:format_num NewNumber is ~p~n",[NewNumber]),
    NewNumber.

%% this function scans a format and returns true if there is
%% a percentage sign *anywhere* in the format
is_percentage([])                 -> false;
is_percentage([{percent,"%"}|_T]) -> true;
is_percentage([_H|T])             -> is_percentage(T).

%% this function takes a formatted number and interpolates any strings at the appropriate
%% point if required
insert(Number,InsertList,Type) -> 
    DecPos=get_dec_pos(Number),
    insert(Number,InsertList,Type,DecPos,0).

insert(Number,[],_Type,_DecPos,_Offset) -> Number;
insert(Number,[{Int,Insert}|T],Type,DecPos,Offset) ->
    io:format("in format:insert Number is ~p Int is ~p Insert is ~p T is ~p Type is ~p "++
	      "DecPos is ~p Offset is ~p~n",[Number,Int,Insert,T,Type,DecPos,Offset]),
    Len=length(Number),
    NewInt=new_ins_pnt(Int,Offset,DecPos,Type),
    io:format("in format:insert Len is ~p DecPos is ~p NewInt is ~p~n",[Len,DecPos,NewInt]),
    InsertString=get_string(Insert),
    NewOffset=Offset+length(InsertString),
    io:format("in format:insert InsertString is ~p NewOffset is ~p~n",[InsertString,NewOffset]),
    {LeftString,RightString}=if
				 (Len-NewInt) =< 0 ->
				     {"",Number};
				 true ->
				     {string:substr(Number,1,Len-NewInt),
				     string:substr(Number,Len-NewInt+1,Len)}
			     end,
    NewNumber=lists:concat([LeftString,InsertString,RightString]),
    io:format("in format:insert RightString is ~p LeftString is ~p NewNumber is ~p~n",
	      [RightString,LeftString,NewNumber]),
    insert(NewNumber,T,Type,DecPos,NewOffset).

get_dec_pos(Number)->
    case regexp:match(Number,"\\.") of
	{match,Start,_Len} -> length(Number)-Start+1;
	nomatch            -> 0
    end.

%% The new insert point has to be shifted by a comma point if there are commas and the
%% current insert point is past the decimal point only - otherwise it just gets 
%% the cumulative offset added to it (clear as mud)...
%%new_ins_pnt(Int,Off,DP,commas) when (Int > DP) -> Int+Off+erlang:trunc((Int-DP-1)/3);
new_ins_pnt(Int,Off,DP,commas) when (Int > DP) -> CommaOffset=erlang:trunc((Int-DP)/3),
						  %% if there is to be a comma and a
						  %% text insert at the same point
						  %% the comma goes to the left
						  %% the case statement deals with it
						  Correction=case (((Int-DP)/3)-CommaOffset) of
								 0.0 -> -1;
								 _   -> 0
							     end,
						  io:format("in new_ins_pnt Int is ~p "++
							    "Off is ~p DP is ~p "++
							    "CommaOffset is ~p and "++
							    "Correction is ~p~n",
							    [Int,Off,DP,CommaOffset,Correction]),
						  Int+Off+CommaOffset+Correction;
new_ins_pnt(Int,Off,_DP,_Type)                 -> Int+Off.

get_string({Type,String}) ->
    io:format("in format:get_string Type is ~p String is ~p~n",[Type,String]),
    String.

%% this function takes the parsed format string and strips out the decoration (eg strings
%% and other stuff that must be interpolated the numberical/date or text value and returns
%% the actual format (which will be applied to the value) and an interpolations structure
%% which will be stripped through it
%%
%% eg a format like "0\"a\".0" with the value "4"
%%
%% this will return the format "0.0" and the interpolation "\"a\"" with the insert point of
%% 2 (I think) ie stick an a at the second point from the right...
%% the value "4" would be formatted to "4.0" and then the string would be yoked in to
%% give "4a.0"
%%
%% We do lists:reverse cos we bust out the measure to the left 'cos of Arabic numerals...
%% ie all insert point indexes go right to left on numerals so buy reversing we can
%% count left to right
make_num_format(Format) -> make_num_format(lists:reverse(Format),[],[]).

make_num_format([],NewF,AccIL) ->
    io:format("in format:make_num_format (1) NewF is ~p AccIL is ~p~n",[NewF,AccIL]),
    {NewF,lists:reverse(AccIL)};
make_num_format([{format,Format}|Rest],NewF,AccIL) -> 
    io:format("in format:make_num_format (2) Format is ~p Rest is ~p "++
	      "NewF is ~p AccIL is ~p~n",[Format,Rest,NewF,AccIL]),  
    make_num_format(Rest,lists:concat([Format,NewF]),AccIL);
make_num_format([Other|Rest],NewF,AccIL) -> 
    io:format("in format:make_num_format (3) Other is ~p Rest is ~p "++
	      "NewF is ~p AccIL is ~p~n",[Other,Rest,NewF,AccIL]),
    %% It's important that the length is the length of the stripped comma
    %% format here... (trust me!)
    make_num_format(Rest,NewF,[{length(strip_commas(NewF)),Other}|AccIL]).

print_num(_Output,[])    -> [];
print_num(Output,Format) ->
    io:format("in format:print_num Output is ~p Format is ~p~n",[Output,Format]),
    %% First up sort out the format
    Split=string:tokens(Format,"."),
    io:format("in format:print_num Split is ~p~n",[Split]),
    {IntFormat,DecFormat} = case Split of
				[A1,B1] -> {A1,B1};
				[A1]    -> {A1,null}
			    end,
    io:format("in format:print_num IntFormat is ~p DecFormat is ~p~n",
	      [IntFormat,DecFormat]),
    %% Now prepare the output
    %% Now round the output before formatting it
    Output2=round(Output,get_len(DecFormat)),
    io:format("in format:print_num Output2 is ~p~n",[Output2]),
    Output3=make_list(Output2),
    io:format("in format:print_num Output3 is ~p~n",[Output3]),
    Split2=string:tokens(Output3,"."),
    io:format("in format:print_num Split2 is ~p~n",[Split2]),
    {Integers,Decimals} = case Split2 of
			      [A2,B2] -> {A2,B2};
			      [A2]    -> {A2,null}
			  end,
    io:format("in format:print_num Integers is ~p IntFormat is ~p Decimals is ~p "++
	      "DecFormat is ~p~n",[Integers,IntFormat,Decimals,DecFormat]),
    get_num_output(Integers,IntFormat,Decimals,DecFormat).

%% this function gets the actual formatted number PRIOR to interpolation
%% of any additional strings stuff or other components...
get_num_output(Integers,IntFormat,Decimals,DecFormat)->
    {ok,Has_Exp}=has(exponent,DecFormat),
    io:format("in format:get_num_output Integers is ~p IntFormat is ~p Decimals is ~p "++
	      "DecFormat is ~p Has_Exp is ~p~n",
	      [Integers,IntFormat,Decimals,DecFormat,Has_Exp]),
    Return=case Has_Exp of
	       false -> A=get_num_output1(Integers,IntFormat),
			B=get_num_output2(Decimals,DecFormat),
			io:format("in format:get_num_output A is ~p B is ~p~n",[A,B]),
			A++B;
	       true  -> get_num_exp(Integers,IntFormat,Decimals,DecFormat)
	   end,
    io:format("in format:get_num_output Return is ~p~n",[Return]),
    Return.

get_num_output1(Integers,IntFormat)->
    {ok,Has_Commas}=has(commas,IntFormat),
    IntFormat2=case Has_Commas of
		   true  -> strip_commas(IntFormat);
		   false -> IntFormat
	       end,
    io:format("in format:get_num_output1 Has_Commas is ~p~n",[Has_Commas]),
    %%Output=format_int(Integers,IntFormat2),
    Output=format_int(Integers,IntFormat2),
    io:format("in format:get_num_output1 Output is ~p~n",[Output]),
    Return=case Has_Commas of
	       true  -> add_commas(Output);
	       false -> Output
	   end,
    io:format("In format:get_num_output1 Return is ~p~n",[Return]),
    Return.

%% if the decimal format is null display no decimals
get_num_output2(_Decimals,null) -> "";
%% if there are no decimals make it a zero
get_num_output2(null,DecFormat) -> 
    io:format("in format:get_num_output2 (1) DecFormat is ~p~n",[DecFormat]),
    get_num_output2("0",DecFormat);
%% if there are both decimals and a decimal output then format them
get_num_output2(Decimals,DecFormat)->
    io:format("In format:get_num_output2 (2) Decimals is ~p and DecFormat is ~p~n",
	      [Decimals,DecFormat]),
    DecLen=string:len(Decimals),
    FormatLen=string:len(DecFormat),
    io:format("in format:get_num_output2 (2) Decimals is ~p DecFormat is ~p "++
	      "DecLen is ~p FormatLen is ~p~n",[Decimals,DecFormat,DecLen,FormatLen]),
    Return=case (DecLen >= FormatLen) of
	       true  -> trunc(Decimals,DecLen-FormatLen);
	       false -> Decimals
	   end,
    %% Decimals are always truncated so nobust is used
    Return2=app_fmt(Return,DecFormat,nobust),
    io:format("in format:get_num_output2 (2) Return is ~p Return2 is ~p "++
	      "FormatLen is ~p~n",[Return,Return2,FormatLen]),
    Return3=bodge2(Return2,FormatLen),
    io:format("in get_num_output2 Return3 is ~p~n",[Return3]),
    Return3.

%% the fmt can be applied in either busting or not busting the measure
app_fmt(Number,Format,Type) -> 
    Number2=string:strip(Number,right,?ASC_ZERO),
    io:format("in format:app_fmt Number2 is ~p and Format is ~p Type is ~p~n",
	      [Number2,Format,Type]),
    app_fmt(Number2,Format,Type,[]).

%% the first line lets the measure be bust
app_fmt([],[],bust,A)                         -> lists:reverse(A);
%% the second line truncates the return and doesn't bust the measure
app_fmt(_Number,[],nobust,A)                  -> lists:reverse(A);
app_fmt([N|T],[],bust,A)                      -> app_fmt(T,[],bust,[N|A]);
app_fmt([?ASC_ZERO|T1],[?ASC_ZERO|T2],Type,A) -> app_fmt(T1,T2,Type,[?ASC_ZERO|A]);
app_fmt([?ASC_ZERO|T1],[?ASC_HASH|T2],Type,A) -> app_fmt(T1,T2,Type,A);
app_fmt([?ASC_ZERO|T1],[?ASC_Q|T2],Type,A)    -> app_fmt(T1,T2,Type,[?ASC_SPACE|A]);
app_fmt([N|T1],[?ASC_ZERO|T2],Type,A)         -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([N|T1],[?ASC_HASH|T2],Type,A)         -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([N|T1],[?ASC_Q|T2],Type,A)            -> app_fmt(T1,T2,Type,[N|A]);
app_fmt([],[?ASC_ZERO|T2],Type,A)             -> app_fmt([],T2,Type,[?ASC_ZERO|A]);
app_fmt([],[?ASC_HASH|T2],Type,A)             -> app_fmt([],T2,Type,A);
app_fmt([],[?ASC_Q|T2],Type,A)                -> app_fmt([],T2,Type,[?ASC_SPACE|A]).

%% just rounds off a number to the appropriate number of decimals
round(X,0)                           -> round(X);
round(X,N) when N > 0, is_integer(N) ->
    X2=tconv:to_l(X),
    io:format("in format:round X is ~p X2 is ~p and N is ~p~n",[X,X2,N]),
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
    io:format("in format:get_num_exp Integers is ~p IntFormat is ~p Decimals is ~p DecFormat is ~p~n",
	      [Integers,IntFormat,Decimals,DecFormat]),
    [DecFormat2,Exp]=string:tokens(string:to_upper(DecFormat),"E"),
    Offset=format_exp(Integers,Decimals,list_to_integer(Exp)),
    io:format("in format:get_num_exp Offset is ~p~n",[Offset]),
    %% if the number is bigger than 10 you offset it by a negative
    %% number and 'make it up' in the exponent
    %% if the number is less than 1 you offset it by a positive number
    %% and reduce it in the exponent
    %% which is why we use the negative of the offset in the next line
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
    io:format("in format:trunc Decimals is ~p Len is ~p~n",[Decimals,Len]),
    Divisor=math:pow(10,Len),
    io:format("in format:trunc Divisor is ~p~n",[Divisor]),
    Trunc=round(list_to_integer(Decimals)/Divisor),
    %% we now strip off any trailing zeros
    %% because if the format is something like "00.0?" that trailing zero is
    %% to be replaced by a space...
    Trunc2=string:strip(integer_to_list(Trunc),right,?ASC_ZERO),
    io:format("in format:trunc Trunc is ~p and Trunc2 is ~p~n",[Trunc,Trunc2]),
    Trunc2.

%% formats integers that are passed in as strings
format_int([?ASC_MINUS|Integers],Format)->
    bodge([?ASC_MINUS]++format_int(Integers,Format));
format_int(Integers,Format)->
    io:format("in format:format_int Integers  is ~p and Format is ~p~n",[Integers,Format]),
    %% we  have to feed in the integer and the format reversed...
    %% then obviously re-reverse the answer...
    %% this side always busts the measure so pass in bust
    Padded=lists:reverse(app_fmt(lists:reverse(Integers),lists:reverse(Format),bust)),
%%    Return=display_num(Padded,lists:reverse(Format)),
%%    io:format("in format:format_int Padded is ~p and Return is ~p~n",
%%	      [Padded,Return]),
    bodge(Padded).

%% bodge for when a negative zero is returned...
bodge("-0")   -> "0";
bodge(Number) -> Number.

%% bodge for when a zero is returned that must be padded
bodge2(0,FormatLen)                      -> "."++string:chars(?ASC_ZERO,FormatLen);
bodge2(Num,_FormatLen) when is_list(Num) -> "."++Num.
%%bodge2(Num,_FormatLen)                   -> "."++integer_to_list(Num).

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

%%display_num(A,B)->
%%    io:format("in format:display_num/2~n-A is ~p~n-B is ~p~n",[A,B]),
%%    display_num(A,B,[],false).

%%display_num([null|Rest1],_,Acc,true) ->
%%    NewAcc=[?ASC_ZERO|Acc],
%%    %% We no longer care about the format
%%    display_num(Rest1,[],NewAcc,true);
%%display_num(End,_,Acc,true)-> 
%%    Return=lists:concat([lists:reverse(Acc),End]),
%%    io:format("in format:display_num/4 Return is ~p~n",[Return]),
%%    Return;
%%display_num([null|Rest1],[H|Rest2],Acc,false)->
%%    io:format("in format:display_num/4 (2) Acc is ~p~n",[Acc]),
%%    NewAcc=[get_display_num(H)|Acc],
%%    display_num(Rest1,Rest2,NewAcc,false);
%%display_num(Number,Format,Acc,false)->
%%    io:format("in format:display_num/4 (3)~n-Number is ~p~n-Format is ~p~n-Acc is ~p~n",
%%	      [Number,Format,Acc]),
%%    %% toggle 'true' to throw away the rest of the format
%%    display_num(Number,Format,Acc,true).

%%get_display_num(?ASC_ZERO) -> ?ASC_ZERO;
%%get_display_num(?ASC_HASH) -> [];
%%get_display_num(?ASC_Q)    -> ?ASC_SPACE.

get_pad(N,_)    when N =< 0 -> "";
get_pad(N,Char) when N  > 0 -> get_pad(N,Char,[]).

get_pad(0,_Char,Acc) -> Acc;
get_pad(N,Char,Acc)  -> get_pad(N-1,Char,[Char|Acc]).

has(_Type,null) -> {ok,false};
has(Type,Format) ->
    io:format("in format:has Type is ~p and Format is ~p~n",[Type,Format]),
    RegExp=case Type of
	       commas        -> ",";
	       hashes        -> "#";
	       questionmarks -> "\?";
	       fractions     -> "/";
	       exponent      -> "[E|e]"
	   end,
    {ok,Format2,_}=regexp:gsub(Format,RegExp,""),
    case Format of
	Format2 -> {ok,false};
	_       -> {ok,true}
    end.

%%get_decs(Format)-> {string:len(Format),false}.

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
    Dec2=if X > 0 -> Int++Dec++get_pad(X,?ASC_ZERO)++".";
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
    Int2=if X > 0 -> "0."++get_pad(X,?ASC_ZERO)++Int;
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format dates                                                              %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_date(X,Format)->
    Has_AMPM=has_AMPM(Format),
    format_date(X,Format,Has_AMPM,[]).

format_date(_X,[],_Has_AMPM,Acc)  -> lists:flatten(lists:reverse(Acc));
format_date(X,[H|T],Has_AMPM,Acc) -> io:format("in format:format (1) X is ~p H is ~p T is ~p "++
					       "and Acc is ~p~n",[X,H,T,Acc]),
				     format_date(X,T,Has_AMPM,[execute(X,H,Has_AMPM)|Acc]).

has_AMPM([])                -> false;
has_AMPM([{ampm,_F}|_Rest]) -> true;
has_AMPM([_H|T])            -> has_AMPM(T).

execute(X,{year,two_digit},_)      -> {{Year,_M,_D},_T}=X,
				      io:format("in format:execute Year is ~p~n",[Year]),
				      get_last_two(pad_year(integer_to_list(Year)));
execute(X,{year,four_digit},_)     -> {{Year,_M,_D},_T}=X,
				      io:format("in format:execute Year is ~p~n",[Year]),
				      pad_year(integer_to_list(Year));
execute(X,{mon_min,no_zero},_)     -> {_D,{_H,Min,_S}}=X,
				      io:format("in format:execute **************************"),
				      io:format("in format:execute - problem with mon_min!!!!"),
				      io:format("in format:execute - is it a min or a mon?   "),
				      io:format("in format:execute **************************"),
				      io:format("in format:execute Min is ~p~n",[Min]),
				      integer_to_list(Min);
execute(X,{mon_min,zero},_)        -> {{_Y,Mon,_D},_Time}=X,
				      io:format("in format:execute **************************"),
				      io:format("in format:execute - problem with mon_min!!!!"),
				      io:format("in format:execute - is it a min or a mon?   "),
				      io:format("in format:execute **************************"),
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
execute(X,{min,elapsed},_)         ->  Secs=calendar:datetime_to_gregorian_seconds(X),
				       io:format("in format:execute Elapsed Sec is ~p~n",[Secs]),
				       integer_to_list(round(Secs/60));
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
execute(X,{hour,no_zero},true)     -> {_D,{Hour,_M,_S}}=X,
				      io:format("in format:execute Hour is ~p~n",[Hour]),
				      integer_to_list(clock_12(Hour));
execute(X,{hour,no_zero},false)    -> {_D,{Hour,_M,_S}}=X,
				      io:format("in format:execute Hour is ~p~n",[Hour]),
				      integer_to_list(Hour);
execute(X,{hour,zero},true)        ->  {_D,{Hour,_M,_S}}=X,
                                       io:format("in format:execute Hour is ~p~n",[Hour]),
                                       pad_calendar(integer_to_list(clock_12(Hour)));
execute(X,{hour,zero},false)       ->  {_D,{Hour,_M,_S}}=X,
				       io:format("in format:execute Hour is ~p~n",[Hour]),
				       pad_calendar(integer_to_list(Hour));
execute(X,{hour,elapsed},_)        ->  Secs=calendar:datetime_to_gregorian_seconds(X),
				       io:format("in format:execute Elapsed Secs is ~p~n",[Secs]),
				       integer_to_list(round(Secs/3600));
execute(X,{sec,no_zero},_)         ->  {_D,{_H,_M,Sec}}=X,
                                       io:format("in format:execute Sec is ~p~n",[Sec]),
                                       integer_to_list(Sec);
execute(X,{sec,zero},_)            -> {_D,{_H,_M,Sec}}=X,
				      io:format("in format:execute Sec is ~p~n",[Sec]),
				      pad_calendar(integer_to_list(Sec));
execute(X,{sec,elapsed},_)         ->  Secs=calendar:datetime_to_gregorian_seconds(X),
				       io:format("in format:execute Elapsed Secs is ~p~n",[Secs]),
				       integer_to_list(round(Secs));
execute(X,{ampm,full_caps},_)      ->  {_D,{Hour,_M,_S}}=X,
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
execute(_X,{_Other,String},_)       -> String.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Format other things (bit shite!)                                          %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format(_X,[],Acc)             -> lists:flatten(lists:reverse(Acc));
format(X,[{at,"@"}|T],Acc)    -> format(X,T,[X|Acc]);
format(X,[{_Type,Bit}|T],Acc) -> io:format("in format:format (1) X is ~p Bit is ~p "++
					   "T is ~p and Acc is ~p~n",
					   [X,Bit,T,Acc]),
				 format(X,T,[Bit|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% Utility functions                                                         %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gets lenghts where an object can be null
get_len(null)                    -> 0;
get_len(List) when is_list(List) -> length(List).

%% strips commans out of a string
strip_commas(A) ->
    {ok,Return,_}=regexp:gsub(A,",",""),
    Return.


