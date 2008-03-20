%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

%% Standard Interfaces
-export([
          compile_format/1,
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
compile_format(Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    {ok,Output}=num_format_parser:parse(Tokens),
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
format(X,Format)->
  format(X,Format,[]).
  
format(X,[],Acc)    -> lists:flatten(lists:reverse(Acc));
format(X,[H|T],Acc) -> format(X,T,[execute2(X,H)|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% All these functions are called from format and are internal               %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
execute2(X,H) ->
  io:format("in format:execute2 X is ~p H is ~p~n",[X,H]),
  execute(X,H).

execute(X,{format,F})             -> print(X,F);
execute(X,{year, two_digit})      -> X;
execute(X,{year, four_digit})     -> X;
execute(X,{mon_min, no_zero})     -> X;
execute(X,{mon_min, zero})        -> X;
execute(X,{mon, abbr})            -> X;
execute(X,{mon, full})            -> X;
execute(X,{day, no_zero})         -> X;
execute(X,{day, zero})            -> X;
execute(X,{day, abbr})            -> X;
execute(X,{day, full})            -> X;
execute(X,{hour, no_zero})        -> X;
execute(X,{hour, zero})           -> X;
execute(X,{sec, no_zero})         -> X;
execute(X,{sec, zero})            -> X;
execute(X,{ampm, full_caps})      -> X;
execute(X,{ampm, full_lowercase}) -> X;
execute(X,{ampm, abbr_caps})      -> X;
execute(X,{ampm, abbr_lowercase}) -> X;
execute(X,{at,_})                 -> X.

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
  get_output(Integers,IntFormat,Decimals,DecFormat).
  
get_output(Integers,IntFormat,Decimals,DecFormat)->
  {ok,Has_Exp}=has(exponent,DecFormat),
  case Has_Exp of
    no  -> get_output1(Integers,IntFormat)++get_output2(Decimals,DecFormat);
    yes -> get_exp(Integers,IntFormat,Decimals,DecFormat)
  end.

get_len(null)                     -> 0;
get_len(List) when is_list(List) -> length(List).

round(X,0)                            -> round(X);
round(X,N) when N > 0, is_integer(N) -> round(X*math:power(10,N))/math:pow(10,N).

get_exp(Integers,IntFormat,Decimals,DecFormat) ->
  io:format("in format:get_exp *****Exponents not being processed yet******~n"),
  Integers++Decimals.
  
get_output1(Integers,IntFormat)->
  {ok,Has_Commas}=has(commas,IntFormat),
  IntFormat2=case Has_Commas of
    yes -> strip_commas(IntFormat);
    no  -> IntFormat
  end,
  io:format("in format:get_output1 Has_Commas is ~p~n",[Has_Commas]),
  Output=format_int(Integers,IntFormat2),
  io:format("in format:get_output1 Output is ~p~n",[Output]),  
  case Has_Commas of
    yes -> add_commas(Output);
    no  -> Output
  end.

strip_commas(A) ->
  {ok,Return,_}=regexp:gsub(A,",",""),
  Return.

get_output2(Decimals,null) -> "";
get_output2(Decimals,DecFormat)->
  io:format("in format:get_exp *****get_output2 not being processed yet******~n"),
  Decimals.
    
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

  %% bodge for when a negative zero is returned...
  bodge("-0")   -> "0";
  bodge(Number) -> Number.
  
add_commas(A)->
  B=lists:reverse(A),
  add_commas(B,3,[]).
  
add_commas([],_,Acc)    -> Acc;
add_commas([H|T],1,Acc) -> add_commas(T,3,[?ASC_COMMA,H|Acc]);
add_commas([H|T],N,Acc) -> add_commas(T,N-1,[H|Acc]).
  
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
  Return=lists:concat([Pad,Integers]),
  Return.
  
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
  Dec2=if X > 0 -> Int++get_pad(X,$0)++".";
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
