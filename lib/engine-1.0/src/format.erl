%%% @doc    This module handles the number/text formatting for a cell
%%%         Created the 11th March 2008
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
-module(format).

-export([
          compile_format/1,
          run_format/2,
          format/2
        ]).

%% Debug export
-export([get_pad/2]).

-define(PLUS,43).
-define(MINUS,45).


%%% @doc takes a value and a format and returns the tagged value as per 
%%%      the format
%%% @spec format(Value ,Format) -> {tag,FormattedValue} | {error, InvMsg}
%%% @type Value = [string|number|error|boolean|date]
%%% @type InvMsg = string
compile_format(Format)->
    {ok,Tokens,_}=num_format_lexer:string(Format),
    {ok,Output}=num_format_parser:parse(Tokens),
    {erlang,Output}.
    
run_format(X,Src)->
  io:format("in format:run_format Src is ~p~n",[Src]),
  {ok,ErlTokens,_}=erl_scan:string(Src),
  {ok,ErlAbsForm}=erl_parse:parse_exprs(ErlTokens),
  {value,Fun,_}=erl_eval:exprs(ErlAbsForm,[]),
  Fun(X).

format(X,Format)->
  format(X,Format,[]).
  
format(X,[],Acc)    -> lists:flatten(lists:reverse(Acc));
format(X,[H|T],Acc) -> format(X,T,[execute2(X,H)|Acc]).

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
  io:format("in format:print IntFormat is ~p DecFormat is ~p and ~n",[IntFormat,DecFormat]),
  %% Now prepare the output
  Output2=make_list(Output),
  io:format("in format:print Output2 is ~p~n",[Output2]),
  Split2=string:tokens(Output2,"."),
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
  
get_exp(Integers,IntFormat,Decimals,DecFormat) ->
  io:format("in format:get_exp *****Exponents not being processed yet******~n"),
  Integers++Decimals.
  
get_output1(Integers,IntFormat)->
  {ok,Has_Commas}=has(commas,IntFormat),
  io:format("in format:get_output1 Has_Commas is ~p~n",[Has_Commas]),
  Output=format_int(Integers,IntFormat),
  io:format("in format:get_output1 Output is ~p~n",[Output]),  
  case Has_Commas of
    yes -> add_commas(Output);
    no  -> Output
  end.

get_output2(Decimals,null) -> "";
get_output2(Decimals,DecFormat)->
  io:format("in format:get_exp *****get_output2 not being processed yet******~n"),
  Decimals.
%%  {_,Has_Exp}=has(exponent,DecFormat),
%%  case Has_Exp of
%%    yes -> get_exp
    
    
format_int(Integers,Format)->
  io:format("in format:format_int Integers  is ~p and Format is ~p~n",[Integers,Format]),
  IntLen=string:len(Integers),
  FormatLen=string:len(Format),
  io:format("in format:format_int IntLen is ~p and FormatLen is ~p~n",[IntLen,FormatLen]),
  Return=case (IntLen >= FormatLen) of
    true  -> Integers;
    false -> display(pad(Integers,FormatLen),Format)
  end,
  %% bodge for when a negative zero is returned...
  case Return of
    "-0" -> "0";
    _    -> Return
  end.
  
add_commas(A)->
  io:format("in format:add_commas ****Commas not being added yet****~n"),
  A.
  
display(A,B)->
  io:format("in format:display ****Display not being processed yet****~n"),
  A.

pad(Integers,Length)->
  Pad=get_pad(Length,null),
  Return=lists:concat([Pad,Integers]),
  io:format("in format:pad Return is ~p~n",[Return]),
  Return.
  
get_pad(N,Char) when N > 0 ->
  %%io:format("in format:get_pad/2 N is ~p Char is ~p~n",[N,Char]),
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
    ?MINUS -> [?MINUS]++shift(Rest,Sign,Exp);
    _       -> shift(Number,Sign,Exp)
  end.

shift(Number,?PLUS,Exp) ->
  Exp2=list_to_integer(Exp),
  io:format("in shift (+) Number is ~p Exp is ~p Exp2 is ~p~n",[Number,Exp,Exp2]),
  [Int,Dec]=string:tokens(Number,"."),
  io:format("in shift (+) Int is ~p Dec is ~p~n",[Int,Dec]),
  X=(Exp2-length(Dec)),
  io:format("in format:shift X is ~p~n",[X]),
  Dec2=if X > 0 -> Int++get_pad(X,$0)++".";
            X == 0 -> Int++".";
            true   -> Len=length(Int),
                     Front=string:substr(Dec,1,Exp2),
                     Back=string:substr(Dec,1+Exp2,Len-Exp2),
                     io:format("in format:shift (+) Len is ~p Front is ~p Back is ~p~n",
                        [Len,Front,Back]),
                     Front++"."++Back
          end,
  io:format("in shift (+) Int is ~p Dec2 is ~p~n",[Int,Dec2]),
  Int++Dec2;
shift(Number,?MINUS,Exp) ->
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
