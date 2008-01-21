%%%-------------------------------------------------------------------
%%% File        : excel_rev_comp.erl
%%% Author      : Gordon Guthrie <gordon@hypernumbers.com>
%%% Description : the reverse compiler for the Excel RPN token stream
%%%               that turns the internal representation of an Excel
%%%               formula back into the original form that the user
%%%               entered into the cell
%%%
%%% Created     : 11th Jan 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(excel_rev_comp).

-export([reverse_compile/4]).

%%% Include files with macros encoding Microsoft File Format constants
-include("excel_attributes.hrl").
-include("excel_array_elements.hrl").
-include("excel_errors.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the tokens defined in Section 3 of                   %%%
%%% excelfileformat.pdf (V1.40) - it reverse compiles them into the raw      %%%
%%% formulae                                                                 %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The formulae are stored in reverse Polish Notation within excel
%% The reverse compiler recreates the original formula by running the RPN
reverse_compile(Tokens,TokenArray,Tables,FileOut)->
    io:format("******************Starts******************************~n"),
    io:format("*                                                    *~n"),    
    io:format("in excel_rev_comp:reverse_compile~n-Tokens ~p~n"++
         "-TokenArray is ~p~n",[Tokens,TokenArray]),
    io:format("*                                                    *~n"),    
    io:format("*******************Ends*******************************~n"),
    reverse_compile(Tokens,TokenArray,[],[],Tables,FileOut).

%%	tExp    
reverse_compile([{expr_formula_range,{tExp,[{row_index,Row},{col_index,Col}],{return,none}}}|T],
    TokenArray,Stack,Residuum,Tables,FileOut) ->
    Formula=excel_util:read(Tables,arrayformula,{{row_index,Row},{col_index,Col}}),
    io:format("in excel_rev_comp for tExp Formula is ~p~n",[Formula]),
    NewStack = push(Stack,{string,Formula}),    
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);    
    
%%	tTbl    
	
%%	tAdd tSub tMul tDiv tPower tConcat tLT tLE tEQ tGE tGT tNE tIsect
reverse_compile([{Operator,_Token}|T],TokenArray,Stack,Residuum,Tables,FileOut) when
    Operator =:= addition ; Operator =:= subtraction ; Operator =:= multiply ;
    Operator =:= divide ; Operator =:= power; Operator =:= concatenate; 
    Operator =:= less_than ; Operator =:= less_than_or_equals ; 
    Operator =:= equals ; Operator =:= greater_than_or_equals ;
    Operator =:= greater_than ; Operator =:= not_equals->
    %% Pop two of the stack and the build an operator set backwards
    %% ie second first and first second...
    io:format("~nin excel_rev_comp:reverse_compile for Operators Operator is ~p~n-Stack is ~p~n",[Operator,Stack]),
    {First,NewStack}=pop(Stack),
    %%io:format("in excel_rev_comp:reverse_compile for Operator First is ~p~n-NewStack is ~p~n",[First,NewStack]),
    {Second,Rest2}=pop(NewStack),
    %%io:format("in excel_rev_comp:reverse_compile for Operator Second is ~p~n-Rest2 is ~p~n",[Second,NewStack]),
    Formula = push(Rest2,{Second,Operator,First}),
    %%io:format("in excel_rev_comp:reverse_compile operator T is ~p~n-Formula are ~p~n-Residuum is ~p~n",
    %%[T,Formula,Residuum]),
    reverse_compile(T,TokenArray,Formula,Residuum,Tables,FileOut);

% tIsect
reverse_compile([{intersect,_Token}|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
    %% Pop two of the stack and the build an operator set backwards
    %% ie second first and first second...
    io:format("~nin excel_rev_comp:reverse_compile for tIntersect Stack is ~p~n",[Stack]),
    {{string,First},NewStack}=pop(Stack),
    %%io:format("in excel_rev_comp:reverse_compile for Operator First is ~p~n-NewStack is ~p~n",[First,NewStack]),
    {{string,Second},Rest2}=pop(NewStack),
    %%io:format("in excel_rev_comp:reverse_compile for Operator Second is ~p~n-Rest2 is ~p~n",[Second,NewStack]),
    Formula = push(Rest2,{string,lists:flatten([Second," ",First])}),
    io:format("in excel_rev_comp:reverse_compile operator T is ~p~n-Formula are ~p~n-Residuum is ~p~n",
        [T,Formula,Residuum]),
    reverse_compile(T,TokenArray,Formula,Residuum,Tables,FileOut);

%%	tList   
reverse_compile([{list,{tList,[{op_type,binary}],{return,reference}}}|T],TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tList Stack is ~p~n",[Stack]),
    {First,NewStack}=pop(Stack),
    {Second,Rest2}=pop(NewStack),
    Formula = push(Rest2,{Second,comma,First}),
    io:format("in excel_rev_comp:reverse_compile for tList - dinnae understand!~n"),
    reverse_compile(T,TokenArray,Formula,Residuum,Tables,FileOut);
	
%%	tRange  
	
%%	tUplus and tUminus 
reverse_compile([{Operator,_Token}|T],TokenArray,Stack,Residuum,Tables,FileOut) when
    Operator =:= unaryplus ; Operator =:= unaryminus ->
    io:format("~nin excel_rev_comp:reverse_compile for Unary Plus/Minus is ~p~n-Stack is ~p~n",[Operator,Stack]),
    {First,NewStack}=pop(Stack),
    Formula = push(NewStack,{Operator,First}),
    %%io:format("in excel_rev_comp:reverse_compile operator T is ~p~n-Formula are ~p~n-Residuum is ~p~n",
    %%[T,Formula,Residuum]),
    reverse_compile(T,TokenArray,Formula,Residuum,Tables,FileOut);
	
%%	tPercent
	
%%	tParen  
reverse_compile([{parentheses,{tParen,[],{return,none}}}|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
  {Last,Rest} = pop(Stack),
  io:format("~nin excel_rev_comp:reverse_compile for Brackets~n-Stack is ~p~n-Last is ~p~n-Rest is ~p~n",
            [Stack,Last,Rest]),
  reverse_compile(T,TokenArray,push(Rest,{open,Last,close}),Residuum,Tables,FileOut);
	
%%	tMissArg
	
%%	tStr    
reverse_compile([{string,{tStr,[{value,{[{Encoding,Binary}],Len1,Len2}}],
  {return,value}}}|T],TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tStr Stack is ~p~n",[Stack]),
    NewStack = push(Stack,{string,binary_to_list(Binary)}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tNlr    
	
%%	tAttr   
reverse_compile([{attributes,Attributes}|T],TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tAttr Stack is ~p~n"++
    "-Attributes are ~p~n",[Stack,Attributes]),
    NewStack = case Attributes of
      {tAttrVolatile,volatile_attribute,[],{return,none}}    -> Stack; % do nothing, don't care
      {tAttrIf,if_attribute,[{jump,Jump}],{return,none}}     -> Stack; % do nothing, don't care
      {tAttrChoose,choose_attribute,
              [{no_of_choices,NumberOfChoices},
              {jump_table,JumpTable},
              {error_jump,ErrorJump}],{return,none}}         -> Stack; % do nothing, don't care
      {tAttrSkip,skip_attribute,[{skip,Skip}],{return,none}} -> Stack; % do nothing, don't care
      {tAttrSum,sum_attribute,[],{return,none}}              -> 
            {FunArgs,Rest} = lists:split(1,Stack), % this is an attribute of SUM with 1 arg...
            io:format("in excel_rev_comp:reverse_compile for tAttr Rest is ~p~n-"++
                      "FunArgs is ~p~n",[Rest,FunArgs]),
            push(Rest,{func,4,FunArgs}); % 4 if the index to the func SUM
      {tAttrAssign,assign_attribute,[],{return,none}}        -> Stack; % do nothing, don't care
      {tAttrSpace,special_character,
          [{char,Type},{no_of_chars,NoOfSpecChars}],
            {return,none}}                                   -> Stack; % do nothing, don't care
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Can't be bother to carefully replace the spaces in the functions...
%%
%%            Padding = case Type of 
%%              space           -> string:copies(" ",NoOfSpecChars);
%%              carriage_return -> string:copies("\n",NoOfSpecChars)
%%            end,
%%             {Rest2,PArg} = case Stack of
%%              [] -> {[],"XXX"};
%%              Other -> {Arg,Rest3}=lists:split(1,Stack), % append padding to previous arg
%%                        io:format("in excel_rev_comp:reverse_compile for tAttr Arg is ~p~n-Rest3 is ~p~n-Padding is ~p~n",
%%                        [Arg,Rest3,Padding]),
%%                        PaddedArg=to_str(Arg)++Padding,
%%                        {Rest3,PaddedArg}
%%            end,
%%          io:format("in excel_rev_comp:reverse_compile for tAttr, Rest2 is ~p~n-PArg is ~p~n",[Rest2,PArg]),
%%          push(Rest2,{string, PArg});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    List                                                     -> push(Stack,fucked7) %tAttr_SpaceVolatile
  end,
  io:format("in excel_rev_com:reverse_compile for tAttr T is ~p~n-NewStack is ~p~n-Residuum is ~p~n",
            [T,NewStack,Residuum]),
  reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tSheet  
	
%%	tEndSheet
	
%%	tErr
reverse_compile([{reference_error,{tRefErr,[{type,reference}],{return,reference}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tErr Stack is ~p~n",[Stack]),
    NewStack = push(Stack,{string,"#REF!"}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tBool   
reverse_compile([{boolean,{tBool,[{value,Value}],{return,value}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tBool Stack is ~p~n",[Stack]),
    Boolean = case Value of
           1 -> "TRUE";
           0 -> "FALSE"
        end,
    NewStack = push(Stack,{string,Boolean}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tInt    
reverse_compile([{integer,{tInt,[{value,Val}],{return,value}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for Int Integer is ~p~n-Stack is ~p~n",[Val,Stack]),
    NewStack = push(Stack,{integer,Val}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tNum    
reverse_compile([{number,{tNum,[{value,Val}],{return,value}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for Num Number is ~p~n-Stack is ~p~n",[Val,Stack]),
    NewStack = push(Stack,{float,Val}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);

%%%
%%% Classified Tokens
%%%

%%	tArray
reverse_compile([{array_type,{tArray,[{type,Type}],{return,_Reference}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
        {Array,ArrayTail}=read_const_val_array(TokenArray,FileOut),
    io:format("~nin excel_rev_comp:reverse_compile for tArray Stack is ~p~n"++
        "-Array is ~p~n",[Stack,Array]),
    NewStack=push(Stack,{array1D,Array}),
    reverse_compile(T,ArrayTail,NewStack,Residuum,Tables,FileOut);
	
%%	tFunc   
reverse_compile([{functional_index,{Function,[{value,FuncVar},
        {type,value}],{return,variant}}}|T],TokenArray,Stack,Residuum,Tables,FileOut)
        when Function =:= tFunc ; Function =:= tFuncVar ; 
        Function =:= tFuncVarV ;Function =:= tFuncVarR ; 
        Function =:= tFuncVarA ->
    io:format("~nin excel_rev_comp:reverse_compile for Functions I FuncVar is ~p~n"++
        "-Function is ~p~n-TokenArray is ~p~n-Stack is ~p~n",
        [FuncVar,Function,TokenArray,Stack]),
    NumArgs=macro_no_of_args(FuncVar),
    io:format("NumArgs is ~p Stack is ~p~n",[NumArgs,Stack]),
    {FunArgs,Rest} = lists:split(NumArgs,Stack),
    io:format("FunArgs is ~p and Rest is ~p~n",[FunArgs,Rest]),
    NewStack = push(Rest,{func,FuncVar,FunArgs}),
    io:format("NewStack is ~p~n",[NewStack]),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tFuncVar
reverse_compile([{var_func_idx,{Function,[{value,FuncVar},
        {number_of_args,NumArgs},{user_prompt,Prompt},{type,Type}],
        {return,ReturnType}}}|T],TokenArray,Stack,Residuum,Tables,FileOut) when Function =:= tFuncVar ;
        Function =:= tFuncVarV ;Function =:= tFuncVarR ; Function =:= tFuncVarA ->
    io:format("~nin excel_rev_comp:reverse_compile for Functions II FuncVar is ~p "++
       "NumArgs is ~p Stack is ~p~n",[FuncVar,NumArgs,Stack]),
    {FunArgs,Rest} = lists:split(NumArgs,Stack),
    NewStack = push(Rest,{func,FuncVar,FunArgs}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);

%%	tName   
reverse_compile([{name_index,{tName,[{value,Value},{type,Type}],{return,reference}}}|T],
        TokenArray,Stack,Residuum,Tables,FileOut)  ->
    io:format("~nin excel_rev_comp:reverse_compile for tName Stack is ~p~n",[Stack]),
    NewStack = push(Stack,{string,"write the name lookup, ya bas!"}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tRef    
reverse_compile([{abs_ref,{tRef,[{value,{Row,Col,RowType,ColType}}|{type,Type}],
       {return,reference}}}|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
    io:format("in excel_rev_comp:reverse_compile for Absolute Reference~n"),
    NewStack=push(Stack,{string,make_cell({Row,Col,RowType,ColType})}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tArea   
reverse_compile([{absolute_area,{tArea,[[{start_cell,StartCell}|
    {end_cell,EndCell}]|R1],R2}}|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
    io:format("in excel_rev_comp:reverse_compile for tArea~n"),
    Range=make_range(StartCell,EndCell),
    NewStack=push(Stack,{string,Range}),
    reverse_compile(T,TokenArray,NewStack,Residuum,Tables,FileOut);
	
%%	tMemArea
reverse_compile([{memory_area,{tMemArea,[{value,MemArea},{type,Type}],
				   {return,reference}}}|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
    io:format("in excel_rev_comp:reverse_compile for tMemArea MemArea is ~p~n",
        [MemArea]),
    %% See discussion in Section 3.1.6 of excelfileformat.v1.40.pdf
    %%
    %% for tMemArea there is an appended set of tokens in the TokenArray
    %% which must be read in and then *DISCARDED*
    %%
    %% for instance if the tMemArea is "A1:A2 A2:A3" then the TokenArray
    %% will contain the *RESULT* of the intersection, ie "A2"
    %%
    %% So to calculate Excel would discard the value that would be
    %% in the tMemArea token - 19 in this instance - but use "A2"
    %% whereas reverse compiling is only interested in the full
    %% range "A1:A2 A2:A3" and discards the TokenArray "A2"
    %% ie the first return variable in the next line
    {_Array,ArrayTail}=excel_util:read_cell_range_add_list(TokenArray,FileOut),
    %% Given that this token holds a look up to the results of parsing
    %% the subsequent array we can just chuck it away.
    reverse_compile(T,ArrayTail,Stack,Residuum,Tables,FileOut);
	
%%	tMemErr 
	
%%	tMemNoMem
	
%%	tMemFunc
	
%%	tRefErr 
	
%%	tAreaErr
	
%%	tRefN   

%%	tAreaN  
	
%%	tMemAreaN
	
%%	tMemNoMemN	
	
%%	tFuncCE 
	
%%	tNameX  
	
%%	tRef3d  	
reverse_compile([{three_dee_reference,{tRef3d,[{reference_index,RefIdx},
      {Row,Col,RowType,ColType},{type,Type}],{return,ReturnType}}}|T],
      TokenArray,Stack,Residuum,Tables,FileOut) ->
    io:format("in excel_rev_comp:reverse_compile for tRef3d~n"++
        "-Row is ~p Col is ~p RowType is ~p ColType is ~p"++
        "-RefIdx is ~p and Type is ~p~n",[Row,Col,RowType,ColType,RefIdx,Type]),
    reverse_compile(T,TokenArray,Stack,Residuum,Tables,FileOut);
  
%%	tArea3d 
		
%%	tRefErr3d
	
%%	tAreaErr3d

%%%%%%%%%%%%%%%%%%%%%%

%% This will catch missed out stuff...
reverse_compile([Head|T],TokenArray,Stack,Residuum,Tables,FileOut) ->
    io:format("~nin excel_rev_comp:reverse_compile for What's This? Head is ~p~n",[Head]),
    excel_util:put_log(FileOut++".rev_comp",io_lib:fwrite("Missing Token is ~p",[Head])),
    reverse_compile(T,TokenArray,Stack,Residuum,Tables,FileOut);

%% Clean up?
reverse_compile([],TokenArray,Stack,_Residuum,Tables,FileOut) ->
  io:format("in excel_rev_comp:reverse_compile dropping to to_str, Stack is ~p~n",[Stack]),
  "="++to_str(Stack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                         %%%
%%% These functions read the array tokens that are appended to the formula  %%%
%%% RPN token stream                                                        %%%
%%%                                                                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Read a Constant Value Array
%% defined in Section 2.5.8 of excelfileformatV1-40.pdf
read_const_val_array(Bin,FileOut)->
  %%io:format("in excel_rev_comp:read_const_val_array Bin is ~p~n",[Bin]),
  <<NoCols:8/little-unsigned-integer,
    NoRows:16/little-unsigned-integer,
    Rest/binary>>=Bin,
    Sz=NoCols+1*NoRows+1,
    %%io:format("in excel_rev_comp:read_const_val_array NoCols is ~p~n"++
    %%  "NoRows is ~p~nSz is ~p~n",[NoCols,NoRows,Sz]),
    read_token_array(Sz,Rest,FileOut).

%% This function parses the token array
%% defined in Section 2.5.7 of excelfileformatV1-40.pdf
read_token_array(N,Array,FileOut)->
  io:format("in excel_rev_comp:read_token_array Array is ~p~n",[Array]),
  read_token_array(N,Array,[],FileOut).

read_token_array(0,Bin,Residuum,FileOut)->
  {lists:reverse(Residuum),Bin};
read_token_array(N,<<?EmptyArrayEl:8/little-unsigned-integer,
    _NotUsed:8/binary,
    Rest/binary>>,Residuum,FileOut)->
    io:format("in excel_rev_comp:read_token_array for EmptyArrayEl~n"),
  read_token_array(N-1,Rest,[{string,""}|Residuum],FileOut);
read_token_array(N,<<?NumberArrayEl:8/little-unsigned-integer,
    Float:64/little-float,
    Rest/binary>>,Residuum,FileOut)->
    io:format("in excel_rev_comp:read_token_array for NumberArrayEl Float is ~p~n",
        [Float]),
  read_token_array(N-1,Rest,[{float,Float}|Residuum],FileOut);
read_token_array(N,<<?StringArrayEl:8/little-unsigned-integer,
  Rest/binary>>,Residuum,FileOut)->
  io:format("in excel_rev_comp:read_token_array for StringArrayEl Rest is ~p~n",
    [Rest]),
  <<StringSz:16/little-unsigned-integer,NotUsed/binary>>=Rest,
  io:format("in excel_rev_comp:read_token_array StringSZ is ~p~n",[StringSz]),
  %% The index is always 16 bits in this instance - see note in Section 2.5.7
  %% of excelfileformatV1-40.pdf
  %% so we jump to the index length of 2 (2 time 8 bits)...
  {[{Type,String2}],Len,_}=excel_util:parse_CRS_Uni16(Rest,2,FileOut),
  io:format("in excel_rev_comp:read_token_array String2 is ~p~n",[String2]),
  %% Now cut out the string from Rest
  Len2=Len+1,
  <<CutOut:Len2/binary,Rest2/binary>>=Rest,
  io:format("in excel_rev_comp:read_token_array Rest2 is ~p~n",[Rest2]),
  read_token_array(N-1,Rest2,[{string,binary_to_list(String2)}|Residuum],FileOut);
read_token_array(N,<<?BooleanArrayEl:8/little-unsigned-integer,
    Boolean:8/little-unsigned-integer,
    _NotUsed:7/binary,
    Rest/binary>>,Residuum,FileOut)->
    io:format("in excel_rev_comp:read_token_array for BooleanArrayEl~n"),
    Value = case Boolean of
     0 -> "FALSE";
     1 -> "TRUE"
  end,
  io:format("in excel_rev_comp:read_token_array Boolean is ~p~n",[Boolean]),
  read_token_array(N-1,Rest,[{string,Value}|Residuum],FileOut);
read_token_array(N,<<?ErrorArrayEl:8/little-unsigned-integer,
    Error:8/little-unsigned-integer,
    _NotUsed:7/binary,
    Rest/binary>>,Residuum,FileOut)->
    io:format("in excel_rev_comp:read_token_array for ErrorArrayEl~n"),
  Value = case Error of
      ?NullError    -> "#NULL!";
      ?DivZeroError -> "#DIV/0!";
      ?ValueError   -> "#VALUE!";
      ?RefError     -> "#REF!";
      ?NameError    -> "#NAME?";
      ?NumError     -> "#NUM!";
      ?NAError      -> "N/A"
  end,
    io:format("in excel_rev_comp:read_token_array for ErrorArrayEl Error is ~p~n",
        [Error]),
  read_token_array(N-1,Rest,[{string,Value}|Residuum],FileOut).

%%--------------------------------------------------------------------
%% Function:    implode/2
%%
%% Description: Add all the items in the list together into a string
%%              interspace with 'Token' (number converted to strings).
%%              (Token doesn't get added on to the last element).
%%--------------------------------------------------------------------
implode(List, Token) ->
    F = fun(X, Acc) ->
        UTok = case lists:last(List) of
            X -> [];
            _ -> Token
        end,
        if
            is_integer(X) -> Acc ++ integer_to_list(X) ++ UTok;
            is_float(X)   -> Acc ++ to_str({float,X}) ++ UTok; % we want a float like "2.00000" to come back as "2"
            true          -> Acc ++ X ++ UTok
        end
    end,
    lists:foldl(F, [], List).

%%% set of functions used by reverse_compile to generate the actual 
%%% Formula Strings that are in the cells
operator_to_string(addition)               -> "+";           
operator_to_string(subtraction)            -> "-";
operator_to_string(multiply)               -> "*";
operator_to_string(divide)                 -> "/";
operator_to_string(power)                  -> "^";
operator_to_string(concatenate)            -> "++"; %?
operator_to_string(less_than)              -> "<";
operator_to_string(less_than_or_equals)    -> "=<";
operator_to_string(equals)                 -> "=";
operator_to_string(greater_than_or_equals) -> ">=";
operator_to_string(greater_than)           -> ">";
operator_to_string(not_equals)             -> "!=";
operator_to_string(intersect)              -> " ";
operator_to_string(comma)                  -> ",".

to_str({func,Var,Args})    ->
R = fun(Item) -> 
    case Item of
      {Type,L} -> L;
    Other    -> to_str(Other)
    end
  end,
  macro_to_string(Var)++"("++implode(lists:map(R,Args),",")++")";
to_str([])  -> "";
to_str({H}) -> to_str(H);
to_str([H]) -> to_str(H);
to_str({open,O,close}) -> "(" ++ to_str(O) ++ ")";
to_str({L,O,R}) -> to_str(L) ++ operator_to_string(O) ++ to_str(R);
to_str({string,String}) -> String;
to_str({integer,Val}) -> integer_to_list(Val);
to_str({float,Val}) ->
  case (Val-round(Val)) of
    0.0 -> integer_to_list(round(Val));
    _   -> float_to_list(Val)
  end;
to_str({abs_ref,Y,X,rel_row,rel_col}) -> util2:make_b26(X)++integer_to_list(Y);
to_str({abs_ref,Y,X,abs_row,rel_col}) -> util2:make_b26(X)++integer_to_list(Y)++"$";
to_str({abs_ref,Y,X,rel_row,abs_col}) -> util2:make_b26(X)++"$"++integer_to_list(Y);
to_str({abs_ref,Y,X,abs_row,abs_col}) -> util2:make_b26(X)++"$"++integer_to_list(Y)++"$";
to_str({array1D,Array}) -> array_to_str(Array,["{"]).

array_to_str([],[Comma|Tail]) -> % cut of the additional ","
  TokenArray=lists:reverse(["}"|Tail]),
  io:format("in excel_rev_comp:array_to_str TokenArray is ~p~n",[TokenArray]),
  io:format("in excel_rev_comp:array_to_str TokenArray is ~p~n",[TokenArray]),
  lists:flatten(TokenArray);
array_to_str([H|T],Residuum)-> 
  io:format("in excel_rev_comp:array_to_str H is ~p T is ~p~n-Residuuum is ~p~n",
      [H,T,Residuum]),
  NewResiduum=[to_str(H)|Residuum],
  NewResiduum2=[","|NewResiduum],
  io:format("in excel_rev_comp:array_to_str NewResiduuum2 is ~p~n",[NewResiduum2]),
  array_to_str(T,NewResiduum2).

push([],Val)    -> [lists:append([],Val)];
push(List,Val)  -> 
  %%io:format("in excel_rev_comp:push List is ~p~n- Val is ~p~n",[List,Val]),
  lists:reverse(lists:flatten([Val,lists:reverse(List)])).

pop([H|[]]) -> {H,[]};
pop(List)   -> [Pop|NewList]=lists:reverse(List),
               {Pop,lists:reverse(NewList)}.

%% this function makes a range from the start and end cell specifications
make_range(StartCell,EndCell)->
  io:format("in excel_rev_comp:make_range StartCell is ~p~n-EndCell is ~p~n",
    [StartCell,EndCell]),
    Return=make_cell(StartCell)++":"++make_cell(EndCell),
    io:format("in excel_rev_comp Return is ~p~n",[Return]),
    Return.

%% make a cell from 0-Indexed Row and Column indices
%% - this means adding a 1 to the Indices
make_cell({Row,Col,rel_row,rel_col}) -> string:to_upper(util2:make_b26(Col+1)++integer_to_list(Row+1));
make_cell({Row,Col,abs_row,rel_col}) -> string:to_upper(util2:make_b26(Col+1)++integer_to_list(Row+1)++"$");
make_cell({Row,Col,rel_row,abs_col}) -> string:to_upper(util2:make_b26(Col+1)++"$"++integer_to_list(Row+1));
make_cell({Row,Col,abs_row,abs_col}) -> string:to_upper(util2:make_b26(Col+1)++"$"++integer_to_list(Row+1)++"$").

%% this function looks up the Func ID and converts it to a name
%%
%% Every 10 lines there is a comment dumped from the file xldumper.dat in the OpenOffice source code
%% and gives the numerical lookup for the function names *INCLUDING* the VB ones
%%
%%   0=COUNT,IF,ISNA,ISERROR,SUM,AVERAGE,MIN,MAX,ROW,COLUMN
macro_to_string(0)   -> "COUNT";
macro_to_string(1)   -> "IF";
macro_to_string(2)   -> "ISNA";
macro_to_string(3)   -> "ISERROR";
macro_to_string(4)   -> "SUM";
macro_to_string(5)   -> "AVERAGE";
macro_to_string(6)   -> "MIN";
macro_to_string(7)   -> "MAX";
macro_to_string(8)   -> "ROW";
macro_to_string(9)   -> "COLUMN";
%%   10=NA,NPV,STDEV,DOLLAR,FIXED,SIN,COS,TAN,ATAN,PI
macro_to_string(10)  -> "NA";
macro_to_string(11)  -> "NPV";
macro_to_string(12)  -> "STDEV";
macro_to_string(13)  -> "DOLLAR";
macro_to_string(14)  -> "FIXED";
macro_to_string(15)  -> "SIN";
macro_to_string(16)  -> "COS";
macro_to_string(17)  -> "TAN";
macro_to_string(18)  -> "ATAN";
macro_to_string(19)  -> "PI";
%%   20=SQRT,EXP,LN,LOG10,ABS,INT,SIGN,ROUND,LOOKUP,INDEX
macro_to_string(20)  -> "SQRT";
macro_to_string(21)  -> "EXP";
macro_to_string(22)  -> "LN";
macro_to_string(23)  -> "LOG10";
macro_to_string(24)  -> "ABS";
macro_to_string(25)  -> "INT";
macro_to_string(26)  -> "SIGN";
macro_to_string(27)  -> "ROUND";
macro_to_string(28)  -> "LOOKUP";
macro_to_string(29)  -> "INDEX";
%%   30=REPT,MID,LEN,VALUE,TRUE,FALSE,AND,OR,NOT,MOD
macro_to_string(30)  -> "REPT";
macro_to_string(31)  -> "MID";
macro_to_string(32)  -> "LEN";
macro_to_string(33)  -> "VALUE";
macro_to_string(34)  -> "TRUE";
macro_to_string(35)  -> "FALSE";
macro_to_string(36)  -> "AND";
macro_to_string(37)  -> "OR";
macro_to_string(38)  -> "NOT";
macro_to_string(39)  -> "MOD";
%%   40=DCOUNT,DSUM,DAVERAGE,DMIN,DMAX,DSTDEV,VAR,DVAR,TEXT,LINEST
macro_to_string(40)  -> "DCOUNT";
macro_to_string(41)  -> "DSUM";
macro_to_string(42)  -> "DAVERAGE";
macro_to_string(43)  -> "DMIN";
macro_to_string(44)  -> "DMAX";
macro_to_string(45)  -> "DSTDEV";
macro_to_string(46)  -> "VAR";
macro_to_string(47)  -> "DVAR";
macro_to_string(48)  -> "TEXT";
macro_to_string(49)  -> "LINEST";
%%   50=TREND,LOGEST,GROWTH,GOTO,HALT,RETURN,PV,FV,NPER,PMT
macro_to_string(50)  -> "TREND";
macro_to_string(51)  -> "LOGEST";
macro_to_string(52)  -> "GROWTH";
macro_to_string(56)  -> "PV";
macro_to_string(57)  -> "FV";
macro_to_string(58)  -> "NPER";
macro_to_string(59)  -> "PMT";
%%  60=RATE,MIRR,IRR,RAND,MATCH,DATE,TIME,DAY,MONTH,YEAR
macro_to_string(60)  -> "RATE";
macro_to_string(61)  -> "MIRR";
macro_to_string(62)  -> "IRR";
macro_to_string(63)  -> "RAND";
macro_to_string(64)  -> "MATCH";
macro_to_string(65)  -> "DATE";
macro_to_string(66)  -> "TIME";
macro_to_string(67)  -> "DAY";
macro_to_string(68)  -> "MONTH";
macro_to_string(69)  -> "YEAR";
%%  70=WEEKDAY,HOUR,MINUTE,SECOND,NOW,AREAS,ROWS,COLUMNS,OFFSET,ABSREF
macro_to_string(70)  -> "WEEKDAY";
macro_to_string(71)  -> "HOUR";
macro_to_string(72)  -> "MINUTE";
macro_to_string(73)  -> "SECOND";
macro_to_string(74)  -> "NOW";
macro_to_string(75)  -> "AREAS";
macro_to_string(76)  -> "ROWS";
macro_to_string(77)  -> "COLUMNS";
macro_to_string(78)  -> "OFFSET";
%%  80=RELREF,ARGUMENT,SEARCH,TRANSPOSE,ERROR,STEP,TYPE,ECHO,SET.NAME,CALLER
macro_to_string(82)  -> "SEARCH";
macro_to_string(83)  -> "TRANSPOSE";
macro_to_string(86)  -> "TYPE";
%%  90=DEREF,WINDOWS,SERIES,DOCUMENTS,ACTIVE.CELL,SELECTION,RESULT,ATAN2,ASIN,ACOS
macro_to_string(97)  -> "ATAN2";
macro_to_string(98)  -> "ASIN";
macro_to_string(99)  -> "ACOS";
%%  100=CHOOSE,HLOOKUP,VLOOKUP,LINKS,INPUT,ISREF,GET.FORMULA,GET.NAME,SET.VALUE,LOG
macro_to_string(100) -> "CHOOSE";
macro_to_string(101) -> "HLOOKUP";
macro_to_string(102) -> "VLOOKUP";
macro_to_string(105) -> "ISREF";
macro_to_string(109) -> "LOG";
%%  110=EXEC,CHAR,LOWER,UPPER,PROPER,LEFT,RIGHT,EXACT,TRIM,REPLACE
macro_to_string(111) -> "CHAR";
macro_to_string(112) -> "UPPER";
macro_to_string(113) -> "LOWER";
macro_to_string(114) -> "PROPER";
macro_to_string(115) -> "LEFT";
macro_to_string(116) -> "RIGHT";
macro_to_string(117) -> "EXACT";
macro_to_string(118) -> "TRIM";
macro_to_string(119) -> "REPLACE";
%%  120=SUBSTITUTE,CODE,NAMES,DIRECTORY,FIND,CELL,ISERR,ISTEXT,ISNUMBER,ISBLANK
macro_to_string(120) -> "SUBSTITUTE";
macro_to_string(121) -> "CODE";
macro_to_string(124) -> "FIND";
macro_to_string(125) -> "CELL";
macro_to_string(126) -> "ISERR";
macro_to_string(127) -> "ISTEXT";
macro_to_string(128) -> "ISNUMBER";
macro_to_string(129) -> "ISBLANK";
%%  130=T,N,FOPEN,FCLOSE,FSIZE,FREADLN,FREAD,FWRITELN,FWRITE,FPOS
macro_to_string(130) -> "T";
macro_to_string(131) -> "N";
%% 140=DATEVALUE,TIMEVALUE,SLN,SYD,DDB,GET.DEF,REFTEXT,TEXTREF,INDIRECT,REGISTER
macro_to_string(140) -> "DATEVALUE";
macro_to_string(141) -> "TIMEVALUE";
macro_to_string(142) -> "SLN";
macro_to_string(143) -> "SYD";
macro_to_string(144) -> "DDB";
macro_to_string(148) -> "INDIRECT";
%%  150=CALL,ADD.BAR,ADD.MENU,ADD.COMMAND,ENABLE.COMMAND,CHECK.COMMAND,RENAME.COMMAND,SHOW.BAR,DELETE.MENU,DELETE.COMMAND

%% 160=GET.CHART.ITEM,DIALOG.BOX,CLEAN,MDETERM,MINVERSE,MMULT,FILES,IPMT,PPMT,COUNTA
macro_to_string(162) -> "CLEAN";
macro_to_string(163) -> "MDETERM";
macro_to_string(164) -> "MINVERSE";
macro_to_string(165) -> "MMULT";
macro_to_string(167) -> "IPMT";
macro_to_string(168) -> "PPMT";
macro_to_string(169) -> "COUNTA";
%%  170=CANCEL.KEY,FOR,WHILE,BREAK,NEXT,INITIATE,REQUEST,POKE,EXECUTE,TERMINATE

%%  180=RESTART,HELP,GET.BAR,PRODUCT,FACT,GET.CELL,GET.WORKSPACE,GET.WINDOW,GET.DOCUMENT,DPRODUCT
macro_to_string(183) -> "PRODUCT";
macro_to_string(184) -> "FACT";
macro_to_string(189) -> "DPRODUCT";
%%  190=ISNONTEXT,GET.NOTE,NOTE,STDEVP,VARP,DSTDDEVP,DVARP,TRUNC,ISLOGICAL,DBCOUNTA
macro_to_string(190) -> "ISNONTEXT";
macro_to_string(193) -> "STDEVP";
macro_to_string(194) -> "VARP";
macro_to_string(195) -> "DSDEVP";
macro_to_string(196) -> "DVARP";
macro_to_string(197) -> "TRUNC";
macro_to_string(198) -> "ISLOGICAL";
macro_to_string(199) -> "DCOUNTA";
%%  200=DELETE.BAR,UNREGISTER,,USDOLLAR,FINDB,SEARCHB,REPLACEB,LEFTB,RIGHTB
%%  the lookup file appears to contain 'not enought' records to make this work (ie 9 not 10)
macro_to_string(204) -> "USDOLLAR";
macro_to_string(205) -> "FINDB";
macro_to_string(206) -> "SEARCHB";
macro_to_string(207) -> "REPLACEB";
macro_to_string(208) -> "LEFTB";
macro_to_string(209) -> "RIGHTB";
%%  210=MIDB,LENB,ROUNDUP,ROUNDDOWN,ASC,DBCS,RANK,,,ADDRESS
macro_to_string(210) -> "MIDB";
macro_to_string(211) -> "LENB";
macro_to_string(212) -> "ROUNDUP";
macro_to_string(213) -> "ROUNDDOWN";
macro_to_string(214) -> "ASC";
macro_to_string(215) -> "DBSC";
macro_to_string(216) -> "RANK";
macro_to_string(219) -> "ADDRESS";
%%  220=DAYS360,TODAY,VDB,ELSE,ELSE.IF,END.IF,FOR.CELL,MEDIAN,SUMPRODUCT,SINH
macro_to_string(220) -> "DAYS360";
macro_to_string(221) -> "TODAY";
macro_to_string(222) -> "VDB";
macro_to_string(227) -> "MEDIAN";
macro_to_string(228) -> "SUMPRODUCT";
macro_to_string(229) -> "SINH";
%%  230=COSH,TANH,ASINH,ACOSH,ATANH,DGET,CREATE.OBJECT,VOLATILE,LAST.ERROR,CUSTOM.UNDO
macro_to_string(230) -> "COSH";
macro_to_string(231) -> "TANH";
macro_to_string(232) -> "ASINH";
macro_to_string(233) -> "ACOSH";
macro_to_string(234) -> "ATANH";
macro_to_string(235) -> "DGET";
%%  240=CUSTOM.REPEAT,FORMULA.CONVERT,GET.LINK.INFO,TEXT.BOX,INFO,GROUP,GET.OBJECT,DB,PAUSE,
macro_to_string(244) -> "INFO";
macro_to_string(247) -> "DB";
%%  250=,RESUME,FREQUENCY,ADD.TOOLBAR,DELETE.TOOLBAR,EXTERN.CALL,RESET.TOOLBAR,EVALUATE,GET.TOOLBAR,GET.TOOL
macro_to_string(252) -> "FREQUENCY";
%%  260=SPELLING.CHECK,ERROR.TYPE,APP.TITLE,WINDOW.TITLE,SAVE.TOOLBAR,ENABLE.TOOL,PRESS.TOOL,REGISTER.ID,GET.WORKBOOK,AVEDEV
macro_to_string(261) -> "ERRORTYPE";
macro_to_string(269) -> "AVEDEV";
%%  270=BETADIST,GAMMALN,BETAINV,BINOMDIST,CHIDIST,CHIINV,COMBIN,CONFIDENCE,CRITBINOM,EVEN
macro_to_string(270) -> "BETADIST";
macro_to_string(271) -> "GAMMALN";
macro_to_string(272) -> "BETAINV";
macro_to_string(273) -> "BINOMDIST";
macro_to_string(274) -> "CHIDIST";
macro_to_string(275) -> "CHIINV";
macro_to_string(276) -> "COMBIN";
macro_to_string(277) -> "CONFIDENCE";
macro_to_string(278) -> "CRITBINOM";
macro_to_string(279) -> "EVEN";
%%  280=EXPONDIST,FDIST,FINV,FISHER,FISHERINV,FLOOR,GAMMADIST,GAMMAINV,CEILING,HYPGEOMDIST
macro_to_string(280) -> "EXPONDIST";
macro_to_string(281) -> "FDIST";
macro_to_string(282) -> "FINV";
macro_to_string(283) -> "FISHER";
macro_to_string(284) -> "FISHERINV";
macro_to_string(285) -> "FLOOR";
macro_to_string(286) -> "GAMMADIST";
macro_to_string(287) -> "GAMMAINV";
macro_to_string(288) -> "CEILING";
macro_to_string(289) -> "HYPGEOMVERT";
%%  290=LOGNORMDIST,LOGINV,NEGBINOMDIST,NORMDIST,NORMSDIST,NORMINV,NORMSINV,STANDARDIZE,ODD,PERMUT
macro_to_string(290) -> "LOGNORMDIST";
macro_to_string(291) -> "LOGINV";
macro_to_string(292) -> "NEGBINOMDIST";
macro_to_string(293) -> "NORMDIST";
macro_to_string(294) -> "NORMSDIST";
macro_to_string(295) -> "NORMINV";
macro_to_string(296) -> "NORMSINB";
macro_to_string(297) -> "STANDARDIZE";
macro_to_string(298) -> "ODD";
macro_to_string(299) -> "PERMUT";
%%  300=POISSON,TDIST,WEIBULL,SUMXMY2,SUMX2MY2,SUMX2PY2,CHITEST,CORREL,COVAR,FORECAST
macro_to_string(300) -> "POISSON";
macro_to_string(301) -> "TDIST";
macro_to_string(302) -> "WEIBULL";
macro_to_string(303) -> "SUMXMY2";
macro_to_string(304) -> "SUMX2MY2";
macro_to_string(305) -> "SUMX2PY2";
macro_to_string(306) -> "CHITEST";
macro_to_string(307) -> "CORREL";
macro_to_string(308) -> "COVAR";
macro_to_string(309) -> "FORECAST";
%%  310=FTEST,INTERCEPT,PEARSON,RSQ,STEYX,SLOPE,TTEST,PROB,DEVSQ,GEOMEAN
macro_to_string(310) -> "FTEST";
macro_to_string(311) -> "INTERCEPT";
macro_to_string(312) -> "PEARSON";
macro_to_string(313) -> "RSQ";
macro_to_string(314) -> "STEYX";
macro_to_string(315) -> "SLOPE";
macro_to_string(316) -> "TTEST";
macro_to_string(317) -> "PROB";
macro_to_string(318) -> "DEVSQ";
macro_to_string(319) -> "GEOMEAN";
%%   320=HARMEAN,SUMSQ,KURT,SKEW,ZTEST,LARGE,SMALL,QUARTILE,PERCENTILE,PERCENTRANK
macro_to_string(320) -> "HARMEAN";
macro_to_string(321) -> "SUMSQ";
macro_to_string(322) -> "KURT";
macro_to_string(323) -> "SKEW";
macro_to_string(324) -> "ZTEST";
macro_to_string(325) -> "LARGE";
macro_to_string(326) -> "SMALL";
macro_to_string(327) -> "QUARTILE";
macro_to_string(328) -> "PERCENTILE";
macro_to_string(329) -> "PERCENTRANK";
%%  330=MODE,TRIMMEAN,TINV,,MOVIE.COMMAND,GET.MOVIE,CONCATENATE,POWER,PIVOT.ADD.DATA,GET.PIVOT.TABLE
macro_to_string(330) -> "MODE";
macro_to_string(331) -> "TRIMMEAN";
macro_to_string(332) -> "TINV";
macro_to_string(336) -> "CONCATENATE";
macro_to_string(337) -> "POWER";
%%  340=GET.PIVOT.FIELD,GET.PIVOT.ITEM,RADIANS,DEGREES,SUBTOTAL,SUMIF,COUNTIF,COUNTBLANK,SCENARIO.GET,OPTIONS.LISTS.GET
macro_to_string(342) -> "RADIANS";
macro_to_string(343) -> "DEGREES";
macro_to_string(344) -> "SUBOTAL";
macro_to_string(345) -> "SUMIF";
macro_to_string(346) -> "COUNTIF";
macro_to_string(347) -> "COUNTBLANK";
%%  350=ISPMT,DATEDIF,DATESTRING,NUMBERSTRING,ROMAN,OPEN.DIALOG,SAVE.DIALOG,VIEW.GET,GETPIVOTDATA,HYPERLINK
macro_to_string(350) -> "ISPMT";
macro_to_string(351) -> "DATEDIF";
macro_to_string(352) -> "DATESTRING";
macro_to_string(353) -> "NUMBERSTRING";
macro_to_string(354) -> "ROMAN";
macro_to_string(358) -> "GETPIVOTDATA";
macro_to_string(359) -> "HYPERLINK";
%%  360=PHONETIC,AVERAGEA,MAXA,MINA,STDEVPA,VARPA,STDEVA,VARA,BAHTTEXT,THAIDAYOFWEEK
macro_to_string(360) -> "PHONETIC";
macro_to_string(361) -> "AVERAGEA";
macro_to_string(362) -> "MAXA";
macro_to_string(363) -> "MINA";
macro_to_string(364) -> "STDEVPA";
macro_to_string(365) -> "VARPA";
macro_to_string(366) -> "STDEVA";
macro_to_string(367) -> "VARA".
%%  370=THAIDIGIT,THAIMONTHOFYEAR,THAINUMSOUND,THAINUMSTRING,THAISTRINGLENGTH,ISTHAIDIGIT,ROUNDBAHTDOWN,ROUNDBAHTUP,THAIYEAR,RTD
%%  380=ISHYPERLINK

%% This function looks up the number of parameters that a function takes
%% It only handles functions with fixed numbers of parameters...
%% ie ones where Min Par is the same as Max par in table 3.11.1
%% of excelfileformats.v1.40
%%
%% These functions will all be referenced by a token of type tFunc as opposed
%% one of type tFuncVar/tFuncVarV/tFuncVarR/tFuncVarA
macro_no_of_args(2) -> 1;
macro_no_of_args(3) -> 1;
macro_no_of_args(10) -> 0;
macro_no_of_args(15) -> 1;
macro_no_of_args(16) -> 1;
macro_no_of_args(17) -> 1;
macro_no_of_args(18) -> 1;
macro_no_of_args(19) -> 0;
macro_no_of_args(20) -> 1;
macro_no_of_args(21) -> 1;
macro_no_of_args(22) -> 1;
macro_no_of_args(23) -> 1;
macro_no_of_args(24) -> 1;
macro_no_of_args(25) -> 1;
macro_no_of_args(26) -> 1;
macro_no_of_args(27) -> 2;
macro_no_of_args(30) -> 2;
macro_no_of_args(31) -> 3;
macro_no_of_args(32) -> 1;
macro_no_of_args(33) -> 1;
macro_no_of_args(34) -> 0;
macro_no_of_args(35) -> 0;
macro_no_of_args(38) -> 1;
macro_no_of_args(39) -> 2;
macro_no_of_args(40) -> 3;
macro_no_of_args(41) -> 3;
macro_no_of_args(42) -> 3;
macro_no_of_args(43) -> 3;
macro_no_of_args(44) -> 3;
macro_no_of_args(45) -> 3;
macro_no_of_args(47) -> 3;
macro_no_of_args(48) -> 2;
macro_no_of_args(61) -> 3;
macro_no_of_args(63) -> 0;
macro_no_of_args(65) -> 3;
macro_no_of_args(66) -> 3;
macro_no_of_args(67) -> 1;
macro_no_of_args(68) -> 1;
macro_no_of_args(69) -> 1;
macro_no_of_args(70) -> 1;
macro_no_of_args(71) -> 1;
macro_no_of_args(72) -> 1;
macro_no_of_args(73) -> 1;
macro_no_of_args(74) -> 0;
macro_no_of_args(75) -> 1;
macro_no_of_args(76) -> 1;
macro_no_of_args(77) -> 1;
macro_no_of_args(83) -> 1;
macro_no_of_args(86) -> 1;
macro_no_of_args(97) -> 2;
macro_no_of_args(98) -> 1;
macro_no_of_args(99) -> 1;
macro_no_of_args(101) -> 3;
macro_no_of_args(102) -> 3;
macro_no_of_args(105) -> 1;
macro_no_of_args(111) -> 1;
macro_no_of_args(112) -> 1;
macro_no_of_args(113) -> 1;
macro_no_of_args(114) -> 1;
macro_no_of_args(117) -> 2;
macro_no_of_args(118) -> 1;
macro_no_of_args(119) -> 4;
macro_no_of_args(121) -> 1;
macro_no_of_args(126) -> 1;
macro_no_of_args(127) -> 1;
macro_no_of_args(128) -> 1;
macro_no_of_args(129) -> 1;
macro_no_of_args(130) -> 1;
macro_no_of_args(131) -> 1;
macro_no_of_args(140) -> 1;
macro_no_of_args(141) -> 1;
macro_no_of_args(142) -> 3;
macro_no_of_args(143) -> 4;
macro_no_of_args(162) -> 1;
macro_no_of_args(163) -> 1;
macro_no_of_args(164) -> 1;
macro_no_of_args(165) -> 2;
macro_no_of_args(184) -> 1;
macro_no_of_args(189) -> 3;
macro_no_of_args(190) -> 1;
macro_no_of_args(195) -> 3;
macro_no_of_args(196) -> 3;
macro_no_of_args(197) -> 1;
macro_no_of_args(198) -> 1;
macro_no_of_args(199) -> 3;
macro_no_of_args(207) -> 4;
macro_no_of_args(210) -> 3;
macro_no_of_args(211) -> 1;
macro_no_of_args(212) -> 2;
macro_no_of_args(213) -> 2;
macro_no_of_args(214) -> 1;
macro_no_of_args(215) -> 1;
macro_no_of_args(220) -> 2;
macro_no_of_args(221) -> 0;
macro_no_of_args(229) -> 1;
macro_no_of_args(230) -> 1;
macro_no_of_args(231) -> 1;
macro_no_of_args(232) -> 1;
macro_no_of_args(233) -> 1;
macro_no_of_args(234) -> 1;
macro_no_of_args(235) -> 3;
macro_no_of_args(244) -> 1;
macro_no_of_args(252) -> 2;
macro_no_of_args(261) -> 1;
macro_no_of_args(271) -> 1;
macro_no_of_args(273) -> 4;
macro_no_of_args(274) -> 2;
macro_no_of_args(275) -> 2;
macro_no_of_args(276) -> 2;
macro_no_of_args(277) -> 3;
macro_no_of_args(278) -> 3;
macro_no_of_args(279) -> 1;
macro_no_of_args(280) -> 3;
macro_no_of_args(281) -> 3;
macro_no_of_args(282) -> 3;
macro_no_of_args(283) -> 1;
macro_no_of_args(284) -> 1;
macro_no_of_args(285) -> 2;
macro_no_of_args(286) -> 4;
macro_no_of_args(287) -> 3;
macro_no_of_args(288) -> 2;
macro_no_of_args(289) -> 4;
macro_no_of_args(290) -> 3;
macro_no_of_args(291) -> 3;
macro_no_of_args(292) -> 3;
macro_no_of_args(293) -> 4;
macro_no_of_args(294) -> 1;
macro_no_of_args(295) -> 3;
macro_no_of_args(296) -> 1;
macro_no_of_args(297) -> 3;
macro_no_of_args(298) -> 1;
macro_no_of_args(299) -> 2;
macro_no_of_args(300) -> 3;
macro_no_of_args(301) -> 3;
macro_no_of_args(302) -> 4;
macro_no_of_args(303) -> 2;
macro_no_of_args(304) -> 2;
macro_no_of_args(305) -> 2;
macro_no_of_args(306) -> 2;
macro_no_of_args(307) -> 2;
macro_no_of_args(308) -> 2;
macro_no_of_args(309) -> 3;
macro_no_of_args(310) -> 2;
macro_no_of_args(311) -> 2;
macro_no_of_args(312) -> 2;
macro_no_of_args(313) -> 2;
macro_no_of_args(314) -> 2;
macro_no_of_args(315) -> 2;
macro_no_of_args(316) -> 4;
macro_no_of_args(325) -> 2;
macro_no_of_args(326) -> 2;
macro_no_of_args(327) -> 2;
macro_no_of_args(328) -> 2;
macro_no_of_args(331) -> 2;
macro_no_of_args(332) -> 2;
macro_no_of_args(337) -> 2;
macro_no_of_args(342) -> 1;
macro_no_of_args(343) -> 1;
macro_no_of_args(346) -> 2;
macro_no_of_args(347) -> 1;
macro_no_of_args(350) -> 4;
macro_no_of_args(351) -> 3;
macro_no_of_args(352) -> 1;
macro_no_of_args(353) -> 2;
%% I am forcing GETPIVOTDATA to have a fixed 2 argument list even
%% though I know it doesn't
%% it **OUGHT** to appear in a tFuncVar token only, but I
%% suspect it is a tFunc when there are 2 attributes
%% Don't know enought about Pivot Tables to test this so wil
%% bodge for now
%%
%% GG 2007_01_18
macro_no_of_args(358) -> 
    io:format("in excel_rev_comp:macro_no_of_args for GETPIVOTDATA this is a cludge!~n"),
    2;
macro_no_of_args(360) -> 1.
