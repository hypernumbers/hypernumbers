%%%-------------------------------------------------------------------
%%% File        : excel_tokens.erl
%%% Author      : Gordon Guthrie <gordon@hypernumberftAs.com>
%%% Description : parses the RPN tokens what Excel uses to store
%%%               the formulae in cells
%%%
%%% Created     : 11th Jan 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(excel_tokens).

-export([parse_tokens/4]).

%%% Include files with macros encoding Microsoft File Format constants
-include("excel_base_tokens.hrl").
-include("excel_classified_tokens.hrl").
-include("excel_attributes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process formulas as described in Section 3 of            %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_tokens(<<>>,_Name,TokenArray,Residuum)->
    Tokens=lists:reverse(Residuum),
    {Tokens,TokenArray};
parse_tokens(Bin,Name,TokenArray,Residuum)->
    <<BaseTokenID:8/little-unsigned-integer,Rest/binary>>=Bin,
    parse_tokens(BaseTokenID,Name,Bin,TokenArray,Residuum).

%% Parsing base tokens
%% tExp
parse_tokens(?tExp,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{expr_formula_range,
                                      {tExp,[{sheet,Name},{row_index,RowIndex},
                                             {col_index,ColIndex}],
                                       {return,none}}}|Residuum]);

%% tTbl
parse_tokens(?tTbl,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{table_formula_range,
                                      {tRbl,[{row_index,RowIndex},
                                             {col_index,ColIndex}],
                                       {return,none}}}|Residuum]);
%% tAdd
parse_tokens(?tAdd,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{addition,{tAdd,[{op_type,binary}],
                                                {return,value}}}
                                     |Residuum]);

%% tSub
parse_tokens(?tSub,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{subtraction,{tSub,[{op_type,binary}],
                                                   {return,value}}}
                                     |Residuum]);

%% tMul
parse_tokens(?tMul,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{multiply,{tMul,[{op_type,binary}],
                                                {return,value}}}
                                     |Residuum]);

%% tDiv
parse_tokens(?tDiv,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{divide,{tDiv,[{op_type,binary}],
                                              {return,value}}}
                                     |Residuum]);

%% tPower
parse_tokens(?tPower,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{power,{tPower,[{op_type,binary}],
                                             {return,value}}}
                                     |Residuum]);

%% tConcat
parse_tokens(?tConcat,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{concatenate,{tConcat,[{op_type,binary}],
                                                   {return,value}}}
                                     |Residuum]);

%% tLT
parse_tokens(?tLT,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{less_than,{tLT,[{op_type,binary}],
                                                 {return,value}}}
                                     |Residuum]);

%% tLE
parse_tokens(?tLE,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{less_than_or_equal,
                                      {tLE,[{op_type,binary}],
                                       {return,value}}}|Residuum]);

%% tEQ
parse_tokens(?tEQ,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{equals,{tEQ,[{op_type,binary}],
                                              {return,value}
                                             }}
                                     |Residuum]);

%% tGE
parse_tokens(?tGE,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{greater_than_or_equal,
                                      {tGE,[{op_type,binary}],
                                       {return,value}}}
                                     |Residuum]);

%% tGT
parse_tokens(?tGT,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{greater_than,
                                      {tGT,[{op_type,binary}],{return,value}}}
                                     |Residuum]);

%% tNE
parse_tokens(?tNE,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{not_equal,{tNE,{op_type,binary},
                                                 {return,value}}}
                                     |Residuum]);

%% tIsect
parse_tokens(?tIsect,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{intersect,{tIsect,[{op_type,binary}],
                                                 {return,reference}}}
                                     |Residuum]);

%% tList
parse_tokens(?tList,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{list,{tList,[{op_type,binary}],
                                            {return,reference}}}
                                     |Residuum]);
%% tRange
parse_tokens(?tRange,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{range,{tRange,[{op_type,binary}],
                                             {return,reference}}}
                                     |Residuum]);
%% tUplus
parse_tokens(?tUplus,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{plus,{tUplus,[{op_type,unary}],
                                            {return,value}
                                           }}|Residuum]);

%% tUminus
parse_tokens(?tUminus,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{minus,{tUminus,[{op_type,unary}],
                                             {return,value}}}
                                     |Residuum]);
%% tPercent
parse_tokens(?tPercent,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{percent,{tPercent,[{op_type,unary}],
                                               {return,value}}}
                                     |Residuum]);
%% tParen
parse_tokens(?tParen,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{parentheses,{tParen,[],
                                                   {return,none}}}|Residuum]);

%% tMissArg
parse_tokens(?tMissArg,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{missing_argument,
                                      {tMissArg,[],
                                       {return,value}}}|Residuum]);

%% tStr
parse_tokens(?tStr,Name,Bin,TokenArray,Residuum)->
    %% all strings in formulae have a 1-byte index
    <<_Tk:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    <<Len:8/little-unsigned-integer,
     NFlags:8/little-unsigned-integer,
     R3/binary>>=Rest,
    BinLen=excel_util:get_len_CRS_Uni16(Len,1,R3,NFlags),
    %% The 2nd byte which contains the encoding flags needs to be included...
    %% if the String is Rich Text or Asian this next bit might blow up
    %% (who knows!)
    <<StrBin:BinLen/binary,R4/binary>>=Rest,
    String2=excel_util:get_utf8(excel_util:parse_CRS_Uni16(StrBin,1)),
    String3=list_to_binary(lists:flatten([$",String2,$"])),
    parse_tokens(R4,Name,TokenArray,[{string,{tStr,[{value,String3}],
                                              {return,value}}}|Residuum]);

%% tNlr
parse_tokens(?tNlr,Name,Bin,TokenArray,Residuum)->
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[Tk|Residuum]);

%% tAttr
parse_tokens(?tAttr,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    Return=parse_attr(R2),
    {Token,R3}=Return,
    parse_tokens(R3,Name,TokenArray,[Token|Residuum]);

%% tErr
parse_tokens(?tErr,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Error:8/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{error,{tErr,[{value,Error}],
                                             {return,none}}}|Residuum]);

%% tBool
parse_tokens(?tBool,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Bool:8/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{boolean,{tBool,[{value,Bool}],
                                               {return,value}}}|Residuum]);

%% tInt
parse_tokens(?tInt,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Int:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{integer,{tInt,[{value,Int}],
                                               {return,value}}}|Residuum]);

%% tNum
parse_tokens(?tNum,Name,Bin,TokenArray,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Num:64/little-float,
     R2/binary>>=Bin,
    %%io:format("in excel_tokens:parse_tokens for tNum Num is ~p~n",[Num]),
    parse_tokens(R2,Name,TokenArray,[{number,{tNum,[{value,Num}],
                                              {return,value}}}|Residuum]);

%% Parsing Classified Tokens
%%
%% Classifieds come in three flavours - this bloq boils them
%% down to one call - these come after

%% tArray
parse_tokens(?tArrayR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArray,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tArrayV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArray,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tArrayA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArray,Name,Bin,TokenArray,array,Residuum);

%% tFuncR
parse_tokens(?tFuncR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFunc,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tFuncV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFunc,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tFuncA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFunc,Name,Bin,TokenArray,array,Residuum);

%% tFuncVar
parse_tokens(?tFuncVarR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFuncVar,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tFuncVarV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFuncVar,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tFuncVarA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tFuncVar,Name,Bin,TokenArray,array,Residuum);

%% tName
parse_tokens(?tNameR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tName,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tNameV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tName,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tNameA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tName,Name,Bin,TokenArray,array,Residuum);

%% tRef
parse_tokens(?tRefR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tRefV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tRefA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef,Name,Bin,TokenArray,array,Residuum);

%% tArea
parse_tokens(?tAreaR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tAreaV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tAreaA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea,Name,Bin,TokenArray,array,Residuum);

%% tMemArea
parse_tokens(?tMemAreaR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemArea,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tMemAreaV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemArea,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tMemAreaA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemArea,Name,Bin,TokenArray,array,Residuum);

%% tMemErr
parse_tokens(?tMemErrR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemErr,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tMemErrV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemErr,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tMemErrA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemErr,Name,Bin,TokenArray,array,Residuum);

%% tMemNoMem
parse_tokens(?tMemNoMemR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemNoMem,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tMemNoMemV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemNoMem,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tMemNoMemA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemNoMem,Name,Bin,TokenArray,array,Residuum);

%% tMemFunc
parse_tokens(?tMemFuncR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemFunc,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tMemFuncV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemFunc,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tMemFuncA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tMemFunc,Name,Bin,TokenArray,array,Residuum);

%% tRefErr
parse_tokens(?tRefErrR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tRefErrV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tRefErrA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr,Name,Bin,TokenArray,array,Residuum);

%% tAreaErr
parse_tokens(?tAreaErrR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tAreaErrV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tAreaErrA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr,Name,Bin,TokenArray,array,Residuum);

%% tRefN
parse_tokens(?tRefNR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefN,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tRefNV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefN,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tRefNA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefN,Name,Bin,TokenArray,array,Residuum);

%% tAreaN
parse_tokens(?tAreaNR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaN,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tAreaNV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaN,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tAreaNA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaN,Name,Bin,TokenArray,array,Residuum);

%% tMemAreaN
%% parse_tokens(?tMemAreaNR,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemAreaN,Name,Bin,TokenArray,reference,Residuum);
%% parse_tokens(?tMemAreaNV,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemAreaN,Name,Bin,TokenArray,value,Residuum);
%% parse_tokens(?tMemAreaNA,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemAreaN,Name,Bin,TokenArray,array,Residuum);

%% tMemNoMemN
%% parse_tokens(?tMemNoMemNR,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemNoMemN,Name,Bin,TokenArray,reference,Residuum);
%% parse_tokens(?tMemNoMemNV,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemNoMemN,Name,Bin,TokenArray,value,Residuum);
%% parse_tokens(?tMemNoMemNA,Name,Bin,TokenArray,Residuum)->
%%     parse_tokens(tMemNoMemN,Name,Bin,TokenArray,array,Residuum);

%% tNameX
parse_tokens(?tNameXR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tNameX,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tNameXV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tNameX,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tNameXA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tNameX,Name,Bin,TokenArray,array,Residuum);

%% tRef3d
parse_tokens(?tRef3dR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef3d,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tRef3dV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef3d,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tRef3dA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRef3d,Name,Bin,TokenArray,array,Residuum);

%% tArea3d
parse_tokens(?tArea3dR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea3d,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tArea3dV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea3d,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tArea3dA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tArea3d,Name,Bin,TokenArray,array,Residuum);

%% tRefErr3d
parse_tokens(?tRefErr3dR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr3d,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tRefErr3dV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr3d,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tRefErr3dA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tRefErr3d,Name,Bin,TokenArray,array,Residuum);

%% tAreaErr3d
parse_tokens(?tAreaErr3dR,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr3d,Name,Bin,TokenArray,reference,Residuum);
parse_tokens(?tAreaErr3dV,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr3d,Name,Bin,TokenArray,value,Residuum);
parse_tokens(?tAreaErr3dA,Name,Bin,TokenArray,Residuum)->
    parse_tokens(tAreaErr3d,Name,Bin,TokenArray,array,Residuum);

%% skip zero tokens
parse_tokens(0,Name,Bin,TokenArray,Residuum)->
    <<_Skip:8/little-unsigned-integer,_R/binary>>=Bin,
    parse_tokens(<<>>,Name,TokenArray,Residuum);

%% catch and terminate unexpected tokens
parse_tokens(Duff,_Name,Bin,_TokenArray,Residuum)->
    io:format("duff token parsed~p~n"++
              "binary is ~p~n"++
              "residuum is ~p~n",
              [Duff,Bin,Residuum]),
    exit("duff token parsed!").

%% these are the functions that parse the classified token sets

%% tArray
parse_tokens(tArray,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _Array:56/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{array_type,{tArray,[{type,Type}],
                                                  {return,array}}}
                                     |Residuum]);

%% tFunc
parse_tokens(tFunc,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Func:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{functional_index,
                                      {tFunc,[{value,Func},{type,Type}],
                                       {return,variant}}}|Residuum]);

%% tFuncVar
parse_tokens(tFuncVar,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Flags:8/little-unsigned-integer,
     Index:16/little-unsigned-integer,
     R2/binary>>=Bin,
    {NArgs,UserPrompt} = case Flags band 16#80 of
			     16#80 -> {Flags-16#80,true};
			     _     -> {Flags,false}
			 end,
    {FuncIndex,_FuncType} = case Index band 16#8000 of
                                16#8000 -> {Index-16#8000,built_in};
                                _       -> {Index,macro}
                            end,
    parse_tokens(R2,Name,TokenArray,[{var_func_idx,{tFuncVar,
                                                    [{value,FuncIndex},
                                                     {number_of_args,NArgs},
                                                     {user_prompt,UserPrompt},
                                                     {type,Type}],
                                                    {return,variant}
                                                   }}
                                     |Residuum]);

%% tName
parse_tokens(tName,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Index:16/little-unsigned-integer,
     _NotUsed:16/little-unsigned-integer,
     R2/binary>>=Bin,
    %% this is a *ONE* based index so drop it by one...
    parse_tokens(R2,Name,TokenArray,[{name_index,
                                      {tName,[{value,Index-1},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tRef
parse_tokens(tRef,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Ref:32/little-unsigned-integer,
     R2/binary>>=Bin,
    ParsedAddy=parse_cell_addy(<<Ref:32/little-unsigned-integer>>),
    parse_tokens(R2,Name,TokenArray,[{abs_ref,{tRef,[{value,ParsedAddy}|
                                                     {type,Type}],
                                               {return,reference}}}|Residuum]);

%% tArea
parse_tokens(tArea,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Range:8/binary,
     R2/binary>>=Bin,
    {ParsedStart,ParsedEnd}=parse_cell_range(Range),
    NewDetails=[{start_cell,ParsedStart}|{end_cell,ParsedEnd}],
    parse_tokens(R2,Name,TokenArray,[{absolute_area,
                                      {tArea,[NewDetails|{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tMemArea
parse_tokens(tMemArea,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     MemArea:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{memory_area,
                                      {tMemArea,[{value,MemArea},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tMemErr
parse_tokens(tMemErr,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{memory_err,
                                      {tMemErr,[{value,Size},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tMemNoMe
parse_tokens(tMemNoMem,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{memory_no_memory,
                                      {tMemNoMem,[{value,Size},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tMemFunc
parse_tokens(tMemFunc,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{memory_function,
                                      {tMemFunc,[{value,Size},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tRefErr
parse_tokens(tRefErr,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{reference_error,{tRefErr,[{type,Type}],
                                                       {return,reference}}}|
                                     Residuum]);

%% tAreaErr
parse_tokens(tAreaErr,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     _NotUsed:64/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{area_error,{tAreaErr,[{type,Type}],
                                                  {return,reference}}}
                                     |Residuum]);

%% tRefN
parse_tokens(tRefN,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     Cell:32/little-unsigned-integer,
     R2/binary>>=Bin,
    ParsedAddy=parse_cell_addy(<<Cell:32/little-unsigned-integer>>),
    parse_tokens(R2,Name,TokenArray,[{relative_reference,
                                      {tRefN,[{value,ParsedAddy},{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tAreaN
parse_tokens(tAreaN,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     FirstRow:16/little-unsigned-integer,
     LastRow:16/little-unsigned-integer,
     RawFirstCol:16/little-unsigned-integer,
     RawLastCol:16/little-unsigned-integer,
     R2/binary>>=Bin,
    {FirstCol,FirstRowType,FirstColType}=unpack_raw(RawFirstCol),
    {LastCol,LastRowType,LastColType}=unpack_raw(RawLastCol),
    NewDetails=[{start_cell,{FirstRow,FirstCol,FirstRowType,FirstColType}}|
                {end_cell,{LastRow,LastCol,LastRowType,LastColType}}],
    parse_tokens(R2,Name,TokenArray,[{relative_area,
                                      {tAreaN,[NewDetails|{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tMemAreaN
%% parse_tokens(?tMemAreaN,Name,Bin,TokenArray,Type,Residuum)->
%%     <<_Tk:8/little-unsigned-integer,
%%      Size:16/little-unsigned-integer,
%%      R2/binary>>=Bin,
%%     parse_tokens(R2,Name,TokenArray,[Tk|Residuum]);

%% tMemNoMemN
%% parse_tokens(tMemNoMemN,Name,Bin,TokenArray,Type,Residuum)->
%%     <<_Tk:24/little-unsigned-integer,R2/binary>>=Bin,
%%     parse_tokens(R2,Name,TokenArray,[Tk|Residuum]);

%% tNameX
parse_tokens(tNameX,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     NameIndex:16/little-unsigned-integer,
     _NotUsed:16/little-unsigned-integer,
     R2/binary>>=Bin,
    %% this is a *ONE* based index so drop it by one...
    parse_tokens(R2,Name,TokenArray,[{name_xref,{tNameX,
                                                 [{reference_index,RefIndex-1},
                                                  {name_index,NameIndex},
                                                  {type,Type}],
                                                 {return,reference}}}|Residuum]);

%% tRef3d
parse_tokens(tRef3d,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     Cell:4/binary,
     R2/binary>>=Bin,
    ParsedAddy=parse_cell_addy(Cell),
    parse_tokens(R2,Name,TokenArray,[{three_dee_reference,
                                      {tRef3d,[{reference_index,RefIndex},
                                               ParsedAddy,{type,Type}],
                                       {return,reference}}}|Residuum]);

%% tArea3d
parse_tokens(tArea3d,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     Range:64/little-unsigned-integer,
     R2/binary>>=Bin,
    {PStart,PEnd}=parse_cell_range(<<Range:64/little-unsigned-integer>>),
    NewDetails=[{start_cell,PStart}|{end_cell,PEnd}],
    parse_tokens(R2,Name,TokenArray,[{three_dee_area,
                                      {tArea3d,[{reference_index,RefIndex},
                                                NewDetails,
                                                {type,Type}],{return,reference}}}
                                     |Residuum]);

%% tRefErr3d
parse_tokens(tRefErr3d,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{three_dee_error_reference,
                                      {tRefErr3d,[{reference_index,RefIndex},
                                                  {type,Type}],
                                       {return,reference}}}|Residuum]);

%% tAreaErr3d
parse_tokens(tAreaErr3d,Name,Bin,TokenArray,Type,Residuum)->
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     _NotUsed:64/little-unsigned-integer,
     R2/binary>>=Bin,
    parse_tokens(R2,Name,TokenArray,[{three_dee_area_error,
                                      {tAreaErr3d,[{reference_index,RefIndex},
                                                   {type,Type}],
                                       {return,reference}}}|Residuum]);
parse_tokens(Other,_Name,_Bin,_TokenArray,_Type,_Residuum)->
    io:format("***************~nin parse_tokens Other is ~p~n***************~n",
              [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These helper functions help process formulas as described in Section 3   %%%
%%% of excelfileformat.pdf (V1.40)                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Described in Section 3.3.4 excelfileformat.pdf (V1.40)
parse_cell_range(Range)->
    <<FirstRowIdx:16/little-unsigned-integer,
     LastRowIdx:16/little-unsigned-integer,
     FirstRawCol:16/little-unsigned-integer,
     LastRawCol:16/little-unsigned-integer>>=Range,
    {FirstCol,FirstRowType,FirstColType}=unpack_raw(FirstRawCol),
    {LastCol,LastRowType,LastColType}=unpack_raw(LastRawCol),
    {{FirstRowIdx,FirstCol,FirstRowType,FirstColType},{LastRowIdx,LastCol,
                                                       LastRowType,LastColType}}.

parse_cell_addy(Addy)->
    <<RowIdx:16/little-unsigned-integer,
     RawCol:16/little-unsigned-integer>>=Addy,
    %% Now unpack the flags
    {Col,RowType,ColType}=unpack_raw(RawCol),
    {RowIdx,Col,RowType,ColType}.

unpack_raw(RawCol)->
    case RawCol band 16#C000 of
	16#C000 -> {RawCol-16#C000,rel_row,rel_col};
 	16#8000 -> {RawCol-16#8000,rel_row,abs_col};
 	16#4000 -> {RawCol-16#4000,abs_row,rel_col};
 	_       -> {RawCol,abs_row,abs_col}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process attributes as described in Section 3.10.5 of     %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_attr(<<?tAttrVolatile:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>)->
    {{attributes,{tAttrVolatile,volatile_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrIf:8/little-unsigned-integer,
	    Jump:16/little-unsigned-integer,
	    R/binary>>)->
    {{attributes,{tAttrIf,if_attribute,[{jump,Jump}],{return,none}}},R};
parse_attr(<<?tAttrChoose:8/little-unsigned-integer,
	    NumberOfChoices:16/little-unsigned-integer,
	    R/binary>>)->
    JumpTableSize=NumberOfChoices*2,
    <<JumpTable:JumpTableSize/binary,
     ErrorJump:16/little-unsigned-integer,
     R2/binary>>=R,
    {{attributes,{tAttrChoose,choose_attribute,
                  [{no_of_choices,NumberOfChoices+1},
                   {jump_table,JumpTable},
                   {error_jump,ErrorJump}],{return,none}}},R2};
parse_attr(<<?tAttrSkip:8/little-unsigned-integer,
	    Skip:16/little-unsigned-integer,
	    R/binary>>)->
    {{attributes,{tAttrSkip,skip_attribute,[{skip,Skip+1}],{return,none}}},R};
parse_attr(<<?tAttrSum:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>)->
    {{attributes,{tAttrSum,sum_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrAssign:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>)->
    {{attributes,{tAttrAssign,assign_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrSpace:8/little-unsigned-integer,
	    SpecCharType:8/little-unsigned-integer,
	    NoOfSpecChars:8/little-unsigned-integer,
	    R/binary>>)->
    Type=case SpecCharType of
	     ?attr_SC_SPACES1   -> space;
	     ?attr_SC_CARRIAGE1 -> carriage_return;
	     ?attr_SC_SPACES2   -> space;
	     ?attr_SC_CARRIAGE2 -> carriage_return;
	     ?attr_SC_SPACES3   -> space;
	     ?attr_SC_CARRIAGE3 -> carriage_return;
	     ?attr_SC_SPACES4   -> space
	 end,
    {{attributes,{tAttrSpace,special_character,[{char,Type},
                                                {no_of_chars,NoOfSpecChars}],
                  {return,none}}},R};
parse_attr(<<?tAttrSpaceVolatile:8/little-unsigned-integer,R/binary>>)->
    {Tk,R2}=parse_attr(<<?tAttrSpace:8/little-unsigned-integer,
                        R/binary>>),
    {{attributes,[{tAttrSpaceVolatile,volatile_attribute,[],
                   {return,none}}|Tk]},R2};
parse_attr(Other)->
    io:format("in parse_attr for Other ~p gonnae die!~n",[Other]),
    exit("goodbye cruel world from excel_tokens:parse_atrr").
