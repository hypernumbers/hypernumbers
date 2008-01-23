%%%-------------------------------------------------------------------
%%% File        : excel_tokens.erl
%%% Author      : Gordon Guthrie <gordon@hypernumberftAs.com>
%%% Description : parses the RPN tokens what Excel uses to store
%%%               the formulae in cells
%%%
%%% Created     : 11th Jan 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(excel_tokens).

-export([parse_tokens/5]).

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
parse_tokens(<<>>,TokenArray,Residuum,Tables,FileOut)->
    Return=lists:reverse(Residuum),
    %%io:format("in excel_tokens:parse_tokens Return is ~p~n",[Return]),
    excel_rev_comp:reverse_compile(Return,TokenArray,Tables,FileOut);
parse_tokens(Bin,TokenArray,Residuum,Tables,FileOut)->
    <<BaseTokenID:8/little-unsigned-integer,_Rest/binary>>=Bin,
    io:format("in excel_tokens:parse_tokens BaseTokenID is ~p~n"++
     "-Bin  is ~p~n",[BaseTokenID,Bin]),
    parse_tokens(BaseTokenID,Bin,TokenArray,Residuum,Tables,FileOut).

%% Parsing base tokens
%% tExp
parse_tokens(?tExp,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tExp~n"),
    %%io:format("in excel_tokens:parse_tokens Bin is ~p~n",[Bin]),
    <<Tk:8/little-unsigned-integer,
     RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tExp: ~p",[Tk])),
    %%io:format("in excel_tokens:parse_tokens Tk is ~p RowIndex is ~p "++
    %%          "ColIndex is ~p R2 is ~p~n",[Tk,RowIndex,ColIndex,R2]),
    parse_tokens(R2,TokenArray,[{expr_formula_range,{tExp,[{row_index,RowIndex},
						{col_index,ColIndex}],
					  {return,none}}}|Residuum],Tables,FileOut);
%% tTbl
parse_tokens(?tTbl,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tTbl~n"),
    <<Tk:8/little-unsigned-integer,
     RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tTbl: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{table_formula_range,{tRbl,[{row_index,RowIndex},
						 {col_index,ColIndex}],
					   {return,none}}}|Residuum],Tables,FileOut);
%% tAdd
parse_tokens(?tAdd,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tAdd~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tAdd: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{addition,{tAdd,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tSub
parse_tokens(?tSub,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tSub~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tSub: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{subtraction,{tSub,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tMul
parse_tokens(?tMul,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tMul~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMul: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{multiply,{tMul,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tDiv
parse_tokens(?tDiv,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tDiv~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tDiv: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{divide,{tDiv,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tPower
parse_tokens(?tPower,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tPower~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tPower: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{power,{tPower,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tConcat
parse_tokens(?tConcat,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tConcat~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tConcat: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{concatenate,{tConcat,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tLT
parse_tokens(?tLT,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tLT~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tLT: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{less_than,{tLT,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tLE
parse_tokens(?tLE,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tLE~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tLE: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{less_than_or_equal,{tLE,[{op_type,binary}],
					  {return,value}}}|Residuum],Tables,FileOut);
%% tEQ
parse_tokens(?tEQ,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tEQ~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tEQ: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{equals,{tEQ,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tGE
parse_tokens(?tGE,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tGE~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tGE: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{greater_than_or_equal,{tGE,[{op_type,binary}],
					      {return,value}}}
		     |Residuum],Tables,FileOut);
%% tGT
parse_tokens(?tGT,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tGT~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tGT: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{greater_than,{tGT,[{op_type,binary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tNE
parse_tokens(?tNE,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tNE~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tNE: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{not_equal,{tNE,{op_type,binary},{return,value}}}
		     |Residuum],Tables,FileOut);
%% tIsect
parse_tokens(?tIsect,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tISect~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tIsect: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{intersect,{tIsect,[{op_type,binary}],{return,reference}}}
		     |Residuum],Tables,FileOut);
%% tList
parse_tokens(?tList,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tList TokenArray is ~p~n",
    %%  [TokenArray]),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tList: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{list,{tList,[{op_type,binary}],{return,reference}}}
		     |Residuum],Tables,FileOut);
%% tRange
parse_tokens(?tRange,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tRange~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRange: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{range,{tRange,[{op_type,binary}],{return,reference}}}
		     |Residuum],Tables,FileOut);
%% tUplus
parse_tokens(?tUplus,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tUplus~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tUplus: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{plus,{tUplus,[{op_type,unary}],{return,value}}}|Residuum],
		  Tables,FileOut);
%% tUminus
parse_tokens(?tUminus,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tUminus~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tUminus: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{minus,{tUminus,[{op_type,unary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tPercent
parse_tokens(?tPercent,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tPercent~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tPercent: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{percent,{tPercent,[{op_type,unary}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tParen
parse_tokens(?tParen,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tParen~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tParen: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{parentheses,{tParen,[],{return,none}}}|Residuum],Tables,FileOut);
%% tMissArg
parse_tokens(?tMissArg,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tMissArg~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMissArg: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{missing_argument,{tMissArg,[],{return,value}}}|Residuum],
		  Tables,FileOut);
%% tStr
parse_tokens(?tStr,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tStr~n"),
    %% all strings in formulae have a 1-byte index
    %%io:format("in excel_tokens:parse_tokens for tStr Bin is ~p~n",[Bin]),
    <<Tk:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tStr: ~p",[Tk])),
    <<Len:8/little-unsigned-integer,
      NFlags:8/little-unsigned-integer,
      R3/binary>>=Rest,
    BinLen=excel_util:get_len_CRS_Uni16(Len,1,R3,NFlags,FileOut),
    %%io:format("in excel_tokens:parse_tokens for tStr BinLen is ~p~n",[BinLen]),
    %% The 2nd byte which contains the encoding flags needs to be included...
    %% if the String is Rich Text or Asian this next bit might blow up
    %% (who knows!)
    <<StrBin:BinLen/binary,R4/binary>>=Rest,
    {[{_,String2}],_,_}=excel_util:parse_CRS_Uni16(StrBin,1,FileOut),
    excel_util:put_log(FileOut,io_lib:fwrite("string String2 is ~p",[String2])),
    parse_tokens(R4,TokenArray,[{string,{tStr,[{value,String2}],{return,value}}}|Residuum],
		  Tables,FileOut);
%% parse_tokens(?tStr,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tStr~n"),
    %% all strings in formulae have a 1-byte index
%%     io:format("in excel_tokens:parse_tokens for tStr Bin is ~p~n",[Bin]),
%%     <<Tk:8/little-unsigned-integer,
%%      Len:8/little-unsigned-integer,
%%      R2/binary>>=Bin,
%%     io:format("in excel_tokens:parse_tokens for tStr R2 is ~p~n",[R2]),
%%     excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tStr: ~p",[Tk])),
    %% The 2nd byte which contains the encoding flags needs to be included...
    %% if the String is Rich Text or Asian this next bit might blow up
    %% (who knows!)
%%     Len2=Len+1,
%%     <<String:Len2/binary,R3/binary>>=R2,
%%     io:format("in excel_tokens:parse_tokens for tStr R3 is ~p String is ~p~n",[R3,String]),
%%     StrBin=list_to_binary([Len,String]),
%%     io:format("in excel_tokens:parse_tokens for tStr StrBin is ~p~n",[StrBin]),
%%     String2=excel_util:parse_CRS_Uni16(StrBin,1,FileOut),
%%     excel_util:put_log(FileOut,io_lib:fwrite("string String2 is ~p",[String2])),
%%     parse_tokens(R3,TokenArray,[{string,{tStr,[{value,String2}],{return,value}}}|Residuum],
%% 		  Tables,FileOut);
%% tNlr
parse_tokens(?tNlr,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tNlr~n"),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("parsing token tNlr: ~p",[Tk])),
    %%io:format("in excel_tokens:parse_tokens - Fix me~n"),
    parse_tokens(R2,TokenArray,[Tk|Residuum],Tables,FileOut);
%% tAttr
parse_tokens(?tAttr,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAttr Bin is ~p~n",[Bin]),
    <<Tk:8/little-unsigned-integer,R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("parsing token tAttr: ~p",[Tk])),
    Return=parse_attr(R2,FileOut),
    io:format("in excel_tokens:parse_tokens tAttr Return is ~p~n",[Return]),
    {Token,R3}=Return,
    excel_util:put_log(FileOut,io_lib:fwrite("~p",[Token])),
    io:format("in excel_tokens:parse_tokens Token is ~p~n",[Token]),
    parse_tokens(R3,TokenArray,[Token|Residuum],Tables,FileOut);
%% tErr
parse_tokens(?tErr,Bin,TokenArray,Residuum,Tables,FileOut)->
    %%io:format("in excel_tokens:parse_tokens for tErr~n"),
    <<Tk:8/little-unsigned-integer,
     Error:8/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tErr: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{error,{tErr,[{value,Error}],{return,none}}}
		     |Residuum],Tables,FileOut);
%% tBool
parse_tokens(?tBool,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tBool~n"),
    <<Tk:8/little-unsigned-integer,
     Bool:8/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tBool: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{boolean,{tBool,[{value,Bool}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tInt
parse_tokens(?tInt,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tInt~n"),
    <<Tk:8/little-unsigned-integer,
     Int:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tInt: ~p~n"++
      "-Int is ~p~n",[Tk,Int])),
    parse_tokens(R2,TokenArray,[{integer,{tInt,[{value,Int}],{return,value}}}
		     |Residuum],Tables,FileOut);
%% tNum
parse_tokens(?tNum,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNum~n"),
    <<Tk:8/little-unsigned-integer,
     Num:64/little-float,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tNum: ~p",[Tk])),
    parse_tokens(R2,TokenArray,[{number,{tNum,[{value,Num}],{return,value}}}
		     |Residuum],Tables,FileOut);

%% Parsing Classified Tokens
%%
%% Classifieds come in three flavours - this bloq boils them
%% down to one call - these come after
%%
%% tArray
parse_tokens(?tArrayR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArrayR~n"),
    parse_tokens(tArray,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tArrayV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArrayV~n"),
    parse_tokens(tArray,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tArrayA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArrayV~n"),
    parse_tokens(tArray,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tFuncR
parse_tokens(?tFuncR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncR~n"),
    parse_tokens(tFunc,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tFuncV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncV~n"),
    parse_tokens(tFunc,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tFuncA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncA~n"),
    parse_tokens(tFunc,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tFuncVar
parse_tokens(?tFuncVarR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncVarR~n"),
    parse_tokens(tFuncVar,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tFuncVarV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncVarV~n"),
    parse_tokens(tFuncVar,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tFuncVarA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncVarA~n"),
    parse_tokens(tFuncVar,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tName
parse_tokens(?tNameR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameR~n"),
    parse_tokens(tName,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tNameV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameV~n"),
    parse_tokens(tName,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tNameA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameA~n"),
    parse_tokens(tName,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tRef
parse_tokens(?tRefR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefR~n"),
    parse_tokens(tRef,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tRefV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefV~n"),
    parse_tokens(tRef,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tRefA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefA~n"),
    parse_tokens(tRef,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tArea
parse_tokens(?tAreaR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaR~n"),
    parse_tokens(tArea,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tAreaV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaV~n"),
    parse_tokens(tArea,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tAreaA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaA~n"),
    parse_tokens(tArea,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemArea
parse_tokens(?tMemAreaR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemAreaR~n"),
    parse_tokens(tMemArea,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tMemAreaV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemAreaV~n"),
    parse_tokens(tMemArea,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tMemAreaA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemAreaA~n"),
    parse_tokens(tMemArea,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemErr
parse_tokens(?tMemErrR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemErrR~n"),
    parse_tokens(tMemErr,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tMemErrV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemErrV~n"),
    parse_tokens(tMemErr,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tMemErrA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemErrA~n"),
    parse_tokens(tMemErr,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemNoMem
parse_tokens(?tMemNoMemR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemNoMemR~n"),
    parse_tokens(tMemNoMem,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tMemNoMemV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemNoMemV~n"),
    parse_tokens(tMemNoMem,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tMemNoMemA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemNoMemA~n"),
    parse_tokens(tMemNoMem,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemFunc
parse_tokens(?tMemFuncR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemFuncR~n"),
    parse_tokens(tMemFunc,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tMemFuncV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemFuncV~n"),
    parse_tokens(tMemFunc,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tMemFuncA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemFuncA~n"),
    parse_tokens(tMemFunc,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tRefErr
parse_tokens(?tRefErrR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErrR~n"),
    parse_tokens(tRefErr,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tRefErrV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErrV~n"),
    parse_tokens(tRefErr,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tRefErrA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErrA~n"),
    parse_tokens(tRefErr,Bin,TokenArray,array,Residuum,Tables,FileOut);
%5 tAreaErr
parse_tokens(?tAreaErrR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErrR~n"),
    parse_tokens(tAreaErr,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tAreaErrV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErrV~n"),
    parse_tokens(tAreaErr,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tAreaErrA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErrA~n"),
    parse_tokens(tAreaErr,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tRefN
parse_tokens(?tRefNR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefNR~n"),
    parse_tokens(tRefN,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tRefNV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefNV~n"),
    parse_tokens(tRefN,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tRefNA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefNA~n"),
    parse_tokens(tRefN,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tAreaN
parse_tokens(?tAreaNR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaNR~n"),
    parse_tokens(tAreaN,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tAreaNV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaNV~n"),
    parse_tokens(tAreaN,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tAreaNA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaNA~n"),
    parse_tokens(tAreaN,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemAreaN
%% parse_tokens(?tMemAreaNR,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemAreaN,Bin,TokenArray,reference,Residuum,Tables,FileOut);
%% parse_tokens(?tMemAreaNV,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemAreaN,Bin,TokenArray,value,Residuum,Tables,FileOut);
%% parse_tokens(?tMemAreaNA,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemAreaN,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tMemNoMemN
%% parse_tokens(?tMemNoMemNR,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemNoMemN,Bin,TokenArray,reference,Residuum,Tables,FileOut);
%% parse_tokens(?tMemNoMemNV,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemNoMemN,Bin,TokenArray,value,Residuum,Tables,FileOut);
%% parse_tokens(?tMemNoMemNA,Bin,TokenArray,Residuum,Tables,FileOut)->
%%     parse_tokens(tMemNoMemN,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tNameX
parse_tokens(?tNameXR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameXR~n"),
    parse_tokens(tNameX,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tNameXV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameXV~n"),
    parse_tokens(tNameX,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tNameXA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameXA~n"),
    parse_tokens(tNameX,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tRef3d
parse_tokens(?tRef3dR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefedR~n"),
    parse_tokens(tRef3d,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tRef3dV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRef3dV~n"),
    parse_tokens(tRef3d,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tRef3dA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRef3dA~n"),
    parse_tokens(tRef3d,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tArea3d
parse_tokens(?tArea3dR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArea3dR~n"),
    parse_tokens(tArea3d,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tArea3dV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArea3dv~n"),
    parse_tokens(tArea3d,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tArea3dA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArea3dA~n"),
    parse_tokens(tArea3d,Bin,TokenArray,array,Residuum,Tables,FileOut);
%5 tRefErr3d
parse_tokens(?tRefErr3dR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErr3dR~n"),
    parse_tokens(tRefErr3d,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tRefErr3dV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefEr3dV~n"),
    parse_tokens(tRefErr3d,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tRefErr3dA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErr3dA~n"),
    parse_tokens(tRefErr3d,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% tAreaErr3d
parse_tokens(?tAreaErr3dR,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErr3dR~n"),
    parse_tokens(tAreaErr3d,Bin,TokenArray,reference,Residuum,Tables,FileOut);
parse_tokens(?tAreaErr3dV,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErr3dV~n"),
    parse_tokens(tAreaErr3d,Bin,TokenArray,value,Residuum,Tables,FileOut);
parse_tokens(?tAreaErr3dA,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErr3dA~n"),
    parse_tokens(tAreaErr3d,Bin,TokenArray,array,Residuum,Tables,FileOut);
%% skip zero tokens
parse_tokens(0,Bin,TokenArray,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens Token is 0~n"),
    excel_util:put_log(FileOut++zero,io_lib:fwrite("0 Token parsed~n",[])),
    <<_Skip:8/little-unsigned-integer,R/binary>>=Bin,
    %%parse_tokens(R,TokenArray,Residuum,FileOut);
    parse_tokens(<<>>,TokenArray,Residuum,Tables,FileOut);
%% catch and terminate unexpected tokens
parse_tokens(Duff,Bin,TokenArray,Residuum,Tables,FileOut)->
    io:format("duff token parsed~p~n"++
				      "binary is ~p~n"++
				      "residuum is ~p~n",
				      [Duff,Bin,Residuum]),
    excel_util:put_log(FileOut,io_lib:fwrite("duff token parsed~p~n"++
				  "binary is ~p~n"++
				  "residuum is ~p",
				  [Duff,Bin,Residuum])),
    exit("duff token parsed!").

%% these are the functions that parse the classified token sets
%% tArray
parse_tokens(tArray,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArray~n"),
    <<_Tk:8/little-unsigned-integer,
     _Array:56/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tArray of type ~p",
				  [Type])),
    parse_tokens(R2,TokenArray,[{array_type,{tArray,[{type,Type}],{return,array}}}
		     |Residuum],Tables,FileOut);
%% tFunc
parse_tokens(tFunc,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFunc~n"),
    <<_Tk:8/little-unsigned-integer,
     Func:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tFunc ~p",[Type])),
    %% io:format("in excel_tokens:parse_tokens fix reference stuff...~n"),
    parse_tokens(R2,TokenArray,[{functional_index,{tFunc,[{value,Func},{type,Type}],
					{return,variant}}}|Residuum],Tables,FileOut);
%% tFuncVar
parse_tokens(tFuncVar,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tFuncVar~n"),
    <<_Tk:8/little-unsigned-integer,
     Flags:8/little-unsigned-integer,
     Index:16/little-unsigned-integer,
     R2/binary>>=Bin,
    {NArgs,UserPrompt} = case Flags band 16#80 of
			     16#80 -> {Flags-16#80,true};
			     _    -> {Flags,false}
			 end,
    {FuncIndex,FuncType} = case Index band 16#8000 of
			       16#8000 -> {Index-16#8000,built-in};
			       _       -> {Index,macro}
			   end,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tFuncVar ~p",
				  [Type])),
    excel_util:put_log(FileOut,io_lib:fwrite("NArgs is ~p~nUserPrompt is ~p~n"++
				  "FuncType is ~pFuncIndex is ~p",
				  [NArgs,UserPrompt,
				   FuncType,FuncIndex])),
    parse_tokens(R2,TokenArray,[{var_func_idx,{tFuncVar,
					 [{value,FuncIndex},
					  {number_of_args,NArgs},
					  {user_prompt,UserPrompt},
					  {type,Type}],{return,variant}}}
		     |Residuum],Tables,FileOut);
%% tName
parse_tokens(tName,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tName~n"),
    <<_Tk:8/little-unsigned-integer,
     Index:16/little-unsigned-integer,
     __NotUsed:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tName ~p",[Type])),
    parse_tokens(R2,TokenArray,[{name_index,{tName,[{value,Index},{type,Type}],
				  {return,reference}}}|Residuum],Tables,FileOut);
%% tRef
parse_tokens(tRef,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRef~n"),
    <<_Tk:8/little-unsigned-integer,
     Ref:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRef ~p",[Type])),
    ParsedAddy=parse_cell_addy(<<Ref:32/little-unsigned-integer>>),
    %% io:format("in excel_tokens:parse_tokens for tRef ParsedAddy is ~p~n",[ParsedAddy]),
    excel_util:put_log(FileOut,io_lib:fwrite("ParsedAddy is ~p",[ParsedAddy])),
    %% io:format("in excel_tokens:parse_tokens for tRef ParsedAddy is ~p~n",[ParsedAddy]),
    parse_tokens(R2,TokenArray,[{abs_ref,{tRef,[{value,ParsedAddy}|{type,Type}],
					  {return,reference}}}
		     |Residuum],Tables,FileOut);
%% tArea
parse_tokens(tArea,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArea Bin is ~p~n",[Bin]),
    <<_Tk:8/little-unsigned-integer,
     Range:8/binary,
     R2/binary>>=Bin,
    {ParsedStart,ParsedEnd}=parse_cell_range(Range),
    excel_util:put_log(FileOut,io_lib:fwrite("ParsedStart is ~p~nParsedEnd is ~p",
				  [ParsedStart,ParsedEnd])),
    NewDetails=[{start_cell,ParsedStart}|{end_cell,ParsedEnd}],
    %% io:format("in excel_tokens:parse_tokens for tArea NewDetails are ~p~n",[NewDetails]),
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tArea ~p",[Type])),
    parse_tokens(R2,TokenArray,[{absolute_area,{tArea,[NewDetails|{type,Type}],
				     {return,reference}}}
		     |Residuum],Tables,FileOut);
%% tMemArea
parse_tokens(tMemArea,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemArea Bin is ~p Type is ~p~n",[Bin,Type]),
    <<_Tk:8/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     MemArea:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMemArea ~p",[Type])),
    parse_tokens(R2,TokenArray,[{memory_area,{tMemArea,[{value,MemArea},{type,Type}],
				   {return,reference}}}|Residuum],Tables,FileOut);
%% tMemErr
parse_tokens(tMemErr,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemErr~n"),
    <<_Tk:8/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMemErr ~p",[Type])),
    parse_tokens(R2,TokenArray,[{memory_err,{tMemErr,[{value,Size},{type,Type}],
				  {return,reference}}}|Residuum],Tables,FileOut);
%% tMemNoMe
parse_tokens(tMemNoMem,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemNoMem~n"),
    <<_Tk:8/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMemNoMem ~p",[Type])),
    parse_tokens(R2,TokenArray,[{memory_no_memory,{tMemNoMem,[{value,Size},{type,Type}],
					{return,reference}}}|Residuum],Tables,FileOut);
%% tMemFunc
parse_tokens(tMemFunc,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tMemFunc~n"),
    <<_Tk:8/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tMemFunc ~p",[Type])),
    parse_tokens(R2,TokenArray,[{memory_function,{tMemFunc,[{value,Size},{type,Type}],
				       {return,reference}}}|Residuum],Tables,FileOut);
%% tRefErr
parse_tokens(tRefErr,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErr~n"),
    <<_Tk:8/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRefErr ~p",[Type])),
    parse_tokens(R2,TokenArray,[{reference_error,{tRefErr,[{type,Type}],
				       {return,reference}}}|Residuum],Tables,FileOut);
%% tAreaErr
parse_tokens(tAreaErr,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErr~n"),
    <<_Tk:8/little-unsigned-integer,
     __NotUsed:64/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tAreaErr ~p",[Type])),
    parse_tokens(R2,TokenArray,[{area_error,{tAreaErr,[{type,Type}],{return,reference}}}
		     |Residuum],Tables,FileOut);
%% tRefN
parse_tokens(tRefN,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefN~n"),
    <<_Tk:8/little-unsigned-integer,
     Cell:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRefN ~p",[Type])),
    ParsedAddy=parse_cell_addy(<<Cell:32/little-unsigned-integer>>),
    parse_tokens(R2,TokenArray,[{relative_reference,{tRefN,[ParsedAddy,{type,Type}],
		      {return,reference}}}|Residuum],Tables,FileOut);
%% tAreaN
parse_tokens(tAreaN,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaN~n"),
    <<_Tk:8/little-unsigned-integer,
     StartCell:32/little-unsigned-integer,
     EndCell:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tAreaN ~p",[Type])),
    ParsedStart=parse_cell_addy(<<StartCell:32/little-unsigned-integer>>),
    ParsedEnd=parse_cell_addy(<<EndCell:32/little-unsigned-integer>>),
    excel_util:put_log(FileOut,io_lib:fwrite("ParsedStart is ~p~nParsedEnd is ~p",
				  [ParsedStart,ParsedEnd])),
    NewDetails=[{start_cell,ParsedStart}|{end_cell,ParsedEnd}],
    parse_tokens(R2,TokenArray,[{relative_area,{tAreaN,[NewDetails|{type,Type}],
		      {return,reference}}}|Residuum],Tables,FileOut);
%% tMemAreaN
%% parse_tokens(?tMemAreaN,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
%%     <<Tk:8/little-unsigned-integer,
%%      Size:16/little-unsigned-integer,
%%      R2/binary>>=Bin,
%%     excel_util:put_log(FileOut,io_lib:fwrite("parsing token tMemAreaN ~p",[Type])),
%%     %% io:format("You need to fix me!"),
%%     parse_tokens(R2,TokenArray,[Tk|Residuum],Tables,FileOut);
%% tMemNoMemN
%% parse_tokens(tMemNoMemN,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
%%     <<Tk:24/little-unsigned-integer,R2/binary>>=Bin,
%%     excel_util:put_log(FileOut,io_lib:fwrite("parsing token tMemNoMemN ~p",[Type])),
%%     %% io:format("You need to fix me!"),
%%     parse_tokens(R2,TokenArray,[Tk|Residuum],Tables,FileOut);
%% tNameX
parse_tokens(tNameX,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tNameX~n"),
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     NameIndex:16/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tNameX ~p",[Type])),
    parse_tokens(R2,TokenArray,[{name_xref,{tNameX,[{reference_index,RefIndex},
					{name_index,NameIndex},{type,Type}],
		      {return,reference}}}|Residuum],Tables,FileOut);
%% tRef3d
parse_tokens(tRef3d,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRef3d~n"),
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     Cell:4/binary,
     R2/binary>>=Bin,
    %% io:format("***WARNING*** excel_tokens:parse_tokens for tRef3d is fucked"++
    %%          "it flattens the Array to 1D!~n"),
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRef3d ~p",[Type])),
    io:format("in excel_tokens:parse_tokens for tRef3d Cell is ~p~n",[Cell]),
    ParsedAddy=parse_cell_addy(Cell),
    parse_tokens(R2,TokenArray,[{three_dee_reference,
		      {tRef3d,[{reference_index,RefIndex},ParsedAddy,{type,Type}],
		       {return,reference}}}|Residuum],Tables,FileOut);
%% tArea3d
parse_tokens(tArea3d,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tArea3d~n"),
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     StartCell:32/little-unsigned-integer,
     EndCell:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tArea3d ~p",[Type])),
    ParsedStart=parse_cell_addy(<<StartCell:32/little-unsigned-integer>>),
    ParsedEnd=parse_cell_addy(<<EndCell:32/little-unsigned-integer>>),
    excel_util:put_log(FileOut,io_lib:fwrite("ParsedStart is ~p~n"++
				  "ParsedEnd is ~p",
				  [ParsedStart,ParsedEnd])),
    NewDetails=[{start_cell,ParsedStart}|{end_cell,ParsedEnd}],
    parse_tokens(R2,TokenArray,[{three_dee_area,
		      {tArea3d,[{reference_index,RefIndex},NewDetails,
				{type,Type}],{return,reference}}}
		     |Residuum],Tables,FileOut);
%% tRefErr3d
parse_tokens(tRefErr3d,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tRefErr3d~n"),
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tRefErr3d ~p",[Type])),
    parse_tokens(R2,TokenArray,[{three_dee_error_reference,
		      {tRefErr3d,[{reference_index,RefIndex},{type,Type}],
		      {return,reference}}}|Residuum],Tables,FileOut);
%% tAreaErr3d
parse_tokens(tAreaErr3d,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    %% io:format("in excel_tokens:parse_tokens for tAreaErr3d~n"),
    <<_Tk:8/little-unsigned-integer,
     RefIndex:16/little-unsigned-integer,
     _NotUsed:64/little-unsigned-integer,
     R2/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("*DONE* parsing token tAreaErr3d ~p",[Type])),
    parse_tokens(R2,TokenArray,[{three_dee_area_error,
		      {tAreaErr3d,[{reference_index,RefIndex},{type,Type}],
		      {return,reference}}}|Residuum],Tables,FileOut);
parse_tokens(Other,Bin,TokenArray,Type,Residuum,Tables,FileOut)->
    io:format("in parse_tokens Other is ~p~n",[Other]).

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
    {unpack_raw(FirstRowIdx,FirstRawCol),unpack_raw(LastRowIdx,LastRawCol)}.

parse_cell_addy(Addy)->
    <<RowIdx:16/little-unsigned-integer,
     RawCol:16/little-unsigned-integer>>=Addy,
    %% Now unpack the flags
    unpack_raw(RowIdx,RawCol).
    
unpack_raw(RowIdx,RawCol)->
    case RawCol band 16#C000 of
	16#C000 -> {RowIdx,RawCol-16#C000,rel_row,rel_col};
	16#8000 -> {RowIdx,RawCol-16#8000,rel_row,abs_col};
	16#4000 -> {RowIdx,RawCol-16#4000,abs_row,rel_col};
	_       -> {RowIdx,RawCol,abs_row,abs_col}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process attributes as described in Section 3.10.5 of     %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_attr(<<?tAttrVolatile:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>,
	   _FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrVolatile~n"),
  {{attributes,{tAttrVolatile,volatile_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrIf:8/little-unsigned-integer,
	    Jump:16/little-unsigned-integer,
	    R/binary>>,
	   _FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrIf~n"),
  {{attributes,{tAttrIf,if_attribute,[{jump,Jump}],{return,none}}},R};
parse_attr(<<?tAttrChoose:8/little-unsigned-integer,
	    NumberOfChoices:16/little-unsigned-integer,
	    R/binary>>,
	   _FileOut)->
    io:format("in excel_tokens:parse_attr for tAttrChoose NumberOfChoices is ~p~n",
    [NumberOfChoices]),
    JumpTableSize=NumberOfChoices*2,
    <<JumpTable:JumpTableSize/binary,
     ErrorJump:16/little-unsigned-integer,
     R2/binary>>=R,
  {{attributes,{tAttrChoose,choose_attribute,[{no_of_choices,NumberOfChoices+1},
				     {jump_table,JumpTable},
				     {error_jump,ErrorJump}],{return,none}}},R2};
parse_attr(<<?tAttrSkip:8/little-unsigned-integer,
	    Skip:16/little-unsigned-integer,
	    R/binary>>,_FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrSkip~n"),
  {{attributes,{tAttrSkip,skip_attribute,[{skip,Skip+1}],{return,none}}},R};
parse_attr(<<?tAttrSum:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>,_FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrSum~n"),
  {{attributes,{tAttrSum,sum_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrAssign:8/little-unsigned-integer,
	    _NotUsed:16/little-unsigned-integer,
	    R/binary>>,_FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrAssign~n"),
  {{attributes,{tAttrAssign,assign_attribute,[],{return,none}}},R};
parse_attr(<<?tAttrSpace:8/little-unsigned-integer,
	    SpecCharType:8/little-unsigned-integer,
	    NoOfSpecChars:8/little-unsigned-integer,
	    R/binary>>,_FileOut)->
    io:format("in excel_tokens:parse_attr for tAttrSpace~n"),
    Type=case SpecCharType of
	     ?attr_SC_SPACES1   -> space;
	     ?attr_SC_CARRIAGE1 -> carriage_return;
	     ?attr_SC_SPACES2   -> space;
	     ?attr_SC_CARRIAGE2 -> carriage_return;
	     ?attr_SC_SPACES3   -> space;
	     ?attr_SC_CARRIAGE3 -> carriage_return;
	     ?attr_SC_SPACES4   -> space
	 end,
  {{attributes,{tAttrSpace,special_character,[{char,Type},{no_of_chars,NoOfSpecChars}],
      {return,none}}},R};
parse_attr(<<?tAttrSpaceVolatile:8/little-unsigned-integer,R/binary>>,FileOut)->
  io:format("in excel_tokens:parse_attr for tAttrSpaceVolatile~n"),
  {Tk,R2}=parse_attr(<<?tAttrSpace:8/little-unsigned-integer,
              R/binary>>,FileOut),
  {{attributes,[{tAttrSpaceVolatile,volatile_attribute,[],{return,none}}|Tk]},R2};
parse_attr(Other,FileOut)->
  io:format("in parse_attr for Other gonnae die!~n"),
  exit("goodbye cruel world from excel_tokens:parse_atrr").
  