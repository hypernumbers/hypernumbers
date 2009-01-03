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
-import(lists,[flatten/1,duplicate/2]).

%%% Include files with macros encoding Microsoft File Format constants
-include("excel_attributes.hrl").
-include("excel_array_elements.hrl").
-include("excel_errors.hrl").
-include("excel_supbook.hrl").

-define(read,excel_util:read).
-define(write,excel_util:write).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the tokens defined in Section 3 of                   %%%
%%% excelfileformat.pdf (V1.40) - it reverse compiles them into the raw      %%%
%%% formulae                                                                 %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The formulae are stored in reverse Polish Notation within excel
%% The reverse compiler recreates the original formula by running the RPN
reverse_compile(Index,Tokens,TokArr,Tbl)->
    rev_comp(Index,Tokens,TokArr,[],Tbl).

%% When the tokens are exhausted the Stack is just flattened to a string
rev_comp(_Index,[],_TokArr,Stack,_Tbl) ->
    "="++to_str(lists:reverse(Stack));

%% tExp
%% tExp is a placeholder for a shared or array formula 
%% we treat them differently
%% 
%% so first we read the array/shared formula then we push the 
%% shared formula onto the Stack in place of the tExp one and carry on
rev_comp(I,[{expr_formula_range,{tExp,[Sheet,Row,Col], {return,none}}}|T],
         TokArr,Stack,Tbl) ->
    [{_Id2,[{type,Type},{tokens,Tokens},{tokenarrays,_TkArr}]}] =
        excel_util:read_shared(Tbl,{Sheet,Row,Col}),
    case Type of
        array  -> {ok, dont_process};
        shared -> rev_comp(I,lists:append(Tokens,T),TokArr,Stack,Tbl)
    end;

%% tTbl
%% used in what-if tables and stuff - not implemented yet!    

%% tAdd tSub tMul tDiv tPower tConcat tLT tLE tEQ tGE tGT tNE tIsect
%% Pop two off the stack and the build an operator set backwards
%% ie second first and first second...
rev_comp(I,[{Op,_Token}|T],TokArr,Stack,Tbl) when
Op =:= addition ;    Op =:= subtraction;
Op =:= divide ;      Op =:= power;
Op =:= less_than;    Op =:= less_than_or_equal;
Op =:= equals;       Op =:= greater_than_or_equal;
Op =:= greater_than; Op =:= not_equal;
Op =:= multiply;     Op =:= concatenate ->

    {Spaces1,[First|Rest]}  = popSpaces(Stack),
    {Spaces2,[Second|Last]} = popSpaces(Rest),
    rev_comp(I,T,TokArr,[{Second,Spaces1,Op,Spaces2,First}|Last],Tbl);

%% tIsect
%% Pop two off the stack and the build an operator set backwards
%% ie second first and first second...
rev_comp(I,[{intersect,_Token}|T],TokArr,[First,Second|Rest],Tbl) ->
    rev_comp(I,T,TokArr,[{string,to_str(Second)++" "++to_str(First)}|Rest],Tbl);

%% tList
rev_comp(I,[{list,{tList,[{op_type,binary}],{return,reference}}}|T],TokArr,
         [First,Second|Rest],Tbl) ->
    rev_comp(I,T,TokArr,[{Second,comma,First}|Rest],Tbl);

%% tRange

%% tUplus
%% Special double case to handle unary plus with spaces after them
rev_comp(I,[{plus,{tUplus,[{op_type,unary}],{return,_Value}}}|T],TokArr,
         [First|Rest],Tbl) ->
    rev_comp(I,T,TokArr,[{string,"+"++to_str(First)}|Rest],Tbl);

%% tUminus
%% Special double case to handle unary minus with spaces after them!
rev_comp(I,[{minus,{tUminus,[{op_type,unary}],{return,_Value}}}|T],TokArr,
         [First|Rest],Tbl) ->
    rev_comp(I,T,TokArr,[{string,"-"++to_str(First)}|Rest],Tbl);

%% tPercent
rev_comp(I,[{percent,{tPercent,[{op_type,unary}],{return,_Value}}}|T],TokArr,
         [First|Rest],Tbl) ->
    rev_comp(I,T,TokArr,[{string,to_str(First)++"%"}|Rest],Tbl);

%% tParen
rev_comp(I,[{parentheses,{tParen,[],{return,none}}}|T],TokArr,Stack,Tbl) ->
    {Spaces1,[First |Rest]} = popSpaces(Stack),
    rev_comp(I,T,TokArr,[{open,First,close,Spaces1}|Rest],Tbl);

%% tMissArg
rev_comp(I,[{missing_argument,{tMissArg,[],{return,value}}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,""}|Stack],Tbl);

%% tStr
rev_comp(I,[{string,{tStr,[{value,Binary}],{return,value}}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,binary_to_list(Binary)}|Stack],Tbl);

%% tNlr

%% tAttr
rev_comp(I,[{attributes,Attributes}|T],TokArr,Stack,Tbl)  ->
    NewStack = case Attributes of
                   {tAttrVolatile,volatile_attribute,[],_Ret}      -> Stack;
                   {tAttrIf,if_attribute,_Jump,_Ret}               -> Stack;
                   {tAttrChoose,choose_attribute,_Jump,_Ret}       -> Stack;
                   {tAttrSkip,skip_attribute,[{skip,_Skip}],_Ret}  -> Stack;
                   {tAttrAssign,assign_attribute,[],_Ret}          -> Stack;
                   % this is an attribute of SUM with 1 arg...
                   % 4 is the index to the func SUM
                   {tAttrSum,sum_attribute,[],_Ret} ->
                       [FunArgs|Rest] = Stack,
                       [{func,4,lists:flatten([FunArgs])}|Rest];
                   % Preserve Spaces
                   % {tAttrSpace,special_character,[{char,_Type},
                   %                              {no_of_chars,Chars}],_Ret} ->
                   %   [{space, flatten(duplicate(Chars, " "))} | Stack]
                   % Kill spaces
                   {tAttrSpace,special_character,[{char,_Type},
                                                  {no_of_chars,_Chars}],_Ret} -> Stack
               end,

    rev_comp(I,T,TokArr,NewStack,Tbl);

%% tSheet

%% tEndSheet

%% tErr
rev_comp(Index,[{error,{tErr,[{value,Value}],{return,_Return}}}|T],TokArr,Stack,Tbl)  ->
    Error = case Value of
                ?NullError    -> "#NULL!";
                ?DivZeroError -> "#DIV/0!";
                ?ValueError   -> "#VALUE!";
                ?RefError     -> "#REF!";
                ?NameError    -> "#NAME?";
                ?NumError     -> "#NUM!";
                ?NAError      -> "#N/A"
            end,
    rev_comp(Index,T,TokArr,[{string,Error}|Stack],Tbl);

%% tBool
rev_comp(Index,[{boolean,{tBool,[{value,Value}],{return,value}}}|T],TokArr,Stack,Tbl) ->
    Bool = case Value of
               1 -> "TRUE";
               0 -> "FALSE"
           end,
    rev_comp(Index,T,TokArr,[{string,Bool}|Stack],Tbl);

%% tInt
rev_comp(Index,[{integer,{tInt,[{value,Val}],{return,value}}}|T],TokArr,Stack,Tbl)  ->
    rev_comp(Index,T,TokArr,[{integer,Val}|Stack],Tbl);

%% tNum
rev_comp(Index,[{number,{tNum,[{value,Val}],{return,value}}}|T],TokArr,Stack,Tbl)  ->
    rev_comp(Index,T,TokArr,[{float,Val}|Stack],Tbl);

%%%
%%% Classified Tokens
%%%

%% tArray
rev_comp(Index,[{array_type,{tArray,[{type,_Type}],{return,_Reference}}}|T],TokArr,Stack,Tbl)  ->
    {{NoCols,NoRows,Array},ArrayTail}=read_const_val_array(TokArr),
    ArrayString=array_to_str(Array,NoCols,NoRows),
    rev_comp(Index,T,ArrayTail,[{string,ArrayString}|Stack],Tbl);

%% tFunc
rev_comp(Index,[{functional_index,{Function,[{value,FuncVar},{type,_Type}],
                                   {return,_Return}}}|T],TokArr,Stack,Tbl)
  when Function =:= tFunc ; Function =:= tFuncVar ;
       Function =:= tFuncVarV ;Function =:= tFuncVarR ;
       Function =:= tFuncVarA ->
    % lists:reverse has to be overused because Stack is build backwards
    % (new items append to tail opposed to head
    NumArgs=macro_no_of_args(FuncVar),
    {Rest,FunArgs} = popVars(NumArgs,Stack,[]),
    rev_comp(Index,T,TokArr,[{func,FuncVar,lists:reverse(FunArgs)}|Rest],Tbl);

%% tFuncVar
rev_comp(Index,[{var_func_idx,{Fun,[{value,FuncVar},
                                    {number_of_args,NumArgs},{user_prompt,_Prompt},
                                    {type,_Type}],
                               {return,_ReturnType}}}|T],TokArr,Stack,Tbl) when
Fun =:= tFuncVar; Fun =:= tFuncVarV; Fun =:= tFuncVarR; Fun =:= tFuncVarA ->
    {Rest,FunArgs} = popVars(NumArgs,Stack,[]),
    rev_comp(Index,T,TokArr,[{func,FuncVar,lists:reverse(FunArgs)}|Rest],Tbl);

%% tName
rev_comp(Index,[{name_index,{tName,[{value,Value},{type,_Type}],
                             {return,reference}}}|T],TokArr,Stack,Tbl) ->
    [{_Index1, [{extbook, _EXBIdx},{sheetindex, _SheetIdx}, {type, Type},
                {name, NameVal}, _Val]}] = ?read(Tbl,tmp_names,Value),
    % if the type is local look up the sheet name
    NameStr = case Type of
                  global -> "../"++"@"++NameVal;
                  local  -> {{sheet,SheetName},_,_}=Index,
                            EscSheet=excel_util:esc_tab_name(SheetName),
                            "../"++EscSheet++"/"++"@"++NameVal
              end,
    rev_comp(Index,T,TokArr,[{string,NameStr}|Stack],Tbl);

%% tRef
rev_comp(I,[{abs_ref,{tRef,[{value,{Row,Col,RType,CType}}|{type,_Type}],
                      {return,reference}}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,make_cell({Row,Col,RType,CType})}|Stack],Tbl);

%% tArea
rev_comp(I,[{absolute_area,{tArea,[[{start_cell,Start}|{end_cell,End}]|_R1],
                            _R2}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,make_range(Start,End)}|Stack],Tbl);

%% tMemArea
rev_comp(Index,[{memory_area,{tMemArea,[{value,_MemArea},{type,_Type}],
                              {return,reference}}}|T],TokArr,Stack,Tbl) ->
%% See discussion in Section 3.1.6 of excelfileformat.v1.40.pdf
%%
%% for tMemArea there is an appended set of tokens in the TokArr
%% which must be read in and then *DISCARDED*
%%
%% for instance if the tMemArea is "A1:A2 A2:A3" then the TokArr
%% will contain the *RESULT* of the intersection, ie "A2"
%%
%% So to calculate Excel would discard the value that would be
%% in the tMemArea token - 19 in this instance - but use "A2"
%% whereas reverse compiling is only interested in the full
%% range "A1:A2 A2:A3" and discards the TokArr "A2"
%% ie the first return variable in the next line
    {_Array,ArrayTail}=excel_util:read_cell_range_add_list(TokArr,'16bit'),
    % Given that this token holds a look up to the results of parsing
    % the subsequent array we can just chuck it away.
    rev_comp(Index,T,ArrayTail,Stack,Tbl);

%% tMemErr - this token does nothing for reverse compile - skip...
rev_comp(I,[{memory_err,{tMemErr,_ValType,_Ret}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,Stack,Tbl);

%% tMemNoMem

%% tMemFunc - this token does nothing for reverse compile - skip...
rev_comp(I,[{memory_function,{tMemFunc,_ValType,_Ret}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,Stack,Tbl);

%% tRefErr
rev_comp(I,[{reference_error,{tRefErr,_Type,_Ret}}|T],TokArr,Stack,Tbl)  ->
    rev_comp(I,T,TokArr,[{string,"#REF!"}|Stack],Tbl);

%% tAreaErr
rev_comp(I,[{area_error,{tAreaErr,_Type,_Ret}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,"#REF!"}|Stack],Tbl);

%% tRefN
%% these addresses may or may not be relative and must be added to
%% the top point Excel has a row limit of 65535 (2^16) rows and a column
%% limit of 256 (2^8) columns so the relative addresses must
%% 'overflow' those  bounds
rev_comp(I,[{relative_reference,{tRefN,[{value,{Row,Col,RowType,ColType}},_Type],_Ret}}|T],TokArr,Stack,Tbl) ->
    {_Sheet,{row_index,TopRow},{col_index,TopCol}} = I,
    NewRow=get_row(Row,TopRow,RowType),
    NewCol=get_col(Col,TopCol,ColType),
    NewCell=make_cell({NewRow,NewCol,RowType,ColType}),
    % by definition it is a relative address so just 'make it so'
    rev_comp(I,T,TokArr,[{string,NewCell}|Stack],Tbl);

%% tAreaN
%% these addresses are relative and must be added to the top point
%% Excel has a row limit of 65535 (2^16) rows and a column limit of
%% 256 (2^8) columns so the relative addresses must 'overflow' those
%% bounds
rev_comp(Index,[{relative_area,{tAreaN,[NewDetails|{type,_Type}],
                                {return,reference}}}|T],TokArr,Stack,Tbl) ->
    {{sheet,_Sheet},{row_index,TopRow},{col_index,TopCol}}=Index,
    [{start_cell,{StartRow,StartCol,StartRowType,StartColType}}|
     {end_cell,{EndRow,EndCol,EndRowType,EndColType}}]=NewDetails,
    NewStartRow=get_row(StartRow,TopRow,StartRowType),
    NewStartCol=get_col(StartCol,TopCol,StartColType),
    NewEndRow=get_row(EndRow,TopRow,EndRowType),
    NewEndCol=get_col(EndCol,TopCol,EndColType),
    StartCell=make_cell({NewStartRow,NewStartCol,StartRowType,StartColType}),
    EndCell=make_cell({NewEndRow,NewEndCol,EndRowType,EndColType}),
    NewRange=StartCell++":"++EndCell,
    rev_comp(Index,T,TokArr,[{string,NewRange}|Stack],Tbl);

%% tMemAreaN

%% tMemNoMemN

%% tFuncCE

%% tNameX
rev_comp(Index,[{name_xref,{tNameX,
                            [{reference_index,Ref},{name_index,NameIdx},_Type],
                            _Ret}}|T],TokArr,Stack,Tbl) ->
    Return = ?read(Tbl,tmp_extsheets, Ref),
    [{_Idx, [{extbook_index, EXBIdx}, {firstsheet,FirstIdx}, _]}] = Return,
    % now work out if this EXBIdx points to the root page of the workbook
    [{_I, [Worksheet, _]}] = ?read(Tbl, tmp_externalbook, EXBIdx),
    NameVal =
        case FirstIdx of
            ?EXTERNALBOOK ->
                case Worksheet of
                    {this_file, expanded} ->
                        [{_I2, [{extbook, _E},{sheetindex, _S}, {type, _},
                                {name, LocalName}, _V]}] = ?read(Tbl,tmp_names,NameIdx),
                        {{sheet,SheetName},_,_}=Index,
                        EscSheet=excel_util:esc_tab_name(SheetName),
                        "../"++EscSheet++"/"++"@"++LocalName;
                    {skipped, add_ins} ->
                        % First get all the externames with the EXBindex of EXBIdx
                        % get the cell name
                        {{sheet,S}, {row_index, R}, {col_index, C}} = Index,
                        % now get the sheet name
                        {value,{_TableName,Tid}}=lists:keysearch(tmp_externnames,1,Tbl),
                        ExtNameList = ets:lookup(Tid, {extbook_index, EXBIdx}), 
                        ExtName = get_extname(ExtNameList, NameIdx),
                        ExtName;
                    _ ->
                        % First get all the externames with the EXBindex of EXBIdx
                        % get the cell name
                        {{sheet,S}, {row_index, R}, {col_index, C}} = Index,
                        % now get the sheet name
                        {value,{_TableName,Tid}}=lists:keysearch(tmp_externnames,1,Tbl),
                        ExtNameList = ets:lookup(Tid, {extbook_index, EXBIdx}), 
                        ExtName = get_extname(ExtNameList, NameIdx),
                        % now get the file name
                        {value,{_TableName2,Tid2}}=lists:keysearch(tmp_externalbook,1,Tbl),
                        Lookup = ets:lookup(Tid2, {index, EXBIdx}),
                        case Lookup of
                            [{_I, [{name, FileName}, _]}] ->
                                Str = io_lib:format("Cell ~s on page ~s has a "++
                                                    "reference to the name ~s"++
                                                    "on file ~s",
                                                    [test_util:rc_to_a1(R,C), S,
                                                     ExtName, FileName]),
                                excel_util:append(Tbl, warnings, lists:flatten(Str)),
                                "#REF!";
                            [{_I, [{skipped, Reason}, _]}] ->
                                Str = io_lib:format("Cell ~s on page ~s has a "++
                                                    "reference to the function "++
                                                    "~s which is not implemented",
                                                    [test_util:rc_to_a1(R,C), S, ExtName]),
                                "#REF!"
                        end
                end
        end,
    rev_comp(Index,T,TokArr,[{string,NameVal}|Stack],Tbl);
%% tRef3d
rev_comp(I,[{three_dee_ref,{tRef3d,[{reference_index,RefIdx},
                                    Ref,_Type],_Ret}}|T],TokArr,Stack,Tbl) ->
    Sheet = get_sheet_ref(RefIdx,Tbl),
    Sheet2=excel_util:esc_tab_name(Sheet),
    rev_comp(I,T,TokArr,[{string,"../"++Sheet2++"/"++make_cell(Ref)}|Stack],
             Tbl);

%% tArea3d
rev_comp(I,[{three_dee_area,{tArea3d,[{reference_index,RefIdx},
                                      Reference,_Type],_Ret}}|T],
         TokArr,Stack,Tbl) ->
    Sheet = get_sheet_ref(RefIdx,Tbl),
    Sheet2=excel_util:esc_tab_name(Sheet),
    [{start_cell,Ref1}|{end_cell,Ref2}] = Reference,
    Range=make_range(Ref1,Ref2),
    rev_comp(I,T,TokArr,[{string,"../"++Sheet2++"/"++Range}|Stack],Tbl);

%% tRefErr3d
rev_comp(I,[{three_dee_error_ref,{tRefErr3d,[{reference_index,_RefIndex},
                                             {type,_Type}],
                                  {return,reference}}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,"!REF"}|Stack],Tbl);

%% tAreaErr3d
rev_comp(I,[{three_dee_area_error,{tAreaErr3d,[{reference_index,_RefIndex},
                                               {type,_Type}],
                                  {return,reference}}}|T],TokArr,Stack,Tbl) ->
    rev_comp(I,T,TokArr,[{string,"!REF"}|Stack],Tbl);

%%%%%%%%%%%%%%%%%%%%%%

%% This will catch missed out stuff...
rev_comp(_Index,Form,TokArr,_Stack,_Tbl) ->
    io:format("in reverse compile missing tokens are ~p with a TokArr of ~p~n",
              [Form,TokArr]),
    exit("missing tokens in excel_rev_comp:reverse_compile").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                         %%%
%%% Internal functions                                                      %%%
%%%                                                                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_extname([{_SBIdx, [{ext_index, NameIdex}, {name, Name}]} | _T], NameIdx) -> Name;
get_extname([_H | T], NameIdx) -> get_extname(T, NameIdx).


get_row(Row,TopRow,RowType)->
    case RowType of
        rel_row -> Row+TopRow-65536*erlang:round((Row+TopRow)/65536);
        abs_row -> Row
    end.

get_col(Col,TopCol,ColType)->
    case ColType of
        rel_col -> Col+TopCol-256*erlang:round((Col+TopCol)/256);
        abs_col -> Col
    end.

%% Looks up a reference to an externsheet and turns it into a sheet ref
get_sheet_ref(Index,Tbl)->
    Record1=?read(Tbl,tmp_extsheets,Index),
    case Record1 of
        [{_,[{extbook_index,EXBIdx},{firstsheet,?EXTERNALBOOK_REF_ERROR},{lastsheet,_}]}] ->
            "#REF";
        [{_,[{extbook_index,EXBIdx},{firstsheet,?EXTERNALBOOK},{lastsheet,_}]}] ->
            exit("cant use get_sheet_ref to get a sheet for a non-local reference");
        [{_,[{extbook_index,EXBIdx},{firstsheet,FS},{lastsheet,FS}]}]->
            get_ref(EXBIdx,FS,Tbl);
        [{_,[{extbook_index,EXBIdx},{firstsheet,FS},{lastsheet,LS}]}] ->
            get_range(EXBIdx,FS,LS,Tbl)
    end.

get_ref(SubRec,FirstSheet,Tbl)->
    Record=?read(Tbl,tmp_externalbook,SubRec),
    [{{index,SubRec},[Location,SheetList]}]=Record,
    % lists:nth is '1' based but the index is 'zero' based!
    case Location of
        {this_file,expanded} -> lists:nth(FirstSheet+1,SheetList);
        {name,Name}          -> Name
    end.

get_range(SubRec,FirstSheet,LastSheet,Tbl)->
    Record=?read(Tbl,tmp_externalbook,SubRec),
    [{{index,SubRec},[Location,SheetList]}]=Record,
    % lists:nth is '1' based but the index is 'zero' based!
    case Location of
        {this_file,expanded} -> Sheet1=lists:nth(FirstSheet+1,SheetList),
                                Sheet2=lists:nth(LastSheet+1, SheetList),
                                lists:concat([Sheet1,":",Sheet2]);
        {name,Name}          -> Name
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                         %%%
%%% These functions read the array tokens that are appended to the formula  %%%
%%% RPN token stream                                                        %%%
%%%                                                                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Read a Constant Value Array
%% defined in Section 2.5.8 of excelfileformatV1-40.pdf
read_const_val_array(Bin)->
    <<NoCols:8/little-unsigned-integer,
     NoRows:16/little-unsigned-integer,
     Rest/binary>>=Bin,
    Sz=(NoCols+1)*(NoRows+1),
    {Array,ArrayTail}= read_token_array(Sz,Rest),
    {{NoCols+1,NoRows+1,Array},ArrayTail}.

%% This function parses the token array
%% defined in Section 2.5.7 of excelfileformatV1-40.pdf
read_token_array(N,Array)->
    read_token_array(N,Array,[]).

read_token_array(0,Bin,Residuum)->
    {lists:reverse(Residuum),Bin};
read_token_array(N,<<?EmptyArrayEl:8/little-unsigned-integer,
                    _NotUsed:8/binary,
                    Rest/binary>>,Residuum)->
    read_token_array(N-1,Rest,[{string,""}|Residuum]);
read_token_array(N,<<?NumberArrayEl:8/little-unsigned-integer,
                    Float:64/little-float,
                    Rest/binary>>,Residuum)->
    read_token_array(N-1,Rest,[{float,Float}|Residuum]);
read_token_array(N,<<?StringArrayEl:8/little-unsigned-integer,
                    Rest/binary>>,Residuum)->
    % The index is always 16 bits in this instance - see note in Section 2.5.7
    % of excelfileformatV1-40.pdf
    % so we jump to the index length of 2 (2 time 8 bits)...
    <<Len:8/little-unsigned-integer,
     NFlags:8/little-unsigned-integer,
     R3/binary>>=Rest,
    BinLen=excel_util:get_len_CRS_Uni16(Len,2,R3,NFlags),
    % The 2nd byte which contains the encoding flags needs to be included...
    % if the String is Rich Text or Asian this next bit might blow up
    % (who knows!)
    <<StrBin:BinLen/binary,R4/binary>>=Rest,
    String2=excel_util:get_utf8(excel_util:parse_CRS_Uni16(StrBin,2)),
    read_token_array(N-1,R4,[{string,"\""++String2++"\""}|Residuum]);
read_token_array(N,<<?BooleanArrayEl:8/little-unsigned-integer,
                    Boolean:8/little-unsigned-integer,
                    _NotUsed:7/binary,
                    Rest/binary>>,Residuum)->
    Value = case Boolean of
                0 -> "FALSE";
                1 -> "TRUE"
            end,
    read_token_array(N-1,Rest,[{string,Value}|Residuum]);
read_token_array(N,<<?ErrorArrayEl:8/little-unsigned-integer,
                    Error:8/little-unsigned-integer,
                    _NotUsed:7/binary,
                    Rest/binary>>,Residuum)->
    Value = case Error of
                ?NullError    -> "#NULL!";
                ?DivZeroError -> "#DIV/0!";
                ?ValueError   -> "#VALUE!";
                ?RefError     -> "#REF!";
                ?NameError    -> "#NAME?";
                ?NumError     -> "#NUM!";
                ?NAError      -> "#N/A"
            end,
    read_token_array(N-1,Rest,[{string,Value}|Residuum]).

%%--------------------------------------------------------------------
%% Function:    implode/2
%%
%% Description: Add all the items in the list together into a string
%%              interspace with 'Token' (number converted to strings).
%%              (Token doesn't get added on to the last element).
%%--------------------------------------------------------------------
%%implode(List, Token) ->
%%    F = fun(X, Acc) ->
%%        if
%%            is_integer(X) -> [Token,integer_to_list(X)|Acc];
%%            %% float like "2.000" to come back as "2"
%%            is_float(X)   -> [Token,to_str({float,X})|Acc];
%%            true          -> [Token,X|Acc]
%%        end
%%    end,
%%    [_TrailingToken|RealList]=lists:foldl(F, [], List),
%%    lists:flatten(lists:reverse(RealList)).

%%% set of functions used by reverse_compile to generate the actual
%%% Formula Strings that are in the cells
to_str(addition)              -> "+";
to_str(subtraction)           -> "-";
to_str(multiply)              -> "*";
to_str(divide)                -> "/";
to_str(power)                 -> "^";
to_str(concatenate)           -> "&";
to_str(less_than)             -> "<";
to_str(less_than_or_equal)    -> "<=";
to_str(equals)                -> "=";
to_str(greater_than_or_equal) -> ">=";
to_str(greater_than)          -> ">";
to_str(not_equal)             -> "<>";
to_str(intersect)             -> " ";
to_str(comma)                 -> ",";

to_str({func,Var,Args})       ->

    R = fun(Item) ->
                case Item of
                    {space,Val} -> Val;
                    Other       ->
                        Tmp = to_str(Other),
                        Tmp ++ ","
                end
        end,

    % Above function will always append an extra comma, strip it
    % without stripping any spaces after it
    F = fun(X,Self) ->
                case X of
                    [$,|Rest] -> lists:reverse(Rest);
                    [32|Rest] -> Self(Rest,Self)++" ";
                    Else      -> lists:reverse(Else)
                end
        end,
    % If the Value of Var is 255 then this is a non-excel function
    % and the 'first arg' is the name that the user actually typed in
    case Var of
        255 ->
            [{string,FuncName}|Args2]=Args,
            case Args2 of
                [] -> FuncName++"()";
                _  ->
                    TmpArgs = lists:flatten(lists:map(R,Args2)),
                    FuncName++"("++string:strip(TmpArgs,right, $,)++")"
            end;
        _Other ->
            case Args of
                [] -> macro_to_string(Var)++"()";
                _  ->
                    TmpArgs = lists:flatten(lists:map(R,Args)),
                    macro_to_string(Var)++"("++F(lists:reverse(TmpArgs),F)++")"
            end
    end;

to_str([])  -> "";
to_str({H}) -> to_str(H);
to_str({space,S}) -> S;
to_str(List) when is_list(List) ->
    case io_lib:deep_char_list(List) of
        true -> lists:flatten(List);
        false ->
            [H|T] = List,
            to_str(H)++to_str(T)
    end;

to_str({open,O,close}) -> "(" ++ to_str(O) ++ ")";
to_str({open,O,close,S}) -> to_str(S)++"(" ++ to_str(O) ++ ")";
to_str({L,O,R}) -> to_str(L) ++ to_str(O) ++ to_str(R);
to_str({string,String}) -> String;
to_str({integer,Val}) -> integer_to_list(Val);
to_str({float,Val}) ->
    case (Val-round(Val)) of
        0.0 -> integer_to_list(round(Val));
        _   -> String=mochinum:digits(Val),
               {_,String2,_}=regexp:gsub(String,[e],$e),
               String2
    end;

to_str({abs_ref,Y,X,rel_row,rel_col}) ->
    util2:make_b26(X)++integer_to_list(Y);
to_str({abs_ref,Y,X,abs_row,rel_col}) ->
    util2:make_b26(X)++"$"++integer_to_list(Y); %"; fix syntax highlight
to_str({abs_ref,Y,X,rel_row,abs_col}) ->
    "$"++util2:make_b26(X)++integer_to_list(Y); %"; fix syntax highlight
to_str({abs_ref,Y,X,abs_row,abs_col}) ->
    "$"++util2:make_b26(X)++"$"++integer_to_list(Y); 
to_str({L,S1,O,S2,R}) ->
    to_str(L)++to_str(S1)++to_str(O)++to_str(S2)++to_str(R).

%% builds up 2D arrays - 1st dimension is given by "," and second by ";"
array_to_str(Array,NoCols,_NoRows) ->
    array_to_str(Array,NoCols,1,["{"]).

array_to_str([],_NoCols,_N,[_Comma|Tail]) -> % cut off the additional ","
    TokArr=lists:reverse(["}"|Tail]),
    lists:flatten(TokArr);
array_to_str([H|T],NoCols,NoCols,Residuum)-> % list seperator is a semi-colon
    NewResiduum=[to_str(H)|Residuum],
    NewResiduum2=[";"|NewResiduum],
    array_to_str(T,NoCols,1,NewResiduum2);
array_to_str([H|T],NoCols,N,Residuum)-> % list seperator is a comma
    NewResiduum=[to_str(H)|Residuum],
    NewResiduum2=[","|NewResiduum],
    array_to_str(T,NoCols,N+1,NewResiduum2).


popSpaces([{space,Val}|T]) -> {{space,Val},T};
popSpaces(List)            -> {[],List}.

%% Used in tFunc, will grab the last I items from the list that
%% aren't {space,Spaces}, {unary,Op} or {return}
%% 
%% WARNING - the terminating clause is placed third from the top
%%           because we are greedy on no-arg values (ie spaces, 
%%           returns and unaries) which we want to keep sooking
%%           into the function until they all run out...
%%           
%%           which also is why the three clauses before the terminator 
%%           don't decrement the counter I :)
%%           
popVars(I,[{space,Val}|T],Args) ->
    popVars(I,T,[{space,Val}|Args]);
popVars(I,[{unary,Op}|T],Args) ->
    popVars(I,T,[{unary,Op}|Args]);
popVars(I,[{return}|T],Args) ->
    popVars(I,T,[{return}|Args]);
%% THIS IS THE TERMINATOR
popVars(0,Stack,Args) ->
    {Stack,lists:reverse(Args)};
popVars(I,[Else|T],Args) ->
    popVars(I-1,T,[Else|Args]).

%% this function makes a range from the start and end cell specifications
make_range(StartCell,EndCell)->
    Return=make_cell(StartCell)++":"++make_cell(EndCell),
    Return.

%% make a cell from 0-Indexed Row and Column indices
%% - this means adding a 1 to the Indices
make_cell({Row,Col,rel_row,rel_col}) ->
    string:to_upper(util2:make_b26(Col+1)++integer_to_list(Row+1));
make_cell({Row,Col,abs_row,rel_col}) ->
    string:to_upper(util2:make_b26(Col+1)++"$"++integer_to_list(Row+1)); %"
                    make_cell({Row,Col,rel_row,abs_col}) ->
                           string:to_upper("$"++util2:make_b26(Col+1)++integer_to_list(Row+1)); %"
                                           make_cell({Row,Col,abs_row,abs_col}) ->
                                                  string:to_upper("$"++util2:make_b26(Col+1)++"$"++integer_to_list(Row+1)).

%% this function looks up the Func ID and converts it to a name
%%
%% Every 10 lines there is a comment dumped from the file xldumper.dat in "
%% the OpenOffice source code  and gives the numerical lookup for the
%% function names *INCLUDING* the VB ones
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
%%  90=DEREF,WINDOWS,SERIES,DOCUMENTS,ACTIVE.CELL,SELECTION,RESULT,ATAN2,
%%     ASIN,ACOS
macro_to_string(97)  -> "ATAN2";
macro_to_string(98)  -> "ASIN";
macro_to_string(99)  -> "ACOS";
%%  100=CHOOSE,HLOOKUP,VLOOKUP,LINKS,INPUT,ISREF,GET.FORMULA,GET.NAME,
%%      SET.VALUE,LOG
macro_to_string(100) -> "CHOOSE";
macro_to_string(101) -> "HLOOKUP";
macro_to_string(102) -> "VLOOKUP";
macro_to_string(105) -> "ISREF";
macro_to_string(109) -> "LOG";
%%  110=EXEC,CHAR,LOWER,UPPER,PROPER,LEFT,RIGHT,EXACT,TRIM,REPLACE
macro_to_string(111) -> "CHAR";
macro_to_string(112) -> "LOWER";
macro_to_string(113) -> "UPPER";
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
%%  150=CALL,ADD.BAR,ADD.MENU,ADD.COMMAND,ENABLE.COMMAND,CHECK.COMMAND,
%%      RENAME.COMMAND,SHOW.BAR,DELETE.MENU,DELETE.COMMAND

%% 160=GET.CHART.ITEM,DIALOG.BOX,CLEAN,MDETERM,MINVERSE,MMULT,FILES,IPMT,
%%     PPMT,COUNTA
macro_to_string(162) -> "CLEAN";
macro_to_string(163) -> "MDETERM";
macro_to_string(164) -> "MINVERSE";
macro_to_string(165) -> "MMULT";
macro_to_string(167) -> "IPMT";
macro_to_string(168) -> "PPMT";
macro_to_string(169) -> "COUNTA";
%%  170=CANCEL.KEY,FOR,WHILE,BREAK,NEXT,INITIATE,REQUEST,POKE,EXECUTE,TERMINATE

%%  180=RESTART,HELP,GET.BAR,PRODUCT,FACT,GET.CELL,GET.WORKSPACE,GET.WINDOW,
%%      GET.DOCUMENT,DPRODUCT
macro_to_string(183) -> "PRODUCT";
macro_to_string(184) -> "FACT";
macro_to_string(189) -> "DPRODUCT";
%%  190=ISNONTEXT,GET.NOTE,NOTE,STDEVP,VARP,DSTDDEVP,DVARP,TRUNC,ISLOGICAL,
%%      DBCOUNTA
macro_to_string(190) -> "ISNONTEXT";
macro_to_string(193) -> "STDEVP";
macro_to_string(194) -> "VARP";
macro_to_string(195) -> "DSTDEVP";
macro_to_string(196) -> "DVARP";
macro_to_string(197) -> "TRUNC";
macro_to_string(198) -> "ISLOGICAL";
macro_to_string(199) -> "DCOUNTA";
%%  200=DELETE.BAR,UNREGISTER,,USDOLLAR,FINDB,SEARCHB,REPLACEB,LEFTB,RIGHTB
%%  the lookup file appears to contain 'not enought' records to make
%%  this work (ie 9 not 10)
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
%%  230=COSH,TANH,ASINH,ACOSH,ATANH,DGET,CREATE.OBJECT,VOLATILE,LAST.ERROR,
%%      CUSTOM.UNDO
macro_to_string(230) -> "COSH";
macro_to_string(231) -> "TANH";
macro_to_string(232) -> "ASINH";
macro_to_string(233) -> "ACOSH";
macro_to_string(234) -> "ATANH";
macro_to_string(235) -> "DGET";
%%  240=CUSTOM.REPEAT,FORMULA.CONVERT,GET.LINK.INFO,TEXT.BOX,INFO,GROUP,
%%      GET.OBJECT,DB,PAUSE,
macro_to_string(244) -> "INFO";
macro_to_string(247) -> "DB";
%%  250=,RESUME,FREQUENCY,ADD.TOOLBAR,DELETE.TOOLBAR,EXTERN.CALL,
%%       RESET.TOOLBAR,EVALUATE,GET.TOOLBAR,GET.TOOL
macro_to_string(252) -> "FREQUENCY";
macro_to_string(255) -> "FUNCTIONDOESNTEXIST";
%%  260=SPELLING.CHECK,ERROR.TYPE,APP.TITLE,WINDOW.TITLE,SAVE.TOOLBAR,
%%      ENABLE.TOOL,PRESS.TOOL,REGISTER.ID,GET.WORKBOOK,AVEDEV
macro_to_string(261) -> "ERRORTYPE";
macro_to_string(269) -> "AVEDEV";
%%  270=BETADIST,GAMMALN,BETAINV,BINOMDIST,CHIDIST,CHIINV,COMBIN,
%%      CONFIDENCE,CRITBINOM,EVEN
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
%%  280=EXPONDIST,FDIST,FINV,FISHER,FISHERINV,FLOOR,GAMMADIST,GAMMAINV,
%%      CEILING,HYPGEOMDIST
macro_to_string(280) -> "EXPONDIST";
macro_to_string(281) -> "FDIST";
macro_to_string(282) -> "FINV";
macro_to_string(283) -> "FISHER";
macro_to_string(284) -> "FISHERINV";
macro_to_string(285) -> "FLOOR";
macro_to_string(286) -> "GAMMADIST";
macro_to_string(287) -> "GAMMAINV";
macro_to_string(288) -> "CEILING";
macro_to_string(289) -> "HYPGEOMDIST";
%%  290=LOGNORMDIST,LOGINV,NEGBINOMDIST,NORMDIST,NORMSDIST,NORMINV,NORMSINV,
%%      STANDARDIZE,ODD,PERMUT
macro_to_string(290) -> "LOGNORMDIST";
macro_to_string(291) -> "LOGINV";
macro_to_string(292) -> "NEGBINOMDIST";
macro_to_string(293) -> "NORMDIST";
macro_to_string(294) -> "NORMSDIST";
macro_to_string(295) -> "NORMINV";
macro_to_string(296) -> "NORMSINV";
macro_to_string(297) -> "STANDARDIZE";
macro_to_string(298) -> "ODD";
macro_to_string(299) -> "PERMUT";
%%  300=POISSON,TDIST,WEIBULL,SUMXMY2,SUMX2MY2,SUMX2PY2,CHITEST,CORREL,
%%      COVAR,FORECAST
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
%%   320=HARMEAN,SUMSQ,KURT,SKEW,ZTEST,LARGE,SMALL,QUARTILE,PERCENTILE,
%%       PERCENTRANK
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
%%  330=MODE,TRIMMEAN,TINV,,MOVIE.COMMAND,GET.MOVIE,CONCATENATE,POWER,
%%      PIVOT.ADD.DATA,GET.PIVOT.TABLE
macro_to_string(330) -> "MODE";
macro_to_string(331) -> "TRIMMEAN";
macro_to_string(332) -> "TINV";
macro_to_string(336) -> "CONCATENATE";
macro_to_string(337) -> "POWER";
%%  340=GET.PIVOT.FIELD,GET.PIVOT.ITEM,RADIANS,DEGREES,SUBTOTAL,SUMIF,
%%      COUNTIF,COUNTBLANK,SCENARIO.GET,OPTIONS.LISTS.GET
macro_to_string(342) -> "RADIANS";
macro_to_string(343) -> "DEGREES";
macro_to_string(344) -> "SUBTOTAL";
macro_to_string(345) -> "SUMIF";
macro_to_string(346) -> "COUNTIF";
macro_to_string(347) -> "COUNTBLANK";
%%  350=ISPMT,DATEDIF,DATESTRING,NUMBERSTRING,ROMAN,OPEN.DIALOG,SAVE.DIALOG,
%%      VIEW.GET,GETPIVOTDATA,HYPERLINK
macro_to_string(350) -> "ISPMT";
macro_to_string(351) -> "DATEDIF";
macro_to_string(352) -> "DATESTRING";
macro_to_string(353) -> "NUMBERSTRING";
macro_to_string(354) -> "ROMAN";
macro_to_string(358) -> "GETPIVOTDATA";
macro_to_string(359) -> "HYPERLINK";
%%  360=PHONETIC,AVERAGEA,MAXA,MINA,STDEVPA,VARPA,STDEVA,VARA,BAHTTEXT,
%%      THAIDAYOFWEEK
macro_to_string(360) -> "PHONETIC";
macro_to_string(361) -> "AVERAGEA";
macro_to_string(362) -> "MAXA";
macro_to_string(363) -> "MINA";
macro_to_string(364) -> "STDEVPA";
macro_to_string(365) -> "VARPA";
macro_to_string(366) -> "STDEVA";
macro_to_string(367) -> "VARA".
%%  370=THAIDIGIT,THAIMONTHOFYEAR,THAINUMSOUND,THAINUMSTRING,THAISTRINGLENGTH,
%%      ISTHAIDIGIT,ROUNDBAHTDOWN,ROUNDBAHTUP,THAIYEAR,RTD
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
%% Don't know enought about Pivot Tables to test this so will
%% bodge for now
%%
%% GG 2007_01_18
macro_no_of_args(358) ->
    io:format("in excel_rev_comp:macro_no_of_args for GETPIVOTDATA "++
              "this is a cludge!~n"),
    2;
macro_no_of_args(360) -> 1.

