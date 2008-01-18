-module(muin_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("parser_desc.yrl", 164).

-import(lists, [all/2, filter/2, foldl/3, map/2, member/2]).

%% Make a function name for the AST from lexer tokens.
func_name({id, NameAsStr}) ->
    list_to_atom(NameAsStr).


%% TODO: Worth keeping?
lit({cellref, Data}) ->
    [sscellref, "./" ++ Data];

lit({sscellref, Data}) ->
    [sscellref, Data];

lit({var, Data}) ->
    [var, Data];

lit({date, Data}) ->
    [date, Data]; %% Not sure what to do here yet.

lit({_Type, Data}) ->
    Data.


col({id, Data}) ->
    {col, Data}.


row({integer, Data}) ->
    {row, integer_to_list(Data)}. %% Makes things more straightforward in the interpreter.


%% Make an op function call for the AST from lexer tokens.
op(Arg1, OpTuple, Arg2) ->
    [element(1, OpTuple), Arg1, Arg2].


%% Make a straight-up function call for the AST from a token and a list of args.
func(IdTuple, Args) ->
    [func_name(IdTuple)] ++ Args.


make_intersection({intersection, YYtext}) ->
    {ok, Tmp, _} = regexp:gsub(YYtext, "(\s)+", " "), % Normalize whitespace.
    Ranges = map(fun string:to_lower/1, % lowercase
                 string:tokens(Tmp, " ")), % split into ranges
    Cells = map(fun muin_util:expand_cellrange/1, Ranges),

    %% Calculate intersections pairwise. At each step, Acc is the intersection
    %% of two previous ranges.
    Intersection = foldl(fun(L, Acc) ->
                                 filter(fun(X) ->
                                                member(X, Acc)
                                        end,
                                        L)
                         end,
                         hd(Cells),
                         tl(Cells)),
    
    map(fun(X) ->
                [sscellref, {"./" ++ X}]
        end,
        Intersection).
                      

all_rows_eql(MyArray) ->
    RowLen = length(element(2, hd(MyArray))),
    all(fun(X) ->
                {row, Values} = X,
                length(Values) == RowLen
        end,
        MyArray).


%% Convert representation of array in AST into Erlang's native list-of-lists.
to_native_list(MyArray) ->
    case all_rows_eql(MyArray) of
        false ->
            throw(invalid_array);
        true ->
            %% Tail cos there'll be an extra [] in the list after the fold.
            tl(foldl(fun(Row, Acc) ->
                             {row, Elts} = Row,
                             Acc ++ [Elts]
                     end,
                     [[]], %% <== See, here it is.
                     MyArray))
    end.

-file("/usr/local/lib/erlang/lib/parsetools-1.4.2/include/yeccpre.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, 
              Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("muin_parser.erl", 238).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(S, boolean, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, date, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, open_curly, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, sscellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'E\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'E\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'E\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'E\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, gt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, gte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, lt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, lte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, neq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Formula\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_15: see yeccpars2_2

yeccpars2_16(S, boolean, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_17: see yeccpars2_2

%% yeccpars2_18: see yeccpars2_2

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, gt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, gte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, lt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, lte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, neq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Uplus\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_23: see yeccpars2_2

%% yeccpars2_24: see yeccpars2_2

%% yeccpars2_25: see yeccpars2_2

%% yeccpars2_26: see yeccpars2_2

%% yeccpars2_27: see yeccpars2_2

%% yeccpars2_28: see yeccpars2_2

%% yeccpars2_29: see yeccpars2_2

%% yeccpars2_30: see yeccpars2_2

%% yeccpars2_31: see yeccpars2_2

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_33: see yeccpars2_2

%% yeccpars2_34: see yeccpars2_2

%% yeccpars2_35: see yeccpars2_2

yeccpars2_36(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, close_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, gt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, gte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, lt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, lte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, neq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'E\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_50(S, close_curly, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, semicolon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccpars2('yeccgoto_\'ArrayRows\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccpars2('yeccgoto_\'ArrayRow\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2('yeccgoto_\'ArrayLiteral\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccpars2('yeccgoto_\'ArrayLiteral\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccpars2('yeccgoto_\'ArrayLiteral\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccpars2('yeccgoto_\'ArrayLiteral\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_57: see yeccpars2_16

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ArrayRow\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_16

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ArrayRows\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Array\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Uminus\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(S, boolean, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, close_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, date, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, intersection, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, open_curly, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, sscellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, sscolref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, ssrowref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Range\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccpars2('yeccgoto_\'Args\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_66(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Args\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'E\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(S, caret, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, comma, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, concat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, eq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, gt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, gte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, lt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, lte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, neq, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, percent, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, slash, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, times, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccpars2('yeccgoto_\'Args\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Range\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Range\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(S, close_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Funcall\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccpars2('yeccgoto_\'Intersection\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccpars2('yeccgoto_\'Literal\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RowRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RowRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ColRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ColRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_89(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RowRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RowRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ColRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ColRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'Funcall\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, boolean, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, date, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, intersection, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, minus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, open_curly, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, plus, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, sscellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, sscolref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, ssrowref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, cellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, id, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, sscellref, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, open_paren, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'ColRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RowRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'CellRange\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_107: see yeccpars2_99

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_109: see yeccpars2_99

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Args\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'Args\''(63) -> 71;
'yeccgoto_\'Args\''(99) -> 100;
'yeccgoto_\'Args\''(107) -> 108;
'yeccgoto_\'Args\''(109) -> 110.

'yeccgoto_\'Array\''(2) -> 8;
'yeccgoto_\'Array\''(15) -> 8;
'yeccgoto_\'Array\''(17) -> 8;
'yeccgoto_\'Array\''(18) -> 8;
'yeccgoto_\'Array\''(23) -> 8;
'yeccgoto_\'Array\''(24) -> 8;
'yeccgoto_\'Array\''(25) -> 8;
'yeccgoto_\'Array\''(26) -> 8;
'yeccgoto_\'Array\''(27) -> 8;
'yeccgoto_\'Array\''(28) -> 8;
'yeccgoto_\'Array\''(29) -> 8;
'yeccgoto_\'Array\''(30) -> 8;
'yeccgoto_\'Array\''(31) -> 8;
'yeccgoto_\'Array\''(33) -> 8;
'yeccgoto_\'Array\''(34) -> 8;
'yeccgoto_\'Array\''(35) -> 8;
'yeccgoto_\'Array\''(63) -> 8;
'yeccgoto_\'Array\''(99) -> 8;
'yeccgoto_\'Array\''(107) -> 8;
'yeccgoto_\'Array\''(109) -> 8.

'yeccgoto_\'ArrayLiteral\''(16) -> 52;
'yeccgoto_\'ArrayLiteral\''(57) -> 52;
'yeccgoto_\'ArrayLiteral\''(59) -> 52.

'yeccgoto_\'ArrayRow\''(16) -> 51;
'yeccgoto_\'ArrayRow\''(57) -> 58;
'yeccgoto_\'ArrayRow\''(59) -> 51.

'yeccgoto_\'ArrayRows\''(16) -> 50;
'yeccgoto_\'ArrayRows\''(59) -> 60.

'yeccgoto_\'CellRange\''(63) -> 70;
'yeccgoto_\'CellRange\''(99) -> 70;
'yeccgoto_\'CellRange\''(107) -> 70;
'yeccgoto_\'CellRange\''(109) -> 70.

'yeccgoto_\'ColRange\''(63) -> 69;
'yeccgoto_\'ColRange\''(99) -> 69;
'yeccgoto_\'ColRange\''(107) -> 69;
'yeccgoto_\'ColRange\''(109) -> 69.

'yeccgoto_\'E\''(2) -> 7;
'yeccgoto_\'E\''(15) -> 62;
'yeccgoto_\'E\''(17) -> 48;
'yeccgoto_\'E\''(18) -> 22;
'yeccgoto_\'E\''(23) -> 47;
'yeccgoto_\'E\''(24) -> 46;
'yeccgoto_\'E\''(25) -> 45;
'yeccgoto_\'E\''(26) -> 44;
'yeccgoto_\'E\''(27) -> 43;
'yeccgoto_\'E\''(28) -> 42;
'yeccgoto_\'E\''(29) -> 41;
'yeccgoto_\'E\''(30) -> 40;
'yeccgoto_\'E\''(31) -> 39;
'yeccgoto_\'E\''(33) -> 38;
'yeccgoto_\'E\''(34) -> 37;
'yeccgoto_\'E\''(35) -> 36;
'yeccgoto_\'E\''(63) -> 68;
'yeccgoto_\'E\''(99) -> 68;
'yeccgoto_\'E\''(107) -> 68;
'yeccgoto_\'E\''(109) -> 68.

'yeccgoto_\'Formula\''(0) -> 1.

'yeccgoto_\'Funcall\''(2) -> 6;
'yeccgoto_\'Funcall\''(15) -> 6;
'yeccgoto_\'Funcall\''(17) -> 6;
'yeccgoto_\'Funcall\''(18) -> 6;
'yeccgoto_\'Funcall\''(23) -> 6;
'yeccgoto_\'Funcall\''(24) -> 6;
'yeccgoto_\'Funcall\''(25) -> 6;
'yeccgoto_\'Funcall\''(26) -> 6;
'yeccgoto_\'Funcall\''(27) -> 6;
'yeccgoto_\'Funcall\''(28) -> 6;
'yeccgoto_\'Funcall\''(29) -> 6;
'yeccgoto_\'Funcall\''(30) -> 6;
'yeccgoto_\'Funcall\''(31) -> 6;
'yeccgoto_\'Funcall\''(33) -> 6;
'yeccgoto_\'Funcall\''(34) -> 6;
'yeccgoto_\'Funcall\''(35) -> 6;
'yeccgoto_\'Funcall\''(63) -> 67;
'yeccgoto_\'Funcall\''(80) -> 81;
'yeccgoto_\'Funcall\''(83) -> 84;
'yeccgoto_\'Funcall\''(86) -> 87;
'yeccgoto_\'Funcall\''(89) -> 90;
'yeccgoto_\'Funcall\''(92) -> 93;
'yeccgoto_\'Funcall\''(95) -> 96;
'yeccgoto_\'Funcall\''(99) -> 67;
'yeccgoto_\'Funcall\''(101) -> 102;
'yeccgoto_\'Funcall\''(107) -> 67;
'yeccgoto_\'Funcall\''(109) -> 67.

'yeccgoto_\'Intersection\''(63) -> 66;
'yeccgoto_\'Intersection\''(99) -> 66;
'yeccgoto_\'Intersection\''(107) -> 66;
'yeccgoto_\'Intersection\''(109) -> 66.

'yeccgoto_\'Literal\''(2) -> 5;
'yeccgoto_\'Literal\''(15) -> 5;
'yeccgoto_\'Literal\''(17) -> 5;
'yeccgoto_\'Literal\''(18) -> 5;
'yeccgoto_\'Literal\''(23) -> 5;
'yeccgoto_\'Literal\''(24) -> 5;
'yeccgoto_\'Literal\''(25) -> 5;
'yeccgoto_\'Literal\''(26) -> 5;
'yeccgoto_\'Literal\''(27) -> 5;
'yeccgoto_\'Literal\''(28) -> 5;
'yeccgoto_\'Literal\''(29) -> 5;
'yeccgoto_\'Literal\''(30) -> 5;
'yeccgoto_\'Literal\''(31) -> 5;
'yeccgoto_\'Literal\''(33) -> 5;
'yeccgoto_\'Literal\''(34) -> 5;
'yeccgoto_\'Literal\''(35) -> 5;
'yeccgoto_\'Literal\''(63) -> 5;
'yeccgoto_\'Literal\''(99) -> 5;
'yeccgoto_\'Literal\''(107) -> 5;
'yeccgoto_\'Literal\''(109) -> 5.

'yeccgoto_\'Range\''(63) -> 65;
'yeccgoto_\'Range\''(99) -> 65;
'yeccgoto_\'Range\''(107) -> 65;
'yeccgoto_\'Range\''(109) -> 65.

'yeccgoto_\'RowRange\''(63) -> 64;
'yeccgoto_\'RowRange\''(99) -> 64;
'yeccgoto_\'RowRange\''(107) -> 64;
'yeccgoto_\'RowRange\''(109) -> 64.

'yeccgoto_\'Uminus\''(2) -> 4;
'yeccgoto_\'Uminus\''(15) -> 4;
'yeccgoto_\'Uminus\''(17) -> 4;
'yeccgoto_\'Uminus\''(18) -> 4;
'yeccgoto_\'Uminus\''(23) -> 4;
'yeccgoto_\'Uminus\''(24) -> 4;
'yeccgoto_\'Uminus\''(25) -> 4;
'yeccgoto_\'Uminus\''(26) -> 4;
'yeccgoto_\'Uminus\''(27) -> 4;
'yeccgoto_\'Uminus\''(28) -> 4;
'yeccgoto_\'Uminus\''(29) -> 4;
'yeccgoto_\'Uminus\''(30) -> 4;
'yeccgoto_\'Uminus\''(31) -> 4;
'yeccgoto_\'Uminus\''(33) -> 4;
'yeccgoto_\'Uminus\''(34) -> 4;
'yeccgoto_\'Uminus\''(35) -> 4;
'yeccgoto_\'Uminus\''(63) -> 4;
'yeccgoto_\'Uminus\''(99) -> 4;
'yeccgoto_\'Uminus\''(107) -> 4;
'yeccgoto_\'Uminus\''(109) -> 4.

'yeccgoto_\'Uplus\''(2) -> 3;
'yeccgoto_\'Uplus\''(15) -> 3;
'yeccgoto_\'Uplus\''(17) -> 3;
'yeccgoto_\'Uplus\''(18) -> 3;
'yeccgoto_\'Uplus\''(23) -> 3;
'yeccgoto_\'Uplus\''(24) -> 3;
'yeccgoto_\'Uplus\''(25) -> 3;
'yeccgoto_\'Uplus\''(26) -> 3;
'yeccgoto_\'Uplus\''(27) -> 3;
'yeccgoto_\'Uplus\''(28) -> 3;
'yeccgoto_\'Uplus\''(29) -> 3;
'yeccgoto_\'Uplus\''(30) -> 3;
'yeccgoto_\'Uplus\''(31) -> 3;
'yeccgoto_\'Uplus\''(33) -> 3;
'yeccgoto_\'Uplus\''(34) -> 3;
'yeccgoto_\'Uplus\''(35) -> 3;
'yeccgoto_\'Uplus\''(63) -> 3;
'yeccgoto_\'Uplus\''(99) -> 3;
'yeccgoto_\'Uplus\''(107) -> 3;
'yeccgoto_\'Uplus\''(109) -> 3.

-compile({inline,{yeccpars2_7_,1}}).
-file("parser_desc.yrl", 50).
yeccpars2_7_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("parser_desc.yrl", 90).
yeccpars2_9_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("parser_desc.yrl", 93).
yeccpars2_10_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("parser_desc.yrl", 92).
yeccpars2_11_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("parser_desc.yrl", 89).
yeccpars2_12_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("parser_desc.yrl", 88).
yeccpars2_14_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("parser_desc.yrl", 94).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("parser_desc.yrl", 91).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("parser_desc.yrl", 95).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("parser_desc.yrl", 78).
yeccpars2_22_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("parser_desc.yrl", 71).
yeccpars2_32_([__2,__1 | Stack]) ->
 [begin
   [ divide , __1 , [ integer , 100 ] ]
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("parser_desc.yrl", 66).
yeccpars2_36_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("parser_desc.yrl", 67).
yeccpars2_37_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , { divide } , __3 )
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("parser_desc.yrl", 64).
yeccpars2_38_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("parser_desc.yrl", 54).
yeccpars2_39_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("parser_desc.yrl", 65).
yeccpars2_40_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("parser_desc.yrl", 58).
yeccpars2_41_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("parser_desc.yrl", 56).
yeccpars2_42_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("parser_desc.yrl", 57).
yeccpars2_43_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("parser_desc.yrl", 55).
yeccpars2_44_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("parser_desc.yrl", 53).
yeccpars2_45_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("parser_desc.yrl", 61).
yeccpars2_46_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("parser_desc.yrl", 68).
yeccpars2_47_([__3,__2,__1 | Stack]) ->
 [begin
   op ( __1 , { power } , __3 )
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("parser_desc.yrl", 81).
yeccpars2_49_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("parser_desc.yrl", 101).
yeccpars2_51_([__1 | Stack]) ->
 [begin
   [ { row , __1 } ]
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("parser_desc.yrl", 104).
yeccpars2_52_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("parser_desc.yrl", 109).
yeccpars2_53_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("parser_desc.yrl", 108).
yeccpars2_54_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("parser_desc.yrl", 107).
yeccpars2_55_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("parser_desc.yrl", 110).
yeccpars2_56_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("parser_desc.yrl", 105).
yeccpars2_58_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 ] ++ __3
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("parser_desc.yrl", 102).
yeccpars2_60_([__3,__2,__1 | Stack]) ->
 [begin
   [ { row , __1 } ] ++ __3
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("parser_desc.yrl", 99).
yeccpars2_61_([__3,__2,__1 | Stack]) ->
 [begin
   to_native_list ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("parser_desc.yrl", 75).
yeccpars2_62_([__2,__1 | Stack]) ->
 [begin
   [ negate , __2 ]
  end | Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("parser_desc.yrl", 120).
yeccpars2_65_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("parser_desc.yrl", 116).
yeccpars2_68_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("parser_desc.yrl", 93).
yeccpars2_72_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("parser_desc.yrl", 113).
yeccpars2_73_([__3,__2,__1 | Stack]) ->
 [begin
   [ func_name ( __1 ) ]
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("parser_desc.yrl", 88).
yeccpars2_75_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("parser_desc.yrl", 125).
yeccpars2_76_([__1 | Stack]) ->
 [begin
   make_intersection ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("parser_desc.yrl", 94).
yeccpars2_77_([__1 | Stack]) ->
 [begin
   lit ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("parser_desc.yrl", 156).
yeccpars2_81_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("parser_desc.yrl", 151).
yeccpars2_82_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , row ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("parser_desc.yrl", 148).
yeccpars2_84_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("parser_desc.yrl", 143).
yeccpars2_85_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , col ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("parser_desc.yrl", 139).
yeccpars2_87_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("parser_desc.yrl", 133).
yeccpars2_88_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("parser_desc.yrl", 155).
yeccpars2_90_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , row ( __1 ) , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("parser_desc.yrl", 153).
yeccpars2_91_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , { ssrowref , "./" ++ integer_to_list ( element ( 2 , __1 ) ) } , row ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("parser_desc.yrl", 147).
yeccpars2_93_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , col ( __1 ) , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("parser_desc.yrl", 145).
yeccpars2_94_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , { sscolref , "./" ++ element ( 2 , __1 ) } , col ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("parser_desc.yrl", 140).
yeccpars2_96_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("parser_desc.yrl", 135).
yeccpars2_97_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , { sscellref , "./" ++ element ( 2 , __1 ) } , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("parser_desc.yrl", 114).
yeccpars2_98_([__4,__3,__2,__1 | Stack]) ->
 [begin
   func ( __1 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("parser_desc.yrl", 118).
yeccpars2_100_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 ] ++ __3
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("parser_desc.yrl", 136).
yeccpars2_102_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("parser_desc.yrl", 137).
yeccpars2_103_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("parser_desc.yrl", 146).
yeccpars2_104_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , col ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("parser_desc.yrl", 154).
yeccpars2_105_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , row ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("parser_desc.yrl", 138).
yeccpars2_106_([__3,__2,__1 | Stack]) ->
 [begin
   [ ':' , __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("parser_desc.yrl", 119).
yeccpars2_108_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 ] ++ __3
  end | Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("parser_desc.yrl", 121).
yeccpars2_110_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 ] ++ __3
  end | Stack].


-file("parser_desc.yrl", 254).
