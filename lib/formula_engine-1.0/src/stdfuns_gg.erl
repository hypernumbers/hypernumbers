%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc various odds and sods
%%% @todo should be broken up and redistributed into proper functions
%%% @private

-module(stdfuns_gg).
-export([harmean/1,
         mirr/1,
         npv/1,
         poisson/1,
         rand/1,
         cumpoisson/1,
         roman/1,
         sln/1,
         syd/1,
         text/1]).

-include("typechecks.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

%% Constants for roman - types of return
-define(CLASSIC,0).
-define(CONCISE1,1).
-define(CONCISE2,2).
-define(CONCISE3,3).
-define(SIMPLIFIED,4).

harmean(List) when is_list(List) ->
    harmean(List,0,0).

harmean([],Num,Acc)   -> Num/Acc;
harmean([H|T],Num,Acc)-> harmean(T,Num+1,(1/H)+Acc).

%% hyperlink(URL,Text) -> "<a href=\""++URL++"\">"++Text++"</a>".

mirr(List) when is_list(List) -> 
    io:format("in stdfuns_gg:mirr this aint working! "++
	      "dunno why I think it correctly implements "++
	      "the bloody algorythm!"),
    mirr2(lists:reverse(List)).

mirr2([ReRate,InvRate|Transactions]) ->
    {ReTrans,InvTrans,NumTrans}=transsort(Transactions),
    io:format("in stdfuns_gg:mirr2 ReRate is ~p InvRate is ~p~n-"++
	      "Transactions are ~p~n-ReTrans are ~p~n-InvTrans are ~p~n"++
	      "-NumTrans is ~p~n",
	      [ReRate,InvRate,Transactions,ReTrans,InvTrans,NumTrans]),
    ReNPV=npv([ReRate|ReTrans]),
    InvNPV=npv([InvRate|InvTrans]),
    io:format("in stdfuns_gg:mirr2 ReNPV is ~p InvNPV is ~p~n",[ReNPV,InvNPV]),
    Numerator=-ReNPV*math:pow((1+ReRate),NumTrans),
    Denominator=InvNPV*(1+InvRate),
    io:format("in stdfuns_gg:mirr2 Numerator is ~p Denominator is ~p~n",
	      [Numerator,Denominator]),
    Fraction=Numerator/Denominator,
    io:format("in stdfuns_gg:mirr2 Fraction is ~p~n",[Fraction]),
    math:pow((Numerator/Denominator),(1/(NumTrans-1)))-1.

%% internal function for mirr
transsort(List) when is_list(List) -> transsort(List,[],[],0).

%% BEWARE!!
%% there is a double list reverse
%% - we list reverse in going from mirr to mirr2
%%   so we DONT list reverse when returning these lists!
%%   ('cos we reversed them in building them, duh!)
transsort([],Pos,Neg,Num)               -> {Pos,Neg,Num};
transsort([H|T],Pos,Neg,Num) when H > 0 -> transsort(T,[H|Pos],Neg,Num+1);
transsort([H|T],Pos,Neg,Num) when H < 0 -> transsort(T,Pos,[H|Neg],Num+1).

npv([Rate|Payments]) -> io:format("in stdfns_gg:npv Rate is ~p "++
				  "Payments are ~p~n",[Rate,Payments]),
			npv1(Rate,Payments,1,0).

npv1(_Rate,[],_Num,Acc)   -> Acc;
npv1(Rate,[H|T],Num,Acc) -> npv1(Rate,T,Num+1,Acc+H/(math:pow((1+Rate),Num))).
    
poisson([X,Lambda,false]) when X >= 0, Lambda; is_float(X) > 0 ->
    noncumpoisson(trunc(X),Lambda);
poisson([X,Lambda,false]) when X >= 0, Lambda; is_integer(X) > 0 ->
    noncumpoisson(X,Lambda);
poisson([X,Lambda,true]) when X >= 0, Lambda > 0; is_float(X) ->
    cumpoisson([trunc(X),Lambda,0]);
poisson([X,Lambda,true]) when X >= 0, Lambda > 0; is_float(X) ->
    cumpoisson([X,Lambda,0]).

noncumpoisson(X,Lambda) ->
    %% get Euler's number
    %% yeah, yeah case is fucked, don't blame me, blame Joe :)
    %% this is 'e' - that irrational number, blah, blah...
    E=math:exp(1),
    io:format("in stdfuns:noncumpoisson E is ~p X is ~p Lambda is ~p~n",
	      [E,X,Lambda]),
    math:pow(E,-Lambda)*math:pow(Lambda,X)/stdfuns_math:fact([X]).

cumpoisson([X,Lambda]) -> cumpoisson2(X,Lambda,0).

cumpoisson2(-1,_Lambda,Acc) -> Acc;
cumpoisson2(K,Lambda,Acc)  -> cumpoisson([K-1,Lambda,
                                          Acc+noncumpoisson(K,Lambda)]).

rand([]) -> gen_server:call(random_srv,{random,float}).

roman([X])        -> roman([X, 0]);
roman([X, true])  -> roman([X, ?CLASSIC]);
roman([X, false]) -> roman([X, ?CONCISE3]);
roman([X, Type])  when is_integer(X) ->
    %% we need to build the roman numbers right to left so we have to
    %% reverse the string representation of the number
    List=make_list(integer_to_list(X)),
    get_roman(List, Type);
roman([X, Type])  when is_float(X) ->
    %% we need to build the roman numbers right to left so we have to
    %% reverse the string representation of the number
    List=make_list(integer_to_list(round(X))),
    get_roman(List, Type).

%% first deal with the single digit number
get_roman(["0"],_) -> "";
get_roman(["1"],_) -> "I";
get_roman(["2"],_) -> "II";
get_roman(["3"],_) -> "III";
get_roman(["4"],_) -> "IV";
get_roman(["5"],_) -> "V";
get_roman(["6"],_) -> "VI";
get_roman(["7"],_) -> "VII";
get_roman(["8"],_) -> "VIII";
get_roman(["9"],_) -> "IX";

%% Now deal with the 2 digit numbers
%% 45 to 49 and 95 to 99 are the problems here

%% First classic
get_roman(["4","5"],?CLASSIC) -> "XLV";
get_roman(["4","6"],?CLASSIC) -> "XLVI";
get_roman(["4","7"],?CLASSIC) -> "XLVII";
get_roman(["4","8"],?CLASSIC) -> "XLVIII";
get_roman(["4","9"],?CLASSIC) -> "XLIX";

get_roman(["9","5"],?CLASSIC) -> "XCV";
get_roman(["9","6"],?CLASSIC) -> "XCVI";
get_roman(["9","7"],?CLASSIC) -> "XCVII";
get_roman(["9","8"],?CLASSIC) -> "XCVIII";
get_roman(["9","9"],?CLASSIC) -> "XCIX";
     
%% Then the rest...
get_roman(["4","5"],_) -> "VL";
get_roman(["4","6"],_) -> "VLI";
get_roman(["4","7"],_) -> "VLII";
get_roman(["4","8"],_) -> "VLIII";
%% special for 49
get_roman(["4","9"],?CONCISE1) -> "VLIV";
get_roman(["4","9"],_)         -> "IL";

get_roman(["9","5"],_) -> "VC";
get_roman(["9","6"],_) -> "VCI";
get_roman(["9","7"],_) -> "VCII";
get_roman(["9","8"],_) -> "VCIII";
%% special for 99
get_roman(["9","9"],?CONCISE1) -> "VCIV";
get_roman(["9","9"],_)         -> "IC";

%% Now build the rest of the 2 digits
get_roman([Second,First],_) -> get_roman2([Second])++get_roman([First],?CLASSIC);

%% Now build the three digits

%% the issue is the digits representing 440-499 and 940-999
%% deal with 445 to 449 first
%% First classic
get_roman(["4","4","5"],?CLASSIC) -> "CDXLV";
get_roman(["4","4","6"],?CLASSIC) -> "CDXLVI";
get_roman(["4","4","7"],?CLASSIC) -> "CDXLVII";
get_roman(["4","4","8"],?CLASSIC) -> "CDXLVIII";
get_roman(["4","4","9"],?CLASSIC) -> "CDXLIX";

get_roman(["4","4","5"],_) -> "CDVL";
get_roman(["4","4","6"],_) -> "CDVLI";
get_roman(["4","4","7"],_) -> "CDVLII";
get_roman(["4","4","8"],_) -> "CDVLIII";
%% special for 449
get_roman(["4","4","9"],?CONCISE1) -> "CDVLIV";
get_roman(["4","4","9"],_)         -> "CDIL";
%% now the 450's, 460's, 470's and 480's
get_roman(["4","5",First],?CLASSIC) -> "CDL"   ++get_roman([First],?CLASSIC);
get_roman(["4","6",First],?CLASSIC) -> "CDLX"  ++get_roman([First],?CLASSIC);
get_roman(["4","7",First],?CLASSIC) -> "CDLXX" ++get_roman([First],?CLASSIC);
get_roman(["4","8",First],?CLASSIC) -> "CDLXXX" ++get_roman([First],?CLASSIC);

get_roman(["4","5",First],_) -> "LD"    ++get_roman([First],?CLASSIC);
get_roman(["4","6",First],_) -> "LDX"   ++get_roman([First],?CLASSIC);
get_roman(["4","7",First],_) -> "LDXX"  ++get_roman([First],?CLASSIC);
get_roman(["4","8",First],_) -> "LDXXX" ++get_roman([First],?CLASSIC);
%% now the 490'syes
%% Classic and Concise2 are both straightforward
get_roman(["4","9",First],?CLASSIC)  -> "CDXC" ++get_roman([First],?CLASSIC);
get_roman(["4","9",First],?CONCISE2) -> "XD" ++get_roman([First],?CONCISE2);
% Now do 495, 496, 497, 498 and 499 for Concise1, Concise2 and Simple
get_roman(["4","9","5"],?CONCISE1) -> "LDVL";
get_roman(["4","9","6"],?CONCISE1) -> "LDVLI";
get_roman(["4","9","7"],?CONCISE1) -> "LDVLII";
get_roman(["4","9","8"],?CONCISE1) -> "LDVLIII";
get_roman(["4","9","9"],?CONCISE1) -> "LDVLIV";

get_roman(["4","9","5"],?CONCISE3) -> "VD";
get_roman(["4","9","6"],?CONCISE3) -> "VDI";
get_roman(["4","9","7"],?CONCISE3) -> "VDII";
get_roman(["4","9","8"],?CONCISE3) -> "VDIII";
get_roman(["4","9","9"],?CONCISE3) -> "VDIV";

get_roman(["4","9","5"],?SIMPLIFIED) -> "VD";
get_roman(["4","9","6"],?SIMPLIFIED) -> "VDI";
get_roman(["4","9","7"],?SIMPLIFIED) -> "VDII";
get_roman(["4","9","8"],?SIMPLIFIED) -> "VDIII";
get_roman(["4","9","9"],?SIMPLIFIED) -> "ID";
%% Now do 490, 491, 492, 493 and 494 for all the types
get_roman(["4","9",First],?CLASSIC)  -> "CDXC" ++get_roman([First],?CLASSIC);
get_roman(["4","9",First],?CONCISE1) -> "LDXL" ++get_roman([First],?CONCISE1);
get_roman(["4","9",First],Type)      -> "XD" ++get_roman([First],Type);

%% Now deal with 945 to 999 first
%% First 945 to 949
%% First classic
get_roman(["9","4","5"],?CLASSIC) -> "CMXLV";
get_roman(["9","4","6"],?CLASSIC) -> "CMXLVI";
get_roman(["9","4","7"],?CLASSIC) -> "CMXLVII";
get_roman(["9","4","8"],?CLASSIC) -> "CMXLVIII";
get_roman(["9","4","9"],?CLASSIC) -> "CMXLIX";

get_roman(["9","4","5"],_) -> "CMVL";
get_roman(["9","4","6"],_) -> "CMVLI";
get_roman(["9","4","7"],_) -> "CMVLII";
get_roman(["9","4","8"],_) -> "CMVLIII";
%% special for 949
get_roman(["9","4","9"],?CONCISE1) -> "CMVLIV";
get_roman(["9","4","9"],_)         -> "CMIL";
%% now the 950's, 960's, 970's and 980's
get_roman(["9","5",First],?CLASSIC) -> "CML"   ++get_roman([First],?CLASSIC);
get_roman(["9","6",First],?CLASSIC) -> "CMLX"  ++get_roman([First],?CLASSIC);
get_roman(["9","7",First],?CLASSIC) -> "CMLXX" ++get_roman([First],?CLASSIC);
get_roman(["9","8",First],?CLASSIC) -> "CMLXXX" ++get_roman([First],?CLASSIC);

get_roman(["9","5",First],_) -> "LM"    ++get_roman([First],?CLASSIC);
get_roman(["9","6",First],_) -> "LMX"   ++get_roman([First],?CLASSIC);
get_roman(["9","7",First],_) -> "LMXX"  ++get_roman([First],?CLASSIC);
get_roman(["9","8",First],_) -> "LMXXX" ++get_roman([First],?CLASSIC);
%% now the 990's
%% Classic and Concise2 are both straightforward
get_roman(["9","9",First],?CLASSIC)  -> "CMXC" ++get_roman([First],?CLASSIC);
get_roman(["9","9",First],?CONCISE2) -> "XM"   ++get_roman([First],?CONCISE2);
% Now do 995, 996, 997, 998 and 999 for Concise1, Concise2 and Simple
get_roman(["9","9","5"],?CONCISE1) -> "LMVL";
get_roman(["9","9","6"],?CONCISE1) -> "LMVLI";
get_roman(["9","9","7"],?CONCISE1) -> "LMVLII";
get_roman(["9","9","8"],?CONCISE1) -> "LMVLIII";
get_roman(["9","9","9"],?CONCISE1) -> "LMVLIV";

get_roman(["9","9","5"],_) -> "VM";
get_roman(["9","9","6"],_) -> "VMI";
get_roman(["9","9","7"],_) -> "VMII";
get_roman(["9","9","8"],_) -> "VMIII";
%% special for 999
get_roman(["9","9","9"],?CONCISE3) -> "VMIV";
get_roman(["9","9","9"],_)         -> "IM";
%% Now do 990, 991, 992, 993 and 994 for all the types
get_roman(["9","9",First],?CLASSIC)  -> "CMXC" ++get_roman([First],?CLASSIC);
get_roman(["9","9",First],?CONCISE1) -> "LMXL" ++get_roman([First],?CONCISE1);
get_roman(["9","9",First],Type)      -> "XM"   ++get_roman([First],Type);

%% Now do all the other 3 digit numbers
get_roman([Third,Second,First],Type) -> get_roman3([Third])
					    ++get_roman([Second,First|[]],Type);
get_roman([Fourth|Rest],Type)        -> get_roman4([Fourth])++get_roman(Rest,Type).

get_roman2(["0"]) -> "";
get_roman2(["1"]) -> "X";
get_roman2(["2"]) -> "XX";
get_roman2(["3"]) -> "XXX";
get_roman2(["4"]) -> "XL";
get_roman2(["5"]) -> "L";
get_roman2(["6"]) -> "LX";
get_roman2(["7"]) -> "LXX";
get_roman2(["8"]) -> "LXXX";
get_roman2(["9"]) -> "XC".

get_roman3(["0"]) -> "";
get_roman3(["1"]) -> "C";
get_roman3(["2"]) -> "CC";
get_roman3(["3"]) -> "CCC";
get_roman3(["4"]) -> "CD";
get_roman3(["5"]) -> "D";
get_roman3(["6"]) -> "DC";
get_roman3(["7"]) -> "DCC";
get_roman3(["8"]) -> "DCC";
get_roman3(["9"]) -> "CM".

get_roman4(["1"]) -> "M";
get_roman4(["2"]) -> "MM";
get_roman4(["3"]) -> "MMM".
    
make_list(List) when is_list(List) -> 
    make_list(List,[]).

make_list([],Acc) -> lists:reverse(Acc);
make_list([48|T],Acc) -> make_list(T,["0"|Acc]);
make_list([49|T],Acc) -> make_list(T,["1"|Acc]);
make_list([50|T],Acc) -> make_list(T,["2"|Acc]);
make_list([51|T],Acc) -> make_list(T,["3"|Acc]);
make_list([52|T],Acc) -> make_list(T,["4"|Acc]);
make_list([53|T],Acc) -> make_list(T,["5"|Acc]);
make_list([54|T],Acc) -> make_list(T,["6"|Acc]);
make_list([55|T],Acc) -> make_list(T,["7"|Acc]);
make_list([56|T],Acc) -> make_list(T,["8"|Acc]);
make_list([57|T],Acc) -> make_list(T,["9"|Acc]).

sln([Cost,SalvageVal,NoPeriods]) -> (Cost-SalvageVal)/NoPeriods.

syd([Cost,SalvageVal,Life,Period]) -> (Cost-SalvageVal)*(Life-Period+1)*2/(Life*(Life+1)).

text([Value,Format]) -> 
    {erlang,{_Type,Output}}=format:get_src(Format),
    {ok,{_Colour,Text2}}=format:run_format(Value,Output),
    Text2.

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit.hrl").

datedif_test_() ->
    [
     %% Years
%%     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                       #datetime{date = {1987, 8, 17}},
%%                       "Y") == 1),
%%     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                       #datetime{date = {1987, 8, 16}},
%%                       "Y") == 0),
%%     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                       #datetime{date = {1987, 8, 18}},
%%                       "Y") == 1),

    ].
