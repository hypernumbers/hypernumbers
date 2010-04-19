
%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, hypernumbers.com
%%% @doc This is the helper function for num_format.yrl.
%%%
%%% @end
%%% Created :  5 Jan 2009 by Gordon Guthrie
%%% @private
%%%-------------------------------------------------------------------
-module(num_format).

-include("ascii.hrl").

-export([make_src/1,
         get_general/0,
         get_markdown/0,
         get_plain/0,
         verify/1,
         esc/1,
         make_cond/1,
         strip/1         
        ]).


make_src(A) when is_list(A) -> make_src2(lists:flatten(A));
make_src(A)                 -> make_src2([A]).

% This is bad
-define(is_str,
        "(( is_list(X) andalso X=/=[] andalso (not(is_atom(hd(X)))) )
        orelse
        ( is_tuple(X) andalso element(1, X) == ustring))").

get_markdown() ->
    Src = "fun(X) -> "++
        "    if"++
        "      "++?is_str++"     -> {auto, markdown:conv_utf8(X)};"++
        "      not(is_number(X)) -> {auto, X};"++
        "      is_integer(X)     -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))};"++ 
        "      is_float(X)       -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))}"++
        "     end "++
        "end.", 
    {number,Src}.    

get_general() ->
    Src = "fun(X) -> "++
        "    if"++
        "      "++?is_str++"     -> {auto, markdown:conv_utf8(X)};"++
        "      not(is_number(X)) -> {auto, X};"++
        "      is_integer(X)     -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))};"++ 
        "      is_float(X)       -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))}"++
        "     end "++
        "end.", 
    {number,Src}.

get_plain() ->
    Src = "fun(X) -> "++
        "    if"++
        "      "++?is_str++"     -> {auto, X};"++
        "      not(is_number(X)) -> {auto, X};"++
        "      is_integer(X)     -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))};"++ 
        "      is_float(X)       -> {auto, lists:flatten(io_lib:format(\"~p\", [X]))}"++
        "     end "++
        "end.", 
    {number,Src}.


verify(A) when is_list(A) ->
    B=lists:flatten(A),
    case is_text(B) of
        true  -> {text,clean_up_text(B)};
        false -> case is_date(B) of
                     true  -> {date,resolve_date(B)};
                     false -> verify_num(B)
                 end
    end; % yes this is a clause end...
verify(A) -> verify([A]). %% If 'A' is not a list make it so

esc({_Type,A}) -> {char,A}.

make_cond({condition,String})->
    Str2=string:strip(String,left,$[),
    Str3=string:strip(Str2,right,$]),
    {condition,Str3}.

strip({string,A}) -> 
    A1=string:strip(A,both,$"),%" syntax highlighting fix
    {string,A1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify_num(A) when is_list(A) -> verify_num(A,[],0);
verify_num(A)                 -> verify_num([A],[],0).

%% There can be 0 or 1 decimal point - any more and it's not valid...
verify_num([],Acc,0)    -> {number,to_num(lists:reverse(Acc))};
verify_num([],Acc,1)    -> {number,to_num(lists:reverse(Acc))};
verify_num([],_Acc,_N)  -> exit(invalid_format);
verify_num([H|T],Acc,N) -> verify_num(T,[H|Acc],N+get_decimals(H)).

%% resolve_date has to fix up the problem that 'm', 'M', 'mm' and 'MM'
%% are all valid formats for both months and minutes
%% resolve_date just interprets them contextually
%% 'm' next to 'y' or 'd' is month and 'm' next to 'h' or 's' is minute
resolve_date(DateFormat) ->
%% First up strip out all guff from the format to get the underlying
%% date format
    StrippedFormat=strip_date_format(DateFormat),
%% Now examine each 'min_mon' format item in the context of what
%% comes before and after it and decide if it is a 'min' or a 'mon'
    CorrectedFormat=corr(StrippedFormat),
%% Now we have to match up the corrected and stripped format and the
%% original unstripped format and apply the changes to the unstripped
%% format and return that
    FixedUpFormat=fix_up(DateFormat,CorrectedFormat),
    FixedUpFormat.

fix_up(DateFormat,CorrectedFormat) -> fix_up(DateFormat,CorrectedFormat,[]).

fix_up([],[],Acc)                         -> lists:reverse(Acc);
fix_up([{mon_min,X}|T1],[{mon,X}|T2],Acc) -> fix_up(T1,T2,[{mon,X}|Acc]);
fix_up([{mon_min,X}|T1],[{min,X}|T2],Acc) -> fix_up(T1,T2,[{min,X}|Acc]);
fix_up([X|T1],[X|T2],Acc)                 -> fix_up(T1,T2,[X|Acc]);
fix_up([X|T1],L2,Acc)                     -> fix_up(T1,L2,[X|Acc]).

corr(StrippedDateFormat) -> corr(StrippedDateFormat,[]).

corr([],Acc)                              -> lists:reverse(Acc);
corr([{year,X},{mon_min,Y},{day,Z}|T],Acc)-> corr(T,[{day,Z},{mon,Y},{year,X}|Acc]);
corr([{day,X},{mon_min,Y},{year,Z}|T],Acc)-> corr(T,[{year,Z},{mon,Y},{day,X}|Acc]);
corr([{hour,X},{mon_min,Y},{sec,Z}|T],Acc)-> corr(T,[{sec,Z},{min,Y},{hour,X}|Acc]);
corr([{sec,X},{mon_min,Y},{hour,Z}|T],Acc)-> corr(T,[{hour,Z},{min,Y},{sec,X}|Acc]);
corr([{year,X},{mon_min,Y}|T],Acc)        -> corr(T,[{mon,Y},{year,X}|Acc]);
corr([{mon_min,Y},{day,Z}|T],Acc)         -> corr(T,[{day,Z},{mon,Y}|Acc]);
corr([{hour,X},{mon_min,Y}|T],Acc)        -> corr(T,[{min,Y},{hour,X}|Acc]);
corr([{mon_min,Y},{sec,Z}|T],Acc)         -> corr(T,[{sec,Z},{min,Y}|Acc]);
corr([{mon_min,Y}|T],Acc)                 -> corr(T,[{mon,Y}|Acc]);
corr([X|T],Acc)                           -> corr(T,[X|Acc]).

strip_date_format(DateFormat) -> strip_date_format(DateFormat,[]).

strip_date_format([],Acc)              -> lists:reverse(Acc);
strip_date_format([{year,X}|T],Acc)    -> strip_date_format(T,[{year,X}|Acc]);
strip_date_format([{mon_min,X}|T],Acc) -> strip_date_format(T,[{mon_min,X}|Acc]);
strip_date_format([{mon,X}|T],Acc)     -> strip_date_format(T,[{mon,X}|Acc]);
strip_date_format([{day,X}|T],Acc)     -> strip_date_format(T,[{day,X}|Acc]);
strip_date_format([{hour,X}|T],Acc)    -> strip_date_format(T,[{hour,X}|Acc]);
strip_date_format([{sec,X}|T],Acc)     -> strip_date_format(T,[{sec,X}|Acc]);
strip_date_format([_Other|T],Acc)      -> strip_date_format(T,Acc). % Strip other!

%% clean_up_text just silently drops the following format commands from Excel
%% {char,"*"}
clean_up_text(List) -> clean_up_text(clear_underscores(List),[]).

clean_up_text([],Residuum)             -> lists:reverse(Residuum);
clean_up_text([{char,"*"}|T],Residuum) -> clean_up_text(T,Residuum);
clean_up_text([H|T],Residuum)          -> clean_up_text(T,[H|Residuum]).

to_num(List) -> NewList=clear_underscores(List),
                to_num(NewList,[]).

to_num([],Acc)                    -> lists:reverse(Acc);
to_num([{colour,Colour}|T],Acc)   -> to_num(T,[{colour,Colour}|Acc]);
to_num([{condition,Cond}|T],Acc)  -> to_num(T,[{condition,Cond}|Acc]);
to_num([{percent,Perc}|T],Acc)    -> to_num(T,[{percent,Perc}|Acc]);
to_num([{fraction,Frac}|T],Acc)   -> to_num(T,[{fraction,Frac}|Acc]);
to_num([{string,String}|T],Acc)   -> to_num(T,[{string,String}|Acc]);
to_num([{dollar,String}|T],Acc)   -> to_num(T,[{string,String}|Acc]);
to_num([{minus,String}|T],Acc)    -> to_num(T,[{string,String}|Acc]);
to_num([{plus,String}|T],Acc)     -> to_num(T,[{string,String}|Acc]);
to_num([{fwdslash,String}|T],Acc) -> to_num(T,[{string,String}|Acc]);
to_num([{open_bra,String}|T],Acc) -> to_num(T,[{string,String}|Acc]);
to_num([{close_ket,String}|T],Acc)-> to_num(T,[{string,String}|Acc]);
to_num([{colon,String}|T],Acc)    -> to_num(T,[{string,String}|Acc]);
to_num([{space,String}|T],Acc)    -> to_num(T,[{string,String}|Acc]);
to_num([{format,Format}|T],Acc)   -> to_num(T,[{format,Format}|Acc]);
to_num([{quote,"\""}|T],Acc)      -> to_num(T,[{string,"\""}|Acc]);
to_num([{char,"£"}|T],Acc)    -> to_num(T,[{string,"£"}|Acc]);
to_num([{char,"*"}|T],Acc)        -> to_num(T,Acc); % silently drop "*"'s
to_num([{char,"."}|T],Acc)        -> to_num(T,[{format,"."}|Acc]); % cnv to format!
to_num([{char,","}|T],Acc)        -> to_num(T,[{format,","}|Acc]); % cnv to format!
to_num([{char,Char}|T],Acc)       -> to_num(T,[{string,Char}|Acc]);
to_num(Other,_Acc)                -> io:format("in num_format.yrl:to_num Other is ~p~n",[Other]),
                                     exit(invalid_format).

%% the underscore character in Excel is used to make a character non printing
%% but not have it space filling - this means that columns line up in an 
%% appropriate fashion this is a mixed formula/GUI approach suitable for a 
%% file-based spreadsheet but not a web-based one
%% We just delete the underscore and just replace the character with a space
%% (This will work with monospaced characters in the browser)
clear_underscores(List) -> clear_underscores(List,[]).

clear_underscores([],Residuum) ->
	lists:reverse(Residuum);
clear_underscores([{underscore,_UndS},{format,F}|T],Acc) -> 
	clear_underscores(T,[{string," "},{format,F}|Acc]);
clear_underscores([{underscore,_UndS},_|T],Acc) -> 
	clear_underscores(T,[{string," "}|Acc]);
clear_underscores([H|T],Acc) -> 
	clear_underscores(T,[H|Acc]). 

is_text(A) when is_list(A) -> is_text2(A);
is_text(A)                 -> is_text2([A]).

is_text2([])          -> false;
is_text2([{at,_}|_T]) -> true;
is_text2([_H|T])      -> is_text2(T).

is_date(A) when is_list(A) -> is_date2(A);
is_date(A)                 -> is_date([A]).

is_date2([])               -> false;
is_date2([{year,_}|_T])    -> true;
is_date2([{mon_min,_}|_T]) -> true;
is_date2([{mon,_}|_T])     -> true;
is_date2([{min,_}|_T])     -> true;
is_date2([{day,_}|_T])     -> true;
is_date2([{hour,_}|_T])    -> true;
is_date2([{sec,_}|_T])     -> true;
is_date2([{ampm,_}|_T])    -> true;
is_date2([_H|T])           -> is_date2(T).

get_decimals({format,Format}) -> count_dec(Format);
get_decimals({char,"."})      -> 1;
get_decimals(_)               -> 0.

count_dec(Format) -> count_dec(Format,0).

count_dec([],N)                -> N;
count_dec([?ASC_FULLSTOP|T],N) -> count_dec(T,N+1);
count_dec([_H|T],N)            -> count_dec(T,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%% These functions all pertain to make_src                                   %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
make_src2(A) ->
    Type=get_type(A),
    case Type of
        number -> Clauses=organise_clauses(A),
                  {Type, gen_src_num(Clauses,[">0","<0","=0"])};
        date   -> {Type, gen_src_date(A)};
        text   -> {Type, "fun(X) -> {auto, X} end."}
    end.

get_type(A)                                           -> get_type(A,[]).
get_type([],Acc)                                      -> verify_type(lists:reverse(Acc));
get_type([{Type,_}|T],Acc)                            -> get_type(T,[[Type]|Acc]);
get_type([semicolon|T],Acc)                           -> get_type(T,Acc);
get_type([[{condition,_},{colour,_},{Type,_}]|T],Acc) -> get_type(T,[[Type]|Acc]);
get_type([[{condition,_},{Type,_}]|T],Acc)            -> get_type(T,[[Type]|Acc]);
get_type([[{colour,_},{Type,_}]|T],Acc)               -> get_type(T,[[Type]|Acc]).

verify_type([number]) -> number;
verify_type([date])   -> date;
verify_type([text])   -> text;
verify_type(A)        -> case lists:merge(A) of
                             [number] -> number;
                             [date]   -> date;
                             [text]   -> text;
                             _        -> verify_listX(remove_semicolons(A))
                         end.

remove_semicolons(List) -> remove_semicolons(List,[]).

remove_semicolons([],Acc)              -> lists:reverse(Acc);
remove_semicolons([[semicolon]|T],Acc) -> remove_semicolons(T,Acc);
remove_semicolons([H|T],Acc)           -> remove_semicolons(T,[H|Acc]).

verify_listX(A) ->
    % @TODO I think some of the clauses in verify_list are hooky!
	verify_list(A).

verify_list([[A]])                               -> A;
verify_list([[number],[text]])                   -> number; % Hooky!
verify_list([[number],[number]])                 -> number;
verify_list([[number],[number],[text]])          -> number; % Hooky!
verify_list([[number],[number],[number]])        -> number;
verify_list([[number],[number],[number],[text]]) -> number. % This one is OK

gen_src_date([Format]) ->
    "fun(X) ->"++
    "    IsDate = case X of"++
    "      {datetime, _, _} -> true;"++
    "      _                -> false"++
    "    end,"++
    "    if"++
    "       not(IsDate)  -> {auto, X};"++
    "       IsDate       -> {auto, format:format(X, "++
    make_format(Format)++")}"++
    "   end"++
    " end.".
                
gen_src_num(Clauses,Defaults) -> gen_src_num(Clauses,Defaults,[]).

%% generate the source from the list of clauses and the list of default clauses
%% if there aren't enough conditions repeat the first one
gen_src_num([],[],Acc)           -> gen_src_num2(lists:reverse(Acc));
%% this is nasty to fix the ghastly 'feature' of sign swapping for explicity
%% specified '<0' clauses - this one isn't and we won't do it!
gen_src_num([],["<0"|T2],Acc)    -> gen_src_num([],T2,[swap_cond("dont swap sign",
                                                         lists:last(Acc))|Acc]); 
gen_src_num([],[H2|T2],Acc)      -> gen_src_num([],T2,[swap_cond(H2,lists:last(Acc))|Acc]); 
gen_src_num([H1|T1],[],Acc)      -> gen_src_num(T1,[],[make_default(H1)|Acc]);
gen_src_num([H1|T1],[H2|T2],Acc) -> gen_src_num(T1,T2,[make_default(H1,H2)|Acc]).

gen_src_num2(Clauses)->
    {Cond1,Clause1}=make_clause(1,lists:nth(1,Clauses)),
    {Cond2,Clause2}=make_clause(2,lists:nth(2,Clauses)),
    {Cond3,Clause3}=make_clause(3,lists:nth(3,Clauses)),
    NewClauses=rearrange_clauses(Clause1,Clause2,Clause3),
    Clause4 = case length(Clauses) of
                  3 -> default_clause();
                  4 -> RawClause=lists:nth(4,Clauses),
                       {_Cond,MadeClause}=make_clause(4,RawClause), 
                       strip_condition(MadeClause)
              end,
    Src="fun(X) -> "++Cond1++Cond2++Cond3++
        "    if "++
        "      not(is_number(X)) -> "++ Clause4++";"++
        "      true ->"++
        "           if "++NewClauses++
        "           end"++
        "      end"++
        "   end.",
    lists:flatten(Src).

%% Rearrange clauses to ensure that the equality clause appears first
rearrange_clauses(Clause1,Clause2,Clause3)->
    Order={is_eq(Clause1),is_eq(Clause2),is_eq(Clause3)},
    case Order of
        {true,false,false}  -> Clause1++"; "++Clause2++"; "++Clause3++" ";
        {false,true,false}  -> Clause2++"; "++Clause1++"; "++Clause3++" ";
        {false,false,true}  -> Clause3++"; "++Clause1++"; "++Clause2++" ";
        {false,false,false} -> Clause1++"; "++Clause2++"; "++Clause3++" "
    end.

is_eq([$X,?ASC_SPACE,?ASC_EQ,?ASC_EQ|_T]) -> true;
is_eq(_Other)                             -> false.

strip_condition(Clause) -> 
	Loc=string:str(Clause,"-> "),
	Len=string:len(Clause),
    string:right(Clause,Len-(Loc+2)).% 2 for the additional chars

make_clause(_N,[{condition,Cond},{colour,Col}]) ->
    [{condition,Cond},{colour,Col}];
make_clause(N,[{condition,"<0"},{colour,Col},{Type,Format}]) ->
    make_clause2(N,[{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause(N,[{condition,"=<0"},{colour,Col},{Type,Format}]) ->
    make_clause2(N,[{condition,"=<0"},{colour,Col},{Type,Format}]);
%% this clause is a default clause and should *NOT* have the sign change of
%% a custom specified <0 clause
%% you want someone to blame try billgates@microsoft.com
make_clause(N,[{condition,"dont swap sign"},{colour,Col},{Type,Format}]) ->
    make_clause3(N,[{condition,"<0"},{colour,Col},{Type,Format}]);
make_clause(N,[{condition,Cond},{colour,Col},{Type,Format}]) ->
    make_clause3(N,[{condition,Cond},{colour,Col},{Type,Format}]).

%% if the conditional clause is less than (or in our case less than or equals) to zero
%% then the Excel format automatically changes the sign of the number to positive!
make_clause2(N,[{condition,Cond},{colour,Col},{Type,Format}]) ->
    Bits2=make_format({Type, Format}),
    Y = "Cond"++integer_to_list(N),
    Cond2=tart_up(Y,"X",Cond),
    Clause = Y++" -> {"++atom_to_list(Col)++",format:format(-X,"++Bits2++")}",
    {Cond2,Clause}.

%% this is the normal clause - but it also is picked up for automatically generated
%% <0 clauses
make_clause3(N,[{condition,Cond},{colour,Col},{Type,Format}]) ->
    Bits2=make_format({Type, Format}),
    Y = "Cond"++integer_to_list(N),
    Cond2=tart_up(Y,"X",Cond),
    Clause = Y++" -> {"++atom_to_list(Col)++",format:format(X,"++Bits2++")}",
    {Cond2,Clause}.

default_clause() -> "{auto, X}".

%% tart_up just makes the condition clauses well behaved
tart_up(Y, X, [?ASC_GT, ?ASC_EQ | Rest]) -> wrap(Y, X, [?ASC_GT, ?ASC_EQ], Rest);
tart_up(Y, X, [?ASC_EQ, ?ASC_LT | Rest]) -> wrap(Y, X, [?ASC_EQ, ?ASC_LT], Rest);
%% Now make it work for arse backwards GE and LT operators
%% (Polite note Excel formats expect '<=' not '=<' so give it 'em both ways...)
tart_up(Y, X, [?ASC_EQ, ?ASC_GT | Rest]) -> wrap(Y, X, [?ASC_GT, ?ASC_EQ], Rest);
tart_up(Y, X, [?ASC_LT, ?ASC_EQ | Rest]) -> wrap(Y, X, [?ASC_EQ, ?ASC_LT], Rest);
%% switch '=' to '=='
tart_up(Y, X, [?ASC_EQ | Rest])          -> wrap(Y, X, [?ASC_EQ, ?ASC_EQ], Rest);
tart_up(Y, X, [Op | Rest])               -> wrap(Y, X, [Op], Rest).

wrap(Y, X,Cond,Rest) -> Y++" = format:conditional("++X++",\""++
                            Cond++"\","++Rest++"), ".

swap_cond(Cond,[{condition,_OldCond}|Rest]) -> [{condition,Cond}|Rest].

make_default([{colour,Col}|Rest]) -> [{condition,text},{colour,Col}|Rest];
make_default(Plain)               -> [{condition,text},{colour,auto}|Plain].

make_default([{condition,Cond},{colour,Col}|R],_Def)->
    [{condition,Cond},{colour,Col}|R];
make_default([{colour,Col}|R],Def)                  ->
    [{condition,Def},{colour,Col}|R];
make_default([{condition,Cond}|R],_Def)             ->
    [{condition,Cond},{colour,auto}|R];
make_default([],Def)                                ->
    [{condition,Def},{colour,auto},{number,[{format,""}]}];
make_default(Plain,Def)                             ->
    [{condition,Def},{colour,auto}|Plain].

organise_clauses(A) ->
    Clauses=organise_clauses(A,[],[]),
    promote_subclauses(Clauses).

organise_clauses([],Acc1,Acc2)                ->
    [lists:reverse(Acc1)|Acc2]; % don't reverse the Acc!
organise_clauses([{semicolon,_}|T],Acc1,Acc2) ->
    organise_clauses(T,[],[lists:reverse(Acc1)|Acc2]);
organise_clauses([Other|T],Acc1,Acc2)         ->
    organise_clauses(T,[Other|Acc1],Acc2).

promote_subclauses(A) -> promote_subclauses(A,[]).

promote_subclauses([],Acc) -> Acc; % don't reverse the Acc!
%% swap the order of the colour and condition clauses!!
promote_subclauses([[{Type,[{colour,Col},{condition,Cond}|Rest]}]|T],Acc) ->
    promote_subclauses(T,[[{condition,Cond},{colour,Col}|[{Type,Rest}]]|Acc]);
promote_subclauses([[{Type,[{condition,Cond}|Rest]}]|T],Acc) ->
    promote_subclauses(T,[[{condition,Cond}|[{Type,Rest}]]|Acc]);
promote_subclauses([[{Type,[{colour,Col}|Rest]}]|T],Acc) ->
    promote_subclauses(T,[[{colour,Col}|[{Type,Rest}]]|Acc]);
promote_subclauses([H|T],Acc) ->
    promote_subclauses(T,[H|Acc]).

make_format({Type, Format}) ->
    FormatClause={Type,Format},
    Bits=io_lib:fwrite("~p",[FormatClause]),
    lists:flatten(Bits).
