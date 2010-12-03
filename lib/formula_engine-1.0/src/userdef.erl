%%% @private
-module(userdef).

-include("spriki.hrl").
-include("typechecks.hrl").

-import(muin_collect, [col/2, col/3, col/4]).

%% exports for Project
-export([
         vlookuphn/3,
         is_unique/1
         ]).

%% exports for the AXA demo
-export([
         realtime/2,
         get_address/2,
         get_sex/1,
         split/2,
         sum3d/1,
         slow/1
        ]).

-export([make_url/2,
         get_list/1,
         get_path/0,
         get_page_url/0,
         get_last_segment/1,
         add_days/2,
         davie/2]).

-export([get_username/0]).

realtime(A, B) ->
    A + B.

is_unique(Args) ->
    col(Args,
        [eval_funs, {cast, str, num, ?ERRVAL_VAL}, fetch, flatten,
         {ignore, blank}, {ignore, array}, {ignore, str}, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun blah/1),
    "mh, hmm...".

blah(_) -> ok.

vlookuphn(Key, {range, Array}, ResultCol) ->
    {list, v_hn1(Key, Array, ResultCol, [])}.

v_hn1(_Key, [], _, Acc)                -> lists:reverse(Acc);
v_hn1(Key, [[Key | T] | T1], Pos, Acc) -> NewAcc = lists:nth(Pos - 1, T),
                                          v_hn1(Key, T1, Pos, [NewAcc | Acc]);
v_hn1(Key, [_H | T], Pos, Acc)         -> v_hn1(Key, T, Pos, Acc).
    

slow([_X]) -> wait(100000).


wait(0) -> 1;
wait(N) -> wait(N - 1).

davie(A, B) ->
     A* B +100.

get_username() -> Return=get(username),
                  case Return of
                      undefined -> "undefined";
                      anonymous -> "John Smith";
                      _         -> Return
                  end.

sum3d(X) when is_list(X) ->
    sum3d(X,[]).

sum3d([],Acc)    -> Acc;
sum3d([H|T],Acc) -> sum3d(T,H+Acc).

%% splits a postcode
split(X,1) when is_list(X) -> [H|_T]=string:tokens(X," "),
                              H;
split(X,2) when is_list(X) -> [_One,Two]=string:tokens(X," "),
                              Two.

%% Fake name/sex lookup
get_sex(blank)     -> "";
get_sex("Andrew")  -> "Male";
get_sex("Bob")     -> "Male";
get_sex("Charlie") -> "Male";
get_sex("Davie")   -> "Male";
get_sex("Alice")   -> "Female";
get_sex("Betty")   -> "Female";
get_sex("Cheri")   -> "Female";
get_sex("Dottie")  -> "Female";
get_sex(_Other)    -> "Unknown".

%% Fake postcode lookup
get_address(blank,_)            -> "";
get_address("AA1 1XX","address1") -> "Acacia Avenue";
get_address("AA1 2XX","address1") -> "Betty Bypass";
get_address("AA1 3XX","address1") -> "Consuela Court";
get_address("AA1 4XX","address1") -> "Daniel Drive";
get_address("AA1 5XX","address1") -> "Example End";
get_address(_Dummy,"address1")    -> "Yolanda Yett";
get_address("AA1 1XX","address2") -> "Suburbonia";
get_address("AA1 2XX","address2") -> "Suburbonia";
get_address("AA1 3XX","address2") -> "Suburbonia";
get_address("AA1 4XX","address2") -> "Suburbonia";
get_address("AA1 5XX","address2") -> "Suburbonia";
get_address(_Dummy,"address2")    -> "Arcadia";
get_address("AA1 1XX","town")     -> "Newton";
get_address("AA1 2XX","town")     -> "Newton";
get_address("AA1 3XX","town")     -> "Newton";
get_address("AA1 4XX","town")     -> "Newton";
get_address("AA1 5XX","town")     -> "Newton";
get_address(_Dummy,"town")        -> "Egoton";
get_address(Postcode,Type)        -> io:format("in userdef:get_address "++
                                               "Postcode is ~p Type is ~p~n",
                                               [Postcode,Type]),
                                     "fix up postcodes".

%% Other Demo Functions
add_days(blank,_Days) -> "";
add_days({datetime,Date,Time},Days) ->
    Datetime=calendar:datetime_to_gregorian_seconds({Date,Time}),
    NewDatetime=Datetime+Days*24*60*60,
    {{Y,M,D},{H,Min,S}}=calendar:gregorian_seconds_to_datetime(NewDatetime),
    integer_to_list(Y)++"/"++integer_to_list(M)++"/"++integer_to_list(D)++"-"++
        integer_to_list(H)++":"++integer_to_list(Min)++":"++integer_to_list(S).

get_last_segment(String) ->
    [H|_T]=lists:reverse(string:tokens(String,"/")),
    try list_to_integer(H)
    catch
        error:_ -> "";
          exit:_ -> ""
    end.

get_path()-> lists:flatten([string:join(get(path),"/"),"/"]).

get_page_url() -> lists:flatten(get(site)++"/"++
                                lists:flatten([string:join(get(path),"/"),"/"])).

make_url(_Base,blank) -> "";
make_url(Base,Incr) when is_list(Base),is_integer(Incr) ->
    Base++integer_to_list(Incr)++"/";
make_url(Base,Incr) when is_list(Base),is_list(Incr) ->
    Base++Incr.

get_list(A) -> io:format("in userdef:get_list A is ~p~n",[A]),
               get_list(A,[]).

get_list([],Acc)        -> lists:flatten(lists:reverse(Acc));
get_list([blank|T],Acc) -> get_list(T,Acc);
get_list([H|T],Acc)     -> get_list(T,[H|Acc]).

