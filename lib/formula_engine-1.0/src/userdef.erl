-module(userdef).

%% exports for the AXA demo
-export([get_address/2,
	 get_sex/1,
	 split/2]).

-export([make_url/2,
	 get_list/1,
	 get_path/0,
	 get_page_url/0,
	 get_last_segment/1,
	 add_days/2]).

%% AXA demo functions
%%match(Page,Cond,Return) ->
%%    io:format("in userdef:match Page is ~p Cond is ~p Return is ~p~n",
%%	      [Page,Cond,Return]),
    


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
