%%%-----------------------------------------------------------------------------
%%% File        : spriki.erl
%%% Author      : Dale Harvey <dale@arandomurl.com>
%%% Description : This handles user permissions on page / range and
%%%               cell read and write requests, as well as user admin
%%%               functions such as custom interfaces
%%%
%%% Created     : 11 Sep, 2006 by Dale Harvey
%%%-----------------------------------------------------------------------------
-module(users).

-export([create/2,delete/1,login/2,exists/1,gen_authtoken/2]).

-include("yaws_api.hrl").
-include("spriki.hrl").

create(Name,Pass) ->
    create_user_exec(#hn_user{name = Name, password = p(Pass)}).

delete(Name) ->
    mnesia:transaction((fun() ->
        mnesia:delete_object(#hn_user{name=Name, _='_'}) 
    end)).

exists(Name) ->
	
	List = mnesia:transaction((fun() ->
		mnesia:match_object(hn_user, #hn_user{name=Name, _='_'}, read)
	end)),
	
    case List of
        {atomic,[]} -> false;
        _  -> true
    end.

login(Name,Pass) ->
	List = mnesia:transaction((fun() ->
		mnesia:match_object(hn_user, #hn_user{name = Name,
			password=p(Pass), _='_'}, read)
	end)),
	
    case List of
    {atomic,[]}     -> {error,invalid_user};
    {atomic,[User]} -> {ok,User}
    end.

gen_authtoken(User,IP) ->

    Token  = hn_util:random_string(8),
    Salted = salt_token(Token,IP),
    
    {atomic,ok} = mnesia:transaction(fun() -> 
        mnesia:write(User#hn_user{authtoken = Token}) 
    end),
        
    Salted.
    
salt_token(Token,{IP1,IP2,_IP3,_IP4}) -> 
    I = fun(X) -> integer_to_list(X) end,    
    crypto:md5(lists:flatten([Token,I(IP1),I(IP2),?SALT])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Private Functions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_user_exec(Record) ->
    Fun = fun() -> mnesia:write(hn_user,Record,write) end,
    case mnesia:transaction(Fun) of
    {aborted, Reason} -> {error, Reason};
    {atomic, ok}      -> ok
    end.
    
p(Pass) ->
    binary_to_list(crypto:md5(Pass)).
    
