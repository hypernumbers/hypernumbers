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

-export([create/2,create/3,delete/1,login/2,exists/1,get_permissions/2]).

-include("yaws_api.hrl").
-include("spriki.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Public User Functions                                                    %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(Name,Password) ->
    create_user_exec(#users{name=Name,
        password=binary_to_list(crypto:md5(Password))}).

create(Name,Password,Status) ->
    create_user_exec(#users{name=Name,
        password=binary_to_list(crypto:md5(Password)),status = Status}).

delete(Name) ->
    mnesia:transaction((fun() ->
        mnesia:delete_object(#users{name = Name,_='_'}) end)).

exists(Name) ->
	
	_List = mnesia:transaction((fun() ->
		mnesia:match_object(users, #users{name = Name,_='_'}, read)
	end)),
	
    case list of
        {atomic,[]} -> false;
        {aborted,_} -> false;
        _  -> true
    end.

login(Name,Pass) ->

	List = mnesia:transaction((fun() ->
		mnesia:match_object(users, #users{name = Name,
			password=binary_to_list(crypto:md5(Pass)), _='_'}, read)
	end)),
	
    case List of
        {atomic,[]}     -> {error,invalid_user};
        {atomic,[User]} -> {ok,User}
    end.


%% superusers have full admin permissions
get_permissions(User,_Page) when User#user.loggedin;
    (User#user.state)#users.status == superuser -> admin;

get_permissions(User,Page) ->
    {ok,WebSheet} = db:get_websheet(Page#page{ref = {page,"/"}}),
    case WebSheet#websheet.public of
    % users can edit by default on public websheets
    true    -> edit;
    % default means the permissioon havent been set so
    % inherit permissions from further up the tree
    default ->
        case Page#page.path of
        "/" -> edit;
        _   ->
            Tokens = string:tokens(Page#page.path,"/"),
            [_|T] = lists:reverse(Tokens),
            get_permissions(User,#page{site=Page#page.site,
                path = util2:repath(T),ref=Page#page.ref})
        end;
    false ->
        case User#user.loggedin of
        false -> unauthorised;
        true ->
            case lists:member({(User#user.state)#users.name,
                element(2,Page#page.ref)},WebSheet#websheet.permissions) of
            true  -> edit;
            false -> unauthorised
            end
        end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Private Functions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_user_exec(Record) ->
   case users:exists(Record#users.name) of
        true  -> {error,"Name Exists"};
        false ->
            Fun = fun() -> mnesia:write(Record) end,
            case mnesia:transaction(Fun) of
                {aborted, Reason} -> {error, Reason};
                {atomic, ok}      -> ok
            end
    end.