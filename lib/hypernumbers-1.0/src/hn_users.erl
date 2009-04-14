%%% @private
%%% @copyright Hypernumbers Ltd
-module(hn_users).

-export([create/2,delete/1,login/3,exists/1,gen_authtoken/2,
        get_access_level/2, verify_token/1, update/3, get/2, read/1 ]).

-include("hypernumbers.hrl").
-include("yaws_api.hrl").
-include("spriki.hrl").

-define(COOKIE, "somerandomcookie").

create(Name,Pass) ->
    create_user_exec(#hn_user{name = Name, password = p(Pass)}).

delete(Name) ->
    F = fun() ->
                mnesia:delete_object(#hn_user{name=Name, _='_'}) 
        end,
    mnesia:transaction(F).


exists(Name) ->
	
    F = fun() ->
                User = #hn_user{name=Name, _='_'},
                mnesia:match_object(hn_user, User, read)
        end,
	
    case mnesia:transaction(F) of
        {atomic,[]} -> false;
        _           -> true
    end.

read(Name) ->
    mnesia:activity(transaction, fun read_tr/1, [Name]).

read_tr(Name) ->
    case mnesia:match_object(hn_user, #hn_user{name=Name, _='_'}, read) of
        []     -> {error, no_user};
        [User] -> {ok, User}
    end.

get(User, Key) ->
    case dict:is_key(Key, User#hn_user.data) of
        true  -> {ok, dict:fetch(Key, User#hn_user.data)};
        false -> undefined
    end.

update(Name, Key, Val) ->
	
    F = fun() ->
                Rec  = #hn_user{name=Name, _='_'},
                [User] = mnesia:match_object(hn_user, Rec, read),
                NUser = User#hn_user{data=dict:store(Key, Val, User#hn_user.data)},
                mnesia:write(NUser)
        end,
	
    {atomic, ok} = mnesia:transaction(F),
    ok.

login(Name, Pass, Remember) ->

    User = #hn_user{name=Name, password=p(Pass), _='_'},
    F = fun() -> mnesia:match_object(hn_user, User, read) end,
 
    case mnesia:transaction(F) of
        {atomic, []}      -> {error,invalid_user};
        {atomic, [NUser]} -> 
            Token = gen_authtoken(NUser, Remember),
            {ok, Token}
    end.

verify_token(undefined) ->
    {error, invalid_token};
verify_token(Token) ->
    [Expires, User, Hash] = string:tokens(Token, ":"),
    case {is_expired(Expires), gen_hash(User, Expires), Hash} of
        {true,_,_}  -> {error, invalid_token};
        {false,X,X} -> 
            case read(User) of
                {ok, Usr}        -> {ok, Usr};
                {error, no_user} -> {error, invalid_user}
            end;
        _Else       ->  {error, invalid_token}
    end.

is_expired("session") ->
    false;
is_expired(Time) ->
    ltoi(Time) < unix_timestamp().
    
itol(I) ->
    integer_to_list(I).
ltoi(I) ->
    list_to_integer(I).

gen_hash(Name, Expires) ->
    Tmp = ?FORMAT("~s~s~s",[Expires, Name, ?COOKIE]),
    hn_util:bin_to_hexstr(crypto:md5(Tmp)).

expires("true")  -> 
    itol(unix_timestamp() + 2678400);
expires("false") -> 
    "session".
    
gen_authtoken(#hn_user{name=Name}, Remember) ->
    Expires = expires(Remember),
    ?FORMAT("~s:~s:~s",[Expires, Name, gen_hash(Name, Expires)]).

create_user_exec(Record) ->
    Fun = fun() -> mnesia:write(hn_user,Record,write) end,
    case mnesia:transaction(Fun) of
        {aborted, Reason} -> {error, Reason};
        {atomic, ok}      -> ok
    end.

p(Pass) ->
    binary_to_list(crypto:md5(Pass)).

unix_timestamp() ->
    unix_timestamp(erlang:now()).
unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( 
      calendar:now_to_universal_time(Now)) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).


%User   = {user,"username"},
%Group  = {group,"groupname", [Users]}
%Groups = [Groups]

%% Given a user find the most elevated permission
%% granted to that user
get_access_level(User,Ref=#refX{site=Site, path=Path}) ->

    Default = case Path of [User|_] -> write; _ -> no_access end,

    SiteRef = #refX{site=Site, path=[]},
    {ok, Groups} = hn_db_api:read_inherited_value(SiteRef, "__groups", []),
    {ok, Perms}  = hn_db_api:read_inherited_list(Ref, "__permissions"),
    {ok, UserGroups} = get_usergroups(User, Groups, []),    
    {ok, Levels}     = get_perms(User, UserGroups, Perms, [Default]),

    F = fun(X,Y) -> pindex(X) > pindex(Y) end,
    [ H | _Rest] = lists:sort(F, Levels),
    {ok, H}.

%% Get the list of groups a user is in
get_usergroups(_User, [], Acc) ->
    {ok, Acc};
get_usergroups(User, [{Name, Members} | Rest], Acc) ->
    case lists:member(User, Members) of 
        true  -> get_usergroups(User, Rest, [Name | Acc]);
        false -> get_usergroups(User, Rest, Acc)
    end.

%% get all permissions relating to a user or a group the user is in
get_perms(_Name, _Groups, [], Acc) ->
    {ok, Acc};
get_perms(Name, Groups, [{user, Name, Access} | Rest], Acc) ->
    get_perms(Name, Groups, Rest, [Access | Acc]);
get_perms(Name, Groups, [{user, anonymous, Access} | Rest], Acc) ->
    get_perms(Name, Groups, Rest, [Access | Acc]);
get_perms(User, Groups, [{group, Name, Access} | Rest], Acc) ->
    case lists:member(Name, Groups) of 
        true  -> get_perms(User, Groups, Rest, [Access | Acc]);
        false -> get_perms(User, Groups, Rest, Acc)
    end;
get_perms(Name, Groups, [_H | Rest], Acc) ->
    get_perms(Name, Groups, Rest, Acc).

%% Rank permission atoms in order of precedence
pindex(no_access)           -> 0;
pindex({protected_read,_X}) -> 1;
pindex({protected_write,_X})-> 2;
pindex(read)                -> 3;
pindex(write)               -> 4;
pindex(admin)               -> 5.

