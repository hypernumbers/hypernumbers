%%% @private
%%% @copyright Hypernumbers Ltd
-module(hn_users).

-export([create/2,delete/1,login/3,exists/1,gen_authtoken/2,
        get_permissions/2, verify_token/1 ]).

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
    invalid;
verify_token(Token) ->
    [Expires, User, Hash] = string:tokens(Token, ":"),
    case {is_expired(Expires), gen_hash(User, Expires), Hash} of
        {true,_,_}  -> invalid;
        {false,X,X} -> {ok, User};
        _Else       -> invalid
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

%% Given a user find the most elevated permission
%% granted to that user
get_permissions(User,Ref=#ref{site=Site}) ->

    Groups = hn_db:get_item_val(#ref{site=Site,ref={page,"/"},
                                     name='__groups'}),

    {ok,Perms}   = hn_db:get_item_list(Ref#ref{name='__permissions'}),
    {ok,UGroups} = find_groups(Groups,User),
    {ok,Access}  = find_perms(User,UGroups,[no_access],Perms),
    %% Sort permissions in order of precedence then pick
    %% the highest
    Sort = fun(X,Y) -> perm_index(X) > perm_index(Y) end,
    [H|_Rest] = lists:sort(Sort,Access),
    {ok,H}.

%% Rank permission atoms in order of precedence
perm_index(no_access)           -> 0;
perm_index({protected_read,_X}) -> 1;
perm_index({protected_write,_X})-> 2;
perm_index(read)                -> 3;
perm_index(write)               -> 4;
perm_index(admin)               -> 5.

%% Find the permissions pertaining to a user
find_perms(_User,_Groups,Perms,[]) ->
    {ok,Perms};
find_perms(User,Groups,Perms,[{user,anonymous,Perm}|T]) ->
    find_perms(User,Groups,[Perm|Perms],T);
find_perms(User,Groups,Perms,[{user,User,Perm}|T]) ->
    find_perms(User,Groups,[Perm|Perms],T);
find_perms(User,Groups,Perms,[{group,Group,Perm}|T]) ->
    P = case lists:member(Group,Groups) of
            true ->
                [Perm|Perms];
            false ->
                Perms
        end,
    find_perms(User,Groups,P,T);
find_perms(User,Groups,Perms,[_H|T]) ->
    find_perms(User,Groups,Perms,T).

%% Create a list of groups that a user is in
%% expanded into nested groups
find_groups(Groups,User) ->
    find_groups(Groups,User,Groups,[]).

find_groups(_GList,_User,[],Groups) ->
    {ok,Groups};
find_groups(GList,User,[{Name,List}|T],Groups) ->
    NGroups = group_list(GList,User,[Name],List,Groups),
    find_groups(GList,User,T,NGroups).

group_list(_GList,_User,_Name,[],Acc) ->
    hslists:uniq(lists:flatten(Acc));
group_list(GList,User,Name,[{user,User}|T],Acc) ->
    group_list(GList,User,Name,T,[Name|Acc]);
group_list(GList,User,Name,[{group,Group}|_T],Acc) ->
    case lists:keysearch(Group,1,GList) of
        false ->
            Acc;
        {value,{Group,List}} ->
            group_list(GList,User,[Group|Name],List,Acc)
                %++ group_list(GList,User,Name,T,Acc)
    end;
group_list(GList,User,Name,[{user,_A}|T],Acc) ->
    group_list(GList,User,Name,T,Acc).
