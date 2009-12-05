%%% @private
%%% @copyright Hypernumbers Ltd
-module(hn_users).

-export([
         create/3,
         create/4,
         delete/2,
         add_groups/3,
         remove_groups/3,
         login/4,
         exists/2,
         gen_authtoken/2,
         get_access_level/2,
         verify_token/2,
         update/4,
         get/2,
         read/2,
         name/1,
         groups/1
        ]).

-export([
         delete_all_users_DEBUG/1,
         prettyprint_DEBUG/1
        ]).

-include("hypernumbers.hrl").
-include("spriki.hrl").

-define(COOKIE, "somerandomcookie").

prettyprint_DEBUG(Site) ->
    
    Match = #hn_user{_ = '_'},
    Fun = fun() ->
                  mnesia:match_object(hn_db_wu:trans(Site, hn_user),
                                      Match, read)
          end,
    {atomic, Users} = mnesia:transaction(Fun),
    "<html><head></head><body>" ++
        pp(Users, []) ++
        "</body></html>".

pp([], Acc)      -> lists:flatten(lists:reverse(Acc));
pp([H | T], Acc) -> NewAcc = "<b>User</b>:&nbsp&nbsp&nbsp&nbsp&nbsp" ++
                        io_lib:fwrite("~p", [H#hn_user.name]) ++
                        "<br /><b>Groups</b>: " ++
                        io_lib:fwrite("~p", [H#hn_user.groups]) ++
                        "<br /><br />",
                    pp(T, [NewAcc | Acc]).

delete_all_users_DEBUG(Site) ->
    mnesia:clear_table(hn_db_wu:trans(Site, hn_user)).

create(Site, Name, Pass) ->
    create_user_exec(Site, #hn_user{name = Name, password = p(Pass)}).

create(Site, Name, Groups, Pass) ->
    create_user_exec(Site, #hn_user{name = Name, password = p(Pass), groups = Groups}).

add_gr(Site, Name, Groups) ->
    {ok, #hn_user{groups = G} = User} = read(Site, Name),
    NG = hslists:dedup([Groups, G]),
    NU = User#hn_user{groups = NG},
    mnesia:write(hn_db_wu:trans(Site, hn_user), NU, write).

add_groups(Site, Name, Groups) ->
    mnesia:activity(transaction, fun add_gr/3, [Site, Name, Groups]).

remove_gr(Site, Name, Groups) ->
    {ok, #hn_user{groups = G} = User} = read(Site, Name),
    NG = hslists:dedup([lists:subtract(G, Groups)]),
    NU = User#hn_user{groups = NG},
    mnesia:write(hn_db_wu:trans(Site, hn_user), NU, write).    

remove_groups(Site, Name, Groups) ->
    mnesia:activity(transaction, fun remove_gr/3, [Site, Name, Groups]).

delete_tr(Site, Name) ->
    {ok, User} = read(Site, Name),
    mnesia:delete_object(hn_db_wu:trans(Site, hn_user), User, write).

delete(Site, Name) ->
    mnesia:activity(transaction, fun delete_tr/2, [Site, Name]).

name(anonymous) -> "anonymous";
name(User)      -> User#hn_user.name.

groups(anonymous) -> [];
groups(User)      -> User#hn_user.groups.

exists(Site, Name) ->
	
    F = fun() ->
                User = #hn_user{name=Name, _='_'},
                mnesia:match_object(hn_db_wu:trans(Site, hn_user), User, read)
        end,
	
    case mnesia:transaction(F) of
        {atomic,[]} -> false;
        _           -> true
    end.

read(Site, Name) ->
    mnesia:activity(transaction, fun read_tr/2, [Site, Name]).

read_tr(Site, Name) ->
    Rec = #hn_user{name=Name, _='_'},
    case mnesia:match_object(hn_db_wu:trans(Site, hn_user), Rec, read) of
        []     -> {error, no_user};
        [User] -> {ok, User}
    end.

get(User, Key) ->
    case dict:is_key(Key, User#hn_user.data) of
        true  -> {ok, dict:fetch(Key, User#hn_user.data)};
        false -> undefined
    end.

update_tr(Site, User, Key, Val) ->
    NUser = User#hn_user{data=dict:store(Key, Val, User#hn_user.data)},
    mnesia:write(hn_db_wu:trans(Site, hn_user), NUser, write).

update(Site, UserName, Key, Val) when is_list(UserName) ->
    {ok, User} = read(Site, UserName),
    update(Site, User, Key, Val);

update(Site, User, Key, Val) when is_record(User, hn_user) ->
    mnesia:activity(transaction, fun update_tr/4, [Site, User, Key, Val]).

login(Site, Name, Pass, Remember) ->

    User = #hn_user{name=Name, password=p(Pass), _='_'},
    F = fun() ->
                mnesia:match_object(hn_db_wu:trans(Site, hn_user), User, read)
        end,
 
    case mnesia:transaction(F) of
        {atomic, []}      -> {error,invalid_user};
        {atomic, [NUser]} -> Token = gen_authtoken(NUser, Remember),
                             {ok, Token}
    end.

verify_token(_Site, undefined) ->
    {error, no_token};
verify_token(Site, Token) ->
    [Expires, User, Hash] = string:tokens(Token, ":"),
    
    case {is_expired(Expires), gen_hash(User, Expires), Hash} of
        {true,_,_}  -> {error, invalid_token};
        {false,X,X} ->
            case read(Site, User) of
                {ok, Usr}        -> {ok, Usr};
                {error, no_user} -> {error, invalid_user}
            end;
        _Else ->  {error, invalid_token}
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

create_user_exec(Site, Rec) ->
    Fun = fun() ->
                  mnesia:write(hn_db_wu:trans(Site, hn_user), Rec, write) 
          end,
    case mnesia:transaction(Fun) of
        {aborted, Reason} -> {error, Reason};
        {atomic, ok}      ->

            %% Add permissions

            auth_srv:add_controls(Site, [{"dale", []}],
                                  ["u", "dale"], [read, write],
                                  [], ["_global/userhome"]),
            
            auth_srv:add_controls(Site, [{"dale", []}],
                                  ["u", "dale", "[**]"], [read, write],
                                  [], ["*"]),
            ok
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
get_access_level(Usr, #refX{site = Site, path = Path}) ->

    User = case Usr of
               anonymous -> anonymous;
               _Else     -> Usr#hn_user.name
           end,
    
    Default = case Path of ["u",User|_] -> write; _ -> no_access end,

    SiteRef = #refX{site=Site, path=[]},
    {ok, Groups}     = hn_db_api:read_inherited_value(SiteRef, "__groups", []),
    {ok, Perms}      = hn_db_api:read_inherited_list(SiteRef, "__permissions"),
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
get_perms(Name, Groups, [{user, logged_in, Access} | Rest], Acc) 
  when Name /= anonymous ->
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

