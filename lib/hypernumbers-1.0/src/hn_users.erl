%%% @private
%%% @copyright Hypernumbers Ltd
-module(hn_users).

-export([
         create/3, create/4, create/5, create_raw/5,
         delete/2,
         add_groups/3,
         remove_groups/3,
         login/4,
         dump_script/1,
         exists/2,
         gen_authtoken/2,
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

-define(USER_SALT, "somerandomcookie").

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
    create(Site, Name, [], Pass, dict:new()).
create(Site, Name, Groups, Pass) ->
    create(Site, Name, Groups, Pass, dict:new()).
create(Site, Name, Groups, Pass, Data) ->
    create_raw(Site, Name, Groups, p(Pass), Data).

create_raw(Site, Name, Groups, Pass, Data) ->
    Rec = #hn_user{name = Name, 
                   password = Pass,
                   groups = Groups,
                   data = Data},
    Fun = fun() ->
                  mnesia:write(hn_db_wu:trans(Site, hn_user), Rec, write) 
          end,
    case mnesia:transaction(Fun) of
        {aborted, Reason} -> {error, Reason};
        {atomic, ok}      -> ok = hn_setup:add_user(Site, Rec)
    end.

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

-spec dump_script(string()) -> iodata(). 
dump_script(Site) ->
    Fun = fun(#hn_user{name = N, password = P, groups = G, 
                       data = D, created = C}, Acc) ->
                  Rec = [{name, N}, {password, P}, {groups, G},
                         {data, D}, {created, C}],
                  Print = io_lib:format("~p.~n", [Rec]),
                  [Print | Acc]
          end,
    Tbl = hn_db_wu:trans(Site, hn_user),
    Listing = mnesia:activity(transaction, fun mnesia:foldl/3, [Fun, [], Tbl]),
    lists:flatten(Listing).

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
    [Expires, User, Hash] = string:tokens(mochiweb_util:unquote(Token), ":"),
    User2 = xmerl_ucs:to_utf8(unescapeUnicode(User)),
    case {is_expired(Expires), gen_hash(User2, Expires), Hash} of
        {true,_,_}  -> {error, invalid_token};
        {false,X,X} ->
            case read(Site, User2) of
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
    Tmp = ?FORMAT("~s~s~s",[Expires, Name, ?USER_SALT]),
    mochihex:to_hex(crypto:md5(Tmp)).

expires("true")  -> 
    itol(unix_timestamp() + 2678400);
expires("false") -> 
    "session".
    
gen_authtoken(#hn_user{name=Name}, Remember) ->
    Expires = expires(Remember),
    ?FORMAT("~s:~ts:~s",[Expires, Name, gen_hash(Name, Expires)]).

p(Pass) ->
    binary_to_list(crypto:md5(Pass)).

unix_timestamp() ->
    unix_timestamp(erlang:now()).
unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( 
      calendar:now_to_universal_time(Now)) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

unescapeUnicode(List) -> unesc_1(List, []).

unesc_1([], Acc) -> lists:reverse(Acc);
unesc_1([$%, $u, A, B, C, D | T], Acc) -> 
         {ok, [N], []} = io_lib:fread("~16u", [A, B, C, D]),
         unesc_1(T, [N | Acc]);
unesc_1([H | T], Acc) -> unesc_1(T, [H | Acc]).
