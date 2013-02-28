%%% @copyright (C) 2010, Hypernumbers Ltd.
%%% Note: The admin grouped is treated specially, and is expected to be there.
%%% it must always be created, and never deleted.

-module(hn_groups).

-export([
         make_admin/2,
         get_groups/1,
         get_all_groups/1,
         create_group/2,
         delete_group/2,
         add_user/3,
         rem_user/3,
         set_users/3,
         get_a_users_groups/2,
         any_admin/1,
         is_member/3,
         dump_script/1,
         load_script/2
        ]).

% debugging
-export([
         list_users_DEBUG/1
        ]).

-include("spriki.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

% a fn to create a new site admin
-spec make_admin(string(), string()) -> ok | {error, list()}.
make_admin(Site, Email) when is_list(Site) andalso is_list(Email) ->
    case hn_util:valid_email(Email) of
        false -> {error, "invalid email"};
        true  -> {ok, _, User} = passport:get_or_create_user(Email),
                 passport:validate_uid(User),
                 add_user(Site, "admin", User)
    end.

-spec get_groups(string()) -> [string()].
get_groups(Site) -> new_db_api:get_groups(Site).

-spec get_all_groups(string()) -> [string()].
get_all_groups(Site) -> new_db_api:get_all_groups(Site).

-spec is_member(auth_srv:uid(), string(), [string()])
               -> boolean().
is_member(Uid, Site, Groups) -> new_db_api:is_memberD(Site, Uid, Groups).

% don't let anyone create an administrator group
% we map the WordPress administrator group to our admin group so
% letting this happen would be a bad idea
% also do this in hn_wordpress
-spec create_group(string(), string()) -> ok.
create_group(Site, "administrator") -> create_group(Site, "admin");
create_group(Site, GroupN)          -> new_db_api:create_groupD(Site, GroupN).

-spec delete_group(string(), string()) -> ok.
delete_group(Site, GroupN) -> new_db_api:delete_groupD(Site, GroupN).

-spec add_user(string(), string(), auth_srv:uid()) -> ok | no_group.
add_user(Site, GroupN, Uid) -> new_db_api:add_userD(Site, Uid, GroupN).

-spec rem_user(string(), string(), auth_srv:uid()) -> ok | no_group.
rem_user(Site, GroupN, Uid) -> new_db_api:rem_userD(Site, Uid, GroupN).

-spec set_users(string(), string(), [auth_srv:uid()]) -> ok | no_group.
set_users(Site, GroupN, Users) -> new_db_api:set_usersD(Site, Users, GroupN).

-spec get_a_users_groups(string(), auth_srv:uid()) -> list().
get_a_users_groups(Site, UID) -> new_db_api:get_a_users_groups(Site, UID).

-spec any_admin(string()) -> no_admin | string().
any_admin(Site) -> new_db_api:any_adminD(Site).

-spec list_users_DEBUG(string()) -> string().
list_users_DEBUG(Site) ->
    Tbl = new_db_wu:trans(Site, group),
    Terms = mnesia:activity(transaction, fun mnesia:foldl/3,
                            [fun list_u2/2, [], Tbl]),
    Terms.

list_u2(#group{members = M}, Acc) ->
    NewAcc = [passport:uid_to_email(X) || X <- gb_sets:to_list(M)],
    {_, NewAcc2} = lists:unzip(NewAcc),
    lists:merge(NewAcc2, Acc).

-spec dump_script(string()) -> string().
dump_script(Site) ->
    Tbl = new_db_wu:trans(Site, group),
    Terms = mnesia:activity(transaction, fun mnesia:foldl/3,
                            [fun dump_term/2, [], Tbl]),
    make_script_terms(Terms, []).

dump_term(#group{name=N, members=M}, Acc) ->
    Users = [{add_user, [{uid, U}, {group, N}]}
             || U <- gb_sets:to_list(M)],
    [{create_group, [{name, N}]} | Users ++ Acc].

make_script_terms([], Acc) ->
    FirstLine = io_lib:format("~s~n",["%%-*-erlang-*-"]),
    lists:flatten([FirstLine | lists:reverse(Acc)]);
make_script_terms([H | T], Acc) ->
    NewAcc = lists:flatten(io_lib:format("~p.~n", [H])),
    make_script_terms(T, [NewAcc | Acc]).

-spec load_script(string(), [term()]) -> ok.
load_script(Site, Terms) ->
    [ok = exec_script_term(T, Site) || T <- Terms],
    ok.

-define(lget(Key, List), (element(2, lists:keyfind(Key, 1, List)))).
exec_script_term({create_group, T}, Site) ->
    create_group(Site, ?lget(name, T));
exec_script_term({add_user, T}, Site) ->
    UID = case lists:keyfind(uid, 1, T) of
              false -> Email = ?lget(email, T),
                       Pwd = ?lget(password, T),
                       {ok, Status, UID2} = passport:get_or_create_user(Email),
                       case Status of
                           new      -> passport:validate_uid(UID2),
                                       passport:set_password(UID2, Pwd);
                           existing -> ok
                       end,
                       UID2;
              _     -> ?lget(uid, T)
          end,
    add_user(Site, ?lget(group, T), UID);
exec_script_term({set_users, T}, Site) ->
    UIDS = case lists:keyfind(uids, 1, T) of
               false -> Names = ?lget(emails, T),
                        L = [passport:email_to_uid(X) || X <- Names],
                        [X || {ok, X} <- L];
               _     -> ?lget(uids, T)
           end,
    set_users(Site, ?lget(group, T), UIDS).
