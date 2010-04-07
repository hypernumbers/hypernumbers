%%% @copyright (C) 2010, Hypernumbers Ltd.

-module(hn_groups).

-export([create_group/2, delete_group/2,
         add_user/3, rem_user/3, set_users/3,
         is_member/3,
         load_script/2]).

-include("spriki.hrl").
-include("auth.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-spec is_member(uid(), string(), [string()]) -> boolean().
is_member(Uid, Site, Groups) ->
    Tbl = hn_db_wu:trans(Site, group),
    is_member1(Groups, Tbl, Uid).
is_member1([], _Tbl, _Uid) -> false;
is_member1([GroupN|Rest], Tbl, Uid) ->
    case mnesia:dirty_read(Tbl, GroupN) of
        [G] -> 
            case gb_sets:is_member(Uid, G#group.members) of
                true  -> true; 
                false -> is_member1(Rest, Tbl, Uid)
            end;
        _ ->
            is_member1(Rest, Tbl, Uid)
    end.
        
-spec create_group(string(), string()) -> ok. 
create_group(Site, GroupN) ->
    Tbl = hn_db_wu:trans(Site, group),
    F = fun() ->
                case mnesia:read(Tbl, GroupN, read) of
                    [] ->
                        Group = #group{name = GroupN},
                        mnesia:write(Tbl, Group, write);
                    _ ->
                        ok
                end
        end,
    mnesia:activity(transaction, F).

-spec delete_group(string(), string()) -> ok. 
delete_group(Site, GroupN) ->
    Tbl = hn_db_wu:trans(Site, group),
    mnesia:activity(transaction, fun mnesia:delete/2, [Tbl, GroupN]).

-spec add_user(string(), string(), uid()) -> ok | no_group. 
add_user(Site, GroupN, Uid) ->
    Tbl = hn_db_wu:trans(Site, group),
    F = fun() ->
                case mnesia:read(Tbl, GroupN, write) of
                    [G] ->
                        Members = gb_sets:add(Uid, G#group.members),
                        G2 = G#group{members=Members},
                        mnesia:write(Tbl, G2, write);
                    _ ->
                        no_group
                end
        end,
    mnesia:activity(transaction, F).

-spec rem_user(string(), string(), uid()) -> ok | no_group. 
rem_user(Site, GroupN, Uid) ->
    Tbl = hn_db_wu:trans(Site, group),
    F = fun() ->
                case mnesia:read(Tbl, GroupN, write) of
                    [G] ->
                        Members = gb_sets:delete_any(Uid, G#group.members),
                        G2 = G#group{members=Members},
                        mnesia:write(Tbl, G2, write);
                    _ ->
                        no_group
                end
        end,
    mnesia:activity(transaction, F).

-spec set_users(string(), string(), [uid()]) -> ok | no_group.
set_users(Site, GroupN, Users) ->
    Tbl = hn_db_wu:trans(Site, group),
    F = fun() ->
                case mnesia:read(Tbl, GroupN, write) of
                    [G] ->
                        Members = gb_sets:from_list(Users),
                        G2 = G#group{members=Members},
                        mnesia:write(Tbl, G2, write);
                    _ ->
                        no_group
                end
        end,
    mnesia:activity(transaction, F).
    
load_script(Site, Terms) ->
    [ok = exec_script_term(T, Site) || T <- Terms],
    ok.

exec_script_term({create_group, GroupN}, Site) ->
    create_group(Site, GroupN);
exec_script_term({add_user, GroupN, Uid}, Site) ->
    add_user(Site, GroupN, Uid);
exec_script_term({set_users, GroupN, Users}, Site) ->
    set_users(Site, GroupN, Users).
