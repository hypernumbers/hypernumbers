%%% @copyright (C) 2010, Hypernumbers Ltd.

-module(hn_groups).

-export([create_group/2, delete_group/2,
         add_user/3, rem_user/3, set_users/3,
         is_member/3,
         dump_script/1, load_script/2]).

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
    mnesia:activity(transaction, fun mnesia:delete/3, [Tbl,GroupN,write]).

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

-spec dump_script(string()) -> string(). 
dump_script(Site) ->
    Tbl = hn_db_wu:trans(Site, group),
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
    add_user(Site, ?lget(group, T), ?lget(uid, T));
exec_script_term({set_users, T}, Site) ->
    set_users(Site, ?lget(group, T), ?lget(uids, T)).