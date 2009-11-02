%%%-------------------------------------------------------------------
%%% @author gordonguthrie <>
%%% @copyright (C) 2009, gordonguthrie
%%% @doc This module sets up permissions
%%%
%%% @end
%%% Created : 30 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------
-module(setup_perms).

-export([setup/0, old_style/0]).


setup() ->
    set_all(fun setup1/1).

old_style() ->
    set_all(fun old_style1/1).

set_all(F) ->
    Hosts = hn_config:get(hosts),
    lists:foreach(fun({_,Port,Site}) ->
                          Path = lists:flatten(io_lib:format("http://~s:~b", [Site,Port])),
                          io:format("~p~n", [Path]),
                          F(Path)
                  end, 
                  Hosts).
    
old_style1(Site) ->
    %
    % Users and Groups First
    %    
    % first delete all the users
    % hn_users:delete_all_users_DEBUG(Site),

    % now create some users
    % hn_users:create(Site, "gordon", ["dev", "admin"], "password"),
    % hn_users:create(Site, "tom", ["dev", "admin"], "password"),
    % hn_users:create(Site, "dale", ["dev", "admin"], "password"),
    % hn_users:create(Site, "stephen", ["dev", "admin"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG(Site),

    % the home page
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["[**]"],
                      [read, write],
                      "hypernumbers/index", ["hypernumbers/index"]).    

setup1(Site) ->
    %
    % Users and Groups First
    %    
    % first delete all the users
    hn_users:delete_all_users_DEBUG(Site),

    % now create some users
    hn_users:create(Site, "gordon", ["dev", "admin"], "password"),
    hn_users:create(Site, "tom", ["dev", "admin"], "password"),
    hn_users:create(Site, "dale", ["dev", "admin"], "password"),
    hn_users:create(Site, "stephen", ["dev", "admin"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG(Site),

    % the home page
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], [], [read],
                      "site/home", ["site/home"]),

    % the login page
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["_user", "login"],
                      [read, write], "hypernumbers/login",
                      ["hypernumbers/login"]),

    % now make the public spreadsheets public
    % and give the admin group write access
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["public", "[**]"],
                      [read],
                      "hypernumbers/index", ["hypernumbers/index"]),
    auth_srv:add_perm(Site, [{group, "admin"}], ["public", "[**]"],
                      [read, write],
                      "hypernumbers/index", ["hypernumbers/index"]),

    % now set up the user space
    % * first up the user home pages
    auth_srv:add_perm(Site, [{user, "gordon"}], ["u", "gordon"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(Site, [{user, "tom"}], ["u", "tom"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(Site, [{user, "dale"}], ["u", "dale"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(Site, [{user, "stephen"}], ["u", "stephen"], [read],
                      "site/userhome", ["site/userhome"]),
 
    % * now the user spreadsheets
    auth_srv:add_perm(Site, [{user, "gordon"}], ["u", "gordon", "[**]"],
                      [read, write], "hypernumbers/index",
                      ["hypernumbers/index"]), 
    auth_srv:add_perm(Site, [{user, "tom"}], ["u", "tom", "[**]"],
                      [read, write], "hypernumbers/index",
                      ["hypernumbers/index"]), 
    auth_srv:add_perm(Site, [{user, "dale"}], ["u", "dale", "[**]"],
                      [read, write], "hypernumbers/index",
                      ["hypernumbers/index"]), 
    auth_srv:add_perm(Site, [{user, "stephen"}], ["u", "stephen", "[**]"],
                      [read, write], "hypernumbers/index",
                      ["hypernumbers/index"]),
 
    % now create the dev space
    auth_srv:add_perm(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                      "hypernumbers/index", ["hypernumbers/index"]).
    
    

