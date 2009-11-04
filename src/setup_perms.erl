%%%-------------------------------------------------------------------
%%% @author gordonguthrie <>
%%% @copyright (C) 2009, gordonguthrie
%%% @doc This module sets up permissions
%%%
%%% @end
%%% Created : 30 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------
-module(setup_perms).

-export([setup/0,
        old_style/0]).

-define(SITE, "http://127.0.0.1:9000").

old_style() ->
    %
    % Users and Groups First
    %    
    % first delete all the users
    % hn_users:delete_all_users_DEBUG(?SITE),

    % now create some users
    % hn_users:create(?SITE, "gordon", ["dev", "admin"], "password"),
    % hn_users:create(?SITE, "tom", ["dev", "admin"], "password"),
    % hn_users:create(?SITE, "dale", ["dev", "admin"], "password"),
    % hn_users:create(?SITE, "stephen", ["dev", "admin"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG("http://127.0.0.1:9000"),

    % the home page
    auth_srv:add_perm("http://127.0.0.1:9000", [{user, "*"}, {group, "*"}],
                      ["[**]"],[read, write],
                      "_global/spreadsheet", ["_global/spreadsheet", "_global/pagebuilder"]),

    auth_srv:add_perm("http://127.0.0.1:9000", [{user, "*"}, {group, "*"}],
                      [],[read, write],
                      "_global/spreadsheet", ["_global/spreadsheet", "_global/pagebuilder"]).    


setup() ->
    %
    % Users and Groups First
    %    
    % first delete all the users
    hn_users:delete_all_users_DEBUG(?SITE),

    % now create some users
    hn_users:create(?SITE, "gordon", ["dev", "admin"], "password"),
    hn_users:create(?SITE, "tom", ["dev", "admin"], "password"),
    hn_users:create(?SITE, "dale", ["dev", "admin"], "password"),
    hn_users:create(?SITE, "stephen", ["dev", "admin"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG(?SITE),

    % the home page
    auth_srv:add_perm(?SITE, [{user, "*"}, {group, "*"}], [], [read],
                      "site/home", ["site/home"]),

    % the login page
    auth_srv:add_perm(?SITE, [{user, "*"}, {group, "*"}], ["_user", "login"],
                      [read, write], "hypernumbers/login",
                      ["_global/login"]),

    % now make the public spreadsheets public
    % and give the admin group write access
    auth_srv:add_perm(?SITE, [{user, "*"}, {group, "*"}], ["public", "[**]"],
                      [read],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),
    auth_srv:add_perm(?SITE, [{group, "admin"}], ["public", "[**]"],
                      [read, write],
                       "_global/spreadsheet", ["_global/spreadsheet"]),

    % now set up the user space
    % * first up the user home pages
    auth_srv:add_perm(?SITE, [{user, "gordon"}], ["u", "gordon"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(?SITE, [{user, "tom"}], ["u", "tom"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(?SITE, [{user, "dale"}], ["u", "dale"], [read],
                      "site/userhome", ["site/userhome"]),
    auth_srv:add_perm(?SITE, [{user, "stephen"}], ["u", "stephen"], [read],
                      "site/userhome", ["site/userhome"]),
 
    % * now the user spreadsheets
    auth_srv:add_perm(?SITE, [{user, "gordon"}], ["u", "gordon", "[**]"],
                      [read, write],  "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_perm(?SITE, [{user, "tom"}], ["u", "tom", "[**]"],
                      [read, write],  "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_perm(?SITE, [{user, "dale"}], ["u", "dale", "[**]"],
                      [read, write],  "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_perm(?SITE, [{user, "stephen"}], ["u", "stephen", "[**]"],
                      [read, write],  "_global/spreadsheet",
                      ["_global/spreadsheet", "_global/pagebuilder"]),
 
    % now create the dev space
    auth_srv:add_perm(?SITE, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]).
    
    

