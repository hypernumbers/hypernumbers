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
        old_style/0,
        hypernumbers_style/0]).

%-define(SITES, ["http://127.0.0.1:9000", "http://localhost:9000", "http://192.168.56.101:9000"]).
-define(SITES, ["http://127.0.0.1:9000", "http://localhost:9000"]).

old_style() ->
    %%
    %% Users and Groups First
    %%    
    %% first delete all the users
    %% hn_users:delete_all_users_DEBUG(?SITE),

    %% now create some users
    %% hn_users:create(?SITE, "gordon", ["dev", "admin"], "password"),
    %% hn_users:create(?SITE, "tom", ["dev", "admin"], "password"),
    %% hn_users:create(?SITE, "dale", ["dev", "admin"], "password"),
    %% hn_users:create(?SITE, "stephen", ["dev", "admin"], "password"),

    %%
    %% now setup perms
    %%
    F = fun(S) ->
                auth_srv:clear_all_perms_DEBUG(S),

                                                % the home page
                auth_srv:add_controls(S, [{user, "*"}, {group, "*"}],
                                      ["[**]"],[read, write],
                                      "_global/spreadsheet",
                                      ["_global/spreadsheet", "_global/pagebuilder"]),

                auth_srv:add_controls(S, [{user, "*"}, {group, "*"}],
                                      [],[read, write],
                                      "_global/spreadsheet",
                                      ["_global/spreadsheet", "_global/pagebuilder"])
        end,
    [F(S) || S <- ?SITES],
    ok.

setup() ->
        [setup1(S) || S <- ?SITES],
    ok.

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
    hn_users:create(Site, "user", ["user"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG(Site),

    % the home page
    auth_srv:add_controls(Site, [{user, "*"}, {group, "*"}], [], [read],
                      "_global/home", ["_global/home", "_global/spreadsheet"]),

    % the login page
    auth_srv:add_controls(Site, [{user, "*"}, {group, "*"}], ["_user", "login"],
                      [read, write], "_global/login",
                      ["_global/login"]),

    % now make the public spreadsheets public
    % and give the admin group write access
    auth_srv:add_controls(Site, [{user, "*"}, {group, "*"}], ["public", "[**]"],
                      [read],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["public", "[**]"],
                      [read, write],
                       "_global/spreadsheet", ["_global/spreadsheet"]),

    % now set up the user space
    % * first up the user home pages
    auth_srv:add_controls(Site, [{user, "gordon"}], ["u", "gordon"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "tom"}], ["u", "tom"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "dale"}], ["u", "dale"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "stephen"}], ["u", "stephen"], [read],
                      "_global/userhome", ["_global/userhome"]),
 
    % * now the user spreadsheets
    auth_srv:add_default(Site, ["u"], "_global/spreadsheet"),
    auth_srv:add_controls(Site, [{user, "gordon"}], ["u", "gordon", "[**]"],
                      [read, write], [],
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "tom"}], ["u", "tom", "[**]"],
                      [read, write], [],
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "dale"}], ["u", "dale", "[**]"],
                      [read, write], [],
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "stephen"}], ["u", "stephen", "[**]"],
                      [read, write], [],
                      ["_global/spreadsheet", "_global/pagebuilder"]),
 
    % now create the dev space
    auth_srv:add_controls(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),

    % now get the tree as json and print it
    %     Json = auth_srv:get_as_json(Site, ["u"]),
    %     Json2 = (mochijson:encoder([{input_encoding, utf8}]))(Json),
    %     io:format("Json representation of the permissions tree is~n-~p~n", [Json2]),
    PP = auth_srv:pretty_print(Site, [], text),
    io:format(PP),
    ok.

hypernumbers_style() ->
            [hn_style(S) || S <- ?SITES],
    ok.

hn_style(Site) ->

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
    hn_users:create(Site, "user", ["user"], "password"),

    %
    % now setup perms
    %
    auth_srv:clear_all_perms_DEBUG(Site),

    % the home page
    auth_srv:add_controls(Site, [{user, "*"}, {group, "*"}], [], [read],
                      "_global/home", ["_global/home"]),

    % the login page
    auth_srv:add_controls(Site, [{user, "*"}, {group, "*"}], ["_user", "login"],
                      [read, write], "_global/login",
                      ["_global/login"]),

    % /about/ and /team/
    % note that anyone can read the data but they can't get a page to view it
    % (ie the only acceptable view is JSON)
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["about"], [read]),
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["team"], [read]),
    % now make admin able to edit these pages
    auth_srv:add_controls(Site, [{group, "admin"}], ["about"], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet"]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["team"], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet"]),

    % /application/ is writable but not readable by anyone
    % not that secure !
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["application"], [write]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["application"], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet"]),

    % now create the dev space
    auth_srv:add_controls(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),
    
    % now set up the user space
    % * first up the user home pages
    auth_srv:add_controls(Site, [{user, "gordon"}], ["u", "gordon"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "tom"}], ["u", "tom"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "dale"}], ["u", "dale"], [read],
                      "_global/userhome", ["_global/userhome"]),
    auth_srv:add_controls(Site, [{user, "stephen"}], ["u", "stephen"], [read],
                      "_global/userhome", ["_global/userhome"]),
 
    % * now the user spreadsheets
    auth_srv:add_default(Site, ["u"], "_global/spreadsheet"),
    auth_srv:add_controls(Site, [{user, "gordon"}], ["u", "gordon", "[**]"],
                      [read, write], "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "tom"}], ["u", "tom", "[**]"],
                      [read, write], "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "dale"}], ["u", "dale", "[**]"],
                      [read, write], "_global/spreadsheet",
                      ["_global/spreadsheet",  "_global/pagebuilder"]), 
    auth_srv:add_controls(Site, [{user, "stephen"}], ["u", "stephen", "[**]"],
                      [read, write], "_global/spreadsheet",
                      ["_global/spreadsheet", "_global/pagebuilder"]),
 
    % now create the dev space
    auth_srv:add_controls(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),

    io:format(auth_srv:pretty_print(Site, [], text)).
    

