%%%-------------------------------------------------------------------
%%% @author gordonguthrie <>
%%% @copyright (C) 2009, gordonguthrie
%%% @doc This module sets up permissions
%%%
%%% @end
%%% Created : 30 Oct 2009 by gordonguthrie <>
%%%-------------------------------------------------------------------
-module(setup_perms).

-export([setup/1,setup/0,
        old_style/0,
        hypernumbers_style/0,
        alpha/0,
        alpha_users/0,
        beta/0,
        beta_users/0]).

-include("spriki.hrl").

-define(SITES, ["http://127.0.0.1:9000", "http://localhost:9000",
                "http://192.168.56.101:9000",
                "http://127.0.0.1:8000", "http://localhost:8000",
                "http://poll.tiny.hn:80"]).

%-define(SITES, ["http://127.0.0.1:9000", "http://localhost:9000"]).

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
                                      [],[read, write],
                                      "_global/spreadsheet",
                                      ["**", "_global/spreadsheet", "_global/pagebuilder"]),
                
                auth_srv:add_controls(S, [{user, "*"}, {group, "*"}],
                                      ["[**]"],[read, write],
                                      "_global/spreadsheet",
                                      ["**", "_global/spreadsheet", "_global/pagebuilder"])
        end,
    [F(S) || S <- ?SITES],
    ok.

setup() ->
    setup(?SITES).

setup(Sites) ->
    [setup1(S) || S <- Sites],
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
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["application"],
                      [write]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["application"],
                          [read, write],
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

    io:format(auth_srv:pretty_print(Site, [], text)).
    
alpha() ->
    Site = "http://alpha.hypernumbers.com:80",
    %
    % Users and Groups First
    %    

    %
    % now setup perms
    %
    %auth_srv:clear_all_perms_DEBUG(Site),

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
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["application"],
                      [write]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["application"],
                          [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet"]),

    % now create the dev space
    auth_srv:add_controls(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),

    % now let devs read all the user pages
    auth_srv:add_controls(Site, [{group, "dev"}], ["u", "[**]"],
                          [read],
                          "_global/spreadsheet", [ "**", "_global/spreadsheet"]),

    % now create the admin space
    auth_srv:add_controls(Site, [{group, "dev"}], ["admin", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),
    
    io:format(auth_srv:pretty_print(Site, [], text)).

alpha_users() ->
    Site = "http://alpha.hypernumbers.com:80",
    Users = [
             {"sean", []},
             {"hasan", []},
             {"evdawg", []},
             {"klacke", []},
             {"jonpuleston", []},
             {"threeby", []},
             {"sebastienmenant", []},
             {"beatricealex2", []},
             {"Andrzej\n", []},
             {"roxanaseaton2", []},
             {"edwardoka", []},
             {"roxanaseaton", []},
             {"reshmasohoni", []},
             {"stuart", []},
             {"sebastienmenant2", []},
             {"gordon", ["dev", "admin"]},
             {"beatricealex", []},
             {"stevie", ["dev", "admin"]},
             {"dale", ["dev", "admin"]},
             {"tommcnulty", ["dev", "admin"]}
            ],
    % setup global user guff
    auth_srv:add_default(Site, ["u"], "_global/spreadsheet"),
    [user_perms(User, Site) || {User, _Groups} <- Users],
    [add_groups(User, Groups, 'alpha.hypernumbers.com&80&hn_user') || {User, Groups} <- Users],
    Pattern = {'_', '_', '_','_', '_','_', '_'},
    Fun = fun() -> mnesia:match_object('alpha.hypernumbers.com&80&hn_user',
                                       Pattern, read) end,
    mnesia:transaction(Fun).

user_perms(User, Site) ->
% now set up the user space
    % * first up the user home pages
    auth_srv:add_controls(Site, [{user, User}], ["u", User], [read],
                      "_global/userhome", ["_global/userhome"]),
 
    % * now the user spreadsheets
    auth_srv:add_controls(Site, [{user, User}], ["u", User, "[**]"],
                      [read, write], "_global/spreadsheet",
                      ["*", "_global/spreadsheet", "_global/pagebuilder"]),

    io:format(auth_srv:pretty_print(Site, [], text)).

add_groups(User, Groups, Table) ->
    Fun = fun() ->
                  Record  = mnesia:read({Table, User}),
                  case Record of
                      [] -> ok;
                      [R]  -> io:format("R is ~p~n", [R]),
                            mnesia:write(Table, R#hn_user{groups = Groups},
                                         write)
                  end
          end,
    Ret = mnesia:transaction(Fun),
    io:format("Ret is ~p~n", [Ret]).

beta() ->
    Site = "http://127.0.0.1:9000",
    %
    % Users and Groups First
    %    

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
    auth_srv:add_controls(Site, [{group, "admin"}], [], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet", "_global/pagebuilder"]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["about"], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet", "_global/pagebuilder"]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["team"], [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet", "_global/pagebuilder"]),

    % /application/ is writable but not readable by anyone
    % not that secure !
    auth_srv:add_perm(Site, [{user, "*"}, {group, "*"}], ["application"],
                      [write]),
    auth_srv:add_controls(Site, [{group, "admin"}], ["application"],
                          [read, write],
                      "_global/spreadsheet", ["_global/spreadsheet"]),

    % now create the dev space
    auth_srv:add_controls(Site, [{group, "dev"}], ["dev", "[**]"],
                      [read, write],
                       "_global/spreadsheet", [ "_global/spreadsheet"]),

    % now let devs read all the user pages
    auth_srv:add_controls(Site, [{group, "dev"}], ["u", "[**]"],
                          [read],
                          "_global/spreadsheet", ["**", "_global/pagebuilder",
                                                  "_global/spreadsheet"]),

    % now create the admin space
    auth_srv:add_controls(Site, [{group, "dev"}], ["admin", "[**]"],
                          [read, write],
                          "_global/spreadsheet", ["**", "_global/pagebuilder",
                                                  "_global/spreadsheet"]),
    
    io:format(auth_srv:pretty_print(Site, [], text)).

beta_users() ->
    Site = "http://127.0.0.1:9000",
    Users = [
             {"gordon", ["dev", "admin"]},
             {"stevie", ["dev", "admin"]},
             {"dale", ["dev", "admin"]},
             {"tommcnulty", ["dev", "admin"]}
            ],
    % setup global user guff
    auth_srv:add_default(Site, ["u"], "_global/spreadsheet"),
    [user_perms(User, Site) || {User, _Groups} <- Users],
    [add_groups(User, Groups, '127.0.0.1&9000&hn_user') || {User, Groups} <- Users],
    Pattern = {'_', '_', '_','_', '_','_', '_'},
    Fun = fun() -> mnesia:match_object('127.0.0.1&9000&hn_user',
                                       Pattern, read) end,
    mnesia:transaction(Fun).
