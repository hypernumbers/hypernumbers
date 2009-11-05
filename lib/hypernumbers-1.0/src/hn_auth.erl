%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hn_auth).

-export( [ init_permissions/1 ]).

% Default permissions for hypernumbers.com (use the same for dev)
init_permissions(Host) ->
    [ auth_srv:add_perm(Host, Users, Path, Perms, Default, Pages)
      || {Users, Path, Perms, Default, Pages} <- default_permissions() ],

    Views = hn_config:get(default_pages),
    
    auth_srv:add_views(Host, [{group, "dev"}], [], Views),
    auth_srv:add_views(Host, [{group, "dev"}], ["[**]"], Views),
    
    ok.

default_permissions() ->
    [
     % Home Page
     { [{user, "*"}], [], [read], "_global/home", ["_global/home"] },
          
     % Login Page
     { [{user, "*"}], ["_user", "login"], [read,write],
       "_global/login", ["_global/login"] },
     
     % Public Section
     { [{user, "*"}], ["public", "[**]"], [read],
       "_global/spreadsheet", ["_global/spreadsheet"] },
     { [{group, "admin"}], ["public", "[**]"], [read, write],
       "_global/spreadsheet", hn_config:get(default_pages) },
     
     % Dev section
     { [{group, "dev"}], ["dev", "[**]"], [read, write],
       "_global/spreadsheet", hn_config:get(default_pages) }    
    ].
