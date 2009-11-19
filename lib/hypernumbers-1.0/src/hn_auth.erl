%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hn_auth).

-export( [ init_permissions/1 ]).

% Default permissions for hypernumbers.com (use the same for dev)
init_permissions(Host) ->
    
    [ auth_srv:add_controls(Host, Users, Path, Perms, Default, Pages)
      || {Users, Path, Perms, Default, Pages} <- default_permissions() ],

    % Views = hn_config:get(default_pages),

    %% auth_srv:add_default(Host, [], Views),
    %% auth_srv:add_default(Host, ["[**]"], Views),
    
    ok.

default_permissions() ->
    [
     % Home Page
     { [{user, "anonymous"}], [], [read], [], ["_global/home"] },
     { [{group, "admin"}], [], [read, write],
       [], ["_global/home" | hn_config:get(default_pages) ] },

     % About Page
     { [{user, "anonymous"}], ["about"], [read], [], ["_global/about"] },
     { [{group, "admin"}], ["about"], [read, write],
       [], ["_global/about" | hn_config:get(default_pages) ] },
     
     % Team Page
     { [{user, "anonymous"}], ["team"], [read], "_global/team", ["_global/team"] },
     { [{group, "admin"}], ["team"], [read, write],
       "_global/team", ["_global/team" | hn_config:get(default_pages) ] },

     % Edit Page
     { [{group, "admin"}], ["edit"], [read, write],
       "_global/edit", ["_global/edit" | hn_config:get(default_pages) ] },

     
     % Sign Up Page
     { [{user, "anonymous"}], ["application"], [write, read], "_global/login", ["_global/login"] },
     { [{group, "admin"}], ["application"], [read, write],
       "_global/spreadsheet", hn_config:get(default_pages) },
     
     % Login Page
     { [{user, "*"}], ["_user", "login"], [read,write],
       "_global/login", ["_global/login"] }
    ].
