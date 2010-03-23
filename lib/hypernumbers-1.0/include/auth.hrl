
-record(control, {champion = [] :: string(),
                  challenger = [] :: string(),
                  views = gb_trees:empty()}).

-record(view, {everyone = false :: true | false,
               users = gb_sets:empty() :: gb_set(),
               groups = gb_sets:empty() :: gb_set() }).
        
-type auth_spec() :: [everyone | 
                      {user, string()} | 
                      {group, string()}
                     ].

-type auth_req() :: {string(), [string()]}.
