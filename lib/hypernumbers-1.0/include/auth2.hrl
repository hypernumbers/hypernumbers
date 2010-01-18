
-record(control, {champion = [] :: string(),
                  challenger = [] :: string(),
                  views = gb_trees:empty()}).

-record(view, {users = gb_sets:empty() :: gb_set(),
               groups = gb_sets:empty() :: gb_set() }).
        
