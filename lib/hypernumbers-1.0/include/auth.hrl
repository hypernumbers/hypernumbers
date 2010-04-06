-type uid() :: string().

-type auth_spec() :: [everyone | 
                      {user, string()} | 
                      {group, string()}
                     ].

-type auth_req() :: {string(), [string()]}.
