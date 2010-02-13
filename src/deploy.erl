-module(deploy).

-export([run/0]).

run() -> bits:init_memory_trace(),
         run1(1).


run1(1000) -> io:format("one thousand is enough, thank you...~n");
run1(N)    -> Dom = "http://" ++ "sub" ++ integer_to_list(N) ++
                  ".localhost:9000",
              hn_setup:site(Dom, pirate, []),
              io:format("Site no ~p deployed~n", [N]),
              bits:mem_csv(),
              ok = hn_db_admin:outof_mem(Dom),
              run1(N + 1).
    
