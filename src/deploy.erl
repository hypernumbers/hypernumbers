-module(deploy).

-export([run/0]).

run() -> run1(1).


run1(1000) -> io:format("one thousand is enough, thank you...~n");
run1(N)    -> Sub = "sub" ++ integer_to_list(N),
              hn_setup:site("http://" ++ Sub ++ ".localhost:9000", pirate, []),
              io:format("Site no ~p deployed~n", [N]),
              run1(N + 1).
             
