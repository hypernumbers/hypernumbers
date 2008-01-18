-module(scratch_boot).

-export([boot/0]).

boot()->
    code:add_patha("/opt/SVN/spriki/trunk/lib/utilities-1.0/ebin"),
    Pid=spawn(filefilters, test_DEBUG, []),
    io:format("filefilters spawned with Pid ~p~n",[Pid]).
