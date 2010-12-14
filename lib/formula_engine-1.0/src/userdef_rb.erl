%%% @private
-module(userdef_rb).

-export([
         stop/0,
         test/2,
         loop/1]
        ).

%% start() ->
%%     spawn(fun() ->
%%                   register(userdef_rb, self()),
%%                   process_flag(trap_exit, true),
%%                   {ok, CurrDir} = file:get_cwd(),
%%                   Root = muin_util:init(string:tokens(CurrDir, "/")),
%%                   DrvPath = filename:join(["/"] ++ Root ++
%%                                           ["lib",
%%                                            "formula_engine-1.0",
%%                                            "src",
%%                                            "userdef_drv.rb"]),
%%                   Cmd = "ruby " ++ DrvPath,
%%                   Port = open_port({spawn, Cmd}, [{packet, 4}, use_stdio,
%%                                                   exit_status, binary]),
%%                   loop(Port)
%%           end).

stop() ->
    userdef_rb ! stop.

test(Fname, Args) ->
    userdef_rb ! {test, self(), Fname, Args},
    io:format(""),
    receive
        Res ->
            Res
    end.

loop(Port) ->
    receive
        {test, Caller, Fname, Args} ->
            Port ! {self(), {command, term_to_binary({test, Fname, Args})}},
            receive
                {Port, {data, Data}} ->
                    {result, Res} = binary_to_term(Data),
                    Caller ! Res;
                Any ->
                    erlang:display(Any)
            end,
            loop(Port);
        
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end
    end.
