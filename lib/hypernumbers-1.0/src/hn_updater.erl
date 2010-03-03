-module(hn_updater).

-export([do/1]).

do(safe)        -> safe();
do(reload)      -> safe(), reload();
do(restart)     -> safe(), reload(), restart();
do(everything)  -> safe(), reload(), restart();
do(Other)       -> io:format("I don't know how to do '~s'", [Other]).


%% Updates which cannot crash the system.
-spec safe() -> ok. 
safe() -> 
    ok = hn_setup:update().

%% Full hot-swap code reload.
-spec reload() -> ok. 
reload() ->
    Root = root(),
    OnDisk = on_disk(Root),
    ok     = unload_deleted(OnDisk, loaded(Root)),
    ok     = load_new(OnDisk, loaded(Root)),
    ok     = reload_current(loaded(Root)).

-spec restart() -> ok.
restart() -> init:restart().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  ______          __                                ___             
%% /\__  _\        /\ \__                            /\_ \            
%% \/_/\ \/     ___\ \ ,_\    __  _ __   ___      __ \//\ \     ____  
%%    \ \ \   /' _ `\ \ \/  /'__`\\`'__\' _ `\  /'__`\ \ \ \   /',__\ 
%%     \_\ \__/\ \/\ \ \ \_/\  __/ \ \//\ \/\ \/\ \L\.\_\_\ \_/\__, `\
%%     /\_____\ \_\ \_\ \__\ \____\ \_\\ \_\ \_\ \__/.\_\\____\/\____/
%%     \/_____/\/_/\/_/\/__/\/____/\/_/ \/_/\/_/\/__/\/_//____/\/___/ 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Unload code which is in memory, but no longer on disk.
-spec unload_deleted(dict(), dict()) -> ok. 
unload_deleted(OnDisk, Loaded) ->
    [begin code:purge(M), 
           code:delete(M)
     end || {M,_} <- dict:to_list(Loaded),
            not(dict:is_key(M, OnDisk))],
    ok.

%% Load code which is on disk, but not yet in memory.
-spec load_new(dict(), dict()) -> ok. 
load_new(OnDisk, Loaded) ->
    [ begin 
          code:purge(M), %% just in case
          {module,M} = code:load_abs(Path) 
      end || {M,Path} <- dict:to_list(OnDisk), 
             not(dict:is_key(M, Loaded))],
    ok.
    
%% Reload the code in memory that has changed on disk.
-spec reload_current(dict()) -> ok. 
reload_current(Loaded) ->
    Reloaded = [{module, Mod} = reload_module(Mod) 
                || {Mod,Path} <- dict:to_list(Loaded),
                   needs_reload(Mod, Path)],
    [code_change_otp(Mod) || {module, Mod} <- Reloaded,
                             is_genserver(Mod)],
    ok.

-spec reload_module(M) -> {module, M} when is_subtype(M, atom()). 
reload_module(Mod) ->
    code:purge(Mod),
    {module, Mod} = code:load_file(Mod).

-spec code_change_otp(atom()) -> any(). 
code_change_otp(Mod) ->
    F = fun(Pid) ->
                sys:suspend(Pid),
                sys:change_code(Pid, Mod, nil, nil),
                sys:resume(Pid)
        end,
    [F(P) || P <- erlang:processes(), is_running(P, Mod)],
    code:purge(Mod).
                 
-spec needs_reload(atom(), string()) -> true | false. 
needs_reload(Mod, Path) ->
    CurrV = mod_version(Mod),
    case beam_lib:version(Path) of
        {ok, {Mod, [CurrV]}} -> false;
        _Else              -> true
    end.

-spec on_disk(string()) -> dict().
on_disk(Root) ->
    dict:from_list([ {list_to_atom(filename:basename(F, ".beam")), 
                      strip_beam(filename:absname(F, Root))}
                     || F <- filelib:wildcard("ebin/*.beam") ++ 
                             filelib:wildcard("lib/*/ebin/*.beam")]).

-spec loaded(string()) -> dict(). 
loaded(Root) ->
    dict:from_list([ KV || KV={_, Path} <- code:all_loaded(), 
                           is_list(Path),
                           lists:prefix(Root, Path)]).

-spec mod_version(atom()) -> integer(). 
mod_version(M) ->
    Attrs = M:module_info(attributes),
    hd(proplists:get_value(vsn, Attrs, [undefined])).

-spec is_genserver(atom()) -> true | false. 
is_genserver(M) ->
    Attrs = M:module_info(attributes),
    case proplists:get_value(behaviour, Attrs) of
        [gen_server] -> true; 
        _Else        -> false
    end.

-spec is_running(pid(), atom()) -> true | false. 
is_running(Pid, M) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, D} ->
            case proplists:get_value('$initial_call', D) of
                {M,_F,_A} -> true;
                _         -> false
            end;
        _ -> 
            false
    end.

-spec root() -> string(). 
root() -> {ok, Dir} = file:get_cwd(), Dir.

strip_beam(Path) -> strip_beam(Path, []). 
strip_beam([], Acc) -> lists:reverse(Acc); 
strip_beam(".beam"++_, Acc) -> lists:reverse(Acc);
strip_beam([X | Rest], Acc) -> strip_beam(Rest, [X | Acc]).
