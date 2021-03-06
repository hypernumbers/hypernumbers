%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009 - 2014, Hypernumbers.com
%%% @doc       hot code loader, etc
%%% @end
%%% Created :  by gordon@hypernumbers.com
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_updater).

-export([do/1, migrate/0, migrate/1]).

do(suspend)    -> suspend();
do(resume)     -> resume();
do(refresh)    -> refresh();
do(hotswap)    -> refresh(), hotswap();
do(restart)    -> refresh(), hotswap(), restart();
do(migrate)    -> migrate();
do(git_pull)   -> git_pull(), refresh(), hotswap();
do(full_build) -> full_build(), refresh(), hotswap();
do(Other)      -> io:format("I don't know how to '~s'", [Other]).

%% does a full rebuild of the system
-spec full_build() -> ok.
full_build() ->
    CWD = filename:absname(""),
    Dir = code:priv_dir(hypernumbers) ++ "/../../../",
    ok = file:set_cwd(Dir),
    _Ret = os:cmd("git pull"),
    ok = file:set_cwd(Dir ++ "ebin/"),
    code:load_file(compile_code),
    compile_code:build(Dir),
    ok = file:set_cwd(CWD).

%% Updates which don't need new data and can't crash the system
-spec git_pull() -> ok.
git_pull() ->
    CWD = filename:absname(""),
    Dir = code:priv_dir(hypernumbers) ++ "/../../../",
    ok = file:set_cwd(Dir),
    _Ret = os:cmd("git pull"),
    ok = file:set_cwd(Dir ++ "ebin/"),
    code:load_file(compile_code),
    compile_code:build_quick(Dir),
    ok = file:set_cwd(CWD).

%% suspend the node by stopping the Hypernumbers application
-spec suspend() -> ok.
suspend() ->
    ok = application:stop(hypernumbers).

%% resume the node by starting the Hypernumbers application
resume() ->
    ok = application:start(hypernumbers).

%% Updates which cannot crash the system.
-spec refresh() -> ok.
refresh() ->
    ok = hn_setup:update().

%% Hot-Swaps in the latest code beams.
-spec hotswap() -> ok.
hotswap() ->
    Root   = root(),
    OnDisk = on_disk(Root),
    ok     = unload_deleted(OnDisk, loaded(Root)),
    ok     = load_new(OnDisk, loaded(Root)),
    ok     = reload_current(loaded(Root)).

-spec restart() -> ok.
restart() -> init:restart().

-spec migrate() -> ok.
migrate() ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    do_migrate(integer_to_list(Secs)).

-spec migrate(fun()) -> ok.
migrate(Fun) ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    do_migrate(integer_to_list(Secs), Fun).

-spec do_migrate(string(), fun()) -> ok.
do_migrate(SecsS, Fun) ->
    Dest = "migrate_" ++ SecsS,
    premigrate(Dest),
    ok = Fun(Dest),
    postmigrate(Dest).

-spec do_migrate(string()) -> ok.
do_migrate(SecsS) ->
    Dest = "migrate_" ++ SecsS,
    premigrate(Dest),
    postmigrate(Dest).

postmigrate(Dest) ->
    % Don't use specific imports, this way we can resume if interrupted.
    hn_archive:import(Dest),
    ok = hypernumbers_sup:resume_mochi().

premigrate(Dest) ->
    ok = hypernumbers_sup:suspend_mochi(),
    Sites = hn_setup:get_sites(),
    hn_archive:export(Dest, Sites),
    [ok = hn_setup:delete_site(S) || S <- Sites],

    % Only now, can we reload the new code.
    do(hotswap).


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
           code:delete(M),
           io:format("purging and deleting> ~s~n", [M])
     end || {M,_} <- dict:to_list(Loaded),
            not(dict:is_key(M, OnDisk))],
    ok.

%% Load code which is on disk, but not yet in memory.
-spec load_new(dict(), dict()) -> ok.
load_new(OnDisk, Loaded) ->
    [ begin
          code:purge(M), %% just in case
          {module,M} = code:load_abs(strip_beam(Path)),
          io:format("loading> ~s~n", [M]),
          run_test(M)
      end || {M,Path} <- dict:to_list(OnDisk),
             not(dict:is_key(M, Loaded))],
    ok.


%% Reload the code in memory that has changed on disk.
-spec reload_current(dict()) -> ok.
reload_current(Loaded) ->
    Reloaded = [reload_module(Mod) || {Mod,Path} <- dict:to_list(Loaded),
                                      needs_reload(Mod, Path)],
    [code_change_otp(Mod) || Mod <- Reloaded,
                             is_genserver(Mod)],
    ok.

-spec reload_module(M) -> M when is_subtype(M, atom()).
reload_module(Mod) ->
    code:purge(Mod),
    {module, Mod} = code:load_file(Mod),
    io:format("reloading> ~s~n", [Mod]),
    run_test(Mod),
    Mod.

-spec code_change_otp(atom()) -> any().
code_change_otp(Mod) ->
    F = fun(Pid) ->
                sys:suspend(Pid),
                sys:change_code(Pid, Mod, nil, nil),
                sys:resume(Pid)
        end,
    [F(P) || P <- erlang:processes(), is_running(P, Mod)],
    code:purge(Mod).

-spec needs_reload(atom(), string()) -> boolean().
needs_reload(Mod, Path) ->
    CurrV = mod_version(Mod),
    case beam_lib:version(Path) of
        {ok, {Mod, [NewV]}} when NewV /= CurrV -> true;
        _Else                                  -> false
    end.

-spec on_disk(string()) -> dict().
on_disk(Root) ->
    Files = filelib:wildcard(Root ++ "/*.beam") ++
               filelib:wildcard(Root ++ "/lib/*/ebin/*.beam"),
    List = [ {list_to_atom(filename:basename(F, ".beam")),
                        filename:absname(F, Root)}
             		        || F <- Files],
    dict:from_list(List).

-spec loaded(string()) -> dict().
loaded(Root) ->
    dict:from_list([ KV || KV={_, Path} <- code:all_loaded(),
                           is_list(Path),
                           lists:prefix(Root, Path)]).

-spec run_test(atom()) -> ok.
run_test(excel_util) -> ok;
run_test(odf_criteria) -> ok;
run_test(M) ->
    Attrs = M:module_info(exports),
    case lists:member({test,0}, Attrs) of
        true ->
            case application:get_env(hypernumbers, environment) of
                {ok, development} -> M:test();
                _ -> ok
            end;
        false -> ok
    end,
    ok.

-spec mod_version(atom()) -> integer().
mod_version(M) ->
    Attrs = M:module_info(attributes),
    hd(proplists:get_value(vsn, Attrs, [undefined])).

-spec is_genserver(atom()) -> boolean().
is_genserver(M) ->
    Attrs = M:module_info(attributes),
    case proplists:get_value(behaviour, Attrs) of
        [gen_server] -> true;
        _Else        -> false
    end.

-spec is_running(pid(), atom()) -> boolean().
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
root() -> code:lib_dir(hypernumbers) ++ "/../../".

strip_beam(Path) -> strip_beam(Path, []).
strip_beam([], Acc) -> lists:reverse(Acc);
strip_beam(".beam"++_, Acc) -> lists:reverse(Acc);
strip_beam([X | Rest], Acc) -> strip_beam(Rest, [X | Acc]).
