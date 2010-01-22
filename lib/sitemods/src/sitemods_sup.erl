%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @TODO better documentation - what this supervisor is used for, etc, etc
-module(sitemods_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @spec start_link() -> Return
%% @doc  Supervisor call back
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> {ok,Children}
%% @doc  Supervisor call back
init([]) ->
    ChildList = bootstrap_sites(),
    {ok,{{one_for_one,60,1}, ChildList}}.

bootstrap_sites() ->
    case application:get_env(hypernumbers, bootstrapped) of
        {ok, Sites} -> consolidate([bootstrap_site(S, T, O) || {S, T, O} <- Sites]);
        _Else       -> []
    end.

bootstrap_site(S, T, O) ->
    ok = case mnesia:transaction(fun() -> mnesia:read(core_site, S) end) of
             {atomic, []} -> hn_setup:site(S, T, O);
             _Else        -> ok
         end,
    
    case list_to_atom(atom_to_list(T)++"_sup") of
        hypernumbers_sup ->
            [];
        App ->
            case catch App:module_info() of
                {'EXIT', _ } -> [];
                _Mod         ->
                    case proplists:get_value(init_args, O) of
                        undefined -> {App, []};
                        Args      -> {App, Args}
                    end
            end
    end.

consolidate(List) -> consol(List, []).

consol([], Acc)                -> make_children(Acc);
consol([[] | T], Acc)          -> consol(T, Acc);
consol([{App, Args} | T], Acc) ->
    NewAcc = case proplists:get_value(App, Acc) of
                 undefined -> [{App, [Args]} | Acc];
                 OldArgs   -> NewTuple = {App, [Args | OldArgs]},
                              lists:keyreplace(App, 1, Acc, NewTuple)
             end,
    consol(T, NewAcc).

make_children(List) -> make_c1(List, []).

make_c1([], Acc)                -> Acc;
make_c1([{App, Args} | T], Acc) ->
    NewAcc =  {App, {App, start_link, [Args]},
               permanent, 2000, worker, [start]},
    make_c1(T, [NewAcc | Acc]).
