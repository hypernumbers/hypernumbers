%%% @copyright Hypernumbers Ltd.
%%% @TODO better documentation - what this supervisor is used for, etc, etc
-module(sitemods_sup).
-behaviour(supervisor).

-export([ start_link/0, init/1 ]).

-export([ add_site/3, delete_site/2 ]).

%% @spec start_link() -> Return
%% @doc  Supervisor call back
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> {ok,Children}
%% @doc  Supervisor call back
init([]) ->
    ChildList = [ make_child_spec(X)
                  || X<-bootstrap_sites(), X =/= no_supervisor],

    {ok,{{one_for_one,60,1}, ChildList}}.

add_site(Site, Type, Args) ->
    Spec = make_child_spec({Site, Type, Args}),
    supervisor:start_child(sitemods_sup, Spec).

delete_site(Site, Type) ->
    ok = supervisor:terminate_child(sitemods_sup, server_id(Site, Type)),
    supervisor:delete_child(sitemods_sup, server_id(Site, Type)).
    
bootstrap_sites() ->
    case application:get_env(hypernumbers, bootstrapped) of
        {ok, Sites} -> [ bootstrap_site(S, T, O) || {S, T, O} <- Sites];
        _Else       -> []
    end.

bootstrap_site(Site, Type, Opts) ->
    
    ok = case mnesia:transaction(fun() -> mnesia:read(core_site, Site) end) of
             {atomic, []} -> hn_setup:site(Site, Type, Opts);
             _Else        -> ok
         end,

    case srv_name(Type) of
        App ->
            case catch App:module_info() of
                {'EXIT', _ } -> no_supervisor;
                _Mod         ->
                    io:format("got to here ya bas...~n"),
                    case proplists:get_value(init_args, Opts) of
                        undefined -> {Site, Type, []};
                        Args      -> {Site, Type, Args}
                    end
            end
    end.

server_id(Site, Type) ->
    list_to_atom(fmt("~s_~p", [hn_util:site_to_fs(Site), Type])).

make_child_spec({Site, Type, Args}) ->
    Srv = srv_name(Type),
    { server_id(Site, Type),
      {Srv, start_link, [Site, Args]}, permanent, 2000, worker, [start]}.

fmt(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).

srv_name(Type) -> list_to_atom("mod_"++atom_to_list(Type)++"_srv").
