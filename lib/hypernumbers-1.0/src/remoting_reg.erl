%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
-module(remoting_reg).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(HEAP_SIZE, 250000).

-include("spriki.hrl").

%% gen_server callbacks
-export([
         start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         notify_site/1,
         notify_pages/1,
         notify_change/4,
         notify_delete/3,
         notify_delete_attrs/4,
         notify_style/3,
         notify_error/5,
         request_update/4,
         notify_refresh/2
        ]).

-record(state, {servers = []}).

%%
%% Gen Server API
%%

start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:start_link({global, Id}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%%
%% Handle messages
%%

handle_call(_Req, _From, State) -> {reply, invalid_message, State}.

%% @doc  Handle site messages
handle_cast({sitemsg, Site, Path, _Msg} = M, State) ->
    #state{servers = Servers} = State,
    {NS, _PID} = get_pid(Site, Path, Servers),
    {_, PIDs} = lists:unzip(NS),
    % cast site messages to all pages
    [gen_server:cast(X, M) || X <- PIDs],
    {noreply, State#state{servers = NS}};
%% @doc  Handle incoming update message
handle_cast({msg, Site, Path, _Msg} = M, State) ->
    #state{servers = Servers} = State,
    {NS, PID} = get_pid(Site, Path, Servers),
    gen_server:cast(PID, M),
    {noreply, State#state{servers = NS}};
%% @doc  Handle incoming request for updates
handle_cast({fetch, Site, Paths, Time, PID}, State) ->
    #state{servers = Servers} = State,
    Fun = fun(X, Acc) ->
                  Msg = {fetch, Site, X, Time, PID},
                  {NS, PID2} = get_pid(Site, term_to_binary(X), Acc),
                  gen_server:cast(PID2, Msg),
                  NS
          end,
    NS = lists:foldl(Fun, Servers, Paths),
    {noreply, State#state{servers = NS}}.

handle_info(_Info, State)    -> {noreply, State}.
terminate(_Reason, _State)   -> ok.
code_change(_Old, State, _E) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% API Calls
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_update(Site, Paths, Time, Pid) ->
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {fetch, Site, Paths, Time, Pid}).

%% @doc Notify server of site details refresh
notify_site(Site) ->
    Msg = {struct, [{"type", "site_refresh"}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {sitemsg, Site, term_to_binary(["/"]),
                                   term_to_binary(Msg)}).

%% @doc Notify server that the pages have changed
notify_pages(Site) ->
    Msg = {struct, [{"type", "pages_refresh"}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    PID = global:whereis_name(Id),
    gen_server:cast(PID, {sitemsg, Site, term_to_binary(["/"]),
                          term_to_binary(Msg)}).

%% @doc  Notify server of full page refresh
notify_refresh(Site, Path) ->
    Msg = {struct, [{"type", "refresh"},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of change to a cell
notify_change(Site, Path, {RefType, _} = R, Attrs) ->
    Fun = fun({"__"++_Hidden, _}) -> false; (_X) -> true end,
    FilteredAttrs = lists:filter(Fun, Attrs),
    Attrs2 = hn_util:jsonify_attrs(FilteredAttrs),
    Msg = {struct, [{"type", "change"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)},
                    {"attrs", {struct, Attrs2}}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

notify_delete_attrs(Site, Path, {RefType, _} = R, Attrs) ->
    Msg = {struct, [{"type", "delete_attrs"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)},
                    {"attrs", {struct, [{X, ""} || X <- Attrs]}}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

notify_delete(Site, Path, {RefType, _} = R) ->
    Msg = {struct, [{"type", "delete"},
                    {"reftype", RefType},
                    {"path", hn_util:list_to_path(Path)},
                    {"ref", hn_util:obj_to_change_msg(R)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of a new style
notify_style(Site, Path, Style) ->
    {Key, CSS} = hn_mochi:style_to_css(Style),
    Msg = {struct, [{"path", hn_util:list_to_path(Path)},
                    {"type", "style"},
                    {"index", Key},
                    {"css", CSS}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).

%% @doc  Notify server of an error to a cell
notify_error(Site, Path, Ref, error_in_formula, Value) ->
    Msg = {struct, [{"type", "error"},
                    {"reftype", "cell"},
                    {"ref", hn_util:obj_to_change_msg(Ref)},
                    {"original", Value},
                    {"path", hn_util:list_to_path(Path)}]},
    Id = hn_util:site_to_atom(Site, "_remoting"),
    gen_server:cast({global, Id}, {msg, Site, term_to_binary(Path),
                                   term_to_binary(Msg)}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Internal Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pid(Site, Path, Servers) ->
    case lists:keyfind(Path, 1, Servers) of
        false ->
            {ok, PID2} = remoting_sup:add_page_register(Site, Path),
            {[{Path, PID2} | Servers], PID2};
        {Path, PID2} ->
            case is_process_alive(PID2) of
                true ->
                    {Servers, PID2};
                false ->
                    {ok, PID3} = remoting_sup:add_page_register(Site, Path),
                    NS2 = lists:keyreplace(Path, 1, Servers, {Path, PID3}),
                    {NS2, PID3}
            end
    end.
