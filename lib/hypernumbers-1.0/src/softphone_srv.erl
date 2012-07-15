%%%-------------------------------------------------------------------
%%% @author Gordon Guthrie <gordon@gordon.dev>
%%% @copyright (C) 2012, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2012 by Gordon Guthrie <gordon@gordon.dev>
%%%-------------------------------------------------------------------
-module(softphone_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
         make_free_dial_call/1,
         register_dial/3,
         register_phone/2,
         has_phone/2
        ]).

-define(SERVER, ?MODULE).

-include("spriki.hrl").
-include("twilio.hrl").
-include("phonecall_srv.hrl").

-record(state, {softphones = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
        case application:get_env(hypernumbers, startup_debug) of
       {ok, true} -> io:format("...starting softphone_srv for ~p~n", [Site]);
       _Other     -> ok
    end,
    Id = hn_util:site_to_atom(Site, "_softphone"),
    gen_server:start_link({global, Id}, ?MODULE, [Site], []).

has_phone(S, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {has_phone, Uid}).

register_phone(#refX{site = S, path = P, obj = O}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {register_phone, P, O, Uid}).

register_dial(#refX{site = S, path = P, obj = O}, Uid, Numbers) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {register_dial, P, O, Uid, Numbers}).

make_free_dial_call(State) ->
    HyperTag = phonecall_srv:get_hypertag(State),
    S = phonecall_srv:get_site(State),
    P = ["_services", "phone"],
    HT = passport:open_hypertag(S, P, HyperTag),
    {ok, _Uid, _EMail, [Idx, _OrigEmail], _, _} = HT,
    XRefX = new_db_api:idx_to_xrefX(S, Idx),
    #xrefX{path = OrigP, obj = OrigCell} = XRefX,
    OrigRef = hn_util:xrefX_to_refX(XRefX),
    [Phone] = new_db_api:get_phone(OrigRef),
    Config = Phone#phone.softphone_config,
    % the config has to be "free dial" or this is a bummer
    % and should wig out
    "free dial" = get_perms(Config, "phone_out_permissions"),
    Id = hn_util:site_to_atom(S, "_softphone"),
    TwiML = gen_server:call({global, Id}, {get_twiml, OrigP, OrigCell}),
    {TwiML, []}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([_Site]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_twiml, Path, Obj}, _From, State) ->
    io:format("in get_twiml for ~p ~p~n", [Path, Obj]),
    io:format("State is ~p~n", [State]),
    Key = {Path, Obj},
    {_, _, Numbers} = lists:keyfind(Key, 1, State#state.softphones),
    Reply = make_dial(Numbers),
    {reply, Reply, State};
handle_call({register_dial, Path, Obj, Uid, Numbers}, _From, State) ->
    io:format("in register_dial for ~p ~p ~p~n", [Path, Obj, Uid]),
    Softphones = State#state.softphones,
    Key = {Path, Obj},
    NewSoftphones = lists:keystore(Key, 1, Softphones, {Key, Uid, Numbers}),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({register_phone, Path, Obj, Uid}, _From, State) ->
    io:format("in register_phone for ~p ~p ~p~n", [Path, Obj, Uid]),
    Softphones = State#state.softphones,
    Key = {Path, Obj},
    NewSoftphones = lists:keystore(Key, 1, Softphones, {Key, Uid, []}),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({has_phone, Uid}, _From, State) ->
    io:format("in has_phone for ~p~n", [Uid]),
    Reply = case lists:keyfind(Uid, 2, State#state.softphones) of
                false -> false;
                _     -> true
            end,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_perms({"permissions", {struct, List}}, Key) ->
    case proplists:lookup(Key, List) of
        none       -> none;
        {Key, Val} -> Val
    end.

make_dial(Numbers) -> make_d2(Numbers, []).

make_d2([], Acc)      -> #dial{body = lists:reverse(Acc)};
make_d2([H | T], Acc) -> NewAcc = #number{number = H},
                         make_d2(T, [NewAcc | Acc]).
