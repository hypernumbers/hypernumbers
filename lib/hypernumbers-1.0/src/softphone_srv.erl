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
         make_free_dial_call/1
        ]).

-define(SERVER, ?MODULE).

-include("spriki.hrl").
-include("twilio.hrl").
-include("phonecall_srv.hrl").

-record(state, {}).

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

make_free_dial_call(State) ->
    HyperTag = phonecall_srv:get_hypertag(State),
    S = phonecall_srv:get_site(State),
    P = ["_services", "phone"],
    io:format("HyperTag is ~p~n", [HyperTag]),
    HT = passport:open_hypertag(S, P, HyperTag),
    io:format("HT is ~p~n", [HT]),
    {ok, _Uid, _EMail, [Idx, _OrigEmail], _, _} = HT,
    XRefX = new_db_api:idx_to_xrefX(S, Idx),
    #xrefX{path = OrigP, obj = OrigCell} = XRefX,
    OrigRef = hn_util:xrefX_to_refX(XRefX),
    io:format("OrigRef is ~p~n", [OrigRef]),
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
handle_call(Request, _From, State) ->
    io:format("Request is ~p~n", [Request]),
    Number = #number{number="++44776251669"},
    Dial = #dial{body = [Number]},
    Reply = [Dial],
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

