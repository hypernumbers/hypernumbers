%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie
%%% @copyright  (C) 2012, Hypernumbers Ltd
%%% @doc        registers phone handsets and maintains their
%%%             state
%%%
%%% @end
%%% Created :   13 Jul 2012 by <gordon@vixo.com>
%%%-------------------------------------------------------------------
-module(softphone_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% normal api
-export([
         make_free_dial_call/1,
         reg_dial/3,
         reg_phone/2,   % register a phone
         break_phone/2, % sets a break when the socket times out - see expire/1
         unreg_phone/2, % actually unregisters the phone
         has_phone/2,
         dump_phones/1
        ]).

% for use internally by softphone_srv only
% expire is used to check if a softphone has really gone away
% or it is just reregistering with a new TCP connection
-export([
         expire/2
        ]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 3000).

-include("spriki.hrl").
-include("twilio.hrl").
-include("phonecall_srv.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {site, softphones = []}).
-record(phone_status, {uid, key, away = false, numbers}).

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

dump_phones(S) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, dump_phones).

has_phone(S, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {has_phone, Uid}).

expire(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    {ok, _TRef} = timer:apply_after(?TIMEOUT, gen_server, call,
                                    [{global, Id}, {expire, Uid}]),
    ok.

break_phone(#refX{site = S} = Ref, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {break_phone, Ref, Uid}).

unreg_phone(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {unreg_phone, Uid}).

reg_phone(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {reg_phone, Uid}).

reg_dial(#refX{site = S, path = P, obj = O}, Uid, Numbers) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {reg_dial, P, O, Uid, Numbers}).

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
init([Site]) ->
    {ok, #state{site = Site}}.

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
handle_call(dump_phones, _From, State) ->
    io:format("about to dump softphones for ~p~n", [State#state.site]),
    Fun = fun(#phone_status{uid = U, key = K, away = A, numbers = N}) ->
                  {ok, Email} = passport:uid_to_email(U),
                  io:format("Phone for ~p: on ~p~n- is away?     : ~p~n"
                            ++ "- with numbers : ~p~n",
                            [Email, K, A, N])
          end,
    [Fun(X) || X <- State#state.softphones],
    {reply, ok, State};
handle_call({get_twiml, Path, Obj}, _From, State) ->
    io:format("in get_twiml for ~p ~p~n", [Path, Obj]),
    Key = {Path, Obj},
    #phone_status{numbers = N} = lists:keyfind(Key, 2, State#state.softphones),
    Reply = make_dial(N),
    {reply, Reply, State};
handle_call({reg_dial, Path, Obj, Uid, Numbers}, _From, State) ->
    Softphones = State#state.softphones,
    Key = {Path, Obj},
    NewPhone = #phone_status{uid = Uid, key = Key, numbers = Numbers},
    NewSoftphones = lists:keyreplace(Uid, 2, Softphones, NewPhone),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({unreg_phone, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    NewSoftphones = lists:keydelete(Uid, 2, Softphones),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({reg_phone, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    NewPhone = #phone_status{uid = Uid},
    NewSoftphones = lists:keystore(Uid, 2, Softphones, NewPhone),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({expire, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    Phone = lists:keysearch(Uid, 2, Softphones),
    #phone_status{away = A} = Phone,
    NewSoftphones = case A of
                        break -> lists:keydelete(Uid, 2, Softphones);
                        _     -> Softphones
                    end,
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({break_phone, Ref, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    Phone = lists:keysearch(Uid, 2, Softphones),
    NewPhone = Phone#phone_status{away = break},
    NewSoftphones = lists:keystore(Uid, 2, Softphones, NewPhone),
    ok = softphone_srv:expire(Ref, Uid),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({has_phone, Uid}, _From, State) ->
    Reply = case lists:keyfind(Uid, 1, State#state.softphones) of
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

%%%===================================================================
%% EUnit Tests
%%%===================================================================
%testA19{{S, P}) ->

