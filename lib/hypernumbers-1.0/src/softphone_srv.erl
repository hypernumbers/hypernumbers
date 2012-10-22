%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie
%%% @copyright  (C) 2012, Hypernumbers Ltd
%%% @doc        registers phone handsets and maintains their
%%%             state. There are three states:
%%%             * registered
%%%             * unregistered
%%%             * break
%%%             You get a break state when the long poll times out
%%%             Break sends an expire message after an interval - if
%%%             if the phone hasn't been reregistered then it is
%%%             unregistered.
%%%             Similary an inbound call will go to a registered
%%%             phone, wait and retry on a 'break' phone and
%%%             divert on an unregistered one
%%%
%%%             Availability is tested by a round trip
%%%             The phone service asks if a Uid is available and waits
%%%             in receive on the waiting_pid
%%%             If there is no registered phone then the answer is no
%%%             If there is a registered phone it asked to reregister
%%%             On registration all phones that have a waiting PID
%%%             tell that PID that they are available
%%%             The waiting PID times itself out
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
         % these fns register the phone with the server for inbound calls
         reg_phone/5,   % register a phone
         break_phone/2, % sets a break when the socket times out - see expire/1
         unreg_phone/2, % actually unregisters the phone
         % this fn handle the busy/idle states
         reg_dial/3,
         idle/2,
         % this fun is called for a free dial
         make_free_dial_call/1,
         % these funs allow the operator to be away
         away/2,
         back/2,
         % ensure that each person has one phone only open
         has_phone/2,
         % checks is the user can take an inbound call
         is_available/2,
         % funs for finding a particular user
         hunt/2,
         % debugging
         dump_phones/1
        ]).

% for use internally by softphone_srv only
% expire is used to check if a softphone has really gone away
% or it is just reregistering with a new TCP connection
-export([
         expire/2
        ]).

-define(SERVER, ?MODULE).
-define(EXPIRETIMEOUT,  3000).
-define(WAITINGTIMEOUT, 4000).

-include("spriki.hrl").
-include("twilio.hrl").
-include("phonecall_srv.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {site, softphones = []}).
-record(phone_status, {uid = "", key = none, break = false, away = false,
                       busy = false, registered_pid = none,
                       waiting_pid = none, numbers = [], id = [],
                       groups = []}).

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

away(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {away, Uid}).

back(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {back, Uid}).

idle(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {idle, Uid}).

dump_phones(S) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, dump_phones).

has_phone(S, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {has_phone, Uid}).

expire(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    {ok, _TRef} = timer:apply_after(?EXPIRETIMEOUT, gen_server, call,
                                    [{global, Id}, {expire, Uid}]),
    ok.

hunt(#refX{site = S}, Group) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {hunt, Group}).

is_available(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    Pid = self(),
    gen_server:call({global, Id}, {waiting, Pid, Uid}),
    receive
        {msg, available}   -> {ok, available};
        {msg, unavailable} -> {ok, unavailable}
    after
        ?WAITINGTIMEOUT    -> {ok, unavailable}
    end.

break_phone(#refX{site = S} = Ref, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {break_phone, Ref, Uid}).

unreg_phone(#refX{site = S}, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {unreg_phone, Uid}).

reg_phone(#refX{site = S}, Pid, PhoneID, Groups, Uid) ->
    Id = hn_util:site_to_atom(S, "_softphone"),
    gen_server:call({global, Id}, {reg_phone, Pid, PhoneID, Groups, Uid}).

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
    print_phones(State#state.softphones),
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
    CreateFun = fun() ->
                        #phone_status{uid = Uid, busy = true, key = Key,
                                      numbers = Numbers}
                end,
    ChangeFun = fun(P) ->
                        P#phone_status{uid = Uid, busy = true, key = Key,
                                      numbers = Numbers}
                end,
    NewSoftphones = update_state(Softphones, Uid, CreateFun, ChangeFun),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({unreg_phone, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    NewSoftphones = lists:keydelete(Uid, 2, Softphones),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({away, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    CreateFun = fun() ->
                        #phone_status{uid = Uid, busy = false}
                end,
    ChangeFun = fun(P) ->
                        P#phone_status{away = true}
                end,
    NewSoftphones = update_state(Softphones, Uid, CreateFun, ChangeFun),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({back, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    CreateFun = fun() ->
                        #phone_status{uid = Uid, busy = false}
                end,
    ChangeFun = fun(P) ->
                        P#phone_status{away = false}
                end,
    NewSoftphones = update_state(Softphones, Uid, CreateFun, ChangeFun),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({idle, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    CreateFun = fun() ->
                        #phone_status{uid = Uid, busy = false}
                end,
    ChangeFun = fun(P) ->
                        P#phone_status{busy = false, numbers = []}
                end,
    NewSoftphones = update_state(Softphones, Uid, CreateFun, ChangeFun),
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({reg_phone, RegPid, PhoneId, Groups, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    {Reply, NewSoftphones}
        = case lists:keyfind(Uid, 2, Softphones) of
              false ->
                  % create a new phone record
                  Id = {phoneid, PhoneId},
                  NewP = #phone_status{uid = Uid, registered_pid = RegPid,
                                       id = Id, groups = Groups},
                  {Id, lists:keystore(Uid, 2, Softphones, NewP)};
              P ->
                  % check if the registering phone is the one that exists
                  case P#phone_status.id of
                      {phoneid, PhoneId} ->
                          % if a call is waiting on the phone, and the
                          % phone is free then tell it to go ahead and connect
                          W = P#phone_status.waiting_pid,
                          B = P#phone_status.busy,
                          A = P#phone_status.away,
                          case {W, B, A} of
                              {none, _, _} ->
                                  ok;
                              {Waiting, false, false} ->
                                  Waiting ! {msg, available};
                              {Waiting, _, _} ->
                                  Waiting ! {msg, unavailable}
                          end,
                          NewP = P#phone_status{break = false,
                                                registered_pid = RegPid,
                                                waiting_pid = none},
                          NewS = lists:keystore(Uid, 2, Softphones, NewP),
                          {{phoneid, PhoneId}, NewS};
                      _ ->
                          {user_already_registered, Softphones}
                  end
          end,
    {reply, Reply, State#state{softphones = NewSoftphones}};
handle_call({expire, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    % the expire message comes in after the phone coulda been unregistered
    % mebbies the phone no longer exists
    case lists:keysearch(Uid, 2, Softphones) of
        false ->
            {reply, ok, State};
        {value, Phone} ->
            #phone_status{break = B} = Phone,
            NewSoftphones = case B of
                                true  -> lists:keydelete(Uid, 2, Softphones);
                                false -> Softphones
                            end,
            {reply, ok, State#state{softphones = NewSoftphones}}
    end;
handle_call({waiting, WaitPid, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    NewSoftphones =
        case lists:keysearch(Uid, 2, Softphones) of
            false ->
                % phone ain't registered (yet) but tell state that
                % the call is waiting and give it a go
                % mebbies it will time out? mebbies get registered?
                NewP = #phone_status{uid = Uid, waiting_pid = WaitPid},
                lists:keystore(Uid, 2, Softphones, NewP);
            {value, #phone_status{registered_pid = RegPid} = PS} ->
                case RegPid of
                    none ->
                        % there is a phone but it has no registered PID so
                        % mark the status as waiting
                        % mebbies it will time out? mebbies get registered?
                        NewP = PS#phone_status{waiting_pid = WaitPid},
                        lists:keystore(Uid, 2, Softphones, NewP);
                    _ ->
                        % there is a call registered - tell it to return and
                        % reregister, if it does it will trigger a message
                        % back to the calling process
                        RegPid ! {msg, is_available},
                        NewP = PS#phone_status{waiting_pid = WaitPid},
                        lists:keystore(Uid, 2, Softphones, NewP)
                end
        end,
    {reply, ok, State#state{softphones = NewSoftphones}};
handle_call({break_phone, Ref, Uid}, _From, State) ->
    Softphones = State#state.softphones,
    % the break message comes in after the phone coulda be unregistered
    % mebbies the phone no longer exists
    case lists:keysearch(Uid, 2, Softphones) of
        false ->
            {reply, ok, State};
        {value, Phone} ->
            NewPhone = Phone#phone_status{break = true},
            NewSoftphones = lists:keystore(Uid, 2, Softphones, NewPhone),
            ok = softphone_srv:expire(Ref, Uid),
            {reply, ok, State#state{softphones = NewSoftphones}}
    end;
handle_call({has_phone, Uid}, _From, State) ->
    Reply = case lists:keyfind(Uid, 2, State#state.softphones) of
                false -> false;
                _     -> true
            end,
    {reply, Reply, State};
handle_call({hunt, Group}, _From, State) ->
    Softphones = State#state.softphones,
    Fun = fun(#phone_status{uid = U, groups = G}, Acc) ->
                  case lists:member(Group, G) of
                      true  -> [U | Acc];
                      false -> Acc
                  end
          end,
    Reply = lists:foldl(Fun, [], Softphones),
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

print_phones(SoftPhones) ->
    Fun = fun(#phone_status{uid = U, key = K, busy = B, break = Bk, away = A,
                            waiting_pid = W, registered_pid = P,
                            numbers = N, id = I}) ->
                  {ok, Email} = passport:uid_to_email(U),
                  io:format("Phone for        : ~p~n"
                            ++ "- on             : ~p~n"
                            ++ "- phone is       : ~p~n"
                            ++ "- is busy?       : ~p~n"
                            ++ "- is away?       : ~p~n"
                            ++ "- is breaking?   : ~p~n"
                            ++ "- with numbers   : ~p~n"
                            ++ "- Registered Pid : ~p~n"
                            ++ "- Waiting Pid    : ~p~n",
                            [Email, K, I, B, A, Bk, N, P, W])
          end,
    [Fun(X) || X <- SoftPhones],
    ok.

update_state(Softphones, Uid, CreateFun, ChangeFun) ->
    NewP = case lists:keyfind(Uid, 2, Softphones) of
        false -> CreateFun();
        P     -> ChangeFun(P)
    end,
    lists:keystore(Uid, 2, Softphones, NewP).


