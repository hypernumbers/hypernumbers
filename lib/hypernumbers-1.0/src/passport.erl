-module(passport).

-behaviour(gen_server).

%%% Notes:
%%%
%%% Services like facebook, twitter etc which we may use to
%%% externalize authentication will have their own tables. This way users
%%% don't have to be transformed when new authentication methods are added.

%% API
-export([ start_link/0,
          generate_invite/3,
          stamp_invite/3, 
          verify_stamp/1,
          authenticate/3,
          get_or_create_user/1,
          create_user/3,
          delete_user/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hypernumbers.hrl").
-include("date.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(invite, {hid, expiry}).

-record(user, {hid,
               email,
               passMD5 = nil,
               created_on = calendar:universal_time(),
               lastlogin_on = nil,
               data = dict:new()}).

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
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

-spec generate_invite(string(), [string()], string()) -> string(). 
generate_invite(Site, Path, User) ->
    generate_invite(Site, Path, User, 7).
-spec generate_invite(string(), [string()], string(), integer()) -> string().
generate_invite(Site, Path0, User, Days) ->
    Decorator = "USER_DECORATOR",
    Path = ["_invite", Decorator | Path0],
    HalfKey = [Site, Path],
    Invite = #invite{hid = User, expiry = expire_days_from_now(Days)},
    InviteEnc = encrypt_term_hex(HalfKey, Invite),
    lists:concat(["http://", Site, hn_util:list_to_path(Path),
                  "?tag=", InviteEnc]).

stamp_invite(Site, Path, InviteEnc) ->
    HalfKey = [Site, Path],
    Now = calendar:universal_time(),
    case decrypt_term_hex(HalfKey, InviteEnc) of
        #invite{expiry=E, hid=Hid} when E < Now ->
            {ok, stamp(Hid, "true")};
        _Else ->
            {error, expired}
    end.

-spec verify_stamp(atom() | string()) -> boolean(). 
verify_stamp(undefined) ->
    false;
verify_stamp(Stamp) ->
    [Expiry, User, Hash] = string:tokens(Stamp, ":"),
    User2 = xmerl_ucs:to_utf8(unescapeUnicode(mochiweb_util:unquote(User))),
    case {is_expired(Expiry), gen_hash(User2, Expiry), Hash} of
        {true,_,_}  -> false;
        {false,X,X} -> true
    end.

-spec authenticate(string(), string(), string()) 
                  -> {error, term()} | {ok, string()}.
authenticate(Email, Password, Remember) ->
    Msg = {authenticate, Email, Password},
    case gen_server:call({global, ?MODULE}, Msg, 10000) of
        {ok,Hid} -> {ok, stamp(Hid, Remember)};
        Else -> Else
    end.

-spec get_or_create_user(string()) -> string().
get_or_create_user(Email) -> 
    gen_server:call({global, ?MODULE}, {get_or_create_user, Email}).
    
-spec create_user(string(), string(), string()) -> string().
create_user(Hid, Email, Password) ->
    gen_server:call({global, ?MODULE}, {create_user, Hid, Email, Password}).

-spec delete_user(string()) -> string().
delete_user(Hid) ->
    gen_server:call({global, ?MODULE}, {delete_user, Hid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = hn_db_admin:create_table(service_passport_user, 
                                  user, 
                                  record_info(fields, user),
                                  disc_copies,
                                  set,
                                  false,
                                  [email]),
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
handle_call({authenticate, Email, Password}, _From, State) ->
    PassMD5 = crypto:md5_mac(server_key(), Password),
    User = #user{email=Email, passMD5 = PassMD5, _='_'},
    F = fun() ->
                mnesia:match_object(service_passport_user, User, read)
        end,
    Ret = case mnesia:activity(async_dirty, F) of
              [#user{hid=Hid}] -> {ok, Hid};
              _Else            -> {error, invalid_user}
          end,
    {reply, Ret, State};

handle_call({get_or_create_user, Email}, _From, State) ->
    Ms = ets:fun2ms(fun(#user{email=E, hid=H}) when E == Email -> H end),
    T = fun() ->
                case mnesia:select(service_passport_user, Ms, write) of
                    [H] -> 
                        H;
                    _ -> 
                        User = #user{hid = create_hid(), email = Email},
                        mnesia:write(service_passport_user, User, write),
                        User#user.hid
                end
        end,
    Hid = mnesia:activity(async_dirty, T),
    {reply, Hid, State};

handle_call({create, Hid, Email, Password}, _From, State) ->
    Rec = #user{ hid = Hid,
                 email = Email,
                 passMD5 = crypto:md5_mac(server_key(), Password)},
    Fun = fun() -> mnesia:write(service_passport_user, Rec, write) end,
    Ret = mnesia:activity(async_dirty, Fun),
    {reply, Ret, State};

handle_call({delete, Hid}, _From, State) ->
    Ret = mnesia:activity(async_dirty, fun mnesia:delete/3, 
                          [service_passport_user, Hid, write]),
    {reply, Ret, State};    

handle_call(_Request, _From, State) ->
    Reply = ok,
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

-spec create_hid() -> string().
create_hid() ->
    Bin = crypto:rand_bytes(16),
    mochihex:to_hex(Bin).
    
-spec stamp(string(), string()) -> string(). 
stamp(Hid, Remember) ->
    Expiry = expires(Remember),
    Hash = gen_hash(Hid, Expiry),
    ?FORMAT("~s:~ts:~s", [Expiry, Hid, Hash]).

-spec encrypt_term_hex(iolist(), term()) -> string(). 
encrypt_term_hex(Key0, Term) ->
    PlainT = erlang:term_to_binary(Term),
    CipherT = encrypt_bin(Key0, PlainT),
    mochihex:to_hex(CipherT).

-spec decrypt_term_hex(iolist(), string()) -> term().
decrypt_term_hex(Key0, CipherH) ->
    CipherT = mochihex:to_bin(CipherH),
    PlainT = decrypt_bin(Key0, CipherT),
    erlang:binary_to_term(PlainT).

-spec gen_hash(string(), string()) -> string(). 
gen_hash(Hid, Expiry) ->
    mochihex:to_hex(crypto:md5_mac(server_token_key(), [Hid, Expiry])).

-spec is_expired(string()) -> boolean(). 
is_expired("session") ->
    false;
is_expired(TimeS) ->
    list_to_integer(TimeS) < unix_timestamp().

-spec expires(string()) -> string(). 
expires("true")  -> 
    integer_to_list(unix_timestamp() + 2678400);
expires("false") -> 
    "session".

-spec expire_days_from_now(integer()) -> datetime(). 
expire_days_from_now(Days) ->
    NowS = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ExpS = NowS + Days * 86400,
    calendar:gregorian_seconds_to_datetime(ExpS).

-spec encrypt_bin(iolist(), binary()) -> binary(). 
encrypt_bin(Key0, PlainT0) ->
    PlainT = extend(PlainT0),
    Key = crypto:md5_mac(server_key(), Key0),
    crypto:aes_cfb_128_encrypt(Key, ivector(), PlainT).

-spec decrypt_bin(iolist(), binary()) -> binary(). 
decrypt_bin(Key0, CipherT) when is_binary(CipherT) ->
    Key = crypto:md5_mac(server_key(), Key0),
    PlainT0 = crypto:aes_cfb_128_decrypt(Key, ivector(), CipherT),
    <<Len:16, PlainT:Len/binary, _/binary>> = PlainT0,
    PlainT.
    
-spec extend(binary()) -> binary().
extend(Bin) ->
    Len = size(Bin),
    Pad = 16 - ((Len+2) rem 16),
    <<Len:16, Bin/binary, 0:Pad/unit:8>>.

unix_timestamp() ->
    unix_timestamp(erlang:now()).
unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( 
      calendar:now_to_universal_time(Now)) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

unescapeUnicode(List) -> unesc_1(List, []).

unesc_1([], Acc) -> lists:reverse(Acc);
unesc_1([$%, $u, A, B, C, D | T], Acc) -> 
         {ok, [N], []} = io_lib:fread("~16u", [A, B, C, D]),
         unesc_1(T, [N | Acc]);
unesc_1([H | T], Acc) -> unesc_1(T, [H | Acc]).


%% We use a fixed initilization vector, therefore unique keys should
%% be used per recipient, and message type (varying the plaintext is
%% good too). In otherwords, the same IVector and Key combination
%% should not be used to send the same plaintext twice.
ivector() ->
    %% How I generated this.
    %% X = crypto:rand_uniform(round(math:pow(2,128)), round(math:pow(2,129)-1)).
    %% <<X:128>>.
    <<40,209,138,36,199,163,227,165,108,23,129,49,160,221,218,226>>.

%% These should be kept secret, and externalized for private installs.
server_key() ->
    <<"The road to Hades is paved with good intenti0ns">>.

server_token_key() ->
    <<"!Raibeart Bruis%">>.    

%%% 
%%% Tests
%%%
unit_test_() ->
    [fun test_encryption/0].

-spec test_encryption() -> no_return().
test_encryption() ->
    K = "silly",
    Msg = {"I think therefore I am", {1337, speak}, [["..."]]},
    ?assertEqual(Msg, decrypt_term_hex(K, encrypt_term_hex(K, Msg))).
