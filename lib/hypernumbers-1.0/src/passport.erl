-module(passport).

-behaviour(gen_server).

%%% Notes:
%%%
%%% Services like facebook, twitter etc which we may use to
%%% externalize authentication will have their own tables. This way users
%%% don't have to be transformed when new authentication methods are added.

%% API
-export([ start_link/0,
          create_hypertag/5,
          open_hypertag/3, 
          authenticate/3,
          inspect_stamp/1,
          uid_to_email/1,
          email_to_uid/1,
          get_or_create_user/1,
          create_user/3,
          delete_user/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hypernumbers.hrl").
-include("auth.hrl").
-include("date.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WEEK_S, 604800).
-define(DAY_S, 86400).

-record(hypertag, {uid, expiry, data}).

-record(user, {uid,
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

-spec create_hypertag(string(), [string()], uid(), any(), integer() | never)
                     -> string().
create_hypertag(Site, Path, Uid, Data, Age) ->
    HalfKey = [Site, Path],
    HT = #hypertag{uid = Uid, expiry = gen_expiry(Age), data = Data},
    HTEnc = encrypt_term_hex(HalfKey, HT),
    lists:concat([Site, hn_util:list_to_path(Path), "?hypertag=", HTEnc]).

-spec open_hypertag(string(), [string()], string()) 
                   -> {ok, uid(), any(), string(), integer()} |
                      {error, any()}.
open_hypertag(Site, Path, HTEnc) ->
    HalfKey = [Site, Path],
    case decrypt_term_hex(HalfKey, HTEnc) of
        #hypertag{expiry=E, uid=U, data=D} ->
            case is_expired(E) of
                false -> {ok, U, D, stamp(U, ?WEEK_S), ?WEEK_S};
                true -> {error, expired}
            end;
        _Else ->
            {error, bad_invite}
    end.

-spec authenticate(string(), string(), boolean()) 
                  -> {error, term()} | 
                     {ok, uid(), string(), integer() | session}.
authenticate(Email, Password, Remember) ->
    Msg = {authenticate, Email, Password},
    case gen_server:call({global, ?MODULE}, Msg, 10000) of
        {ok, Uid} -> 
            Age = case Remember of 
                         true -> ?WEEK_S;
                         false -> session
                  end,
            {ok, Uid, stamp(Uid, Age), Age};
        Else -> 
            Else
    end.

-spec inspect_stamp(string()) -> {ok, uid()} | {error, term()}. 
inspect_stamp(undefined) ->
    {error, no_stamp};
inspect_stamp(Stamp) ->
    [Expiry, Uid, Hash] = string:tokens(Stamp, "|"),
    case {is_expired(Expiry), gen_hash(Uid, Expiry), Hash} of
        {false,X,X} -> {ok, Uid};
        {true,_,_}  -> {error, bad_stamp}
    end.

-spec uid_to_email(uid()) -> {ok, anonymous | string()} | {error, invalid_uid}.
uid_to_email(anonymous) -> {ok,anonymous};
uid_to_email(Uid) -> 
    gen_server:call({global, ?MODULE}, {uid_to_email, Uid}).

-spec email_to_uid(string()) -> {ok, string()} | {error, invalid_email}.
email_to_uid(Email) -> 
    gen_server:call({global, ?MODULE}, {email_to_uid, Email}).

-spec get_or_create_user(string()) -> string().
get_or_create_user(Email) -> 
    gen_server:call({global, ?MODULE}, {get_or_create_user, Email}).
    
-spec create_user(string(), string(), string()) -> string().
create_user(Uid, Email, Password) when is_list(Uid) ->
    gen_server:call({global, ?MODULE}, {create_user, Uid, Email, Password}).

-spec delete_user(string()) -> string().
delete_user(Uid) when is_list(Uid) ->
    gen_server:call({global, ?MODULE}, {delete_user, Uid}).

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
              [#user{uid=Uid}] -> {ok, Uid};
              _Else            -> {error, invalid_uid}
          end,
    {reply, Ret, State};

handle_call({uid_to_email, Uid}, _From, State) ->
    Ret = case mnesia:activity(async_dirty, fun mnesia:read/3, 
                               [service_passport_user, Uid, read]) of
              [U] -> {ok, U#user.email}; 
              _   -> {error, invalid_uid}
          end,
    {reply, Ret, State};

handle_call({email_to_uid, Email}, _From, State) ->
    Ret = case mnesia:activity(async_dirty, fun mnesia:index_read/3, 
                               [service_passport_user, Email, #user.email]) of
              [U] -> {ok, U#user.uid}; 
              _   -> {error, invalid_email}
          end,
    {reply, Ret, State};

handle_call({get_or_create_user, Email}, _From, State) ->
    Ms = ets:fun2ms(fun(#user{email=E, uid=U}) when E == Email -> U end),
    T = fun() ->
                case mnesia:select(service_passport_user, Ms, write) of
                    [H] -> 
                        H;
                    _ -> 
                        User = #user{uid = create_uid(), email = Email,
                                     passMD5 = crypto:md5_mac(server_key(), "123")},
                        mnesia:write(service_passport_user, User, write),
                        User#user.uid
                end
        end,
    Uid = mnesia:activity(async_dirty, T),
    {reply, Uid, State};

handle_call({create, Uid, Email, Password}, _From, State) ->
    Rec = #user{ uid = Uid,
                 email = Email,
                 passMD5 = crypto:md5_mac(server_key(), Password)},
    Fun = fun() -> mnesia:write(service_passport_user, Rec, write) end,
    Ret = mnesia:activity(async_dirty, Fun),
    {reply, Ret, State};

handle_call({delete, Uid}, _From, State) ->
    Ret = mnesia:activity(async_dirty, fun mnesia:delete/3, 
                          [service_passport_user, Uid, write]),
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

-spec create_uid() -> uid().
create_uid() ->
    Bin = crypto:rand_bytes(16),
    mochihex:to_hex(Bin).

-spec stamp(uid(), integer() | session) -> string().
stamp(Uid, Age) ->
    Expiry = gen_expiry(Age),
    Hash = gen_hash(Uid, Expiry),
    ?FORMAT("~s|~ts|~s", [Expiry, Uid, Hash]).

-spec gen_expiry(integer() | session | never) -> string().
gen_expiry(never) -> "never";
gen_expiry(session) -> "session";
gen_expiry(Age) -> 
    integer_to_list(
      calendar:datetime_to_gregorian_seconds(
        calendar:universal_time()) + Age).

-spec is_expired(string()) -> boolean(). 
is_expired("never") -> false;
is_expired("session") -> false;
is_expired(Expiry) ->
    Exps = list_to_integer(Expiry),
    Now = calendar:datetime_to_gregorian_seconds(
            calendar:universal_time()),
    Exps =< Now.

-spec gen_hash(string(), string()) -> string(). 
gen_hash(Uid, Expiry) ->
    mochihex:to_hex(crypto:md5_mac(server_token_key(), [Uid, Expiry])).

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
    [fun test_encryption/0,
     fun test_hypertag/0].
    
-spec test_encryption() -> no_return().
test_encryption() ->
    K = "silly",
    Msg = {"I think therefore I am", {1337, speak}, [["..."]]},
    ?assertEqual(Msg, decrypt_term_hex(K, encrypt_term_hex(K, Msg))).

-spec test_hypertag() -> no_return().
test_hypertag() -> 
    Site = "http://example.com:1234",
    Path = ["_invite", "alice", "secret", "page"],
    "http://"++Url = create_hypertag(Site, Path, "alice", {"123"}, never),
    {_, "?hypertag="++HyperTag} = httpd_util:split_path(Url),
    {ok, U, D, _Stamp, _Age} = open_hypertag(Site, Path, HyperTag),
    ?assertEqual("alice", U),
    ?assertEqual({"123"}, D).
