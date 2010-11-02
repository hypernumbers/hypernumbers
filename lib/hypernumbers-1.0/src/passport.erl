%%% copyright 2010 Hypernumbers Ltd
%%% written by Tom McNulty

-module(passport).

-behaviour(gen_server).

-define(D2GS, calendar:datetime_to_gregorian_seconds).
-define(UT, calendar:universal_time).

%% API
-export([ start_link/0,
          create_hypertag/6,
          open_hypertag/3, 
          authenticate/3,
          inspect_stamp/1,
          temp_stamp/0,
          set_password/2,
          uid_to_email/1,
          email_to_uid/1,
          validate_uid/1,
          is_valid_uid/1,
          get_or_create_user/1,
          get_or_create_user/2,
          is_user/1,
          delete_uid/1,
          create_uid/0,
          dump_script/0,
          load_script/1,
          issue_pwd_reset/2,
          reset_pwd/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("hypernumbers.hrl").
-include("date.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WEEK_S, 604800).
-define(DAY_S, 86400).

-record(hypertag, {uid, email, expiry, data}).

-record(user, {uid,
               email,
               passMD5 = nil,
               validated = false,
               created_on = calendar:universal_time(),
               lastlogin_on = nil,
               data = dict:new()}).

-record(reset, {age = 0,
                hash = "",
                site = application:get_env(hypernumbers, norefer_url)}).

-record(state, {}).

%%%===================================================================
%%% Local API
%%%===================================================================

-spec create_hypertag(string(), 
                      [string()], 
                      auth_srv:uid(), string(), 
                      any(), 
                      integer() | string())
                     -> string().
create_hypertag(Site, Path, Uid, Email, Data, Age) ->
    HalfKey = [Site, Path],
    HT = #hypertag{uid = Uid, 
                   email = Email, 
                   expiry = gen_expiry(Age), 
                   data = Data},
    HTEnc = encrypt_term_hex(HalfKey, HT),
    lists:concat([Site, hn_util:list_to_path(Path), "?hypertag=", HTEnc]).

-spec open_hypertag(string(), [string()], string()) 
                   -> {ok, auth_srv:uid(), string(), any(), string(),
                       integer()} | {error, any()}.
open_hypertag(Site, Path, HTEnc) ->
    HalfKey = [Site, Path],
    case decrypt_term_hex(HalfKey, HTEnc) of
        #hypertag{expiry=E, uid=U, email=M, data=D} ->
            case is_expired(E) of
                false -> {ok, U, M, D, stamp(U, M, ?WEEK_S), ?WEEK_S};
                true -> {error, expired}
            end;
        _Else ->
            {error, bad_invite}
    end.

-spec temp_stamp() -> string().
temp_stamp() -> stamp([$_|create_uid()], "anonymous", "never").

-spec inspect_stamp(string()) -> {ok,auth_srv:uid(),string()} | {error,term()}. 
inspect_stamp(undefined) ->
    {error, no_stamp};
inspect_stamp(Stamp) ->
    case string:tokens(Stamp, "|") of
        [EscEmail, Uid, Expiry, Hash] ->
            case {is_expired(Expiry), 
                  gen_hash([Expiry, Uid, EscEmail]), 
                  Hash} of
                {false,X,X} -> {ok, Uid, unescape_email(EscEmail)};
                {true,_,_}  -> {error, bad_stamp}
            end;
        _Else ->
            {error, bad_stamp}
    end.

%%%===================================================================
%%% Global API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    global:unregister_name(?MODULE),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

-spec reset_pwd(string(), string(), string())-> {success, string()}
                                                    | {error, weak_password}
                                                    | {error, invalid_reset}
                                                    | {error, invalid_email}
                                                    | {error, expired_reset}.
reset_pwd(Email, Password, Hash) ->
    Msg = {reset_pwd, Email, Password, Hash},
    gen_server:call({global, ?MODULE}, Msg, 10000).
    
-spec issue_pwd_reset(string(), string()) -> ok.
issue_pwd_reset(Email, Site) ->
    Msg = {issue_pwd_reset, Email, Site},
    ok = gen_server:call({global, ?MODULE}, Msg, 10000).

-spec authenticate(string(), string(), boolean()) 
                  -> {error, term()} | 
                     {ok, auth_srv:uid(), string(), integer() | string()}.
authenticate(Email, Password, Remember) ->
    Msg = {authenticate, Email, Password},
    case gen_server:call({global, ?MODULE}, Msg, 10000) of
        {ok, Uid} -> 
            Age = case Remember of 
                         true -> ?WEEK_S;
                         false -> "session"
                  end,
            {ok, Uid, stamp(Uid, Email, Age), Age};
        Else -> 
            Else
    end.

-spec set_password(auth_srv:uid(), string()) -> ok | 
                                       {error, invalid_uid} | 
                                       {error, invalidated } |
                                       {error, weak_password}.
set_password(Uid, Password) ->
    Msg = {set_password, Uid, Password},
    gen_server:call({global, ?MODULE}, Msg).

%% Anonymous users have a _ symbol before their uid.
-spec uid_to_email(auth_srv:uid()) -> {ok, string()} |
                             {error, invalid_uid}.

%% Temporarily put back in to make logs works.
uid_to_email(anonymous) -> {ok, "anonymous"};
uid_to_email([$_|_]) -> {ok, "anonymous"};
uid_to_email(Uid) -> 
    gen_server:call({global, ?MODULE}, {uid_to_email, Uid}).

-spec email_to_uid(string()) -> {ok, string()} | {error, invalid_email}.
email_to_uid(Email) -> 
    gen_server:call({global, ?MODULE}, {email_to_uid, Email}).

-spec validate_uid(auth_srv:uid()) -> ok | {error, invalid_uid}. 
validate_uid(Uid) ->
    gen_server:call({global, ?MODULE}, {validate_uid, Uid}).

-spec is_valid_uid(auth_srv:uid()) -> {ok, boolean} | {error, invalid_uid}.
is_valid_uid(Uid) ->
    gen_server:call({global, ?MODULE}, {is_valid_uid, Uid}).

-spec get_or_create_user(string()) -> {ok, new | existing, string()}.
get_or_create_user(Email) -> 
    SuggestedUid = create_uid(),
    gen_server:call({global, ?MODULE}, {get_or_create_user,
                                        Email, SuggestedUid}).

-spec get_or_create_user(string(), auth_srv:uid()) ->
    {ok, new | existing, string()}.
get_or_create_user(Email, SuggestedUid) -> 
    gen_server:call({global, ?MODULE}, {get_or_create_user,
                                        Email, SuggestedUid}).

-spec is_user(string()) -> true | false.
is_user(Email) -> 
    gen_server:call({global, ?MODULE}, {is_user, Email}).

-spec delete_uid(string()) -> ok.
delete_uid(Uid) when is_list(Uid) ->
    gen_server:call({global, ?MODULE}, {delete_uid, Uid}).

-spec create_uid() -> auth_srv:uid().

create_uid() ->
    Bin = crypto:rand_bytes(16),
    mochihex:to_hex(Bin).

-spec load_script(list()) -> ok.
load_script(Terms) ->
    ok = gen_server:call({global, ?MODULE}, {load_script, Terms}).

-spec dump_script() -> string().
dump_script() ->
    {ok, Terms} = gen_server:call({global, ?MODULE}, dump_script),
    make_script_terms(Terms, []).

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
handle_call({reset_pwd, Email, Password, Hash}, _From, State) ->
    Ret = case acceptablepassword(Password) of
              true  -> reset_p1(Email, Password, Hash);
              false -> {error, weak_password}
          end,
    {reply, Ret, State};
    
handle_call({issue_pwd_reset, Email, Site}, _From, State) ->
    T = fun() ->
                [U] = mnesia:index_read(service_passport_user, Email,
                                        #user.email),
                Age = gen_expiry(?DAY_S),
                % this hash is just checked against
                Hash = mochihex:to_hex(crypto:rand_bytes(24)),
                Reset = #reset{age = Age, hash = Hash, site = Site},
                #user{data = Dict} = U,
                NewDict = dict:store(reset, Reset, Dict),
                ok = mnesia:write(service_passport_user,
                             U#user{data = NewDict},
                             write),
                Hash
        end,
    Hash = mnesia:activity(async_dirty, T),
    ok = emailer:send(reset, Email, "", Site, Hash),
    {reply, ok, State};            
        
handle_call({authenticate, Email, Password}, _From, State) ->
    PassMD5 = crypto:md5_mac(server_key(), Password),
    User = #user{email=Email, passMD5 = PassMD5, _='_'},
    F = fun() ->
                mnesia:match_object(service_passport_user, User, read)
        end,
    Ret = case mnesia:activity(async_dirty, F) of
              [#user{uid=Uid}] -> {ok, Uid};
              _Else            -> {error, authentication_failed}
          end,
    {reply, Ret, State};

handle_call({set_password, Uid, Password}, _From, State) ->
    Ret = case acceptablepassword(Password) of
              true  -> set_p1(Uid, Password);
              false -> {error, weak_password}
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

handle_call({validate_uid, Uid}, _From, State) ->
    F = fun() ->
                case mnesia:read(service_passport_user, Uid, write) of
                    [U] -> 
                        mnesia:write(service_passport_user,
                                     U#user{validated = true},
                                     write);
                    _ ->
                        {error, invalid_uid}
                end
        end,
    Ret = mnesia:activity(async_dirty, F),
    {reply, Ret, State};

handle_call({is_valid_uid, Uid}, _From, State) ->
    Ret = case mnesia:activity(async_dirty, fun mnesia:read/3, 
                               [service_passport_user, Uid, read]) of
              [U] -> {ok, U#user.validated}; 
              _   -> {error, invalid_uid}
          end,
    {reply, Ret, State};

handle_call({get_or_create_user, Email, SuggestedUid}, _From, State) ->
    Ms = ets:fun2ms(fun(#user{email=E, uid=U}) when E == Email -> U end),
    T = fun() ->
                case mnesia:select(service_passport_user, Ms, write) of
                    [U] -> 
                        {ok, existing, U};
                    _ ->
                        case mnesia:read(service_passport_user, SuggestedUid) 
                        of
                            [] ->
                                User = #user{uid = SuggestedUid, 
                                             email = Email},
                                mnesia:write(service_passport_user, 
                                             User, 
                                             write),
                                {ok, new, User#user.uid};
                            _ ->
                                {error, cannot_replace_existing_id}
                        end
                end
        end,
    Ret = mnesia:activity(async_dirty, T),
    {reply, Ret, State};

handle_call({is_user, Email}, _From, State) ->
    Ms = ets:fun2ms(fun(#user{email=E, uid=U}) when E == Email -> U end),
    T = fun() ->
                case mnesia:select(service_passport_user, Ms, write) of
                    [_U] -> true;
                    _   -> false
                end
        end,
    Ret = mnesia:activity(async_dirty, T),
    {reply, Ret, State};

handle_call({delete_uid, Uid}, _From, State) ->
    Ret = mnesia:activity(async_dirty, fun mnesia:delete/3, 
                          [service_passport_user, Uid, write]),
    {reply, Ret, State};    

handle_call({load_script, Terms}, _From, State) ->
    [ok = exec_script_term(T) || T <- Terms],
    {reply, ok, State};

handle_call(dump_script, _From, State) ->
    Terms = mnesia:activity(async_dirty, fun mnesia:foldl/3, 
                            [fun dump_term/2, [], service_passport_user]),
    {reply, {ok, Terms}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% fix from http://github.com/msantos/cerck
acceptablepassword(Password) ->
    case {(length(Password) > 8), has_nonalpha(Password)} of
        {true, true} -> true;
        _            -> false
    end.

has_nonalpha(String) ->
    Fun = fun(X, Acc) ->
                  case {Acc, is_alpha(X)} of
                      {true, _}  -> true;
                      {_, false} -> true;
                      _          -> Acc
                  end
          end,
    lists:foldl(Fun, false, string:to_lower(String)).

is_alpha(N) ->
    if N < 97  -> false;
       N > 122 -> false;
       true    -> true
    end.

reset_p1(Email, Pwd, Hash) ->
    T = fun() ->
                case mnesia:index_read(service_passport_user, Email,
                                       #user.email) of
                    []  -> {error, invalid_email};
                    [U] -> #user{data = Dict} = U,
                           case dict:find(reset, Dict) of
                               error   -> {error, reset_not_issued};
                               {ok, N} -> reset_p2(U, Pwd, Hash, N, Dict)
                           end
                end
        end,
    mnesia:activity(transaction, T).

% test that the hashes are the same
reset_p2(U, Password, Hash, Reset, Dict) ->
    #reset{age = A, hash = H, site = S} = Reset,
    Age2 = list_to_integer(A),
    Now = ?D2GS(?UT()),
    if
        (Hash =/= H) -> {error, invalid_reset};
        (Age2 < Now) -> {error, expired_reset};
        true         -> PwdMD5 = crypto:md5_mac(server_key(), Password),
                        D2 = dict:erase(reset, Dict),
                        U2 = U#user{validated = true, passMD5 = PwdMD5,
                                    data = D2},
                        mnesia:write(service_passport_user, 
                                     U2, write),
                        {success, S}
    end.

set_p1(Uid, Password) ->
    PassMD5 = crypto:md5_mac(server_key(), Password),
    T = fun() ->
                case mnesia:read(service_passport_user, Uid, write) of
                    [U] when not U#user.validated ->
                        {error, not_validated};
                    [U] -> 
                        mnesia:write(service_passport_user, 
                                     U#user{passMD5 = PassMD5}, 
                                     write),
                        ok;
                    _ ->
                        {error, invalid_uid}
                end
        end,
    mnesia:activity(async_dirty, T).

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

-spec stamp(auth_srv:uid(), string(), integer() | string()) -> string().
stamp(Uid, Email, Age) ->
    Expiry = gen_expiry(Age),
    EscEmail = escape_email(Email),
    Hash = gen_hash([Expiry, Uid, EscEmail]),
    ?FORMAT("~s|~s|~s|~s", [EscEmail, Uid, Expiry, Hash]).

-spec escape_email(string()) -> string(). 
escape_email(Email) ->
    [case S of 
         $@ -> $!;
         $+ -> $#;
         S  -> S 
     end || S <- Email].

-spec unescape_email(string()) -> string(). 
unescape_email(EscEmail) ->
    [case S of 
         $! -> $@;
         $# -> $+;
         S  -> S 
     end || S <- EscEmail].

-spec gen_expiry(integer() | string()) -> string().
gen_expiry(X) when X == "session"; X == "never" -> X;
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

-spec dump_term(#user{}, list()) -> list(). 
dump_term(U, Acc) ->
    [{add_user, [{uid, U#user.uid},
                 {email, U#user.email},
                 {pass, U#user.passMD5},
                 {validated, U#user.validated},
                 {created, U#user.created_on},
                 {lastlogin, U#user.lastlogin_on},
                 {data, U#user.data}]} | Acc].

-define(lget(Key, List), (element(2, lists:keyfind(Key, 1, List)))).
exec_script_term({add_user, T}) ->
    U = #user{uid = ?lget(uid, T),
              email = ?lget(email, T),
              passMD5 = ?lget(pass, T),
              validated = ?lget(validated, T),
              created_on = ?lget(created, T),
              lastlogin_on = ?lget(lastlogin, T),
              data = ?lget(data, T)},
    true = U#user.uid /= false,
    mnesia:activity(async_dirty, fun mnesia:write/3, 
                    [service_passport_user, U, write]).

-spec gen_hash([string()]) -> string(). 
gen_hash(Input) ->
    mochihex:to_hex(crypto:md5_mac(server_token_key(), Input)).

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

%% Extend binary to a multiple of 128 bits.
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
    %% X = crypto:rand_uniform(round(math:pow(2,128)), 
    %%                         round(math:pow(2,129)-1)),
    %% <<X:128>>.
    <<121,155,254,177,79,133,224,14,193,76,204,153,223,222,231,143>>.

%% These should be kept secret, and externalized for private installs.
server_key() ->
    <<"now I can ollie and I'm not so shite">>.

server_token_key() ->
    <<"her suntan starts just above the collar">>.    

make_script_terms([], Acc) -> 
    FirstLine = io_lib:format("~s~n",["%%-*-erlang-*-"]),
    lists:flatten([FirstLine | lists:reverse(Acc)]);
make_script_terms([H | T], Acc) ->
    NewAcc = lists:flatten(io_lib:format("~p.~n", [H])),
    make_script_terms(T, [NewAcc | Acc]).

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
    Email = "alice@example.com",
    "http://"++Url = create_hypertag(Site, Path, 
                                     "alice", Email, 
                                     {"123"}, "never"),
    {_, "?hypertag="++HyperTag} = httpd_util:split_path(Url),
    {ok, U, Email, D, _Stamp, _Age} = open_hypertag(Site, Path, HyperTag),
    ?assertEqual("alice", U),
    ?assertEqual({"123"}, D).
