-module(hmac_api_lib).

-include("hmac_api.hrl").
-include("spriki.hrl").
-include_lib("eunit/include/eunit.hrl").

-author("Hypernumbers Ltd <gordon@hypernumbers.com>").

-define(RFC2116_SP, "\x20").
-define(RFC2116_HT, "\t").

-compile(export_all).

%%% this library supports the hmac_sha api on both the client-side
%%% AND the server-side
%%%
%%% sign/5 is used client-side to sign a request
%%% - it returns an HTTPAuthorization header
%%%
%%% authorize_request/1 takes a mochiweb Request as an arguement
%%% and checks that the request matches the signature
%%%
%%% get_api_keypair/0 creates a pair of public/private keys
%%%
%%% THIS LIB DOESN'T IMPLEMENT THE AMAZON API IT ONLY IMPLEMENTS
%%% ENOUGH OF IT TO GENERATE A TEST SUITE.
%%%
%%% THE AMAZON API MUNGES HOSTNAME AND PATHS IN A CUSTOM WAY
%%% THIS IMPLEMENTATION DOESN'T
-export([
         authorize_request/2,
         sign/5,
         get_api_keypair/0
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% API                                                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authorize_request(Site, Req) ->
    Method      = Req:get(method),
    Path        = Req:get(path),
    Headers     = normalise(mochiweb_headers:to_list(Req:get(headers))),
    ContentMD5  = get_header(Headers, "content-md5"),
    ContentType = get_header(Headers, "content-type"),
    Date        = get_header(Headers, "date"),
    IncAuth     = get_header(Headers, "authorization"),
    {Schema, PublicKey, Sig} = breakout(IncAuth),
    % normally you would use the public key to look up the private key
    case new_db_api:read_api(Site, PublicKey) of
        []    -> "no key";
        [Rec] -> #api{privatekey = PrivateKey, urls = URLS} = Rec,
                 Signature = #hmac_signature{method = Method,
                                             contentmd5 = ContentMD5,
                                             contenttype = ContentType,
                                             date = Date,
                                             headers = Headers,
                                             resource = Path},
                 Signed = sign_data(PrivateKey, Signature),
                 {_, AuthHeader} = make_HTTPAuth_header(Signed),
                 case AuthHeader of
                     IncAuth -> {"match", Rec};
                     _       -> "no_match"
                 end
    end.

sign(PrivateKey, Method, URL, Headers, ContentType) ->
    Headers2 = normalise(Headers),
    ContentMD5 = get_header(Headers2, "content-md5"),
    Date = get_header(Headers2, "date"),
    Signature = #hmac_signature{method = Method,
                                contentmd5 = ContentMD5,
                                contenttype = ContentType,
                                date = Date,
                                headers = Headers,
                                resource = URL},
    SignedSig = sign_data(PrivateKey, Signature),
    make_HTTPAuth_header(SignedSig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

breakout(Header) ->
    [Schema, Tail] = string:tokens(Header, " "),
    [PublicKey, Signature] = string:tokens(Tail, ":"),
    {Schema, PublicKey, Signature}.

get_api_keypair() ->
    Public  = mochihex:to_hex(binary_to_list(crypto:strong_rand_bytes(16))),
    Private = mochihex:to_hex(binary_to_list(crypto:strong_rand_bytes(16))),
    {Public, Private}.

make_HTTPAuth_header(Signature) ->
    {"Authorization", ?schema ++ " "
     ++ ?publickey ++ ":" ++ Signature}.

make_signature_string(#hmac_signature{} = S) ->
    Date = get_date(S#hmac_signature.headers, S#hmac_signature.date),
    string:to_upper(atom_to_list(S#hmac_signature.method)) ++ "\n"
        ++ S#hmac_signature.contentmd5 ++ "\n"
        ++ S#hmac_signature.contenttype ++ "\n"
        ++ Date ++ "\n"
        ++ canonicalise_headers(S#hmac_signature.headers)
        ++ canonicalise_resource(S#hmac_signature.resource).

sign_data(PrivateKey, #hmac_signature{} = Signature) ->
    Str = make_signature_string(Signature),
    sign2(PrivateKey, Str).

% this fn is the entry point for a unit test which is why it is broken out...
% if yer encryption and utf8 and base45 doo-dahs don't work then
% yer Donald is well and truly Ducked so ye may as weel test it...
sign2(PrivateKey, Str) ->
    Sign = xmerl_ucs:to_utf8(Str),
    binary_to_list(base64:encode(crypto:sha_mac(PrivateKey, Sign))).

canonicalise_headers([]) -> "\n";
canonicalise_headers(List) when is_list(List) ->
    List2 = [{string:to_lower(K), V} || {K, V} <- lists:sort(List)],
    c_headers2(consolidate(List2, []), []).

c_headers2([], Acc)       -> string:join(Acc, "\n") ++ "\n";
c_headers2([{[?headerprefix | Rest], Key} | T], Acc) ->
    Hd = string:strip([?headerprefix | Rest]) ++ ":" ++ string:strip(Key),
    c_headers2(T, [Hd | Acc]);
c_headers2([_H | T], Acc) -> c_headers2(T, Acc).

consolidate([H | []], Acc) -> [H | Acc];
consolidate([{H, K1}, {H, K2} | Rest], Acc) ->
    consolidate([{H, join(K1, K2)} | Rest], Acc);
consolidate([{H1, K1}, {H2, K2} | Rest], Acc) ->
    consolidate([{rectify(H2), rectify(K2)} | Rest], [{H1, K1} | Acc]).

join(A, B) -> string:strip(A) ++ ";" ++ string:strip(B).

% removes line spacing as per RFC 2616 Section 4.2
rectify(String) ->
    Re = "[\x20* | \t*]+",
    re:replace(String, Re, " ", [{return, list}, global]).

canonicalise_resource("http://"  ++ Rest) -> c_res2(Rest);
canonicalise_resource("https://" ++ Rest) -> c_res2(Rest);
canonicalise_resource(X)                  -> c_res3(X).

c_res2(Rest) ->
    N = string:str(Rest, "/"),
    {_, Tail} = lists:split(N, Rest),
    c_res3("/" ++ Tail).

c_res3(Tail) ->
    URL = case string:str(Tail, "#") of
              0 -> Tail;
              N -> {U, _Anchor} = lists:split(N, Tail),
                   U
          end,
    U3 = case string:str(URL, "?") of
             0  -> URL;
             N2 -> {U2, Q} = lists:split(N2, URL),
                   U2 ++ canonicalise_query(Q)
    end,
    string:to_lower(U3).

canonicalise_query(List) ->
    List1 = string:to_lower(List),
    List2 = string:tokens(List1, "&"),
    string:join(lists:sort(List2), "&").

%% if there's a header date take it and ditch the date
get_date([], Date)            -> Date;
get_date([{K, _V} | T], Date) -> case string:to_lower(K) of
                                    ?dateheader -> [];
                                    _           ->  get_date(T, Date)
                                end.

normalise(List) -> norm2(List, []).

norm2([], Acc) -> Acc;
norm2([{K, V} | T], Acc) when is_atom(K) ->
    norm2(T, [{string:to_lower(atom_to_list(K)), V} | Acc]);
norm2([H | T], Acc) -> norm2(T, [H | Acc]).

get_header(Headers, Type) ->
    case lists:keyfind(Type, 1, Headers) of
        false   -> [];
        {_K, V} -> V
    end.
