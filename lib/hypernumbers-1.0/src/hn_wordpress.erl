%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Hypernumbers Ltd trading as Vixo
%%% @doc       Handles WordPress single sign-on
%%%
%%% @end
%%% Created : 30 Jan 2013 by gordon@vixo.com

-module(hn_wordpress).

-include("spriki.hrl").
-include("hn_mochi.hrl").
-include("wordpress.hrl").

-export([
         get_logon/3,
         do_logon/4
         ]).

-define(DAY_S, 86400).

get_logon(#refX{} = _RefX, #env{mochi = Mochi, headers = Hdrs} = Env, Params) ->
    {url, SingonURL} = lists:keyfind(url, 1, Params),
    {key, Key} = lists:keyfind(key, 1, Params),
    IV = crypto:rand_uniform(round(math:pow(2, 128)),
                             round(math:pow(2, 129) -1)),
    IVector = <<IV:128>>,
    URL = hn_mochi:get_real_uri(Mochi),
    Qry = hn_mochi:get_real_params(Mochi),
    Return = URL ++ Qry,
    Timestamp = integer_to_list(util2:get_timestamp()),
    PlainT = Timestamp ++ Return,
    PT2 = passport:extend(list_to_binary(PlainT)),
    Hypertag = crypto:aes_cfb_128_encrypt(Key, IVector, PT2),
    NewURL = SingonURL ++ "wp-admin/admin-ajax.php?action=vixo_single_sign_on"
        ++ "&hypertag=" ++ binary_to_list(base64:encode(Hypertag))
        ++ "&ivector=" ++ binary_to_list(base64:encode(IVector)),
    Env#env{headers = [{"location", NewURL} | Hdrs]}.

do_logon(Site, Hypertag, IVector, Params) ->
    {key, Key} = lists:keyfind(key, 1, Params),
    Ht2 = re:replace(Hypertag, " ", "\\+", [global, {return, list}]),
    Ht3 = base64:decode(Ht2),
    IV2 = re:replace(IVector, " ", "\\+", [global, {return, list}]),
    IV3 = base64:decode(IV2),
    Text = crypto:aes_cfb_128_decrypt(Key, IV3, Ht3),
    Unpadded = passport:unpad(Text),
    Terms = bert:decode(Unpadded),
    % in WordPress a user can only be a member of one group...
    % this might change - we might pass capabilities so keep it
    % coming in as a list of 1...
    #signon{email = E, group = G, return_url = U, timestamp = TS} = Terms,
    Time = util2:get_timestamp(),
    Diff = abs(Time - (TS * 1000000)),
    if
        Diff <  ?DRIFT -> ok;
        Diff >= ?DRIFT -> exit("WordPress single signon probable replay attack."
                               " Logon terminated with extreme prejudice.")
    end,
    % PHP doesn't really do empty lists to G might be a list with a single
    % item or it might be an empty binary
    G3 = case G of
             <<>> -> G;
             [G2] -> G2
         end,
    % if we don't get an email, or we do get one but with no groups
    % then lets make the user an anonymous one - only able to see public page
    % should never be possible to get no email but a set of groups - so
    % lets make them anonymous too and see hoo the chips fly
    Stamp = case {E, G3} of
                {<<>>, <<>>} -> passport:temp_stamp();
                {<<>>, _}    -> passport:temp_stamp();
                {_, <<>>}    -> passport:temp_stamp();
                {_, _}       ->
                    Email = binary_to_list(E),
                    % don't care if they are new or existing
                    % gonnae synch their groups on logon every time...
                    {ok, _, UID} = passport:get_or_create_user(Email),
                    Groups = hn_groups:get_a_users_groups(Site, UID),
                    {Rem, Add} = split(binary_to_list(G3), Groups),
                    [ok = hn_groups:rem_user(Site, X, UID) || X <- Rem],
                    [ok = hn_groups:add_user(Site, X, UID) || X <- Add],
                    passport:stamp(UID, Email, ?DAY_S) % log on for today
            end,
    {Stamp, binary_to_list(U)}.

% we map the WordPress administrator group to admin
% also done in hn_groups
split("administrator", Groups) -> split("admin", Groups);
split(G, Groups)               -> case lists:member(G, Groups) of
                                      true  -> {lists:delete(G, Groups), []};
                                      false -> {Groups, [G]}
                                  end.
