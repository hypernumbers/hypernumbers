%%% Factory is a hypernumbers service.
%%%
-module(factory).

%% API
-export([provision_site/3, provision_site/4,
         create_invite/3
        ]).

-include("auth.hrl").


provision_site(Zone, Email, SiteType) ->
    Uid = passport:create_uid(),
    provision_site_(Zone, Email, SiteType, Uid).
provision_site(Zone, Email, SiteType, [$_|Uid]) ->
    provision_site_(Zone, Email, SiteType, Uid);
provision_site(Zone, Email, SiteType, Uid) ->
    provision_site_(Zone, Email, SiteType, Uid).

-spec provision_site(string(), string(), atom(), uid()) 
                    -> {ok, new | existing, string(), uid(), string()} | 
                       {error, invalid_email}.
provision_site_(Zone, Email, Type, SuggestedUid) ->
    case valid_email(Email) of
        false ->
            {error, invalid_email};
        true -> 
            {ok, {Host, {_Ip, Port, Node}}} = hns:link_resource(Zone),
            {ok, NE, Uid} = passport:get_or_create_user(Email, SuggestedUid),
            Name = extract_name_from_email(Email),
            Site = lists:flatten(io_lib:format("http://~s:~b", [Host,Port])),
            ok = rpc:call(Node, hn_setup, site, 
                          [Site, Type, [{creator, Uid},
                                        {email, Email},
                                        {name, Name}]]),
            post_provision(NE, Site, Uid, Email, Name),
            {ok, NE, Site, Uid, Name}
    end.

create_invite(Site, Email, Group) ->
    case valid_email(Email) of
        false ->
            {error, invalid_email};
        true ->
            {Zone, SName} = extract_zone(Site),
            {ok, Node} = hns:lookup_node(Zone, SName),
            {ok, NE, Uid} = passport:get_or_create_user(Email),
            UName = extract_name_from_email(Email),
            ok = rpc:call(Node, hn_groups, add_user,
                          [Site, Group, Uid]),
            post_invite(NE, Site, Uid, Email, UName)
    end.

%% %% This will be needed for 'non-generated' zone deployments: ie. 'uses.hn'.
%%     -spec provision_site(string(), string(), atom(), string()) -> no_return().
%% provision_site(_Zone, _Email0, _SiteType, _CustomHost) ->
%%     throw(undefined),
%%     ok.



-spec valid_email(string()) -> boolean(). 
valid_email(Email) ->
    EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
        ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
        ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
        ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
        ++ "|biz|info|mobi|name|aero|jobs|museum)", %" for syntax highighting
    case re:run(Email, EMail_regex) of
        nomatch    -> false;
        {match, _} -> true
    end.

-spec post_provision(new | existing, string(), uid(), string(), string())
                    -> ok.
%% User does not have any existing sites, log them into their new site
%% directly.
post_provision(NE, Site, Uid, Email, Name) ->
    EmailBody = case NE of 
                    new -> new_user_site_email(Site, Uid, Email, Name);
                    existing -> existing_user_site_email(Site, Name)
                end,
    send_email(Email, EmailBody).

-spec post_invite(new | existing, string(), uid(), string(), string())
                 -> ok.
post_invite(NE, Site, Uid, Email, Name) ->
    EmailBody = case NE of
                    new -> new_user_invite_email(Site, Uid, Email, Name);
                    existing -> existing_user_invite_email(Site, Name)
                end,
    send_email(Email, EmailBody).

send_email(To, EmailBody) ->
    case application:get_env(hypernumbers, environment) of
        {ok, development} ->
            io:format("Email Body:~n~s~n--END EMAIL--~n",[EmailBody]);
        {ok, production}  ->
            spawn(hn_net_util, email, 
                  [To, 
                   "\"Hypernumbers Team\" <noreply@hypernumbers.com>",
                   "Your new site is live!", 
                   EmailBody]),
            ok
    end.    

-spec extract_zone(string()) -> {string(), string()}.
extract_zone("http://"++Site) ->
    {Name, [$.|ZoneP]} = lists:splitwith(fun(C) -> C /= $. end, Site),
    Zone = lists:takewhile(fun(C) -> C /= $: end, ZoneP),
    {Zone, Name}.

extract_name_from_email(Email) ->
    [Name | _Rest] =  string:tokens(Email, ".+@"),
    capitalize_name(Name).
    
capitalize_name([X|Rest]) -> [string:to_upper(X)|Rest].

new_user_site_email(Site, Uid, Email, Name) ->
    Path = ["_validate", Name],
    Data = [{emailed, true}],
    HT = passport:create_hypertag(Site, Path, Uid, Email, Data, "never"),
    lists:flatten(
      ["Hi ", Name, ",\n\n",
       "We hope you're having a fun time "
       "building your new site:\n\n ", 
       hn_util:strip80(Site), "\n\n"
       "We just need one more thing. Please click the following "
       "link to validate your site, it only takes a moment and allows "
       "you to set your password.\n\n",
       "Click or paste the following into your browser:\n\n",
       HT,"\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).

existing_user_site_email(Site, Name) ->
    lists:flatten(
      ["Hi ", Name, ",\n\n", 
       "Cool, we're glad you want another site! "
       "We've built it for you, and it's located here:\n\n ",
       hn_util:strip80(Site), "\n\n",
       "Just use your existing account to login.\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).

new_user_invite_email(Site, Uid, Email, Name) ->
    Path = ["_invite", Name],
    Data = [{emailed, true}],
    HT = passport:create_hypertag(Site, Path, Uid, Email, Data, "never"),
    lists:flatten(
      ["Hi ", Name, ",\n\n",
       "You have been invited to ", hn_util:strip80(Site), "\n\n",
       "Click or paste the following into your browser:\n\n",
       HT,"\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).

existing_user_invite_email(Site, Name) ->
    lists:flatten(
      ["Hi ", Name, ",\n\n", 
       "You have been invited to ", hn_util:strip80(Site), "\n\n",
       "You can use your existing account to login.\n\n"
       "Cheers,\n\n"
       "The Hypernumbers team."]).    
