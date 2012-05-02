%%% Factory is a hypernumbers service.
%%%
-module(factory).

%% API
-export([provision_site/3,
         provision_site/4
         % create_invite/3,
         % create_invite/4
        ]).

provision_site(Zone, Email, SiteType) ->
    SuggestedUid = passport:create_uid(),
    provision_site(Zone, Email, SiteType, SuggestedUid).

%% Extract existing uid from anonymous users, otherwise, generate a
%% fresh uid.
provision_site(Zone, Email, SiteType, [$_|SuggestedUid]) ->
    provision_site_(Zone, Email, SiteType, SuggestedUid);
provision_site(Zone, Email, SiteType, _) ->
    SuggestedUid = passport:create_uid(),
    provision_site_(Zone, Email, SiteType, SuggestedUid).

-spec provision_site(string(), string(), atom(), auth_srv:uid())
                    -> {ok , new | existing, string(), atom(),
                        auth_srv:uid(), string()} | {error, invalid_email}.
provision_site_(Zone, Email, Type, SuggestedUid) ->
    case hn_util:valid_email(Email) of
        false ->
            {error, invalid_email};
        true ->
            {ok, {Host, {_Ip, Port, Node}}} = hns:link_resource(Zone),
            {ok, NE, Uid} = passport:get_or_create_user(Email, SuggestedUid),
            Name = hn_util:extract_name_from_email(Email),
            Site = lists:flatten(io_lib:format("http://~s:~b", [Host,Port])),
            {initial_view, IView} = rpc:call(Node, hn_setup, site,
                                            [Site, Type, [{creator, Uid},
                                                          {email, Email},
                                                          {name, Name}]]),
            post_provision(NE, Site, Uid, Email, Name),
            {ok, NE, Site, Node, Uid, Name, IView}
    end.

-spec post_provision(new | existing, string(), auth_srv:uid(),
                     string(), string()) -> ok.
%% User does not have any existing sites, log them into their new site
%% directly.
post_provision(NE, Site, Uid, Email, Name) ->
    IsValid = passport:is_valid_uid(Uid),
    {From, Sig} = emailer:get_details(Site),
    case {NE, IsValid} of
        {existing, true} ->
            emailer:send(new_site_existing, Email, "", From, Site,
                         [{sig, Sig}]);
        {_, _}           ->
            Path = ["_validate", Name],
            Data = [{emailed, true}],
            HT = passport:create_hypertag_url(Site, Path, Uid, Email,
                                              Data, "never"),
            emailer:send(new_site_validate, Email, "", From, Site,
                         [{sig, Sig}, {hypertag, HT}])
    end.
