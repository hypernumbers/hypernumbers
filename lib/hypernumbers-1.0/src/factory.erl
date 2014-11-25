%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011 - 2014, Hypernumbers.com
%%% @doc       provisions Hypernumbers websites
%%% @end
%%% Created :  by gordon@hypernumbers.com
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(factory).

%% API
-export([provision_site/7
        ]).

%% Extract existing uid from anonymous users, otherwise, generate a
%% fresh uid.
provision_site(Zone, Email, From, Sig, SiteType, [$_|SuggestedUid], Data) ->
    provision_site_(Zone, Email, From, Sig, SiteType, SuggestedUid, Data);
provision_site(Zone, Email, From, Sig, SiteType, _, Data) ->
    SuggestedUid = passport:create_uid(),
    provision_site_(Zone, Email, From, Sig, SiteType, SuggestedUid, Data).

-spec provision_site(string(), string(), string(), string(), atom(),
                     auth_srv:uid(), list()) -> {ok , new | existing, string(),
                                                 atom(),
                                                 auth_srv:uid(), string()}
                                                    | {error, invalid_email}.
provision_site_(Zone, Email, From, Sig, Type, SuggestedUid, Data) ->
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
            post_provision(NE, Site, Uid, Email, From, Sig, Name, Data),
            {ok, NE, Site, Node, Uid, Name, IView}
    end.

-spec post_provision(new | existing, string(), string(), string(), auth_srv:uid(),
                     string(), string(), list()) -> ok.
%% User does not have any existing sites, log them into their new site
%% directly.
post_provision(NE, Site, Uid, Email, From, Sig, Name, UserData) ->
    IsValid = passport:is_valid_uid(Uid),
    Recs = make_recs(UserData, Site, []),
    ok = new_db_api:write_attributes(Recs),
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

make_recs([], _Site, Acc) ->
    lists:reverse(Acc);
make_recs([{Ref, Val} | T], Site, Acc) ->
    URL = case Ref of
              "/" ++ _Rest -> Site ++ Ref;
              _            -> Site ++ "/" ++ Ref
          end,
    RefX = hn_util:url_to_refX(URL),
    make_recs(T, Site, [{RefX, [{"formula", Val}]} | Acc]).
