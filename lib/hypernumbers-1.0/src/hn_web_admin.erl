%%% @author     Gordon Guthrie
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       provides the RPC interface for the admin panel
%%%
%%% @end
%%% Created : 22 Feb 2010 by gordon@hypernumbers.com

-module(hn_web_admin).

-include("spriki.hrl").
-include("hn_mochi.hrl").

%% RPC Api
-export([rpc/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% __          __               _              %
% \ \        / /              (_)             %
%  \ \  /\  / /__ _ _ __ _ __  _ _ __   __ _  %
%   \ \/  \/ // _` | '__| '_ \| | '_ \ / _` | %
%    \  /\  /| (_| | |  | | | | | | | | (_| | %
%     \/  \/  \__,_|_|  |_| |_|_|_| |_|\__, | %
%                                       __/ | %
%                                      |___/  %
%                                             %
% This module uses pattern matching to check  %
% that the rpc is being called against the    %
% same site as in the RefX passed in from     %
% hn_mochi                                    %
%                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rpc(UID, Site, Fn, Args) when is_list(Args) ->

    case Fn of

        "save_template" ->
            Path  = kfind("path", Args),
            Template = kfind("name", Args),
            Name  = ustring:pr(ustring:to_lower(ustring:new(Template))),
            RefX  = #refX{site = Site, type = url, path = Path,
                          obj= {page, "/"}},
            hn_templates:save_template(RefX, Name, UID);

        "set_view" ->
            Path            = kfind("path", Args),
            View            = kfind("view", Args),
            {array, Groups} = kfind("groups", Args),

            AuthSpec = case kfind("everyone", Args) of
                           true  -> [everyone | Groups];
                           false -> Groups
                       end,

            ok = auth_srv:set_view(Site, Path, AuthSpec, View),
            ok = remoting_reg:notify_refresh(Site, Path);

        "set_password" ->
            Password = kfind("password", Args),
            case passport:set_password(UID, Password) of
                ok              -> ok;
                {error, Reason} ->
                    Msg = io_lib:format("Error setting password ~p : ~p~n",
                                        [Args, Reason]),
                    error_logger:error_msg(Msg),
                    {error, "Error Setting Password"}
            end;

        "set_champion" ->
            Path            = kfind("path", Args),
            View            = kfind("view", Args),
            %% TODO : shouldn't hardcode
            auth_srv:add_view(Site, Path, ["admin"], View),
            auth_srv:set_champion(Site, Path, View);

        % you invite a user as a member of their personal group
        "invite_user" ->
            Email = kfind("email", Args),
            Args2 = [{"groups", {array, [Email]}} | Args],
            add_user2(UID, Site, Args2, invite);

        "add_user"    ->
            add_user2(UID, Site, Args, add);

        %"delete" ->
        %    [Site, Name] = Args,
        %    hn_users:delete(Site, Name);

        "add_group" ->
            Path           = kfind("path",  Args),
            Name           = kfind("group", Args),
            {array, Views} = kfind("views", Args),
            ok = hn_groups:create_group(Site, Name),
            % now fire off the permissions stuff...
            [ok = auth_srv:add_view(Site, Path, [Name], X) || X <- Views],
            % now tell the front end to update
            % twice - both the site and page!
            ok = remoting_reg:notify_refresh(Site, Path),
            ok = remoting_reg:notify_site(Site);

        % "remove_groups" ->
        %   [Site, Name, Groups] = Args,
        %   hn_users:remove_groups(Site, Name, Groups)

        "save_map" ->
            Name                = kfind("name", Args),
            {struct, Map}       = kfind("map", Args),
            {struct, Head}      = kfind("head", Map),
            {array, Validation} = kfind("validation", Map),
            {array, Mapping}    = kfind("mapping", Map),
            H2 = make_head(Head),
            V2 = clean_up(Validation, validation),
            M2 = clean_up(Mapping, mapping),
            hn_import:save_map(Site, Name, H2, V2, M2),
            ok;

        % curie's cases
        %% "read_user_fn" ->
        %%     Name = kfind("name", Args),
        %%     Site = kfind("site", Args),
        %%     curie:read_user_fn(Site, Name);

        %% "delete_user_fn" ->
        %%     Name = kfind("name", Args),
        %%     Site = kfind("site", Args),
        %%     curie:delete_user_fn(Site, Name);

        %% "write_user_fn"	->
        %%     Site					= kfind("site", Args),
        %%     Function_Name	= kfind("name", Args),
        %%     Page					= kfind("page", Args),
        %%     Function_Desc	= kfind("description", Args),
        %%     Output_Value	= kfind("output_value", Args),
        %%     Parameters		= kfind("parameters", Args),
        %%     curie:create_user_fn(Site, Function_Name, Page,
        %%                          Function_Desc, Output_Value,
        %%                          Parameters);

        Other ->
            {error, Other ++ " is not a valid administrative task"}
    end.

-spec add_user_to_groups(string(), string(), list()) -> ok.
add_user_to_groups(Site, UID, Groups) when is_list(Groups) ->
    [ok = add_utogs(Site, UID, X) || X <- Groups],
    ok.

add_utogs(Site, UID, Group) ->
    ok = hn_groups:create_group(Site, Group),
    ok = hn_groups:add_user(Site, Group, UID).

-spec kfind(string(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.

format_list(List) -> flist(List, []).

flist([], Acc)       -> lists:flatten(Acc);
flist([H | []], Acc) -> flist([], [H | Acc]);
flist([H | T], Acc)  -> flist(T, [", ", H | Acc]).

add_user2(UID, Site, Args, Type) ->
    P           = kfind("path",   Args),
    E           = kfind("email",  Args),
    M           = kfind("msg",    Args),
    {array, G}  = kfind("groups", Args),
    {ok, I}     = passport:uid_to_email(UID),
    {From, Sig} = emailer:get_details(Site),
    case hn_util:valid_email(E) of
        false ->
            Dets =[{person, E},
                   {details, "become a member of the groups "++
                    format_list(G)},
                   {reason, " the email address is invalid"},
                   {sig, Sig}],
            emailer:send(invalid_invite, I, "", From, Site, Dets);
        true ->
            {ok, NE, InvitedUID} = passport:get_or_create_user(E),
            % now add the users to the groups
            ok = add_user_to_groups(Site, InvitedUID, G),
            % only add a view if it is an invite
            if (Type == invite) ->
                    case kfind("view", Args) of
                        none -> ok;
                        V    -> ok = auth_srv:add_view(Site, P, G, V)
                    end;
               (Type == add ) ->
                    ok % do nothing
            end,
            case {NE, passport:is_valid_uid(InvitedUID)} of
                {existing, true} ->
                    Dets = [{invitee, I}, {path, P}, {msg, M}, {sig, Sig}],
                    emailer:send(invite_existing, E, I, From, Site, Dets);
                {_, _} ->
                    Vanity = hn_util:extract_name_from_email(E),
                    HtP = ["_validate", Vanity],
                    P2 = hn_util:list_to_path(P),
                    Data = [{emailed, true}, {redirect, P2}],
                    HT = passport:create_hypertag_url(Site, HtP, InvitedUID, E,
                                                      Data, "never"),
                    Dets = [{invitee, I}, {msg, M},
                            {hypertag, HT}, {sig, Sig}],
                    emailer:send(invite_new, E, I, From, Site, Dets)
            end,
            % tell the front end to update both site and page
            ok = remoting_reg:notify_refresh(Site, P),
            ok = remoting_reg:notify_site(Site)
    end.

clean_up(List, Name) -> cl2(List, Name, []).

cl2([], _, Acc) -> lists:reverse(Acc);
cl2([{struct, List} | T], validation, Acc) ->
           Sheet = kfind("sheet", List),
           Cell  = kfind("cell", List),
           Constraint = kfind("constraint", List),
           cl2(T, validation, [#validation{sheet = Sheet, cell = Cell,
                                           constraint = Constraint} | Acc]);
cl2([{struct, List} | T], mapping, Acc) ->
           Sheet = kfind("sheet", List),
           From  = kfind("from", List),
           To    = kfind("to", List),
           cl2(T, mapping, [#mapping{sheet = Sheet,
                                     from = From, to = To} | Acc]).
make_head(Head) ->
    Type      = kfind("type", Head),
    FileType  = kfind("filetype", Head),
    Template  = kfind("template", Head),
    Overwrite = kfind("overwrite", Head),
    #head{type = Type, filetype = FileType, template = Template, overwrite = Overwrite}.
