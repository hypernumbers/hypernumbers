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
rpc(User, Site, Fn, Args) when is_list(Args) ->

    case Fn of

        "save_template" ->
            Path  = kfind("path", Args),
            Name  = kfind("name", Args),
            NPath = string:tokens(Path, "/"),
            RefX  = #refX{site = Site, path = NPath, obj={page, "/"}},
            hn_templates:save_template(RefX, Name);

        "set_view" ->
            Path            = kfind("path", Args),
            View            = kfind("view", Args),
            {array, Groups} = kfind("groups", Args),

            AuthSpec = case kfind("everyone", Args) of
                           true  -> [everyone | Groups];
                           false -> Groups
                       end,

            NPath = string:tokens(Path, "/"),
            auth_srv:set_view(Site, NPath, AuthSpec, View),
            ok = remoting_reg:notify_refresh(Site, NPath);

        "set_password" ->
            Password = kfind("password", Args),
            case passport:set_password(User, Password) of
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
            NPath           = string:tokens(Path, "/"),
            %% TODO : shouldnt hardcode
            auth_srv:add_view(Site, NPath, ["admin"], View),
            auth_srv:set_champion(Site, NPath, View);

        "invite_user" -> add_user2(User, Site, Args, invite);

        "add_user"    -> add_user2(User, Site, Args, add);

        %"delete" ->
        %    [Site, Name] = Args,
        %    hn_users:delete(Site, Name);

        "add_groups" ->
            Path           = kfind("path", Args),
            Name           = kfind("name", Args),
            {array, Views} = kfind("views", Args),
            NPath = string:tokens(Path, "/"),
            ok = hn_groups:create_group(Site, Name),
            % now fire off the permissions stuff...
            [ok = auth_srv:add_view(Site, NPath, [Name], X) || X <- Views],
            % now tell the front end to update
            % twice - both the site and page!
            ok = remoting_reg:notify_refresh(Site, NPath),
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
            
        %~ curie's cases
        "read_user_fn"		->
			Name	= kfind("name", Args),
			Site	= kfind("site", Args),
			curie:read_user_fn(Site, Name);
			
		"delete_user_fn"	->
			Name	= kfind("name", Args),
			Site	= kfind("site", Args),
			curie:delete_user_fn(Site, Name);
			
		"write_user_fn"		->
			Site					= kfind("site", Args),
			Function_Name			= kfind("name", Args),
			Page					= kfind("page", Args),
			Function_Description	= kfind("description", Args),
			Output_Value			= kfind("output_value", Args),
			Parameters				= kfind("parameters", Args),
			curie:create_user_fn(Site, Function_Name, Page, Function_Description, Output_Value, Parameters);

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

add_user2(User, Site, Args, Type) ->
    P          = kfind("path", Args),
    E          = kfind("email", Args),
    M          = kfind("msg", Args),
    {array, G} = kfind("groups", Args),
    {ok, I}    = passport:uid_to_email(User),
    NPath = string:tokens(P, "/"),
    case hn_util:valid_email(E) of
        false -> Dets =[{person, E},
                        {details, "become a member of the groups "++
                         format_list(G)},
                        {reason, " the email address is invalid"}],
                 emailer:send(invalid_invite, I, "", Site, Dets);
        true ->
            {ok, NE, UID} = passport:get_or_create_user(E),
            % now add the users to the groups
            ok = add_user_to_groups(Site, UID, G),
            % only add a view if it is an invite
            if (Type == invite) -> V = kfind("view", Args),
                                   ok = auth_srv:add_view(Site, NPath, G, V);
               (Type == add )   -> ok % do nothing
            end,
            case {NE, passport:is_valid_uid(UID)} of
                {existing, true} ->
                    Dets = [{invitee, I}, {path, P}, {msg, M}],
                    emailer:send(invite_existing, E, I, Site, Dets);
                {_, _} ->
                    Vanity = hn_util:extract_name_from_email(E),
                    HtP = ["_validate", Vanity],
                    Data = [{emailed, true},{redirect, P}],
                    HT = passport:create_hypertag(Site, HtP, UID, E,
                                                  Data, "never"),
                    Dets = [{invitee, I}, {msg, M},
                            {hypertag, HT}],
                    emailer:send(invite_new, E, I, Site, Dets)
            end,
            ok = remoting_reg:notify_refresh(Site, NPath)
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
