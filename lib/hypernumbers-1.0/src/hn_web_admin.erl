%%% @author     Gordon Guthrie
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       provides the RPC interface for the admin panel
%%%
%%% @end
%%% Created : 22 Feb 2010 by gordon@hypernumbers.com

-module(hn_web_admin).

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
        
        "set_view" ->
            
            Path            = kfind("path", Args),
            View            = kfind("view", Args),
            {array, Groups} = kfind("groups", Args),
            
            AuthSpec = case kfind("everyone", Args) of
                           true  -> [everyone | Groups];
                           false -> Groups
                       end,
            
            NPath = string:tokens(Path, "/"),
            auth_srv:set_view(Site, NPath, AuthSpec, View);
        
        "set_password" ->
            Password = kfind("password", Args),
            case passport:set_password(User, Password) of
                ok               -> ok;
                {error, _Reason} -> {error, "Error Setting Password"}
            end;
        
        "set_champion" ->
            {"path", Path} = lists:keyfind("path", 1, Args),
            {"view", View} = lists:keyfind("view", 1, Args),
            NPath = string:tokens(Path, "/"),
            %% TODO : shouldnt hardcode 
            auth_srv:add_view(Site, NPath, ["admin"], View),
            auth_srv:set_champion(Site, NPath, View);
        "add_user" ->
            {"user", Name} = lists:keyfind("user", 1, Args),
            {"pass", Pass} = lists:keyfind("pass", 1, Args),
            hn_users:create(Site, Name, [], Pass);        
        delete ->
            [Site, Name] = Args,
            hn_users:delete(Site, Name);
        add_groups ->
            [Site, Name, Groups] = Args,
            hn_users:add_groups(Site, Name, Groups);
        remove_groups ->
            [Site, Name, Groups] = Args,
            hn_users:remove_groups(Site, Name, Groups)
    end.


-spec kfind(string(), list()) -> any().
kfind(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.
