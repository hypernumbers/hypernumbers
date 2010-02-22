%%% @author     Gordon Guthrie
%%% @copyright (C) 2010, Hypernumbers Ltd
%%% @doc       provides the RPC interface for the admin panel
%%%
%%% @end
%%% Created : 22 Feb 2010 by gordon@hypernumbers.com

-module(hn_web_admin).

%% RPC Api
-export([rpc/3]).

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
rpc(Site, Fn, Args) when is_list(Args) ->
    case Fn of
        add_view       -> [Site, Path, AuthSpec, View] = Args,
                          auth_srv2:add_view(Site, Path, AuthSpec, View);
        remove_views   -> [Site, Path, AuthSpec, View] = Args,
                          auth_srv2:add_view(Site, Path, AuthSpec, View);
        set_champion   -> [Site, Path, View] = Args,
                          auth_srv2:set_champion(Site, Path, View);
        set_challenger -> [Site, Path, View] = Args,
                          auth_srv2:set_champion(Site, Path, View);
        create         -> [Site, Name, Groups, Password] = Args,
                          hn_users:create(Site, Name, Groups, Password);
        delete         -> [Site, Name] = Args,
                          hn_users:delete(Site, Name);
        add_groups     -> [Site, Name, Groups] = Args,
                          hn_users:add_groups(Site, Name, Groups);
        remove_groups  -> [Site, Name, Groups] = Args,
                          hn_users:remove_groups(Site, Name, Groups)
    end.
        

