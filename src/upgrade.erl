-module(upgrade).

-export([add_perms/0]).

%% adds the new perms for wiki pages to all sites
add_perms() ->
    Tables = mnesia:system_info(tables),
    DeleteList = [service_passport_user, service_hns_record, service_hns_zone,
                  service_hns_resource, core_site, schema],
    Fun = fun(X) ->
                  case lists:member(X, DeleteList) of
                      true  -> false;
                      false -> true
                  end
          end,
    SiteTables = lists:filter(Fun, Tables),
    io:format("SiteTables is ~p~n", [SiteTables]),
    Fun2 = fun(X) ->
                   [T, _, _] = string:tokens(atom_to_list(X), "&"),
                   T
           end,
    Sites = hslists:uniq([Fun2(X) || X <- SiteTables]),
    io:format("Sites is ~p~n", [Sites]),
    % {add_view, [{path, []},       {perms, ["admin"]}, {view, "wikipage"} ]}.
    % {add_view, [{path, ["[**]"]}, {perms, ["admin"]}, {view, "wikipage"} ]}.
    Fun3 = fun(X, _) ->
                   Site = "http://"++X++":80",
                   auth_srv:add_view(Site, [], ["admin"], "wikipage"),
                   auth_srv:add_view(Site, ["[**]"], ["admin"], "wikipage"),
                   []
           end,
    lists:foldl(Fun3, [], Sites).
    
