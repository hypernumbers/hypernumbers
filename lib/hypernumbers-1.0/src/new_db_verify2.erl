%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       A module to verify the status
%%%            of a hypernumbers database and
%%%            make assertions about it...
%%%
%%% @end
%%% Created : 12 Oct 2011 by gordon@hypernumbers.com

-module(new_db_verify2).

-define(m, mnesia:table_info).

-include("spriki.hrl").

-record(ver,
        {
          relation = null,
          local_obj = null,
          item = null,
          form = null,
          include = null,
          timer = null,
          parents = [],
          children = [],
          infparents = [],
          rev_parents = [],
          rev_children = [],
          rev_infparents = [],
          type = null,
          valid_type = false,
          path = null,            % makes output more readable
          obj = null,             % ditto
          revidx = null,          % ditto
          valid_obj = false,
          valid_revidx = invalid,
          has_formula = false,
          formula = [],
          has_include = false,
          has_include_fn = false,
          has_zinf = false,
          zinf_path = null
         }).

% main api
-export([
         check/0
        ]).

% step-by-step api
% in order
-export([
         dump_tables/0
        ]).


check() ->
    {Dir, TermFile, ZinfFile} = dump_tables().

dump_tables() ->
    Dir = get_dir(),
    Stamp = dh_date:format("d_M_y_G_i_s"),
    TermFile = "verification." ++ Stamp ++ ".terms",
    ZinfFile = "zinf." ++ Stamp ++ ".terms",
    ok = dump_mnesia(Dir ++ TermFile),
    io:format("Mnesia tables all dumped...~n"),
    Sites = hn_setup:get_sites(),
    [zinf_srv:dump_to_file(X, Dir ++ ZinfFile) || X <- Sites],
    io:format("Zinfs all dumped...~n"),
    {Dir, TermFile, ZinfFile}.

%%%
%%% Internal functions
%%%

dump_mnesia(File) ->
    % first delete any old version of the file
    ok = delete_file(File),
    case file:open(File, [append]) of
        {ok, FileId} ->
            Tables = mnesia:system_info(tables),
            DB = make_db(Tables, []),
            Fun = fun({Site, Tables}) ->
                          io:format("Dumping ~p for ~p~n", [Tables, Site]),
                          Fun2 = fun(Record, Table) ->
                                         Rec2 = setelement(1, Record, Table),
                                         io:fwrite(FileId, "~p.~n", [Rec2]),
                                         Table
                                 end,
                          Fun3 = fun() ->
                                 [mnesia:foldl(Fun2, X, X) || X <- Tables]
                                 end,
                          mnesia:activity(async_dirty, Fun3)
                  end,
            lists:foreach(Fun, DB),
            file:close(FileId);
        _ -> error
    end.

get_dir() -> Dir = code:lib_dir(hypernumbers) ++ "/../../priv/verification/".

make_db([], Acc) -> lists:reverse(Acc);
make_db([H | T], Acc) ->
    H2 = atom_to_list(H),
    {Site,  Record} = case string:tokens(H2, "&") of
                          [S, _Port | [Rec]] -> {S, Rec};
                          [Table]          -> {Table, Table}
                      end,
    NewAcc = add_table(Site, Record, H, Acc),
    make_db(T, NewAcc).

% first ignore system tables
add_table(_Site, _Record, schema, Acc) -> Acc;
add_table(_Site, _Record, core_site, Acc) -> Acc;
add_table(_Site, _Record, service_hns_resource, Acc) -> Acc;
add_table(_Site, _Record, service_hns_record, Acc) -> Acc;
add_table(_Site, _Record, service_hns_zone, Acc) -> Acc;
add_table(_Site, _Record, service_passport_user, Acc) -> Acc;
% now ignore particular tables in a site database
add_table(_Site, "group", _, Acc) -> Acc;
add_table(_Site, "dirty_zinf", _, Acc) -> Acc;
add_table(_Site, "del_local", _, Acc) -> Acc;
add_table(_Site, "style", _, Acc) -> Acc;
add_table(_Site, "dirty_queue", _, Acc) -> Acc;
add_table(_Site, "kvstore", _, Acc) -> Acc;
add_table(_Site, "dirty_for_zinf", _, Acc) -> Acc;
add_table(_Site, "logging", _, Acc) -> Acc;
% finally do something useful
add_table(Site, _Record, Table, Acc) ->
    case lists:keyfind(Site, 1, Acc) of
        false     -> [{Site, [Table]} | Acc];
        {Site, V} -> NewV = [Table | V],
                     lists:keyreplace(Site, 1, Acc, {Site, NewV})
    end.

delete_file(File) ->
    case file:delete(File) of
        ok              -> ok; % file deleted
        {error, enoent} -> ok; % file doesn't exist
        Other           -> exit(Other)
    end.
