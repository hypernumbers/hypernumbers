%%% @author    Gordon Guthrie
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc       A module to verify the status
%%%            of a hypernumbers datastore and
%%%            make assertions about it...
%%%
%%% @end
%%% Created : 12 Oct 2011 by gordon@hypernumbers.com

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
         check/0,
         check_server/1
        ]).

% step-by-step api
% in order
-export([
         dump_tables/1,
         read_verification/1,
         summarise_problems/1
        ]).

check() ->
    Stamp = dh_date:format("d_M_y_G_i_s"),
    ok = dump_tables(Stamp),
    ok = read_verification(Stamp),
    ok = summarise_problems(Stamp).

check_server(Stamp) ->
    ok = read_verification(Stamp),
    ok = summarise_problems(Stamp).

dump_tables(Stamp) ->
    Dir = get_dir(),
    TermFile = "verification." ++ Stamp ++ ".terms",
    ZinfFile = "zinf." ++ Stamp ++ ".terms",
    ok = dump_mnesia(Dir ++ TermFile),
    io:format("Mnesia tables all dumped...~n"),
    Sites = hn_setup:get_sites(),
    [zinf_srv:dump_to_file(X, Dir ++ ZinfFile) || X <- Sites],
    io:format("Zinfs all dumped...~n"),
    ok.

read_verification(Stamp) ->
    Dir = get_dir(),
    VerFile = "verification." ++ Stamp ++ ".terms",
    ZinfFile = "zinf." ++ Stamp ++ ".terms",
    {ok, Data}  = parse(Dir ++ VerFile),
    io:format("in verification, data parsed...~n"),
    {ok, Zinfs} = case file:consult(Dir ++ ZinfFile) of
                      {error, enoent} ->
                          io:format("No zinfs written, assuming empty~n"),
                          {ok, []};
                      {ok, Zs} ->
                          {ok, Zs}
                  end,
    io:format("in verification, zinfs parsed...~n"),
    garbage_collect(self()),
    ok = write_dets(Data),
    garbage_collect(self()),
    io:format("all data written to dets...~n"),
    ok = process_zinfs(Zinfs),
    io:format("in verification, zinfs processed...~n"),
    FileName = "errors."  ++ Stamp ++ ".terms",
    delete_file(Dir ++ FileName),
    Errors = process_data(Dir, FileName),
    FileName2 = "fixable_errors." ++ Stamp ++ ".terms",
    file:delete(Dir ++ FileName2),
    hn_util:log_terms(Errors, Dir ++ FileName2),
    ok.

summarise_problems(Stamp) ->
    Dir = get_dir(),
    FixableFile = "fixable_errors." ++ Stamp ++ ".terms",
    {ok, [{Data1, Data2}]} = file:consult(Dir ++ FixableFile),
    io:format("Problems loaded~n"),
    [summarise(X, Site, Data2, []) || {Site, X} <- Data1],
    ok.

%%%
%%% Internal functions
%%%
summarise([], Site, Data2, Acc) ->
    io:format("~n~p:~n", [Site]),
    print_summary(Acc),
    case lists:keyfind(Site, 1, Data2) of
        false        -> exit("revidx has gone tits up");
        {Site, List} -> Len = length(List),
                        case Len of
                            0 ->
                                ok;
                            N ->
                                io:format("~p multiple local_objs~n", [N])
                        end
    end;
summarise([{_Idx, Type} | T], Site, Data2, Acc) ->
    NewAcc = case lists:keyfind(Type, 1, Acc) of
                 false     -> [{Type, 1} | Acc];
                 {Type, N} -> lists:keyreplace(Type, 1, Acc, {Type, N + 1})
             end,
    summarise(T, Site, Data2, NewAcc).

print_summary([])                  -> ok;
print_summary([{Type, Count} | T]) -> io:format("~p " ++ Type ++ "~n", [Count]),
                                      print_summary(T).

dump_mnesia(File) ->
    % first delete any old version of the file
    ok = delete_file(File),
    case file:open(File, [append]) of
        {ok, FileId} ->
            Tables = mnesia:system_info(tables),
            DB = make_db(Tables, []),
            Fun = fun({Site, Tabs}) ->
                          io:format("Dumping ~p for ~p~n", [Tabs, Site]),
                          Fun2 = fun(Record, Table) ->
                                         Rec2 = setelement(1, Record, Table),
                                         io:fwrite(FileId, "~p.~n", [Rec2]),
                                         Table
                                 end,
                          Fun3 = fun() ->
                                 [mnesia:foldl(Fun2, X, X) || X <- Tabs]
                                 end,
                          mnesia:activity(async_dirty, Fun3)
                  end,
            lists:foreach(Fun, DB),
            file:close(FileId);
        _ -> error
    end.

get_dets_tables() ->
    Dir = get_dets_dir(),
    Tables = [filename:basename(X) || X <- filelib:wildcard(Dir ++ "*"),
                                        filename:extension(X) =/= ".rev"],
    [{X, X ++ ".rev"} || X <- Tables].

get_dir() -> code:lib_dir(hypernumbers) ++ "/../../priv/verification/".

get_dets_dir() -> get_dir() ++ "dets/".

get_dets_file(Site) -> get_dets_dir() ++ Site.

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

process_data(Dir, FileName) ->
    _Return = filelib:ensure_dir(Dir ++ FileName),
    case file:open(Dir ++ FileName, [append]) of
        {ok, Id} ->
            Tables = get_dets_tables(),
            Return = verify(Tables, Id, {[], []}),
            file:close(Id),
            Return;
        _ ->
            error
    end.

verify([], _, Acc) -> Acc;
verify([{Table, RevTable} | T], FileId, {Acc1, Acc2}) ->
    io:format("Verifying: ~p: ", [Table]),
    Fun1 = fun(X) ->
                  verify2(Table, X, FileId)
          end,
    Broken_Idxs = dets:traverse(get_dets_dir() ++ Table, Fun1),
    io:format("Borked Idxs..."),
    Fun2 = fun(X) ->
                   verify3(Table, X, FileId)
           end,
    Broken_Revs = dets:traverse(get_dets_dir() ++ RevTable, Fun2),
    io:format("Borked RevIdxs...~n"),
    verify(T, FileId, {[{Table, lists:flatten(Broken_Idxs)} | Acc1],
                       [{Table, lists:flatten(Broken_Revs)} | Acc2]}).

verify2(Site, Rec, FileId) ->
    Acc1  = verify_tables(Site, FileId, Rec, []),
    Acc2  = verify_relations(Site, FileId, Rec, Acc1),
    Acc3  = verify_includes(Site, FileId, Rec, Acc2),
    Acc4  = verify_timers(Site, FileId, Rec, Acc3),
    Acc5  = verify_forms(Site, FileId, Rec, Acc4),
    Acc6  = verify_formula(Site, FileId, Rec, Acc5),
    Acc7  = verify_objs(Site, FileId, Rec, Acc6),
    Acc8  = verify_types(Site, FileId, Rec, Acc7),
    Acc9  = verify_grid(Site, FileId, Rec, Acc8),
    Acc10 = verify_revidxs(Site, FileId, Rec, Acc9),
    Acc11 = verify_zinfs(Site, FileId, Rec, Acc10),
    case Acc11 of
        [] -> continue;
        _  -> {continue, Acc11}
    end.

verify3(Site, Rec, FileId) ->
    case Rec of
        {_Path, [_Idx]} ->
            continue;
        {Path, List}    ->
            io:fwrite(FileId, "multiple local objs for ~p ~p~n~p~n",
                      [Site, Path, List]),
            {continue, {Path, List}}
    end.

% an entry in a local table has no formula, or parents could be a row, col
% page
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = null,
                                          relation = null,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          has_formula = false,
                                          obj = {{Type, _}, _},
                                          parents = [],
                                          infparents = [],
                                          rev_parents = []}}, Acc)
  when Type == row
       orelse Type == column
       orelse Type == page->
    Acc;
% a cell with no formula matches this - it has an item but no relations
% or formulae
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = null,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          has_formula = false,
                                          obj = {{cell, _}, _},
                                          parents = [],
                                          infparents = [],
                                          rev_parents = []}}, Acc) ->
    Acc;
% a normal cell with a formula (check the formula stuff elsewhere)
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          has_formula = true}}, Acc) ->
    Acc;
% a cell with a form element in it
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = exists,
                                          include = null,
                                          timer = null,
                                          has_formula = true}}, Acc) ->
    Acc;
% another cell with a form element in it - some form elements
% like map.sheet.button also bring an include through
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = exists,
                                          include = exists,
                                          timer = null,
                                          has_formula = true}}, Acc) ->
    Acc;
% a cell with an include in it
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = null,
                                          include = exists,
                                          timer = null,
                                          has_formula = true}}, Acc) ->
    Acc;
% a cell with a timer in it
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = null,
                                          include = null,
                                          timer = exists,
                                          has_formula = true}}, Acc) ->
    Acc;
% a cell that is refered to by a formula in another cell but which has
% no content - can't have no children
verify_tables(Site, FileId, {Idx, #ver{local_obj = exists,
                                       item = null,
                                       relation = exists,
                                       form = null,
                                       include = null,
                                       timer = null,
                                       children = C,
                                       parents = [],
                                       infparents = [],
                                       has_formula = false,
                                       has_zinf = false} = V}, Acc) ->
    case C of
        [] -> Str = "Invalid tables (type 1)",
              dump(Site, FileId, Str, [Idx, V]),
              [{Idx, Str} | Acc];
        _  -> Acc
    end;
% a cell that had some content and a style but which had its contents cleared
% the contents clearer doesn't clear the relation table
verify_tables(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                          item = exists,
                                          relation = exists,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          parents = [],
                                          infparents = [],
                                          has_formula = false}}, Acc) ->
    Acc;
% a zquery
verify_tables(_Site, _FileId, {_Idx, #ver{relation = null,
                                          local_obj = exists,
                                          item = null,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          type = gurl,
                                          valid_type = true,
                                          obj = {{cell, _}, _}}}, Acc) ->
    Acc;
% rows and columns
verify_tables(_Site, _FileId, {_Idx, #ver{relation = null,
                                          local_obj = exists,
                                          item = exists,
                                          form = null,
                                          include = null,
                                          timer = null,
                                          type = gurl,
                                          valid_type = true,
                                          obj = {{Type, _}, _}}}, Acc)
  when Type == row orelse Type == column ->
    Acc;
% a range
% our old friend handing refXs created by the function
% hnfuns_web:table/1
verify_tables(Site, FileId, {Idx, #ver{relation = null,
                                       local_obj = exists,
                                       item = null,
                                       form = null,
                                       include = null,
                                       timer = null,
                                       type = url,
                                       valid_type = true,
                                       obj = {{Type, _}, _}} = V}, Acc)
  when Type == range ->
    Str = "Invalid tables (type 2) (don't fix)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
% the old page add in for js/css
verify_tables(Site, FileId, {Idx, #ver{relation = null,
                                       local_obj = exists,
                                       item = exists,
                                       form = null,
                                       include = null,
                                       timer = null,
                                       type = url,
                                       valid_type = true,
                                       obj = {{page, _}, _}} = V}, Acc) ->
    Str = "Invalid tables (type 3) (old adding in css/js)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_tables(Site, FileId, {Idx, #ver{local_obj = exists, type = url} = V}, Acc) ->
    Str = "Invalid tables (type 4)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_tables(Site, FileId, {Idx, #ver{local_obj = exists, type = gurl} = V}, Acc) ->
    Str = "Invalid tables (type 5)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_tables(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

verify_relations(Site, FileId, {Idx, #ver{relation = exists,
                                          local_obj = exists,
                                          item = exists,
                                          children = C,
                                          parents = P,
                                          infparents = I,
                                          rev_children = RC,
                                          rev_parents = RP,
                                          rev_infparents = RIP} = V}, Acc) ->
    case {same(C, RC), same(P, RP), same(I, RIP)} of
        {true, true, true} ->
            Acc;
        _ ->
            Str = "Invalid relations (type 1)",
            dump(Site, FileId, Str, [Idx, V]),
            [{Idx, Str} | Acc]
    end;
verify_relations(Site, FileId, {Idx, #ver{relation = exists,
                                          local_obj = exists,
                                          item = null,
                                          children = C,
                                          parents = [],
                                          infparents = [],
                                          rev_children = RC,
                                          rev_parents = []} = V}, Acc) ->
    case same(C, RC) of
        true ->
            Acc;
        _ ->
            Str = "Invalid relations (type 2)",
            dump(Site, FileId, Str, [Idx, V]),
            [{Idx, Str} | Acc]
    end;
verify_relations(Site, FileId, {Idx, #ver{relation = exists} = V}, Acc) ->
    Str = "Invalid relations (type 3)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_relations(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

verify_includes(_Site, _FileId, {_Idx, #ver{relation = exists,
                                            local_obj = exists,
                                            item = exists,
                                            include = exists,
                                            has_include = true,
                                            obj = {{cell, _}, _},
                                            has_formula = true}}, Acc) ->
    Acc;
verify_includes(Site, FileId, {Idx, #ver{include = exists,
                                        has_include = false} = V}, Acc) ->
    Str = "Invalid include (type 1)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_includes(Site, FileId, {Idx, #ver{include = false,
                                         has_include = true} = V}, Acc) ->
    Str = "Invalid include (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_includes(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

verify_timers(_Site, _FileId, {_Idx, #ver{relation = exists,
                                          local_obj = exists,
                                          item = exists,
                                          timer = exists,
                                          obj = {{cell, _}, _},
                                          has_formula = true}}, Acc) ->
    Acc;
verify_timers(Site, FileId, {Idx, #ver{timer = exists} = V}, Acc) ->
    Str = "Invalid timer",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_timers(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

verify_forms(_Site, _FileId, {_Idx, #ver{relation = exists,
                                         local_obj = exists,
                                         item = exists,
                                         form = exists,
                                         obj = {{cell, _}, _},
                                         has_formula = true}}, Acc) ->
    Acc;
verify_forms(Site, FileId, {Idx, #ver{form = exists} = V}, Acc) ->
    Str = "Invalid form",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_forms(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

verify_formula(_Site, _FileId, {_Idx, #ver{local_obj = exists,
                                           item = exists,
                                           relation = exists,
                                           has_formula = true,
                                           obj = {{cell, _}, _},
                                           type = url}}, Acc) ->
    Acc;
% it is a formula but has a type 'gurl' not a type 'url'
verify_formula(Site, FileId, {Idx, #ver{relation = exists,
                                        local_obj = exists,
                                        item = exists,
                                        obj = {{cell, _}, _},
                                        type = gurl,
                                        has_formula = true} = V}, Acc) ->
    Str = "Invalid formula (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_formula(Site, FileId, {Idx, #ver{has_formula = true} = V}, Acc) ->
    Str = "Invalid formula (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_formula(_Site, _FileId, {_Idx, _H}, Acc) ->
    Acc.

% straight forware cell with a formula
verify_objs(_Site, _FileId, {_Idx, #ver{relation = exists,
                                        local_obj = exists,
                                        item = exists,
                                        type = url,
                                        obj = {{cell, _}, _},
                                        valid_obj = true,
                                        has_formula = true}}, Acc) ->
    Acc;
% straight-forward cell without a formula
verify_objs(_Site, _FileId, {_Idx, #ver{relation = exists,
                                        local_obj = exists,
                                        item = exists,
                                        type = url,
                                        obj = {{cell, _}, _},
                                        valid_obj = true,
                                        has_formula = false,
                                        parents = [],
                                        infparents = []}}, Acc) ->
    Acc;
% another cell without a formula
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = exists,
                                        type = url,
                                        obj = {{cell, _}, _},
                                        valid_obj = true,
                                        has_formula = false}}, Acc) ->
    Acc;
% another cell
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = null,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = gurl,
                                        obj = {{cell, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% a page
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = null,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = url,
                                        obj = {{page, "/"}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% a row
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = null,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = gurl,
                                        obj = {{row, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% another row
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = exists,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = gurl,
                                        obj = {{row, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% a column
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = null,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = gurl,
                                        obj = {{column, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% another column
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = exists,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = gurl,
                                        obj = {{column, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% a range
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = exists,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = url,
                                        obj = {{range, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
% another range
verify_objs(_Site, _FileId, {_Idx, #ver{relation = null,
                                        local_obj = exists,
                                        item = null,
                                        form = null,
                                        include = null,
                                        timer = null,
                                        type = url,
                                        obj = {{range, _}, _},
                                        has_formula = false}}, Acc) ->
    Acc;
verify_objs(Site, FileId, {Idx, #ver{relation = exists,
                                     local_obj = exists,
                                     item = null,
                                     form = null,
                                     include = null,
                                     timer = null,
                                     children = C,
                                     parents = [],
                                     infparents = [],
                                     type = url,
                                     obj = {{cell, _}, _},
                                     has_formula = false} = V}, Acc) ->
    case C of
        [] -> Str = "Invalid Object (cell) (type 1)",
              dump(Site, FileId, Str, [Idx, V]),
              [{Idx, Str} | Acc];
        _  -> Acc
    end;
verify_objs(Site, FileId, {Idx, #ver{obj = {{cell, _}, _},
                                     type = url} = V}, Acc) ->
    Str = "Invalid Object (cell) (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_objs(Site, FileId, {Idx, #ver{obj = {{row, _}, _}} = V}, Acc) ->
    Str = "Invalid Object (row) (type 3)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_objs(Site, FileId, {Idx, #ver{obj = {{column, _}, _}} = V}, Acc) ->
    Str = "Invalid Object (column) (type 4)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_objs(Site, FileId, {Idx, #ver{obj = {{page, _}, _}} = V}, Acc) ->
    Str = "Invalid Object (page) (type 5) (old adding in css/js)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_objs(Site, FileId, {Idx, V}, Acc) ->
    Str = "Invalid Object (type 6)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc].

verify_types(_Site, _FileId, {_Idx, #ver{valid_type = true}}, Acc) ->
    Acc;
verify_types(Site, FileId, {Idx, V}, Acc) ->
    Str = "Invalid types",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc].

verify_grid(_Site, _FileId, {_Idx, #ver{valid_obj = true}}, Acc) ->
    Acc;
verify_grid(Site, FileId, {Idx, #ver{relation = null,
                                     local_obj = exists,
                                     item = exists,
                                     obj = {{Type, {N, M}}, _},
                                     valid_obj = false} = V}, Acc)
  when Type == row orelse Type == column
       andalso (N < 1 andalso M < 1) ->
    Str = "Invalid grid (type 1)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_grid(Site, FileId, {Idx, #ver{relation = null,
                                     local_obj = exists,
                                     item = null,
                                     obj = {{cell, {0, 0}}, _},
                                     type = gurl,
                                     valid_type = false,
                                     valid_obj = false} = V}, Acc) ->
    Str = "Invalid grid (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_grid(Site, FileId, {Idx, #ver{relation = exists,
                                     local_obj = exists,
                                     item = null,
                                     valid_obj = false} = V}, Acc) ->
    Str = "Invalid grid (type 3)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_grid(Site, FileId, {Idx, #ver{valid_obj = false} = V}, Acc) ->
    Str = "Invalid grid (type 4)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc].

verify_revidxs(_Site, _FileId, {_Idx, #ver{valid_revidx = true}}, Acc) ->
    Acc;
verify_revidxs(Site, FileId, {Idx, V}, Acc) ->
    Str = "Invalid reverse index",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc].

verify_zinfs(Site, FileId, {Idx, #ver{relation = null,
                                      local_obj = exists,
                                      item = null,
                                      form = null,
                                      include = null,
                                      timer = null,
                                      children = C,
                                      parents = [],
                                      rev_children = RC,
                                      infparents = [],
                                      rev_parents = [],
                                      rev_infparents = [],
                                      has_formula = false,
                                      type = gurl,
                                      obj = {{cell, _}, _}} = V}, Acc) ->
    case same(C, RC) of
        true -> Acc;
        _    -> Str = "Invalid zinf (type 1)",
                dump(Site, FileId, Str, [Idx, V]),
                [{Idx, Str} | Acc]
    end;
verify_zinfs(Site, FileId, {Idx, #ver{type = gurl,
                                      obj = {{cell, _}, _}} = V}, Acc) ->
    Str = "Invalid zinf (type 2)",
    dump(Site, FileId, Str, [Idx, V]),
    [{Idx, Str} | Acc];
verify_zinfs(_Site, _FileId, {_Idx, _V}, Acc) ->
    Acc.

dump(Site, FileId, Str, [Idx, V]) ->
    io:fwrite(FileId, "~nDumping: ~p ~p~n", [Site, Idx]),
    io:fwrite(FileId, "------------------------------------------------~n", []),
    io:fwrite(FileId, "Reason: " ++ Str ++ "~n", []),
    #ver{relation = RE,  %
         local_obj = LE,       %
         item = IE,            %
         form = FE,            %
         include = IncE,       %
         timer = TE,           %
         parents = P,          %
         children = C,         %
         infparents = IP,      %
         rev_parents = RP,     %
         rev_children = RC,    %
         rev_infparents = RIP, %
         type = Ty,            %
         valid_type = VT,      %
         path = Path,          %
         obj = O,              %
         revidx = RevI,        %
         valid_obj = VO,       %
         valid_revidx = VR,    %
         has_formula = HF,     %
         formula = F,
         has_include = HI,     %
         has_include_fn = RHI, %
         has_zinf = Z,         %
         zinf_path = ZPath     %
        } = V,
    case {Path, O} of
        {null, null} ->
            io:fwrite(FileId, "no path or obj ~nReversed: ~p Zinf Path: ~p Formula? ~p~n",
                      [RevI, ZPath, HF]);
        {List, {O1, O2}} when is_list(List) ->
            io:fwrite(FileId, "~p (~p)~nReversed: ~p Zinf Path: ~p~n"
                      ++"Formula? ~p ~p~n",
                      [Path ++ O2, O1, RevI, ZPath, HF, F])
    end,
    io:fwrite(FileId, "Tables:~n"
              ++ "> relation:  ~p~n"
              ++ "> local_obj: ~p~n"
              ++ "> item:      ~p~n"
              ++ "> form:      ~p~n"
              ++ "> include:   ~p~n"
              ++ "> timer:     ~p~n",
              [RE, LE, IE, FE, IncE, TE]),
    io:fwrite(FileId, "Relations:~n"
              ++ "> Children:  ~p ~p~n"
              ++ "> Parents    ~p ~p~n"
              ++ "> InfParents ~p ~p~n",
              [C, RC, P, RP, IP, RIP]),
    io:fwrite(FileId, "Validities:~n"
              ++ "> Type:           ~p~n"
              ++ "> Valid Type?     ~p~n"
              ++ "> Valid Obj?      ~p~n"
              ++ "> Valid RevIdx?   ~p~n"
              ++ "> Has Include?    ~p~n"
              ++ "> Has Include Fn? ~p~n"
              ++ "> Has Zinf?       ~p~n",
              [Ty, VT, VO, VR, HI, RHI, Z]),
    io:fwrite(FileId, "------------------------------------------------~n", []).

same(List1, List2) ->
    L1 = lists:sort(List1),
    L2 = lists:sort(List2),
    case L1 of
        L2 -> true;
        _  -> false
    end.

delete_file(File) ->
    case file:delete(File) of
        ok              -> ok; % file deleted
        {error, enoent} -> ok; % file doesn't exist
        Other           -> exit(Other)
    end.

process_zinfs([]) ->
    ok;
process_zinfs([H | T]) ->
    {"http://" ++ Site, ZIdx, Path, List} = H,
    Idxs = case dets:lookup(get_dets_file(Site) ++ ".rev", Path) of
               none       -> io:format("Non-existant zinf at ~p ~p~n",
                                       [Site, Path]);
               [{Path, V}] -> V
           end,
    Fun = fun(X, A) ->
              process_z2(List, Site, Path, ZIdx, X, A)
          end,
    lists:foldl(Fun, [], Idxs),
    process_zinfs(T).

process_z2([], _Site, _Path, _ZIDx, _Idx, _Acc) ->
    [];
process_z2([H | T], Site, Path, ZIdx, Idx, Acc) ->
    % first put the idx of the zinf into the rev_infparents of the
    % record
    Rec = get_dets_rec(Site, H),
    #ver{rev_infparents = List} = Rec,
    Rec2 = Rec#ver{rev_infparents = [Idx | List], has_zinf = true,
                   zinf_path = Path},
    % now write out the new version of the record
    ok = dets:insert(get_dets_file(Site), {H, Rec2}),
    % now do the reverse make the url the child of the zinf
    Rec3 = get_dets_rec(Site, ZIdx),
    #ver{children = List3} = Rec3,
    Rec4 = Rec3#ver{children = [H | List3]},
    % now write out the new version of the reversed one...
    ok = dets:insert(get_dets_file(Site), {ZIdx, Rec4}),
    process_z2(T, Site, Path, ZIdx, Idx, Acc).

process(Tuple, Acc) ->
    Head = element(2, Tuple),
    Hd2 = element(1, Head),
    {Site, Table} = extract_tables(Hd2),
    process2(Site, Table, Head, Acc).

process2(core, _, _, Acc) -> Acc;
% skip these tables - not interesting
process2(_Site, Table, _, Acc)
  when Table == "api"
       orelse Table == "del_local"
       orelse Table == "dirty_for_zinf"
       orelse Table == "dirty_queue"
       orelse Table == "dirty_zinf"
       orelse Table == "group"
       orelse Table == "kvstore"
       orelse Table == "logging"
       orelse Table == "style"
       orelse Table == "user_fns"
       -> Acc;
process2(Site, Table, Tuple, Acc) ->
    {ITree, RTree, NewAcc} = lookup(Site, Acc),
    process3(Site, ITree, RTree, Table, Tuple, NewAcc).

process3(Site, ITree, RTree, "form", Tuple, Acc) ->
    {_, Idx, _, _, _, _} = Tuple,
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.form of
               null -> Record#ver{form = exists};
               _    -> exit("duplicate form record (can't happen!)")
           end,
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    lists:keyreplace(Site, 1, Acc, {Site, ITree2, RTree});
process3(Site, ITree, RTree, "include", Tuple, Acc) ->
    {_, Idx, _, _, _, _} = Tuple,
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.include of
               null -> Record#ver{include = exists};
               _    -> exit("duplicate include record (can't happen!)")
           end,
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    lists:keyreplace(Site, 1, Acc, {Site, ITree2, RTree});
process3(Site, ITree, RTree, "timer", Tuple, Acc) ->
    {_, Idx, _} = Tuple,
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.timer of
               null -> Record#ver{timer = exists};
               _    -> exit("duplicate timer record (can't happen!)")
           end,
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    lists:keyreplace(Site, 1, Acc, {Site, ITree2, RTree});
process3(Site, ITree, RTree, "local_obj", Tuple, Acc) ->
    {_, Idx, Type, Path, Obj, RevIdx} = Tuple,
    P2 = hn_util:list_to_path(binary_to_term(Path)),
    O2 = hn_util:obj_to_ref(Obj),
    R2 = binary_to_term(RevIdx),
    IsRevIdxValid = case P2 ++ O2 of
                        R2 -> true;
                        _  -> false
                    end,
    {IsTypeValid, IsObjValid} = is_local_obj_valid(Type, Obj, P2),
    Record = get_rec(Idx, ITree),
    Rec2 = Record#ver{local_obj = exists,
                      valid_revidx = IsRevIdxValid,
                      type = Type,
                      valid_type = IsTypeValid,
                      valid_obj = IsObjValid,
                      obj = {Obj, O2},
                      path = P2,
                      revidx = R2},
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    % for local_objs you also need to do the revidx check an aw...
    Rev = case gb_trees:lookup(R2, RTree) of
              none       -> [];
              {value, V} -> V
          end,
    RTree2 = gb_trees:enter(R2, [Idx | Rev], RTree),
    lists:keyreplace(Site, 1, Acc, {Site, ITree2, RTree2});
process3(Site, ITree, RTree, "item", Tuple, Acc) ->
    {_, Idx, Attrs} = Tuple,
    Attrs2 = binary_to_term(Attrs),
    HasFormula = has_attr("formula", Attrs2),
    Formula = case HasFormula of
                  false -> [];
                  true  -> lists:keyfind("formula", 1, Attrs2)
              end,
    HasInclude = has_attr("__hasincs", Attrs2),
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.item of
               null -> Record#ver{item = exists,
                                  has_formula = HasFormula,
                                  formula = Formula,
                                  has_include = HasInclude};
               _    -> exit("duplicate item record (can't happen!)")
           end,
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    lists:keyreplace(Site, 1, Acc, {Site, ITree2, RTree});
process3(Site, ITree, RTree, "relation", Tuple, Acc) ->
    {_, Idx, Children, Parents, InfParents, _Zs, Include} = Tuple,
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.relation of
               null -> Record#ver{relation = exists,
                                  children = Children,
                                  parents = Parents,
                                  infparents = InfParents,
                                  has_include_fn = Include};
               _    -> exit("duplicate relation record (can't happen!)")
           end,
    ITree2 = gb_trees:enter(Idx, Rec2, ITree),
    ITree3 = invert_rel(ITree2, Idx, Children, Parents, InfParents),
    lists:keyreplace(Site, 1, Acc, {Site, ITree3, RTree});
process3(Site, _ITree, _RTree, Table, _Tuple, _Acc) ->
    io:format("Table ~p on ~p doesn't exist~n", [Table, Site]),
    exit("table not being handled...").

extract_tables(Rec) ->
    Rec2 = atom_to_list(Rec),
    [Site, Port | [Table]] = string:tokens(Rec2, "&"),
    {Site ++ ":" ++ Port, Table}.

invert_rel(Tree, Idx, Children, Parents, InfParents) ->
    Tree1 = invert_children(Children, Idx, Tree),
    Tree2 = invert_parents(Parents, Idx, Tree1),
    invert_infparents(InfParents, Idx, Tree2).

invert_children([], _Idx, Tree) -> Tree;
invert_children([H | T], Idx, Tree) ->
    Rec = get_rec(H, Tree),
    #ver{rev_parents = RP} = Rec,
    Rec2 = Rec#ver{rev_parents = [Idx | RP]},
    Tree2 = gb_trees:enter(H, Rec2, Tree),
    invert_children(T, Idx, Tree2).

invert_parents([], _Idx, Tree) -> Tree;
invert_parents([H | T], Idx, Tree) ->
    Rec = get_rec(H, Tree),
    #ver{rev_children = RC} = Rec,
    Rec2 = Rec#ver{rev_children = [Idx | RC]},
    Tree2 = gb_trees:enter(H, Rec2, Tree),
    invert_parents(T, Idx, Tree2).

invert_infparents([], _Idx, Tree) -> Tree;
invert_infparents([H | T], Idx, Tree) ->
    Rec = get_rec(H, Tree),
    #ver{rev_children = RC} = Rec,
    Rec2 = Rec#ver{rev_children = [Idx | RC]},
    Tree2 = gb_trees:enter(H, Rec2, Tree),
    invert_infparents(T, Idx, Tree2).

has_attr(Attr, Attrs) ->
    case lists:keyfind(Attr, 1, Attrs) of
        false -> false;
        _F    -> true
    end.

is_local_obj_valid(url, {range, {X1, Y1, X2, Y2}}, _Path)
  when X1 > 0 andalso Y1 > 0 andalso X2 > 0 andalso Y2 > 0 ->
    {true, true};
is_local_obj_valid(gurl, {range, {X1, Y1, X2, Y2}}, _Path)
  when X1 > 0 andalso Y1 > 0 andalso X2 > 0 andalso Y2 > 0 ->
    {false, true};
is_local_obj_valid(url, {range, _}, _Path) ->
    {false, false};
is_local_obj_valid(gurl, {row, {Y1, Y2}}, _Path)
  when Y1 > 0 andalso Y2 > 0 ->
    {true, true};
is_local_obj_valid(url, {row, {Y1, Y2}}, _Path)
  when Y1 > 0 andalso Y2 > 0 ->
    {false, true};
is_local_obj_valid(gurl, {row, _}, _Path) ->
    {true, false};
is_local_obj_valid(url, {row, _}, _Path) ->
    {false, false};
is_local_obj_valid(gurl, {column, {X1, X2}}, _Path)
  when X1 > 0 andalso X2 > 0 ->
    {true, true};
is_local_obj_valid(url, {column, {X1, X2}}, _Path)
  when X1 > 0 andalso X2 > 0 ->
    {false, true};
is_local_obj_valid(gurl, {column, _}, _Path) ->
    {true, false};
is_local_obj_valid(url, {column, _}, _Path) ->
    {false, false};
is_local_obj_valid(url, {page, "/"}, _Path) ->
    {true, true};
is_local_obj_valid(gurl, {page, "/"}, _Path) ->
    {false, true};
is_local_obj_valid(Type, {cell, {X, Y}}, Path)
  when X > 0 andalso Y > 0 ->
    case type(hn_util:path_tokens(Path)) of
        Type -> {true, true};
        _    -> {false, true}
    end;
is_local_obj_valid(Type, {cell, _}, Path)  ->
    case type(hn_util:path_tokens(Path)) of
        Type -> {true, false};
        _    -> {false, false}
    end.

type([])               -> url;
type(["["++_Rest| _T]) -> gurl;
type([_H | T])         -> type(T).

get_rec(Idx, Tree) ->
    case gb_trees:lookup(Idx, Tree) of
        none       -> #ver{};
        {value, V} -> V
    end.

get_dets_rec(Site, Idx) ->
    File = get_dets_file(Site),
    case dets:lookup(File, Idx) of
        []         -> #ver{};
        [{Idx, V}] -> V
    end.

lookup(Site, List) ->
    case lists:keyfind(Site, 1, List) of
        false                -> E = gb_trees:empty(),
                                {E, E, [{Site, E, E} | List]};
        {Site, ITree, RTree} -> {ITree, RTree, List}
    end.

write_dets([]) -> ok;
write_dets([{Site, Tree, RevIdxs} | T]) ->
    % first write out the main file
    File = get_dets_file(Site),
    filelib:ensure_dir(File),
    {ok, FileId} = dets:open_file(File, []),
    ok = dets:delete_all_objects(File),
    true = dets:insert_new(FileId, gb_trees:to_list(Tree)),
    garbage_collect(self()),
    % now write out the reverse indexes
    RevFile = File ++ ".rev",
    {ok, RevFileId}= dets:open_file(RevFile, []),
    ok = dets:delete_all_objects(RevFile),
    true = dets:insert_new(RevFileId, gb_trees:to_list(RevIdxs)),
    garbage_collect(self()),
    write_dets(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% nicked from mnesia_text - can't use 'as is' because of #Fun<> bug
%%% anyhoo chopped it up...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(File) ->
    file(File).

file(File) ->
    case file:open(File, [read]) of
        {ok, Stream} ->
            Res = read_terms(Stream, File, 1, [], []),
            file:close(Stream),
            io:format("File is closed...~n"),
            Res;
        _Other ->
            {error, open}
    end.

read_terms(Stream, File, Line, L, Acc) ->
    case read_term_from_stream(Stream, File, Line) of
        {ok, Term, NextLine} ->
            NewAcc = process(Term, Acc),
            read_terms(Stream, File, NextLine, [Term|L], NewAcc);
        error ->
            {error, read};
        eof ->
            {ok, Acc}
    end.

read_term_from_stream(Stream, File, Line) ->
    R = io:request(Stream, {get_until,'',erl_scan,tokens,[Line]}),
    case R of
        {ok,Toks,EndLine} ->
            % added this bit to get around duff file dump
            Toks2 = fix_up(Toks, []),
            case erl_parse:parse_term(Toks2) of
                {ok, Term} ->
                    case (trunc(Line/1000) - Line/1000) of
                        0.0 -> io:format(".");
                        _   -> ok
                    end,
                    case (trunc(Line/10000) - Line/10000) of
                        0.0 -> io:format("~nLine ~p~n", [Line]);
                        _   -> ok
                    end,
                    {ok, {Line, Term}, EndLine};
                {error, {NewLine,Mod,What}} ->
                    Str = Mod:format_error(What),
                    io:format("Str is ~p~n", [Str]),
                    io:format("Error in line:~p of:~p ~s\n",
                              [NewLine, File, Str]),
                    error
            end;
        {eof,_EndLine} ->
            eof;
        Other ->
            io:format("Error1 **~p~n",[Other]),
            error
    end.

fix_up([], Acc) ->
    lists:reverse(Acc);
fix_up([{'#', _} | T], Acc) ->
    {NewT, NewAcc} = snip(T, Acc),
    fix_up(NewT, NewAcc);
fix_up([H | T], Acc) ->
    fix_up(T, [H | Acc]).

snip([{'>', N} | T], Acc) -> {T, [{atom, N, removed} | Acc]};
snip([_H | T], Acc)       -> snip(T, Acc).
