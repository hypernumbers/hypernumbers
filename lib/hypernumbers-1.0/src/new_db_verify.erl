%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       A module to verify the status
%%%            of a hypernumbers database and
%%%            make assertions about it...
%%%
%%% @end
%%% Created : 12 Oct 2011 by gordon@hypernumbers.com

-module(new_db_verify).

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
          path = null, % makes it more readable
          obj = null,  % ditto
          revidx = null, % ditto
          valid_obj = false,
          valid_revidx = invalid,
          has_formula = false,
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
         dump_tables/0,
         read_verification/3
        ]).

% debugging api
-export([
         summarise_problems/0,
         summarise_problems/2,
         read_verification/0
        ]).

check() ->
    {Dir, TermFile, ZinfFile} = dump_tables(),
    io:format("~p and ~p created in ~p~n", [TermFile, ZinfFile, Dir]),
    read_verification(Dir, TermFile, ZinfFile).

dump_tables() ->
    Dir = code:lib_dir(hypernumbers) ++ "/../../priv/verification/",
    Stamp = dh_date:format("d_M_y_G_i_s"),
    TermFile = "verification." ++ Stamp ++ ".terms",
    ZinfFile = "zinf." ++ Stamp ++ ".terms",
    Fun = fun() ->
                  mnesia:dump_to_textfile(Dir ++ TermFile)
          end,
    mnesia:activity(transaction, Fun),
    io:format("Mnesia tables all dumped...~n"),
    Sites = hn_setup:get_sites(),
    [zinf_srv:dump_to_file(X, Dir ++ ZinfFile) || X <- Sites],
    io:format("Zinfs all dumped...~n"),
    {Dir, TermFile, ZinfFile}.

read_verification() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    VerFile = "verification.27_Oct_11_8_42_59.terms",
    ZinfFile = "zinf.27_Oct_11_8_42_59.terms",
    read_verification(Dir, VerFile, ZinfFile).

read_verification(Dir, VerFile, ZinfFile) ->
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
    Data2 = process_zinfs(Zinfs, Data),
    io:format("in verification, zinfs processed...~n"),
    [_Prefix, Stamp, FileType] = string:tokens(VerFile, "."),
    DataFile = "verification_data" ++ "." ++ Stamp ++ "." ++ FileType,
    garbage_collect(self()),
    Terms = make_terms(Data2, []),
    ok = file:write_file(Dir ++ DataFile, Terms),
    io:format("Written out processed data to ~p~n", [DataFile]),
    garbage_collect(self()),
    FileName = "errors."  ++ Stamp ++ "." ++ FileType,
    Errors = process_data(Data2, Dir, FileName),
    FileName2 = "fixable_errors." ++ Stamp ++ "." ++ FileType,
    hn_util:log_terms(Errors, Dir ++ FileName2),
    summarise_problems(Dir, FileName2),
    ok.

summarise_problems() ->
    Dir = "/home/gordon/hypernumbers/priv/verification/",
    File = "fixable_errors.20_Oct_11_13_27_19.terms",
    summarise_problems(Dir, File).

summarise_problems(Dir, File) ->
    {ok, [{Data1, Data2}]} = file:consult(Dir ++ File),
    io:format("Problems loaded~n"),
    [summarise(X, Site, Data2, []) || {Site, X} <- Data1],
    ok.

summarise([], Site, Data2, Acc) ->
    io:format("~n~p:~n", [Site]),
    print_summary(Acc),
    case lists:keyfind(Site, 1, Data2) of
        false        -> io:format("no broken revidxs~n");
        {Site, List} -> io:format("~p broken revidxs~n", [length(List)])
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

process_data(Data, Dir, FileName) ->
    _Return = filelib:ensure_dir(Dir ++ FileName),
    case file:open(Dir ++ FileName, [append]) of
        {ok, Id} ->
            Return = verify(Data, Id, {[], []}),
            file:close(Id),
            Return;
        _ ->
            error
    end.

verify([], _, Acc) -> Acc;
verify([H | T], FileId, {Acc1, Acc2}) ->
    {Site, ITree, RTree} = H,
    io:format("Verifying: ~p: ", [Site]),
    Idxes = gb_trees:to_list(ITree),
    io:format("Idxs..."),
    RevIdxs = gb_trees:to_list(RTree),
    io:format("RevIdxs..."),
    Broken_Idxs = verify2(Idxes, Site, FileId, []),
    io:format("Borked Idxs..."),
    Broken_Revs = verify3(RevIdxs, Site, FileId, []),
    io:format("Borked RevIdxs~n"),
    verify(T, FileId, {[{Site, Broken_Idxs} | Acc1],
                       [{Site, Broken_Revs} | Acc2]}).

verify2([], _Site, _FileId, Acc) -> Acc;
verify2([H | T], Site, FileId, Acc) ->
    Acc1 = verify_tables(Site, FileId, H, Acc),
    Acc2 = verify_relations(Site, FileId, H, Acc1),
    Acc3 = verify_includes(Site, FileId, H, Acc2),
    Acc4 = verify_timers(Site, FileId, H, Acc3),
    Acc5 = verify_forms(Site, FileId, H, Acc4),
    Acc6 = verify_formula(Site, FileId, H, Acc5),
    Acc7 = verify_objs(Site, FileId, H, Acc6),
    Acc8 = verify_types(Site, FileId, H, Acc7),
    Acc9 = verify_grid(Site, FileId, H, Acc8),
    Acc10 = verify_revidxs(Site, FileId, H, Acc9),
    Acc11 = verify_zinfs(Site, FileId, H, Acc10),
    verify2(T, Site, FileId, Acc11).

verify3([], _Site, _FileId, Acc) -> Acc;
verify3([H | T], Site, FileId, Acc) ->
    case H of
        {_Path, [_Idx]} ->
            ok;
        {Path, List}    ->
            io:fwrite(FileId, "multiple local objs for ~p ~p~n~p~n",
                      [Site, Path, List])
    end,
    verify3(T, Site, FileId, Acc).

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
verify_tables(Site, FileId, {Idx, #ver{local_obj = exists} = V}, Acc) ->
    Str = "Invalid tables (type 2)",
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
verify_formula(Site, FileId, {Idx, #ver{has_formula = true} = V}, Acc) ->
    Str = "Invalid formula",
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
verify_grid(Site, FileId, {Idx, #ver{valid_obj = false} = V}, Acc) ->
    Str = "Invalid grid (type 3)",
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
    #ver{relation = RE,
         local_obj = LE,
         item = IE,
         form = FE,
         include = IncE,
         timer = TE,
         parents = P,
         children = C,
         infparents = IP,
         rev_parents = RP,
         rev_children = RC,
         rev_infparents = RIP,
         type = Ty,
         valid_type = VT,
         path = Path,
         obj = O,
         revidx = RevI,
         valid_obj = VO,
         valid_revidx = VR,
         has_formula = HF,
         has_include = HI,
         has_include_fn = RHI,
         has_zinf = Z,
         zinf_path = ZPath
        } = V,
    case {Path, O} of
        {null, null} ->
            io:fwrite(FileId, "no path or obj ~nReversed: ~p Zinf Path: ~p Formula? ~p~n",
                      [RevI, ZPath, HF]);
        {List, {O1, O2}} when is_list(List) ->
            io:fwrite(FileId, "~p (~p)~nReversed: ~p Zinf Path: ~p Formula? ~p~n",
                      [Path ++ O2, O1, RevI, ZPath, HF])
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

process_zinfs([], Acc) -> Acc;
process_zinfs([H | T], Acc) ->
    {"http://" ++ Site, ZIdx, Path, List} = H,
    {ITree, RTree, NewAcc} = lookup(Site, Acc),
    [Idx] = case gb_trees:lookup(Path, RTree) of
                none       -> exit("zinf doesn't exist...");
                {value, V} -> V
            end,
    NewAcc2 = process_z2(List, Site, Path, ZIdx, Idx, ITree, RTree, NewAcc),
    process_zinfs(T, NewAcc2).

process_z2([], Site, _Path, _ZIDx, _Idx, ITree, RTree, Acc) ->
    lists:keyreplace(Site, 1, Acc, {Site, ITree, RTree});
process_z2([H | T], Site, Path, ZIdx, Idx, ITree, RTree, Acc) ->
    % first put the idx of the zinf into the rev_infparents of the
    % record
    Rec = get_rec(H, ITree),
    #ver{rev_infparents = List} = Rec,
    Rec2 = Rec#ver{rev_infparents = [Idx | List], has_zinf = true,
                   zinf_path = Path},
    ITree2 = gb_trees:enter(H, Rec2, ITree),
    % now do the reverse make the url the child of the zinf
    Rec3 = get_rec(ZIdx, ITree2),
    #ver{infparents = List3} = Rec3,
    Rec4 = Rec3#ver{children = [H | List3]},
    ITree4 = gb_trees:enter(ZIdx, Rec4, ITree2),
    process_z2(T, Site, Path, ZIdx, Idx, ITree4, RTree, Acc).

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
    HasInclude = has_attr("__hasincs", Attrs2),
    Record = get_rec(Idx, ITree),
    Rec2 = case Record#ver.item of
               null -> Record#ver{item = exists,
                                  has_formula = HasFormula,
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

lookup(Site, List) ->
    case lists:keyfind(Site, 1, List) of
        false                -> E = gb_trees:empty(),
                                {E, E, [{Site, E, E} | List]};
        {Site, ITree, RTree} -> {ITree, RTree, List}
    end.

extract_tables(tables) ->
    {core, tables};
extract_tables(core_site) ->
    {core, core_site};
extract_tables(service_hns_resource) ->
    {core, service_hns_resource};
extract_tables(service_hns_record) ->
    {core, service_hns_record};
extract_tables(service_hns_zone) ->
    {core, service_hns_zone};
extract_tables(service_passport_user) ->
    {core, service_passport_user};
extract_tables(Table) ->
    Tbl2 = atom_to_list(Table),
    [Site, Port, Tbl3] = string:tokens(Tbl2, "&"),
    {Site ++ ":" ++ Port, Tbl3}.

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

make_terms([], Acc) ->
    lists:flatten(lists:reverse(Acc));
make_terms([H | T], Acc) ->
    NewAcc = lists:flatten(io_lib:format("~p.~n", [H])),
    make_terms(T, [NewAcc | Acc]).
