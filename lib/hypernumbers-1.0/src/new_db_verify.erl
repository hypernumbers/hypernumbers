%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumers Ltd
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
          has_include_fn = false
         }).

% debugging api
-export([
         dump_tables/0,
         read_verification/0
        ]).

% main api
-export([
         check/0
        ]).

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

check() ->
    {Dir, TermFile, ZinfFile} = dump_tables(),
    ok = read_verification(Dir ++ TermFile, Dir ++ ZinfFile).

read_verification() ->
    VerFile = "/home/gordon/hypernumbers/priv/verification/"
        ++ "verification.20_Oct_11_13_27_19.terms",
    ZinfFile = "/home/gordon/hypernumbers/priv/verification/"
        ++ "zinf.20_Oct_11_13_27_19.terms",
    read_verification(VerFile, ZinfFile).

read_verification(VerFile, ZinfFile) ->
    {ok, Data}  = parse(VerFile),
    io:format("in verification, data parsed...~n"),
    {ok, Zinfs} = file:consult(ZinfFile),
    io:format("in verification, zinfs parsed...~n"),
    Data2 = process_zinfs(Zinfs, Data),
    io:format("in verification, zinfs processed...~n"),
    ok = verify(Data2).

verify([]) -> ok;
verify([H | T]) ->
    {Site, ITree, RTree} = H,
    io:format("Verifying~p:~n", [Site]),
    Idxes = gb_trees:to_list(ITree),
    RevIdxs = gb_trees:to_list(RTree),
    ok = verify2(Idxes),
    ok = verify3(RevIdxs),
    verify(T).

verify2([]) -> ok;
verify2([H | T]) ->
    ok = verify_tables(H),
    ok = verify_relations(H),
    ok = verify_includes(H),
    ok = verify_timers(H),
    ok = verify_forms(H),
    ok = verify_formula(H),
    ok = verify_objs(H),
    ok = verify_types(H),
    ok = verify_grid(H),
    ok = verify_revidxs(H),
    ok = verify_zinfs(H),
    verify2(T).

verify3([]) -> ok;
verify3([H | T]) ->
    case H of
        {_Path, [_Idx]} -> ok;
        {Path, List}    -> io:format("multiple local objs for ~p ~p~n",
                                     [Path, List])
    end,
    verify3(T).

verify_tables({_Idx, #ver{local_obj = exists,
                          item = null,
                          relation = null,
                          form = null,
                          include = null,
                          timer = null,
                          has_formula = false,
                          parents = [],
                          infparents = [],
                          rev_parents = []}}) ->
    ok;
verify_tables({_Idx, #ver{local_obj = exists,
                          item = exists,
                          relation = null,
                          form = null,
                          include = null,
                          timer = null,
                          has_formula = false,
                          parents = [],
                          infparents = [],
                          rev_parents = []}}) ->
    ok;
verify_tables({_Idx, #ver{local_obj = exists,
                          item = exists,
                          relation = exists,
                          form = null,
                          include = null,
                          timer = null,
                          has_formula = true}}) ->
    ok;
verify_tables({_Idx, #ver{local_obj = exists,
                          item = exists,
                          relation = exists,
                          form = exists,
                          include = null,
                          timer = null,
                          has_formula = true}}) ->
    ok;
verify_tables({_Idx, #ver{local_obj = exists,
                          item = exists,
                          relation = exists,
                          form = null,
                          include = exists,
                          timer = null,
                          has_formula = true}}) ->
    ok;
verify_tables({_Idx, #ver{local_obj = exists,
                          item = exists,
                          relation = exists,
                          form = null,
                          include = null,
                          timer = exists,
                          has_formula = true}}) ->
    ok;
verify_tables({Idx, #ver{local_obj = exists,
                          item = null,
                          relation = exists,
                          form = null,
                          include = null,
                          timer = null,
                          children = C,
                          parents = [],
                          infparents = [],
                          has_formula = false} = V}) ->
    case C of
        [] -> dump("Invalid tables (type 1):", [Idx, V]);
        _  -> ok
    end;
verify_tables({Idx, #ver{local_obj = exists} = V}) ->
    dump("Invalid tables (type 2):", [Idx, V]);
verify_tables({_Idx, _H}) ->
    ok.

verify_relations({Idx, #ver{relation = exists,
                            local_obj = exists,
                            item = exists,
                            children = C,
                            parents = P,
                            infparents = I,
                            rev_children = RC,
                            rev_parents = RP,
                            rev_infparents = RIP} = V}) ->
    case {same(C, RC), same(P, RP), same(I, RIP)} of
        {true, true, true} ->
            ok;
        _ ->
            dump("Invalid relations (type 1):", [Idx, V])
    end;
verify_relations({Idx, #ver{relation = exists,
                            local_obj = exists,
                            item = null,
                            children = C,
                            parents = [],
                            infparents = [],
                            rev_children = RC,
                            rev_parents = []} = V}) ->
    case same(C, RC) of
        true ->
            ok;
        _ ->
            dump("Invalid relations (type 2):", [Idx, V])
    end;
verify_relations({Idx, #ver{relation = exists} = V}) ->
    dump("Invalid relations (type 2):", [Idx, V]);
verify_relations({_Idx, _H}) -> ok.

verify_includes({_Idx, #ver{relation = exists,
                            local_obj = exists,
                            item = exists,
                            form = null,
                            include = exists,
                            timer = null,
                            has_include = true}}) ->
    ok;
verify_includes({Idx, #ver{include = exists} = V}) ->
    dump("Invalid include (type 1):", [Idx, V]),
    ok;
verify_includes({Idx, #ver{has_include = true} = V}) ->
    dump("Invalid include (type 2):", [Idx, V]),
    ok;
verify_includes({_Idx, _H}) ->
    ok.

verify_timers({_Idx, #ver{relation = exists,
                          local_obj = exists,
                          item = exists,
                          form = null,
                          include = null,
                          timer = exists}}) ->
    ok;
verify_timers({Idx, #ver{timer = exists} = V}) ->
    dump("Invalid timer:", [Idx, V]),
    ok;
verify_timers({_Idx, _H}) ->
    ok.

verify_forms({_Idx, #ver{relation = exists,
                         local_obj = exists,
                         item = exists,
                         form = exists,
                         include = null,
                         timer = null}}) ->
    ok;
verify_forms({Idx, #ver{form = exists} = V}) ->
    dump("Invalid form:", [Idx, V]),
    ok;
verify_forms({_Idx, _H}) ->
    ok.

verify_formula({_Idx, #ver{local_obj = exists,
                           item = exists,
                           relation = exists,
                           has_formula = true,
                           obj = {{cell, _}, _},
                           type = url}}) ->
    ok;
verify_formula({Idx, #ver{has_formula = true} = V}) ->
    dump("Invalid formula:", [Idx, V]),
    ok;
verify_formula({_Idx, _H}) ->
    ok.

verify_objs({_Idx, #ver{relation = exists,
                        local_obj = exists,
                        item = exists,
                        type = url,
                        obj = {{cell, _}, _},
                        valid_obj = true,
                        has_formula = true}}) ->
    ok;
verify_objs({_Idx, #ver{relation = exists,
                        local_obj = exists,
                        item = exists,
                        type = url,
                        obj = {{cell, _}, _},
                        valid_obj = true,
                        has_formula = false,
                        parents = [],
                        infparents = []}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = exists,
                        type = url,
                        obj = {{cell, _}, _},
                        valid_obj = true,
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = null,
                        form = null,
                        include = null,
                        timer = null,
                        type = url,
                        obj = {{page, "/"}, _},
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = null,
                        form = null,
                        include = null,
                        timer = null,
                        type = gurl,
                        obj = {{row, _}, _},
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = exists,
                        form = null,
                        include = null,
                        timer = null,
                        type = gurl,
                        obj = {{row, _}, _},
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = null,
                        form = null,
                        include = null,
                        timer = null,
                        type = gurl,
                        obj = {{column, _}, _},
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = exists,
                        form = null,
                        include = null,
                        timer = null,
                        type = gurl,
                        obj = {{column, _}, _},
                        has_formula = false}}) ->
    ok;
verify_objs({_Idx, #ver{relation = null,
                       local_obj = exists,
                       item = null,
                       form = null,
                       include = null,
                       timer = null,
                       type = gurl,
                       obj = {{cell, _}, _},
                       has_formula = false}}) ->
    ok;
verify_objs({Idx, #ver{relation = exists,
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
                       has_formula = false} = V}) ->
    case C of
        [] -> dump("Invalid cell (type 1):", [Idx, V]);
        _  -> ok
    end;
verify_objs({Idx, #ver{obj = {{cell, _}, _},
                       type = url} = V}) ->
    dump("Invalid cell (type 2):", [Idx, V]),
    ok;
verify_objs({Idx, #ver{obj = {{row, _}, _}} = V}) ->
    dump("Invalid cell (type 3):", [Idx, V]),
    ok;
verify_objs({Idx, #ver{obj = {{column, _}, _}} = V}) ->
    dump("Invalid cell (type 4):", [Idx, V]),
    ok;
verify_objs({Idx, #ver{obj = {{page, _}, _}} = V}) ->
    dump("Invalid cell (type 5):", [Idx, V]),
    ok.

verify_types({_Idx, #ver{valid_type = true}}) ->
    ok;
verify_types({Idx, V}) ->
    dump("Invalid types:", [Idx, V]),
    ok.

verify_grid({_Idx, #ver{valid_obj = true}}) ->
    ok;
verify_grid({Idx, #ver{valid_obj = false} = V}) ->
    dump("Invalid grid:", [Idx, V]),
    ok.

verify_revidxs({_Idx, #ver{valid_revidx = true}}) ->
    ok;
verify_revidxs({Idx, V}) ->
    dump("Invalid Reverse Index:", [Idx, V]),
    ok.

verify_zinfs({_Idx, #ver{relation = null,
                        local_obj = exists,
                        item = null,
                        form = null,
                        include = null,
                        timer = null,
                        children = [],
                        parents = [],
                        infparents = [],
                        rev_parents = [],
                        rev_infparents = [],
                        has_formula = false,
                        type = gurl,
                        obj = {{cell, _}, _}}}) ->
    ok;
verify_zinfs({Idx, #ver{type = gurl,
                        obj = {{cell, _}, _}} = V}) ->
    dump("Invalid zinf:", [Idx, V]),
    ok;
verify_zinfs({_Idx, _V}) ->
    ok.

dump(Str, [Idx, V]) ->
    io:format("~nDumping: ~p~n", [Idx]),
    io:format("------------------------------------------------~n"),
    io:format(Str, []),
    #ver{
               relation = RE,
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
               obj = {O1, O2},
               revidx = RevI,
               valid_obj = VO,
               valid_revidx = VR,
               has_formula = HF,
               has_include = HI,
               has_include_fn = RHI
              } = V,
    io:format("~p (~p) Reversed: ~p Formula? ~p~n",[Path ++ O2, O1, RevI, HF]),
    io:format("Tables:~n"
              ++ "> relation:  ~p~n"
              ++ "> local_obj: ~p~n"
              ++ "> item:      ~p~n"
              ++ "> form:      ~p~n"
              ++ "> include:   ~p~n"
              ++ "> timer:     ~p~n",
              [RE, LE, IE, FE, IncE, TE]),
    io:format("Relations:~n"
              ++ "> Children:  ~p ~p~n"
              ++ "> Parents    ~p ~p~n"
              ++ "> InfParents ~p ~p~n",
              [C, RC, P, RP, IP, RIP]),
    io:format("Validities:~n"
              ++ "> Type:           ~p~n"
              ++ "> Valid Type?     ~p~n"
              ++ "> Valid Obj?      ~p~n"
              ++ "> Valid RevIdx?   ~p~n"
              ++ "> Has Include?    ~p~n"
              ++ "> Has Include Fn? ~p~n",
              [Ty, VT, VO, VR, HI, RHI]),
    io:format("------------------------------------------------~n").

same(List1, List2) ->
    L1 = lists:sort(List1),
    L2 = lists:sort(List2),
    case L1 of
        L2 -> true;
        _  -> false
    end.

process_zinfs([], Acc) -> Acc;
process_zinfs([H | T], Acc) ->
    {"http://" ++ Site, Path, List} = H,
    {ITree, RTree, NewAcc} = lookup(Site, Acc),
    [Idx] = case gb_trees:lookup(Path, RTree) of
                none       -> exit("zinf doesn't exist...");
                {value, V} -> V
            end,
    NewAcc2 = process_z2(List, Site, Idx, ITree, RTree, NewAcc),
    process_zinfs(T, NewAcc2).

process_z2([], Site, _Idx, ITree, RTree, Acc) ->
    lists:keyreplace(Site, 1, Acc, {Site, ITree, RTree});
process_z2([H | T], Site, Idx, ITree, RTree, Acc) ->
    Rec = get_rec(H, ITree),
    #ver{rev_infparents = List} = Rec,
    Rec2 = Rec#ver{rev_infparents = [Idx | List]},
    ITree2 = gb_trees:enter(H, Rec2, ITree),
    process_z2(T, Site, Idx, ITree2, RTree, Acc).

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
