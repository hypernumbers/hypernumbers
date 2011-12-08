%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers
%%% @doc       Deubgging functions for the database
%%%            help you trace URL's Idx's etc, etc
%%% @end
%%% Created : 25 Aug 2011 by <gordon@hypernumbers.dev>

-module(new_db_DEBUG).

-include("spriki.hrl").

-export([
         force_dbsrv/1,
         dump_lost_idxs/1,
         tick/0,
         timer/1,
         dirty_for_zinf/1,
         item_and_local_objs/1,
         url/1,
         url/2,
         idx/2,
         idx/3,
         find_rel/2,
         raw_idx/2,
         raw_url/1,
         dump_site_table/2,
         dump_core_table/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Debug Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec force_dbsrv(string()) -> ok.
force_dbsrv(Site) ->
    % use a spoof RefX
    RefX = #refX{site = Site, path = [], type = url, obj = {cell, {1,1}}},
    Op = fun() ->
                 io:format("Forcing dbsrv on ~p~n", [Site])
         end,
    new_db_api:write_activity_DEBUG(RefX, Op, "quiet", "Forced dbsrv").

-spec find_rel(string(), integer()) -> ok.
% walks the rel table looking for an idx
find_rel(Site, Idx) ->
    Tbl = new_db_wu:trans(Site, relation),
    F1 = fun() ->
                 F2 = fun(R, Acc) ->
                              #relation{children = C, parents = P,
                                        infparents = IP, z_parents = ZP} = R,
                              All = lists:merge([C, P, IP, ZP]),
                              case lists:member(Idx, All) of
                                  false ->
                                      ok;
                                  true  ->
                                      io:format("~p is included in ~p~n",
                                                [Idx, R])
                              end,
                              Acc
                      end,
                 mnesia:foldl(F2, [], Tbl)
         end,
    [] = mnesia:activity(transaction, F1),
    ok.

-spec dump_site_table(string(), string()) -> ok.
%% just dumps a table to the shell
dump_site_table(Site, Table) ->
    Record = list_to_atom(Table),
    Table2 = new_db_wu:trans(Site, Record),
    dump2(Table2, Record).

-spec dump_core_table(string(), string()) -> ok.
%% just dumps a table to the shell
dump_core_table(Prefix, Record) ->
    Table = list_to_atom(Prefix ++ Record),
    dump2(Table, list_to_atom(Record)).

dump2(Table, Record) ->
    N = ms_util2:no_of_fields(Record),
    Spec =  list_to_tuple([Record| lists:duplicate(N, '_')]),
    Fun = fun() ->
                  Ret = mnesia:match_object(Table, Spec, write),
                  io:format("Dumping ~p~n~n~p~n", [Table, Ret]),
                  ok
          end,
    mnesia:activity(async_dirty, Fun),
    ok.

dump_lost_idxs(Site) ->
    io:format("looking through all idxs for ~p~n", [Site]),
    % first up basic tables
    Tables = [form, include, item, logging, timer],
    [dump(Site, X) || X <- Tables],
    ok.

dump(Site, Table) ->
    io:format("Dumping ~p on ~p~n", [Table, Site]),
    Tab2 = new_db_wu:trans(Site, Table),
    Fun = fun() ->
                  mnesia:all_keys(Tab2)
          end,
    Keys = mnesia:activity(transaction, Fun),
    dump_keys(Site, Tab2, Keys),
    ok.

dump_keys(Site, Table, Keys) ->
    Fun = fun(X) ->
                  case new_db_wu:idx_to_xrefXD(Site, X) of
                      {error, id_not_found, X} ->
                          io:format("Idx ~p not found in ~p on ~p~n",
                                    [X, Site, Table]);
                      _ -> ok %io:format(".")
                  end
          end,
    Fun2 = fun() ->
                   [Fun(X) || X <- Keys]
           end,
    mnesia:activity(transaction, Fun2),
    io:format("~n").


tick() ->
    S = "http://hypernumbers.dev:9000",
    P = ["tick"],
    O = {cell, {1, 1}},
    F = integer_to_list(remoting_reg:timestamp()),
    RefX = #refX{site = S, path = P, obj = O},
    ok = new_db_api:write_attributes([{RefX, [{"formula", F}]}]),
    timer:apply_after(1000, new_db_DEBUG, tick, []).

timer(Site) ->
    F = fun() ->
                timer_debugD(Site)
        end,
    mnesia:transaction(F).

item_and_local_objs(Site) ->
    F = fun() ->
                item_and_local_objsD(Site)
        end,
    mnesia:transaction(F).

dirty_for_zinf(Site) ->
    F = fun() ->
                dirty_for_zinfD(Site)
        end,
    mnesia:transaction(F).

raw_url(Url) ->
    Fun = fun() ->
                  RefX = hn_util:url_to_refX(Url),
                  #refX{site = S, path = P, obj = O} = RefX,
                  Table = new_db_wu:trans(S, local_obj),
                  RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
                  case mnesia:index_read(Table, term_to_binary(RevIdx),
                                    #local_obj.revidx) of
                      []  -> io:format("no object exists at ~p~n", [Url]);
                      [R] -> io:format("~p has idx of ~p~n",
                                       [Url, R#local_obj.idx]),
                             io:format("The local_obj is for ~p on ~p and "
                                       ++ " has a reverse index of ~p~n",
                                       [hn_util:obj_to_ref(R#local_obj.obj),
                                        binary_to_term(R#local_obj.path),
                                        binary_to_term(R#local_obj.revidx)])
                             end
          end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

raw_idx(Site, Idx) ->
    Fun = fun() ->
                  case new_db_wu:idx_to_xrefXD(Site, Idx) of
                      {error, _, _} ->
                          io:format("Idx does not exist~n"),
                          dump_logs(Site, Idx);
                      XRefX ->
                          io:format("XRefX is ~p~n", [XRefX]),
                          Tab1 = new_db_wu:trans(Site, local_obj),
                          [R1] = mnesia:read(Tab1, Idx, read),
                          io:format("Raw local_obj is ~p~n", [R1]),
                          io:format("local_obj: idx ~p~n type ~p~n path ~p~n "
                                    ++ "obj ~p~n revidx ~p~n",
                                    [R1#local_obj.idx, R1#local_obj.type,
                                     binary_to_term(R1#local_obj.path),
                                     hn_util:obj_to_ref(R1#local_obj.obj),
                                     binary_to_term(R1#local_obj.revidx)]),
                          Tab2 = new_db_wu:trans(Site, item),
                          R2 = mnesia:read(Tab2, Idx, read),
                          io:format("Raw item is ~p~n", [R2]),
                          case R2 of
                              []      -> ok;
                              [RItem] ->
                                  Attrs2 = binary_to_term(RItem#item.attrs),
                                  io:format("item: idx ~p~n attrs ~p~n",
                                            [RItem#item.idx, Attrs2])
                          end,
                          Tab3 = new_db_wu:trans(Site, relation),
                          Rels = mnesia:read(Tab3, Idx, read),
                          io:format("Raw Relations is ~p~n", [Rels]),
                          Tab4 = new_db_wu:trans(Site, form),
                          Forms = mnesia:read(Tab4, Idx, read),
                          io:format("Raw Forms is ~p~n", [Forms]),
                          Tab5 = new_db_wu:trans(Site, include),
                          Includes = mnesia:read(Tab5, Idx, read),
                          io:format("Raw Includes is ~p~n", [Includes]),
                          Tab6 = new_db_wu:trans(Site, timer),
                          Timer = mnesia:read(Tab6, Idx, read),
                          io:format("Raw Timer is ~p~n", [Timer])
                  end
          end,
    mnesia:transaction(Fun).

url(Url) -> url(Url, quiet).

% use the atom 'verbose' for this mode to get everything
% use 'log' to log the results
url(Url, Mode) -> RefX = hn_util:url_to_refX(Url),
                        Output = io_lib:format("Url ~p being debugged", [Url]),
                        'DEBUG'(refX, RefX, Mode, [Output]).

idx(Site, Idx) -> idx(Site, Idx, false).

% use the atom 'verbose' for this mode to get everything
% use 'log' to log the results
idx(Site, Idx, Mode) -> 'DEBUG'(idx, {Site, Idx}, Mode, []).

'DEBUG'(Type, Payload, Mode, Output) ->

    F = fun() ->

                {XRefX, O2, Pt, Obj}
                    = case Type of
                          idx ->
                              {Site, Idx} = Payload,
                              O1 = io_lib:format("Debugging the Idx ~p "
                                                 ++ "on site ~p",
                                                 [Idx, Site]),
                              NewRefX = new_db_wu:idx_to_xrefXD(Site, Idx),
                              #xrefX{path = P, obj = Ob} = NewRefX,
                              O1a = pp(Site, Idx, NewRefX, Mode, O1),
                              {NewRefX, [[O1a] | Output], P, Ob};
                          refX ->
                              NewX = new_db_wu:refX_to_xrefXD(Payload),
                              io:format("NewX is ~p~nPayload is ~p~n",
                                        [NewX, Payload]),
                              Path = Payload#refX.path,
                              Ob   = Payload#refX.obj,
                              {NewX, Output, Path, Ob}
                      end,
                P2 = hn_util:list_to_path(Pt),
                case XRefX of
                    false ->
                        O2a = io_lib:format("The idx doesn't exist.~n", []),
                        Cs = lists:sort(new_db_wu:read_ref(Payload, inside)),
                        O3 = pretty_print(XRefX, Cs, "The idx contains:", Mode,
                                          [[O2a] | O2]),
                        lists:reverse(O3);
                    _     ->
                        O2a  = io_lib:format("The idx points to ~p (~p) on page ~p",
                                             [Obj, hn_util:obj_to_ref(Obj), P2]),
                        Cs = lists:sort(new_db_wu:read_ref(XRefX, inside)),
                        O3 = pretty_print(XRefX, Cs, "The idx contains:", Mode,
                                          [[O2a] | O2]),
                        lists:reverse(O3)
                end
        end,
    {atomic, Msg} = mnesia:transaction(F),
    case Mode of
        log -> [log(X) || X <- Msg];
        _   -> [io:format(X ++ "~n") || X <- Msg]
    end,
    ok.

log(String) ->
    log(String, "../logs/url.log.txt").

log(String, File) ->
    _Return=filelib:ensure_dir(File),

    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.

pretty_print(XRefX, List, Slogan, Mode, Acc) ->
    Marker = io_lib:format(" ", []),
    Slogan2 = io_lib:format(Slogan, []),
    Ret = pretty_p2(List, Mode, [Marker, Slogan2 | Acc]),
    Ret2 = print_relations(XRefX, Ret),
    [Marker | Ret2].

pretty_p2([], _Mode, Acc) -> ["      no attributes" | Acc];
pretty_p2([{X, Vals} | T], Mode, Acc) when is_record(X, xrefX) ->
    #xrefX{idx = Idx, path = P, obj = O} = X,
    NewO = io_lib:format(" ~p (~p) on ~p:",
                         [O, hn_util:obj_to_ref(O), P]),
    NewOa = io_lib:format(" has the following idx ~p", [Idx]),
    Keys = case Mode of
               verbose -> all;
               _       -> ["formula", "value", "__hasform"]
           end,
    NewO2 = pretty_p3(Keys, Vals, [NewOa, NewO | Acc]),
    NO3 = case lists:keymember("__hasform", 1, Vals) of
              true  -> print_form(X#xrefX{obj = {page, "/"}}, NewO2);
              false -> NewO2
          end,
    NO4 = case lists:keymember("__hasincs", 1, Vals) of
              true  -> print_incs(X, NO3);
              false -> NO3
          end,
    pretty_p2(T, Mode, NO4).

print_relations(#xrefX{site = S} = XRefX, Acc) ->
    case lists:sort(new_db_wu:read_relations(XRefX, read)) of
        []  -> Acc;
        [R] -> Ret = io_lib:format("....has the following relationships:", []),
               print_rel2(S, R, [Ret | Acc])
    end.

print_rel2(S, R, Acc) ->
    O1 = print_rel3(S, R#relation.children,   "children",         Acc),
    O2 = print_rel3(S, R#relation.parents,    "parents",          O1),
    O3 = print_rel3(S, R#relation.infparents, "infinite parents", O2),
    O4 = print_rel3(S, R#relation.z_parents,  "z parents",        O3),
    [io_lib:format("      is it an include? ~p", [R#relation.include]) | O4].

print_rel3(_S, [], Type, Acc) -> [io_lib:format("      no " ++ Type, []) | Acc];
print_rel3(S, OrdDict, Type, Acc) ->
    NewAcc = io_lib:format("      " ++ Type ++ " are:", []),
    print_rel4(S, OrdDict, [NewAcc | Acc]).

print_rel4(_S, [], Acc) -> Acc;
print_rel4(S, [H | T], Acc) ->
    XRefX = new_db_wu:idx_to_xrefXD(S, H),
    NewAcc = [io_lib:format("        ~p on ~p",
                            [hn_util:obj_to_ref(XRefX#xrefX.obj),
                             XRefX#xrefX.path]) | Acc],
    print_rel4(S, T, NewAcc).

pretty_p3([], _Vals, Acc) -> Acc;
pretty_p3([K | T], Vals, Acc) ->
    NewO = case lists:keysearch(K, 1, Vals) of
               false ->
                   Acc;
               {value, {K1, V}} when is_list(V) ->
                   [io_lib:format("~20s: ~p", [K1, esc(V)]) | Acc];
               {value, {K1, V}} ->
                       [io_lib:format("~20s: ~p", [K1, V]) | Acc]
    end,
    pretty_p3(T, Vals, NewO);
% dump all attributes
pretty_p3(all, Vals, Acc) ->
    {Ks, _Vs} = lists:unzip(Vals),
    pretty_p3(Ks, Vals, Acc).

print_incs(XRefX, Acc) ->
    io:format("XRefX is ~p~n", [XRefX]),
    Incs = new_db_wu:read_incsD(XRefX),
    io:format("Incs is ~p~n", [Incs]),
    NewAcc = [io_lib:format("....has the following include records:", [])
              | Acc],
    print_i2(XRefX#xrefX.site, Incs, NewAcc).

print_i2(_Site, [], Acc) -> Acc;
print_i2(Site, [H | T], Acc) ->
    Msg = io_lib:format("      Javascript: ~p reloaded by: ~p~n"
                        ++ "      CSS ~p~n",
                        [H#include.js, H#include.js_reload, H#include.css]),
    print_i2(Site, T, [Msg | Acc]).

print_form(XRefX, Acc) ->
    RefX = hn_util:xrefX_to_refX(XRefX),
    Forms = new_db_wu:matching_forms(RefX, common),
    NewAcc = [io_lib:format("....part of a form consisting of:", []) | Acc],
    print_f2(RefX#refX.site, Forms, NewAcc).

print_f2(_Site, [], Acc) -> Acc;
print_f2(Site, [H | T], Acc) ->
    #form{id={_, _, Lable}} = H,
    XRefX = new_db_wu:idx_to_xrefXD(Site, H#form.key),
    NewAcc = [io_lib:format("      ~p on ~p of ~p called ~p",
                            [hn_util:obj_to_ref(XRefX#xrefX.obj),
                             hn_util:list_to_path(XRefX#xrefX.path),
                             H#form.kind, Lable]) | Acc],
    print_f2(Site, T, NewAcc).

pp(Site, Idx, XRefX, verbose, O) ->
    [I] = new_db_wu:idx_DEBUG(Site, Idx),
    O1 = io_lib:format("local_obj contains ~p ~p ~p~n",
                       [binary_to_term(I#local_obj.path),
                        hn_util:obj_to_ref(I#local_obj.obj),
                        binary_to_term(I#local_obj.revidx)]),
    O2 = io_lib:format("XRefX contains ~p ~p~n",
                       [XRefX#xrefX.path, hn_util:obj_to_ref(XRefX#xrefX.obj)]),

    [[O1] | [[O2] | O]];
pp(_, _, _, _, O) -> O.

% fix up escaping!
esc(X) -> X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Data access stuff
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Debug Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timer_debugD(Site) ->
    Tab = new_db_wu:trans(Site, timer),
    Fun = fun(#timer{idx = Idx, spec = Spec}, []) ->
                   io:format("Debugging timer for ~p with ~p~n", [Idx, Spec]),
                   idx(Site, Idx),
                   []
           end,
    mnesia:foldl(Fun, [], Tab).

dirty_for_zinfD(Site) ->
    Tab1 = new_db_wu:trans(Site, dirty_for_zinf),
    io:format("Dumping dirty_for_zinf table:~n"),
    Fun1 = fun(X, []) ->
                   io:format("Id ~p RefX is dirty: ~p~n",
                             [X#dirty_for_zinf.id, X#dirty_for_zinf.dirty]),
                   []
           end,
    mnesia:foldl(Fun1, [], Tab1).

item_and_local_objsD(Site) ->
    Tab1 = new_db_wu:trans(Site, item),
    io:format("Dumping item table:~n"),
    Fun1 = fun(X, []) ->
                   io:format("Record ~p has Attrs ~p~n",
                             [X#item.idx, binary_to_term(X#item.attrs)]),
                   []
           end,
    mnesia:foldl(Fun1, [], Tab1),
    Tab2 = new_db_wu:trans(Site, local_obj),
    io:format("Dumping local_obj table:~n"),
    Fun2 = fun(X, []) ->
                   io:format("Record ~p is ~p ~p with a reverse index of ~p~n",
                             [X#local_obj.idx, binary_to_term(X#local_obj.path),
                              hn_util:obj_to_ref(X#local_obj.obj),
                              binary_to_term(X#local_obj.revidx)]),
                   []
           end,
    mnesia:foldl(Fun2, [], Tab2).

dump_logs(Site, Idx) ->
    Table = new_db_wu:trans(Site, logging),
    Records = mnesia:read(Table, Idx, read),
    io:format("dumping logs for ~p on ~p~n", [Idx, Site]),
    Fun = fun({logging, I, Ts, Uid, Ac, AcT, Ty, P, O, L}) ->
                  L2 = lists:flatten(binary_to_term(L)),
           io:format("Log: ~p ~p ~p ~p~n> ~p ~p ~p ~p~n> ~p~n",
                     [I, Ts, Uid, Ac, AcT, Ty, P, O, L2])
          end,
    [Fun(X) || X <- Records].
