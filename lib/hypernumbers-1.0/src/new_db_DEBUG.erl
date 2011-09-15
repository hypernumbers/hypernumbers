%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers
%%% @doc       Deubgging functions for the database
%%%            help you trace URL's Idx's etc, etc
%%% @end
%%% Created : 25 Aug 2011 by <gordon@hypernumbers.dev>

-module(new_db_DEBUG).

-include("spriki.hrl").

-export([
         dump_lost_idxs/1,
         tick/0,
         timer_DEBUG/1,
         dirty_for_zinf_DEBUG/1,
         item_and_local_objs_DEBUG/1,
         url_DEBUG/1,
         url_DEBUG/2,
         idx_DEBUG/2,
         idx_DEBUG/3,
         raw_idx_DEBUG/2,
         raw_url_DEBUG/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Debug Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
                      _ -> io:format(".")
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

timer_DEBUG(Site) ->
    F = fun() ->
                timer_debugD(Site)
        end,
    mnesia:transaction(F).

item_and_local_objs_DEBUG(Site) ->
    F = fun() ->
                item_and_local_objs_DEBUGD(Site)
        end,
    mnesia:transaction(F).

dirty_for_zinf_DEBUG(Site) ->
    F = fun() ->
                dirty_for_zinf_DEBUGD(Site)
        end,
    mnesia:transaction(F).

raw_url_DEBUG(Url) ->
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
                                       [R#local_obj.obj,
                                        binary_to_term(R#local_obj.path),
                                        binary_to_term(R#local_obj.revidx)])
                             end
          end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

raw_idx_DEBUG(Site, Idx) ->
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
                            binary_to_term(R1#local_obj.path), R1#local_obj.obj,
                                     binary_to_term(R1#local_obj.revidx)]),
                          Tab2 = new_db_wu:trans(Site, item),
                          [R2] = mnesia:read(Tab2, Idx, read),
                          io:format("Raw item is ~p~n", [R2]),
                          Attrs2 = binary_to_term(R2#item.attrs),
                          io:format("item: idx ~p~n attrs ~p~n",
                                    [R2#item.idx, Attrs2])
                  end
          end,
    mnesia:transaction(Fun).

url_DEBUG(Url) -> url_DEBUG(Url, quiet).

% use the atom 'verbose' for this mode to get everything
% use 'log' to log the results
url_DEBUG(Url, Mode) -> RefX = hn_util:url_to_refX(Url),
                        Output = io_lib:format("Url ~p being debugged", [Url]),
                        'DEBUG'(refX, RefX, Mode, [Output]).

idx_DEBUG(Site, Idx) -> idx_DEBUG(Site, Idx, false).

% use the atom 'verbose' for this mode to get everything
% use 'log' to log the results
idx_DEBUG(Site, Idx, Mode) -> 'DEBUG'(idx, {Site, Idx}, Mode, []).

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
                        O3 = pretty_print(Cs, "The idx contains:", Mode,
                                          [[O2a] | O2]),
                        lists:reverse(O3);
                    _     ->
                        O2a  = io_lib:format("The idx points to ~p on page ~p",
                                             [Obj, P2]),
                        Cs = lists:sort(new_db_wu:read_ref(XRefX, inside)),
                        O3 = pretty_print(Cs, "The idx contains:", Mode,
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

pretty_print(List, Slogan, Mode, Acc) ->
    Marker = io_lib:format(" ", []),
    Slogan2 = io_lib:format(Slogan, []),
    Ret = pretty_p2(List, Mode, [Marker, Slogan2 | Acc]),
    [Marker | Ret].

pretty_p2([], _Mode, Acc) -> Acc;
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
    NO5 = print_relations(X, NO4),
    pretty_p2(T, Mode, NO5).

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
                            [XRefX#xrefX.obj, XRefX#xrefX.path]) | Acc],
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
                            [XRefX#xrefX.obj,
                             hn_util:list_to_path(XRefX#xrefX.path),
                             H#form.kind, Lable]) | Acc],
    print_f2(Site, T, NewAcc).

pp(Site, Idx, XRefX, verbose, O) ->
    [I] = new_db_wu:idx_DEBUG(Site, Idx),
    O1 = io_lib:format("local_obj contains ~p ~p ~p~n",
                       [binary_to_term(I#local_obj.path),
                        I#local_obj.obj,
                        binary_to_term(I#local_obj.revidx)]),
    O2 = io_lib:format("XRefX contains ~p ~p~n",
                       [XRefX#xrefX.path, XRefX#xrefX.obj]),

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
                   idx_DEBUG(Site, Idx),
                   []
           end,
    mnesia:foldl(Fun, [], Tab).

dirty_for_zinf_DEBUGD(Site) ->
    Tab1 = new_db_wu:trans(Site, dirty_for_zinf),
    io:format("Dumping dirty_for_zinf table:~n"),
    Fun1 = fun(X, []) ->
                   io:format("Id ~p RefX is dirty: ~p~n",
                             [X#dirty_for_zinf.id, X#dirty_for_zinf.dirty]),
                   []
           end,
    mnesia:foldl(Fun1, [], Tab1).

item_and_local_objs_DEBUGD(Site) ->
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
                              X#local_obj.obj, binary_to_term(X#local_obj.revidx)]),
                   []
           end,
    mnesia:foldl(Fun2, [], Tab2).

%idx_DEBUG(S, Idx) -> mnesia:read(trans(S, local_obj), Idx, read).

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
