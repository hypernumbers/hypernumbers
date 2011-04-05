%%% @author    Gordon Guthrie <
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       new db api (old hn_db_api poured in ang rewritten)
%%%
%%% @end
%%% Created :  5 Apr 2011 by gordon@hypernumbers

-module(new_db_wu).


-define(to_xml_str, simplexml:to_xml_string).
-define(to_refX, hn_util:refX_from_index).

-define(lt, list_to_tuple).
-define(lf, lists:flatten).
-define(dict, orddict:orddict()).

-include("spriki.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-export([
         refX_to_xrefX/1,
         refX_to_xrefX_create/1,
         write_attrs/2, write_attrs/3
         ]).

%% refX_to_xrefX reads the index of an object AND RETURNS 'false'
%% IF IT DOESN'T EXIST
-spec refX_to_xrefX(#refX{}) -> pos_integer() | false.
refX_to_xrefX(#refX{site = S, path = P, obj = O}) ->
    Table = trans(S, local_obj),
    RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
    case mnesia:index_read(Table, term_to_binary(RevIdx), revidx) of
        [I] -> #xrefX{idx = I#local_obj.idx, site = S, path = P, obj = O};
        _   -> false
    end.

-spec refX_to_xrefX_create(#refX{}) -> pos_integer().
%% @doc refX_to_xrefX_create refX_to_xrefX_create gets the index of an object
%% AND CREATES IT IF IT DOESN'T EXIST
refX_to_xrefX_create(#refX{site = S, type = Ty, path = P, obj = O} = RefX) ->
    case refX_to_xrefX(RefX) of
        false -> Idx = util2:get_timestamp(),
                 RevIdx = hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O),
                 Rec = #local_obj{path = term_to_binary(P), type = Ty, obj = O,
                                  idx = Idx, revidx = term_to_binary(RevIdx)},
                 ok = mnesia:write(trans(S, local_obj), Rec, write),
                 #xrefX{idx = Idx, site = S, path = P, obj = O};
        XrefX -> XrefX
    end.

-spec write_attrs(#xrefX{}, [{string(), term()}]) -> ?dict.
write_attrs(XRefX, NewAttrs) -> write_attrs(XRefX, NewAttrs, nil).

-spec write_attrs(#xrefX{}, [{string(), term()}], auth_srv:auth_spec())
-> ?dict.
write_attrs(XRefX, NewAs, AReq) ->
    Op = fun(Attrs) ->
                 Is_Formula = lists:keymember("formula", 1, NewAs),
                 Has_Form = orddict:is_key("__hasform", Attrs),
                 Has_Incs = orddict:is_key("__hasincs", Attrs),
                 Attrs2 = case Is_Formula of
                              true ->
                                  A3 = case Has_Form of
                                           true ->
                                               unattach_form(XRefX),
                                               orddict:erase("__hasform", Attrs);
                                           false  ->
                                               Attrs
                                       end,
                                  case Has_Incs of
                                      true ->
                                          delete_incs(XRefX),
                                          orddict:erase("__hasincs", A3);
                                      false  ->
                                          A3
                                  end;
                              false -> Attrs
                          end,
                 {clean, process_attrs(NewAs, XRefX, AReq, Attrs2)}
         end,
    apply_to_attrs(XRefX, Op, write, AReq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec process_attrs([{string(), term()}], #xrefX{}, auth_srv:auth_spec(), ?dict)
-> ?dict.
process_attrs([], _XRefX, _AReq, Attrs) ->
    Attrs;
process_attrs([{"formula",Val} | Rest], XRefX, AReq, Attrs) ->
    Attrs2 =
        case superparser:process(Val) of
            {formula, Fla} ->
                write_formula1(XRefX, Fla, Val, AReq, Attrs);
            [NVal, Align, Frmt] ->
                write_formula2(XRefX, Val, NVal, Align, Frmt, Attrs)
        end,
    ok = mark_dirty_for_zinf(XRefX),
    process_attrs(Rest, XRefX, AReq, Attrs2);
process_attrs([A = {Key, Val} | Rest], XRefX, AReq, Attrs) ->
    Attrs2  = case ms_util2:is_in_record(magic_style, Key) of
                  true  -> apply_style(XRefX, A, Attrs);
                  false -> orddict:store(Key, Val, Attrs)
              end,
    process_attrs(Rest, XRefX, AReq, Attrs2).

write_formula1(XRefX, Fla, Formula, AReq, Attrs) ->
    Rti = xrefX_to_rti(XRefX, AReq, false),
    case muin:run_formula(Fla, Rti) of
        % General error condition
        {error, {errval, Error}} ->
            % there might have been a preview before - nuke it!
            Attrs2 = orddict:erase("preview", Attrs),
            % mebbies there was incs, nuke 'em
            write_error_attrs(Attrs2, XRefX, Formula, Error);
        % the formula returns as rawform
        {ok, {Pcode, {rawform, RawF, Html}, Parents, InfParents, Recompile}} ->
            {Trans, Label} = RawF#form.id,
            Form = RawF#form{id = {XRefX#refX.path, Trans, Label}},
            ok = attach_form(XRefX, Form),
            Label2 = case Label of
                         "_" -> "Submit Button";
                         _   -> Label
                     end,
            Attrs2 = orddict:store("__hasform", t, Attrs),
            Attrs3 = orddict:store("preview", {Label2, 1, 1}, Attrs2),
            % mebbies there was incs, nuke 'em
            write_formula_attrs(Attrs3, Ref, Formula, Pcode, Html,
                                {Parents, false}, InfParents, Recompile);
        % the formula returns a web control
        {ok, {Pcode, {webcontrol, {Payload, {Title, Wd, Ht, Incs}}, Res},
              Parents, InfParents, Recompile}} ->
            {Trans, Label} = Payload#form.id,
            Form = Payload#form{id = {XRefX#refX.path, Trans, Label}},
            ok = attach_form(XRefX, Form),
            Attrs2 = orddict:store("__hasform", t, Attrs),
            Blank = #incs{},
            Attrs3 = case Incs of
                         Blank -> Attrs2;
                         _     -> ok = update_incs(Ref, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            Attrs4 = orddict:store("preview", {Title, Wd, Ht}, Attrs3),
            write_formula_attrs(Attrs4, Ref, Formula, Pcode, Res,
                                {Parents, false}, InfParents, Recompile);
        % the formula returns a web-hingie that needs to be previewed
        {ok, {Pcode, {preview, {PreV, Wd, Ht, Incs}, Res}, Pars,
              InfPars, Recompile}} ->
            Attrs2 = orddict:store("preview", {PreV, Wd, Ht}, Attrs),
            Blank = #incs{},

            Attrs3 = case Incs of
                         Blank -> Attrs2;
                         _     -> ok = update_incs(XRefX, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            Attrs4 = case {Ht, Wd} of
                         {1, 1} -> orddict:erase("merge", Attrs3);
                         _      -> orddict:store("merge", {struct,
                                                           [{"right", Wd - 1},
                                                            {"down",  Ht - 1}]},
                                                 Attrs3)
                     end,
            write_formula_attrs(Attrs4, XRefX, Formula, Pcode, Res,
                                {Pars, false}, InfPars, Recompile);
        % special case for the include function (special dirty!)
        {ok, {Pcode, {include, {PreV, Ht, Wd}, Res}, Pars, InfPars, Recompile}} ->
            Attrs2 = orddict:store("preview", {PreV, Ht, Wd}, Attrs),
            % with include you might need to bring incs through from
            % whatever is included so some jiggery might be required on the pokey
            Attrs3 = bring_through(Attrs2, XRefX, Pars),
            % mebbies there was incs, nuke 'em
            write_formula_attrs(Attrs3, XRefX, Formula, Pcode, Res,
                                {Pars, true}, InfPars, Recompile);
        % normal functions with a resize
        {ok, {Pcode, {resize, {Wd, Ht, Incs}, Res}, Parents,
              InfParents, Recompile}} ->
            % there might have been a preview before - nuke it!
            Attrs2 = orddict:erase("preview", Attrs),
            Blank = #incs{},
            Attrs3 = case Incs of
                         Blank -> Attrs2;
                         _     -> ok = update_incs(Ref, Incs),
                                  orddict:store("__hasincs", t, Attrs2)
                     end,
            Attrs4 = case {Ht, Wd} of
                         {1, 1} -> orddict:erase("merge", Attrs3);
                         _      -> orddict:store("merge", {struct,
                                                          [{"right", Wd - 1},
                                                           {"down",  Ht - 1}]},
                                                Attrs3)
                     end,
            write_formula_attrs(Attrs4, Ref, Formula, Pcode, Res,
                                {Parents, false}, InfParents, Recompile);
        % bog standard function!
        {ok, {Pcode, Res, Parents, InfParents, Recompile}} ->
            % there might have been a preview before - nuke it!
            Attrs2 = orddict:erase("preview", Attrs),
            % mebbies there was incs, nuke 'em
            ok = update_incs(Ref, #incs{}),
            write_formula_attrs(Attrs2, Ref, Formula, Pcode, Res,
                                {Parents, false}, InfParents, Recompile)
    end.

write_formula2(Ref, OrigVal, {Type, Val},
               {"text-align", Align},
               {"format", Format}, Attrs) ->
    Formula = case Type of
                  quote    -> [$' | Val];
                  datetime -> OrigVal;
                  float    -> OrigVal;
                  int      -> OrigVal;
                  _        -> hn_util:text(Val) end,
    {NewLocPs, _NewRemotePs} = split_local_remote([]),
    ok = set_relations(Ref, NewLocPs, [], false),
    Attrs2 = add_attributes(Attrs, [{"__default-align", Align},
                                    {"__rawvalue", Val},
                                    {"formula", Formula}]),
    % there might have been a preview before - nuke it!
    Attrs3 = orddict:erase("preview", Attrs2),
    Attrs4 = orddict:erase("__ast", Attrs3),
    Attrs5 = orddict:erase("__recompile", Attrs4),
    case Format of
        "null" -> Attrs5;
        _      -> orddict:store("format", Format, Attrs5)
    end.

write_error_attrs(Attrs, XRefX, Formula, Error) ->
    ok = set_relations(XRefX, [], [], false),
    add_attributes(Attrs, [{"formula", Formula},
                           {"__rawvalue", {errval, Error}},
                           {"__ast", []}]).

-spec set_relations(#refX{}, [#refX{}], [#refX{}], boolean()) -> ok.
set_relations(#xrefX{idx = CellIdx, site = Site}, FiniteParents,
              InfParents, IsIncl) ->
    Tbl = trans(Site, relation),
    Rel = case mnesia:read(Tbl, CellIdx, write) of
              [R] -> R;
              []  -> #relation{cellidx = CellIdx}
          end,
    Rel2 = set_parents(Tbl, Site, Rel, FiniteParents, InfParents, IsIncl),
    mnesia:write(Tbl, Rel2, write).

-spec set_parents(atom(), list(), #relation{}, [#refX{}],
                  [#refX{}], boolean()) -> #relation{}.
set_parents(Tbl,
            Site,
            Rel = #relation{cellidx = CellIdx,
                            parents = CurParents,
                            infparents = CurInfPars
                           },
            Parents,
            InfParents,
            IsIncl) ->
    Fun = fun(X) ->
                  XRefX = xrefX_to_idx_create(X),
                  XRefX.idx
          end,
    PIdxs = lists:map(Fun, Parents),
    InfPIdxs = lists:map(Fun, InfParents),
    NewParentIdxs = ordsets:from_list(PIdxs),
    LostParents = ordsets:subtract(CurParents, ParentIdxs),
    [del_child(P, CellIdx, Tbl) || P <- LostParents],
    [ok = add_child(P, CellIdx, Tbl, IsIncl) || P <- NewParentIdxs],
    NewInfParIdxs = ordsets:from_list(InfPIdxs),
    CurInfParsRefXs = [X#xrefX.idx || X <- CurInfPars],
    ok = handle_infs(CellIdx, Site, InfParents, CurInfParsRefXs),
    Rel#relation{parents = ParentIdxs, infparents = NewInfParIdxs}.

%% @doc Make a #muin_rti record out of an xrefX record and a flag that specifies
%% whether to run formula in an array context.
xrefX_to_rti(#xrefX{site = S, path = P, obj = {cell, {C, R}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P,
              col = C, row = R,
              array_context = AC,
              auth_req = AR};
xrefX_to_rti(#xrefX{site = S, path = P, obj = {range, {C, R, _, _}}}, AR, AC)
  when is_boolean(AC) ->
    #muin_rti{site = S, path = P,
              col = C, row = R,
              array_context = AC,
              auth_req = AR}.


delete_incs(XRefX#refX{idx = Idx, site = S}) when is_record(XRefX, xrefX) ->
    bTbl = trans(S, include),
    mnesia:delete(Tbl, Idx, write).

-spec unattach_form(#xrefX{}) -> ok.
unattach_form(#xrefX{site = Site, idx = Idx}) ->
    Tbl = trans(Site, form),
    mnesia:delete(Tbl, Idx, write).

%% Apply to attrs does the actual work of modifying a target ref. The
%% behaviour of the modification is determined by the passed in 'Op'
%% function. Upon completion of 'Op' the post_process function is
%% applied, which sets formats and styles as necessary.
%% the op function returns a tuple of {Status, Ref, Attrs} where
%% Status is either 'clean' or 'dirty'
-spec apply_to_attrs(#xrefX{}, fun((?dict) -> ?dict),
                                 atom(), auth_srv:uid()) -> ?dict.
apply_to_attrs(#xrefX{idx = Idx, site = Site} = XRefX, Op, Action, Uid) ->
    Table = trans(Site, item),
    Attrs = case mnesia:read(Table, Idx, write) of
                [#item{attrs=A}] -> binary_to_term(A);
                _                -> orddict:new()
            end,
    {Status, Attrs2} = Op(Attrs),
    Attrs3 = post_process(Ref, Attrs2),
    % the Op may have shifted the RefX that the Idx now points to, so look it
    % up again for the log
    NewXRefX = idx_to_xrefX(Site, Idx),
    ok = log_write(Idx, NewXRefX, Attrs, Attrs2, Action, Uid),
    Item = #item{idx = Idx, attrs = term_to_binary(Attrs3)},
    case deleted_attrs(Attrs, Attrs3) of
        []   -> ok;
        List -> tell_front_end_delete_attrs(XRefX, List)
    end,
    tell_front_end_change(XRefX, Attrs3),
    mnesia:write(Table, Item, write),
    {Status, Attrs3}.

-spec tell_front_end_delete_attrs(#xrefX{}, list()) -> ok.
tell_front_end_delete_attrs(XRefX, Attrs) ->
    Tuple = {delete_attrs, XRefX, Attrs},
    tell_front_end1(Tuple).

-spec tell_front_end_style(#xrefX{}, #style{}) -> ok.
tell_front_end_style(XRefX, Style) ->
    Tuple = {style, XRefX, Style},
    tell_front_end1(Tuple).

-spec tell_front_end_change(#xrefX{}, ?dict) -> ok.
tell_front_end_change(XRefX, Attrs) ->
    Tuple = {change, XRefX, Attrs},
    tell_front_end1(Tuple).

tell_front_end1(Tuple) ->
    List = get('front_end_notify'),
    put('front_end_notify', [Tuple | List]),
    ok.

idx_to_xrefX(S, Idx) ->
    case mnesia:read(trans(S, local_obj), Idx, read) of
        [Rec] -> #local_obj{path = P,  obj = O} = Rec,
                 #xrefX{idx = Idx, site = S,
                        path = binary_to_term(P), obj = O};
        []    -> {error, id_not_found, Idx}
    end.

%% converts a tablename into the site-specific tablename
trans(Site, TableName) when is_atom(TableName) ->
    Prefix = get_prefix(Site),
    list_to_atom(Prefix ++ "&" ++ atom_to_list(TableName));
trans(Site, Record) when is_tuple(Record) ->
    NRec = trans(Site, element(1, Record)),
    setelement(1, Record, NRec).

get_prefix(R) when is_record(R, refX) ->
    get_prefix(R#refX.site);
get_prefix("http://"++Site) ->
    [case S of $: -> $&; S  -> S end
     || S <- Site].

mark_dirty_for_zinf(#xrefX{site = S, obj = {cell, _}} = XRefX) ->
    Tbl = trans(S, dirty_for_zinf),
    mnesia:write(Tbl, #dirty_for_zinf{dirty = XRefX}, write);
% any other refs ignore 'em
mark_dirty_for_zinf(XRefX) when is_record(XRefX, xrefX) -> ok.
