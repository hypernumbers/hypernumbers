%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie 
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       A utility module for json
%%%
%%% @end
%%% Created : 22 Mar 2009 by U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%%-------------------------------------------------------------------
-module(json_util).

-export([payload_to_json/1,
         json_to_payload/1,
         jsonify/1,
         unjsonify/1]).

jsonify(List) when is_list(List) ->
    {array, [jsonify(X) || X <- List]};
jsonify({version, Page, Vsn}) ->
    {struct, [{"subtype", "version"},
              {"page",    Page},
              {"vsn",     Vsn}]}.

unjsonify({array, List}) when is_list(List) ->
    [unjsonify(X) || X <- List];
unjsonify({struct, [{"subtype", "version"},
                    {"page",    Page},
                    {"vsn",     Vsn}]}) ->
    {version, Page, Vsn}.

%% @spec payload_to_json(Term) -> Json
%% @doc payload_to_json takes a Term representing a payload used in the 
%% horizontal api and makes it into a mochijson encodable
payload_to_json({Type, {column, {X1, X2}}})
  when (Type == insert) orelse (Type == delete) ->
    {insert, {struct, [{"type", Type},
                       {"ref",  "column"},
                       {"X1",   X1},
                       {"X2",   X2}]}};
payload_to_json({Type, {row, {Y1, Y2}}})
  when (Type == insert) orelse (Type == delete) ->
    {insert, {struct, [{"type", Type},
                       {"ref",  "row"},
                       {"Y1",   Y1},
                       {"Y2",   Y2}]}};
payload_to_json({Type, {range, {X1, Y1, X2, Y2}}, Displacement})
  when (Type == insert) orelse (Type == delete) ->
    {insert, {struct, [{"type",        Type},
                       {"ref",         "range"},
                       {"X1",           X1},
                       {"Y1",           Y1},
                       {"X2",           X2},
                       {"Y2",           Y2},
                       {"displacement", Displacement}]}};
payload_to_json({Type, {cell, {X, Y}}, Displacement})
  when (Type == insert) orelse (Type == delete) ->
    {insert, {struct, [{"type",         Type},
                       {"ref",          "cell"},
                       {"X",            X},
                       {"Y",            Y},
                       {"displacement", Displacement}]}};
payload_to_json({new_value, Value, DepTree}) ->
    DepTree2 = {array, flatten_deptree(DepTree)},
    Value2 = case Value of
                 {errval, Error} -> Error;
                 _               -> Value
             end,
    {new_value, {struct, [{"type",            "new_value"},
                          {"value",           Value2},
                          {"dependency-tree", DepTree2}]
                }}.

json_to_payload({struct, [{"type", Type},
                          {"ref",  "column"},
                          {"X1",   X1},
                          {"X2",   X2}]})
  when (Type == "insert") orelse (Type == "delete") ->
    {list_to_existing_atom(Type), {column, {X1, X2}}};
json_to_payload({struct, [{"type", Type},
                          {"ref",  "row"},
                          {"Y1",   Y1},
                          {"Y2",   Y2}]})
  when (Type == "insert") orelse (Type == "delete") ->
    {list_to_existing_atom(Type), {row, {Y1, Y2}}};
json_to_payload({struct, [{"type",        Type},
                          {"ref",         "range"},
                          {"X1",           X1},
                          {"Y1",           Y1},
                          {"X2",           X2},
                          {"Y2",           Y2},
                          {"displacement", Displacement}]})
  when (Type == "insert") orelse (Type == "delete") ->
    {list_to_existing_atom(Type), {range, {X1, Y1, X2, Y2}}, Displacement};
json_to_payload({struct, [{"type",        Type},
                          {"ref",         "cell"},
                          {"X",            X},
                          {"Y",            Y},
                          {"displacement", Displacement}]})
  when (Type == "insert") orelse (Type == "delete") ->
    {list_to_existing_atom(Type), {cell, {X, Y}}, Displacement};    
json_to_payload({struct, [{"type",            "new_value"},
                          {"value",           Value},
                          {"dependency-tree", DepTree}]}) ->
    {new_value, Value, DepTree}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal funtions                                                        %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flatten_deptree({xml, DepTree}) -> flatten_deptree1(DepTree, []);
flatten_deptree([])             -> [].

flatten_deptree1([], Acc)      -> Acc;
flatten_deptree1([H | T], Acc) -> {url, [{type, _Type}], [Url]} = H,
                                  flatten_deptree1(T, [Url| Acc]).
