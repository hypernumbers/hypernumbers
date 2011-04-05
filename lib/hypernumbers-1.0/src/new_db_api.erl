%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       new db work unites
%%%            old hn_db_wu.erl poured in and rewritten
%%% @end
%%% Created :  5 Apr 2011 by gordon@hypernumbers.com

-module(new_db_api).

-include("spriki.hrl").
-include("hypernumbers.hrl").

-export([
         write_attributes/1,
         write_attributes/2,
         write_attributes/3
        ]).

%% @spec write_attributes(RefX :: #refX{}, List) -> ok
%% List = [{Key, Value}]
%% Key = atom()
%% Value = term()
%% @doc writes out all the attributes in the list to the reference.
%%
%% The <code>refX{}</code> can be
%% one of:
%% <ul>
%% <li>a cell</li>
%% <li>a range</li>
%% </ul>

-spec write_attributes([{#refX{}, [tuple()]}]) -> ok.
write_attributes(List) ->
    write_attributes(List, nil, nil).

-spec write_attributes([{#refX{}, [tuple()]}], auth_srv:auth_spec()) -> ok.
write_attributes(List, Uid) ->
    write_attributes(List, Uid, nil).

write_attributes([], _PAr, _VAr) -> ok;
write_attributes(List, PAr, VAr) ->
    [ok = page_srv:page_written(S, P) || {#refX{site = S, path = P}, _} <- List],
    List2 = [{new_db_wu:refX_to_xrefX_create(X), L} || {X, L} <- List],
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  [ok = write_attributes1(XRefX, L, PAr, VAr)
                   || {XRefX, L} <- List2],
                  ok
          end,
    % assumes all refX's are for the same site and page, hmmm...
    {XRefX, _} = hd(List2),
    write_activity(XRefX, Fun, "quiet").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_attributes1(#xrefX{obj = {range, _}} = XRefX, AttrList, PAr, VAr) ->
    List = hn_util:range_to_listx(XRefX),
    [ok = write_attributes1(X, AttrList, PAr, VAr) || X <- List],
    ok;
write_attributes1(#xrefX{site = _S} = XRefX, List, PAr, VAr) ->
    hn_db_wu:write_attrs(XRefX, List, PAr),
    % first up do the usual 'dirty' stuff - this cell is dirty
    case lists:keymember("formula", 1, List) of
       %% true  -> [Rels] = hn_db_wu:read_relations(RefX, read),
       %%          case Rels#relation.children of
       %%              []       -> ok;
       %%              Children -> Ch2 = [hn_db_wu:idx_to_refX(S, X) || X <- Children],
       %%                          hn_db_wu:mark_these_dirty(Ch2, VAr)
       %%          end;
        true  -> hn_db_wu:mark_these_dirty([XRefX], VAr);
        false -> ok
    end,
    % now do the include dirty stuff (ie this cell has had it's format updated
    % so make any cells that use '=include(...)' on it redraw themselves
    ok = hn_db_wu:mark_dirty_for_incl([XRefX], VAr).

init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

-spec write_activity(#xrefX{}, fun(), string() | quiet) -> ok.
write_activity(#xrefX{site = Site} = XRefX, Op, FrontEnd) ->
    Activity = fun() ->
                       Ret = mnesia:activity(transaction, Op),
                       tell_front_end(FrontEnd, XRefX),
                       Ret
               end,
    dbsrv:write_activity(Site, Activity).

tell_front_end(quiet, _XRefX) ->
    ok;
tell_front_end(Type, #xrefX{path = P} = XRefX)
  when Type == "move" orelse Type == "refresh" ->
    Notifications = get('front_end_notify'),
    % the move or refresh notifications are used when a page is changed radically
    % - they tell the front end to request the whole page
    % but if there is a formula on another page referring to a cell on a page with,
    % say an insert or delete, then that page has its formulae rewritten/recalculated
    % that page needs to get details notifications so we pull them out of the process
    % dictionary and fire 'em off here...
    Fun = fun(X) ->
                  case X of
                      {_, #xrefX{path = P}, _} -> false;
                      _                       -> true
                  end
          end,
    Extra = lists:filter(Fun, Notifications),
    Notifications = put('front_end_notify', Extra),
    remoting_reg:notify_refresh(XRefX#xrefX.site, XRefX#xrefX.path),
    % now run the extra notifications
    tell_front_end(extras, XRefX);
tell_front_end(_FnName, _XRefX) ->
    List = lists:reverse(get('front_end_notify')),
    Fun = fun({change, #xrefX{site=S, path=P, obj={page, "/"}}, _Attrs}) ->
                  remoting_reg:notify_refresh(S, P);
             ({change, #xrefX{site=S, path=P, obj=O}, Attrs}) ->
                  remoting_reg:notify_change(S, P, O, Attrs);
             ({style, #xrefX{site=S, path=P}, Style}) ->
                  remoting_reg:notify_style(S, P, Style);
             ({delete_attrs, #xrefX{site=S, path=P, obj=O}, Attrs}) ->
                  remoting_reg:notify_delete_attrs(S, P, O, Attrs)
          end,
    [ok = Fun(X) || X <- List],
    ok.
