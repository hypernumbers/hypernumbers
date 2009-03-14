%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       <h1>Overview</h1>
%%% 
%%%            This module provides the data access api.
%%%            Each function in this should call functions from
%%%            {@link hn_db_wu} which provides work units and it
%%%            should wrap them in an mnesia transaction.
%%%            
%%%            mnesia MUST NOT be called from any function in
%%%            this module.
%%%            
%%%            It makes extensive use of #refX{} records which can
%%%            exist in the the following flavours:
%%%            <ul>
%%%            <li>cell</li>
%%%            <li>range</li>
%%%            <li>column</li>
%%%            <li>row</li>
%%%            <li>page</li>
%%%            </ul>
%%%            These flavours are distingished by the obj attributes
%%%            which have the following forms:
%%%            <ul>
%%%            <li><code>{cell, {X, Y}}</code></li>
%%%            <li><code>{range, {X1, Y1, X2, Y2}}</code></li>
%%%            <li><code>{column, {X1, X2}}</code></li>
%%%            <li><code>{row, {Y1, Y2}}</code></li>
%%%            <li><code>{page, "/"}</code></li>
%%%            </ul>
%%%            
%%%            <h2>Gotcha's</h2>
%%%            
%%%            There is an event cycle artefact that relates to the use of the 
%%%            hypernumbers() function.
%%%            
%%%            When a hypernumber is used is a formula muin asks for the value
%%%            if the remote cell isn't used a hypernumber is setup AND THE 
%%%            NEW REMOTE LINK IS WRITTEN
%%%            
%%%            If the remote cell is <i>already used</i> by another cell the value
%%%            is and there is no remote link a remote like is also written as is
%%%            a notify_back message to add a matching remote link on the remote server
%%%            
%%%            If the remote cell is <i>already used</i> by another cell and the
%%%            remote link is already written, nothing happens...
%%%            
%%%            This is a bit messy, but the alternative is a race condition :(
%%%            
%%%            The remote site only gets a notification that a new cell is linking
%%%            in when the (internal) function {@link update_rem_parents} 
%%%            runs...
%%%            
%%%            <h2>Notes On Terminology</h2>
%%% 
%%%            Various terms that describe the relationships between
%%%            different cells are shown below:
%%%            <img src="./terminology.png" />
%%% 
%%% @TODO should we have a subpages #refX egt {subpages, "/"}
%%% which would alllow you to specify operations like delete and copy
%%% on whole subtrees?
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_api).

-include("spriki.hrl").

-define(mn_ac,mnesia:activity).
-define(mn_sl,mnesia:select).
-define(copy, hn_db_wu:copy_cell).
-define(counter, mnesia:dirty_update_counter).

-export([
         write_attributes/2,
         write_last/1,
         % write_permission/2,
         % write_style/2,
         read_attributes/2,
         read/1,
         read_styles/1,
         % read_permissions/1,
         % update_style/2,
         recalculate/1,
         reformat/1,
         drag_n_drop/2,
         copy_n_paste/2,
         cut_n_paste/2,
         % copy_page/2,
         insert/1,
         insert/2,
         delete/1,
         delete/2,
         clear/1,
         clear/2,
         % delete_permission/1,
         % delete_style/1,
         % notify/5,
         % notify_back/3,
         notify_back_create/2,
         read_incoming_hn/2,
         write_remote_link/3,
         handle_notify/7,
         handle_notify_back/4,
         handle_dirty_cell/1,
         handle_dirty_notify_in/1,
         handle_dirty_notify_out/5,
         handle_dirty_notify_back_in/3,
         handle_dirty_notify_back_out/4
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec handle_dirty_cell(RefX::#refX{}) -> {ok, ok}
%% @doc handles a dirty cell.
%% Silently fails if the cell is part of a shared range
%% @todo extend this to a dirty shared formula
%% @todo stop the silent fail!
handle_dirty_cell(#refX{obj = {cell, _}} = RefX)  ->
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Logging code                                                      %
    % #index{path = Path, row = Row, column = Col} = Index,             %
    % Str=string:join(Path,"/")++" Row "++integer_to_list(Row)++" Col " %
    %     ++integer_to_list(Col),                                       %
    % bits:log(Str),                                                    %
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Fun =
        fun() ->
                {ok, ok} =
                    case hn_db_wu:read_attrs(RefX, ["__shared"]) of
                        [] -> [{C, KV}] = hn_db_wu:read_attrs(RefX, 
                                                              ["formula"]),
                              hn_db_wu:write_attr(C, KV);
                        _  -> {ok, ok}
                    end,
                {ok, ok} = hn_db_wu:clear_dirty_cell(RefX),
                ok
        end,
    ok = mnesia:activity(transaction, Fun),
    {ok, ok}.

%% @spec handle_dirty_notify_in(Parent::#refX{}) -> {ok, ok}
%% DepTree = list()
%% @doc handles a dirty notify in.
%% The reference must be to a cell
%% @todo implement dirty ranges/functions/queries and stuff
handle_dirty_notify_in(Parent) when is_record(Parent, refX) ->
    % read the incoming remote children and mark them dirty
    Fun = fun() ->
                  Cells = hn_db_wu:read_remote_children(Parent, incoming),
                  Fun2 = fun(X) ->
                                 [{RefX, KV}] = hn_db_wu:read_attrs(X, ["formula"]),
                                 {ok, ok} = hn_db_wu:write_attr(RefX, KV)
                         end,
                  [{ok, ok} = Fun2(X)  || X <- Cells],
                  {ok, ok} = hn_db_wu:clear_dirty_notify_in(Parent),
                  {ok, ok}
          end,
    {ok, ok} = mnesia:activity(transaction, Fun),
    {ok, ok}.

%% @spec handle_dirty_notify_out(Parent::#refX{}, Outgoing, Value, 
%% DepTree, Timestamp) -> {ok, ok}
%% DepTree = list()
%% @doc handles a dirty notify out.
%% The reference must be to a cell
%% @todo implement dirty ranges/functions/queries and stuff
handle_dirty_notify_out(Parent, Outgoing, Value, DepTree, Timestamp) ->
    {ok, ok} = horiz_api:notify(Parent, Outgoing, Value, DepTree),
    % now delete the dirty outgoing hypernumber
    PIdx = hn_util:index_from_refX(Parent),
    Rec = #dirty_notify_out{parent = PIdx, outgoing = Outgoing,
                             value = Value, timestamp = Timestamp},
    Fun3 = fun() ->
                   ok = mnesia:delete_object(Rec)
           end,
    ok = mnesia:activity(transaction, Fun3),
    {ok, ok}.

%% @spec handle_dirty_notify_back_in(Parent::#refX{}, Child::#refX{}, Change)
%% -> {ok, ok}
%% @doc handles a dirty_notify_back_in message.
%% Both the parent and the child references must be cell references
%% @TODO list the full set of possible change messages and how they should
%% be handled in the docos
handle_dirty_notify_back_in(P, C, Type)
  when is_record(P, refX), is_record(C, refX), Type =:= "new child" ->
    Fun1 = fun() ->
                   hn_db_wu:read_incoming_hn(P)
           end,
    Hn = mnesia:activity(transaction, Fun1),
    #incoming_hn{biccie = Biccie} = Hn,
    {ok, ok} = horiz_api:notify_back(P, C, Type, Biccie),
    Fun = fun() ->
                  {ok, ok} = hn_db_wu:clear_dirty_notify_back_in(P, C, Type)
          end,
    mnesia:activity(transaction, Fun).

%% @spec handle_dirty_notify_back_out(Parent::#refX{}, Child::#ref{}, Type, Biccie)
%% -> {ok, ok}
%% @doc handles a dirty notify back out event.
%% Both the partent and child references must point to a cell
%% The types of notification back supported are:
%% <ul>
%% <li>"unregister"</li>
%% <li>"new child"</li>
%% </ul>
handle_dirty_notify_back_out(P, C, Type, B)
    when is_record(P, refX), is_record(C, refX), Type =:= "unregister" ->
    Fun =
        fun() ->
                case hn_db_wu:verify_biccie(P, C, B) of
                    true ->
                        {ok, ok} = hn_db_wu:unregister_outgoing_hn(P, C, B);
                    _    ->
                        {ok, ok}
                end,
                {ok, ok} = hn_db_wu:clear_dirty_notify_back_out(P, C, Type)
        end,
    mnesia:actiity(transaction, Fun);
handle_dirty_notify_back_out(P, C, Type, Biccie)
    when is_record(P, refX), is_record(C, refX), Type =:= "new child" ->
    Fun = fun() ->
                  io:format("In handle_dirty_nofity_back_out~n-P is ~p~n-"++
                            "C is ~p~n-Type is outgoing...~n", [P, C]),
                  {ok, ok} = hn_db_wu:write_remote_link(P, C, outgoing),
                  {ok, ok} = hn_db_wu:clear_dirty_notify_back_out(P, C, Type)
          end,
    mnesia:activity(transaction, Fun).

%% @spec handle_notify(Parent::#refX{}, Child::#refX{}, Type, Value, 
%% DepTree, Biccie, Version) -> {ok, ok}
%% DepTree = list()
%% @doc handles a notify message.
%% The parent reference must be cell references.
%% If this value doesn't match the Biccie on record this update will be logged and
%% will silently fail
%% Version is the page version of the remote page. If this version is newer that the
%% page version stored here it will trigger a structural replay of page updates
%% @todo log the biccie failures
%% @todo write the page version handling stuff
handle_notify(Parent, Child, Type, Value, DepTree, Biccie, Version)
  when is_record(Parent, refX), is_record(Child, refX),
       (Type =:= "change" orelse Type =:= "create") ->
    F = fun() ->
                case hn_db_wu:verify_biccie(Parent, Child, Biccie) of
                    true ->
                        {ok, ok} = hn_db_wu:update_inc_hn(Parent, Value, DepTree,
                                                          Biccie, Version),
                        {ok, ok} = hn_db_wu:mark_notify_in_dirty(Parent);
                    false -> {ok, ok}
                end,
                ok
        end,
    ok = mnesia:activity(transaction, F),
    {ok, ok}.
    
%% @spec handle_notify_back(Parent::#refX{}, Child::#refX{}, Biccie, Type) -> 
%% {ok, ok}
%% @doc handles a notify_back message.
%% The parent and child references must be cell references.
handle_notify_back(P, C, B, Type)
  when is_record(P, refX), is_record(C, refX) ->
    Fun =
        fun() ->
                case hn_db_wu:verify_biccie(P, C, B) of
                    true -> hn_db_wu:mark_notify_back_out_dirty(P, C, Type);
                    _    -> {ok, ok}
                end
          end,
    mnesia:activity(transaction, Fun).

%% @spec write_remote_link(Parent::#refX{}, Child::#refX{}, Type) -> {ok, ok}
%% @doc writes a remote link
write_remote_link(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX) ->
    Fun = fun() ->
                  io:format("hn_db_api:write_remote_link~n-Parent is ~p~n-"++
                            "Child is ~p~n-Type is ~p~n", [Type]),
                  hn_db_wu:write_remote_link(Parent, Child, Type)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read_incoming_hn(Parent::#refX{}, Child::#refX{}) -> {ok, ok}
%% @doc gets the value of an incoming hypernumber
%% Parent is a reference to a remote cell only
%% Child is a reference to a local cell only
%% If the hypernumber requested hasn't been set up yet, this function will
%% trigger a creation process and return 'blank' for the moment... when the
%% hypernumber is set up the 'correct' value will come through as per normal...
read_incoming_hn(P, C) when is_record(P, refX),is_record(C, refX) ->
    F = fun() ->
                case hn_db_wu:read_incoming_hn(P) of
                    [] ->
                        {ok, ok} = hn_db_wu:mark_inc_create_dirty(P, C),
                        % need to write a link
                        io:format("In hn_db_api:read_incoming_hn~n-P is ~p~n-"++
                                  "C is ~p~n-and type is incoming~n", [P, C]),
                        {ok, ok} = hn_db_wu:write_remote_link(P, C, incoming),
                        {blank, []};
                     Hn ->
                        #incoming_hn{value = Val, 'dependency-tree' = DepTree} = Hn,
                        % check if there is a remote cell
                        RPs = hn_db_wu:read_remote_parents(C, incoming),
                        {ok, ok} =
                            case lists:keymember(P, 1, RPs) of
                                false ->
                                    hn_db_wu:mark_notify_back_in_dirty(P, C, "new child");
                                true  ->
                                    {ok, ok}
                            end,
                        {Val, DepTree}
               end
        end,
    mnesia:activity(transaction, F). 

%% % @spec notify(Parent::#refX{}, Outgoing, Val, 
%% % DepTree, Timestamp) -> {ok, ok}
%% % Val = term() Outgoing = [#outgoing_hn{}] 
%% % DepTree = list()
%% % @doc notifies any remote sites that a hypernumber has changed.
%% % the reference must be for a cell
%% % @todo generalise the references to row, column, range and page
%% % the structure of the dirty_notify_out is leaking out here because
%% % we use timestamps as identifiers instead of unique keys...
%% % makes it hard to delete
%notify(Parent, Outgoing, Value, DepTree, Timestamp)
%  when is_record(Parent, refX) ->
%    {ok, ok} = horiz_api:notify(Parent, Outgoing, Value, DepTree),
%    % now delete the dirty outgoing hypernumber
%    PIdx = hn_util:index_from_refX(Parent),
%    Rec = #dirty_notify_out{parent = PIdx, outgoing = Outgoing,
%                             value = Value, timestamp = Timestamp},
%    Fun3 = fun() ->
%                   ok = mnesia:delete_object(Rec)
%           end,
%    ok = mnesia:activity(transaction, Fun3),
%    {ok, ok}.
           
%% % @spec notify_back(Parent::#refX{}, Child::#refX{}, Change) -> 
%% % {ok, ok}
%% % @doc notify's a change of a cell back to its remote hypernumber parent
%% % <code>#refX{}</code> can be a cell only
%% % @todo expand the paradigm to include ranges, columns, rows references and 
%% % queries as things that be remote parents.
%notify_back(Parent, Child, Change)
%  when is_record(Child, refX), is_record(Parent, refX) ->
%    Fun1 = fun() ->
%                   hn_db_wu:read_incoming_hn(Parent)
%           end,
%    Hypernumber = mnesia:activity(transaction, Fun1),
%    #incoming_hn{biccie = Biccie} = Hypernumber,
%    {ok, ok} = horiz_api:notify_back(Parent, Child, Change, Biccie),
%    Fun2 =
%        fun() ->
%                {ok, ok} = hn_db_wu:clear_dirty_notify_back(Parent,
%                                                            Child, Change)
%        end,
%    mnesia:activity(transaction, Fun2),
%    {ok, ok}.

%% @spec notify_back_create(Parent::#refX{}, Child::#refX{}) -> 
%% {ok, ok}
%% @doc sets up a hypernumbers
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents.
%% @TODO handle verions
notify_back_create(Parent, Child)
  when is_record(Child, refX), is_record(Parent, refX) ->
    Return = horiz_api:notify_back_create(Parent, Child),
    Fun2 =
        fun() ->
                case Return of
                    {Value, DepTree, Biccie} ->
                        {ok, ok} = hn_db_wu:update_inc_hn(Parent, Value,
                                                           DepTree, Biccie, "0");
                    {error, permission_denied} ->
                        exit("need to fix permission handling in "++
                             "hn_db_api:notify_back_create")
                end,
                {ok, ok} = hn_db_wu:clear_dirty_inc_hn_create(Parent, Child)
        end,
    mnesia:activity(transaction, Fun2),
    {ok, ok}.

%% @spec write_attributes(RefX :: #refX{}, List) -> {ok, ok}     
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
write_attributes(RefX, List) when is_record(RefX, refX), is_list(List) ->
    Fun = fun() ->
                  [{ok, ok} = hn_db_wu:write_attr(RefX, X) || X <- List]
          end,
    mnesia:activity(transaction, Fun).

%% @spec write_last(List) -> {ok, ok}
%% List = [{#refX{}, Val}]
%% Val = [list() | float() | integer()]
%% @doc takes a list of references and appends either a column or row at the end of them
%% 
%% All of the references must be either a:
%% <ul>
%% <li>column</li>
%% <li>row</li>
%% </ul>
%% and they must also be on the same page.
%% 
%% If the List looks like:
%%   <code>[{#refX{obj = {column, {2, 2}}, "=3"}, 
%%   {refX{obj = {column, {4, 5}}, 0}]</code>
%% then the value of highest written row will be got the the following cells 
%% will be written:
%% <ul>
%% <li>{cell, {2, LastRow+1} with formula of "=3"</li>
%% <li>{cell, {4, LastRow+1} with formula of 0</li>
%% <li>{cell, {5, LastRow+1} with formula of 0</li>
%% </ul>
%% It will write the values to the last row/column as if they were
%% 'formula' attributes
%% Formulae can be inserted into row or column using <code>rc</code> notation
write_last(List) when is_list(List) ->
    % all the refX's in the list must have the same site/path/object type
    % so get those from the head of the list and enforce it by matching down
    % --> at FORCE ME!
    [{#refX{site = S, path = P, obj = O} = RefX, _} | _T] = List,
    Fun =
        fun() ->
                {LastColumnRefX, LastRowRefX} = hn_db_wu:get_last_refs(RefX),
                #refX{obj = {cell, {LastCol, _}}} = LastColumnRefX,
                #refX{obj = {cell, {_, LastRow}}} = LastRowRefX,
                % Add 1 to because we are adding data 'as the last row'
                % (or column) ie one more than the current last row/column
                {Type, Pos} = case O of
                                  {row, _}    -> Y = LastRow + 1, 
                                                 {row, {Y, Y}};
                                  {column, _} -> X = LastCol + 1,
                                                 {column, {X, X}}
                              end,
                % now convert the column or row references to cell references

                Fun1 =
                    fun({#refX{site = S1, path = P1, obj = {Type1, Idx}}, Val})  ->
                            % FORCE ME to match (see above)
                            S = S1,
                            P = P1,
                            Type = Type1,
                            Obj = case Type1 of
                                      row    -> {cell, {Pos, Idx}};
                                      column -> {cell, {Idx, Pos}}
                                  end,
                            RefX2 = #refX{site = S1, path = P1, obj = Obj},
                            hn_db_wu:write_attr(RefX2, {"formula", Val})
                    end,
                [Fun1(X) || X <- List]
        end,
    ok = mnesia:activity(transaction, Fun),
    {ok, ok}.

%% @spec read_attributes(#refX{}, AttrList) -> {#refX{}, Val}
%% AttrList = [atom()]
%% Val = term()
%% @doc Given a reference and the name of an attribute, returns the reference
%% and the value of that attribute.
%% 
%% The reference can point to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_attributes(RefX, AttrList) when is_record(RefX, refX),
                                     is_list(AttrList) ->
    Fun = fun() ->
                  hn_db_wu:read_attrs(RefX, AttrList)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc read takes a refererence and returns the cell attributes.
%% 
%% The <code>refX{}</code> can be one of a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_cells(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read_styles(#refX{}) -> [Style]
%% Style = #styles{}
%% @doc read_style gets the list of styles that pertain to a particular 
%% reference.
%% 
%% The <code>#refX{}</code> can point to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read_styles(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_style(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec recalculate(#refX{}) -> {ok, ok}
%% @doc recalculates the cells refered to (and all cells that depend on them)
%% 
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
recalculate(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  Cells = hn_db_wu:read_attrs(RefX, ["formula"]),
                  [{ok, ok} = hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun).

%% @spec reformat(#refX{}) -> {ok, ok}
%% @doc reformats the all cells refered to
%% 
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
reformat(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  Cells = hn_db_wu:read_attrs(RefX, [format]),
                  [hn_db_wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun).

%% @spec insert(RefX::#refX{}) -> ok
%% @doc inserts a single column or a row
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>row</li>
%% <li>column</li>
%% </ul>
%% 
%% @todo This needs to check if it intercepts a shared formula
%% and if it does it should fail...
insert(#refX{obj = {column, _}} = RefX)  ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, column}),
                  RefXs = hn_db_wu:get_refs_right(RefX),
                  % shift doesn't commute so it is up to us to sort
                  % the cells
                  RefXs2 = dbsort(RefXs, 'right-to-left'),
                  [hn_db_wu:shift_cell(F, offset(F, {0, 1})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun);
insert(#refX{obj = {row, _}} = RefX)  ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, row}),
                  RefXs = hn_db_wu:get_refs_below(RefX),
                  % shift doesn't commute so it is up to us to sort
                  % the cells
                  RefXs2 = dbsort(RefXs, 'bottom-to-top'),
                  [hn_db_wu:shift_cell(F, offset(F, {1, 0})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun);
insert(#refX{obj = R} = RefX) when R == cell orelse R == range  ->
    insert(RefX, vertical).

%% @spec insert(RefX :: #refX{}, Type) -> ok 
%% Type = [horizontal | vertical]
%% @doc inserts a cell or range
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% </ul>
insert(#refX{obj = {R, _}} = RefX, Disp) when R == cell orelse R == range ->
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {insert, Disp}),
                  RefXs = hn_db_wu:get_refs_below(RefX),
                  Sort = case Disp of
                             vertical   -> 'bottom-to-top';
                             horizontal -> 'right-to-left'
                         end,
                  RefXs2 = dbsort(RefXs, Sort),
                  [hn_db_wu:shift_cell(F, offset(F, {0,1})) || F <- RefXs2]
          end,
    mnesia:activity(transaction, Fun).

%% @spec delete(Ref :: #refX{}) -> ok
%% @doc deletes a column or a row or a page
%% 
%% @todo this is all bollocks - should be row, column then cell/range as 
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
delete(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    Fun = fun() ->
                  {ok, ok} = write_page_vsn(RefX, {delete, Disp}),
                  hn_db_wu:shift_cell(RefX, Disp, delete)
          end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {page, _}} = RefX) ->
    Fun = fun() ->
                  hn_db_wu:delete(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec delete(RefX :: #refX{}, Type) -> ok
%% Type = [contents | all | horizontal | vertical]
%% @doc deletes a reference.
%% 
%% The <code>refX{}</code> can be one of a:
%% <ul>
%% <li>cell</li>
%% <li>row</li>
%% <li>column</li>
%% <li>range</li>
%% </ul>
%% 
%% For all refs except those to a page this function deletes the cells
%% and closes up the rest of them. If Disp is Horizontal it moves 
%% cells right-to-left to close the gap. If Disp is vertical is moves
%% cells bottom-to-top to close the gap
delete(#refX{obj = {R, _}} = RefX, horizontal) when R == cell orelse R == range ->
    Fun =
        fun() ->
                {ok, ok} = write_page_vsn(RefX, {delete, horizontal}),
                hn_db_wu:shift_cell(RefX, horizontal, delete)
        end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {R, _}} = RefX, vertical) when R == cell orelse R == range ->
    Fun =
        fun() ->
                {ok, ok} = write_page_vsn(RefX, {delete, vertical}),
                hn_db_wu:shift_cell(RefX, vertical, delete)
        end,
    mnesia:activity(transaction, Fun).

%% @spec clear(#refX{}) -> ok
%% @doc same as <code>clear(refX{}, all)</code>.
clear(RefX) when is_record(RefX, refX) ->
    clear(RefX, all).

%% @spec clear(#refX{}, Type) -> ok
%% Type = [contents | style | all]
%% @doc clears the contents of the cell or range
%% (but doesn't delete the cell or range itself).
%% 
%% If <code>Type  = 'contents'</code> it clears:
%% <ul>
%% <li>formula</li>
%% <li>values</li>
%% </ul>
%% If <code>Type = 'style'</code> it clears the style.
%% If <code>Type = 'all'</code> it clears both style and content.
%% It doesn't clear other/user-defined attributes of the cell or range
%%
%% The <code>refX{}</code> can be to a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
clear(RefX, Type) when is_record(RefX, refX) ->
    Fun =
        fun() ->
                hn_db_wu:clear_cells(RefX, Type)
        end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec cut_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination then deletes the original.
%% 
%% (see also {@link hn_db_api:drag_n_drop/2} and {@link hn_db_api:copy_n_paste/2} - 
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that 
%% drag'n'drop increments)
%% 
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li>
%% </ul>
%% 
%% If a range is to be cut'n'pasted to a range then one of the following criteria MUST
%% true:
%% <ul>
%% <li>the <b>from</b> range must be the same dimensions as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be one cell high and the same width as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be one cell wide and the same height as the
%% <b>to</b> range</li>
%% </ul> 
%% 
%% @todo cut'n'paste a page
cut_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  {ok, ok} = copy_n_paste2(From, To),
                  {ok, ok} = hn_db_wu:clear_cells(From, all)
          end,
    mnesia:activity(transaction, Fun).

%% @spec copy_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination.
%% 
%% (see also {@link hn_db_api:drag_n_drop/2} and {@link hn_db_api:cut_n_paste/2} - 
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that 
%% drag'n'drop increments)
%%  
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%% @todo copy'n'paste a page
copy_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  {ok, ok} = copy_n_paste2(From, To)
          end,
    mnesia:activity(transaction, Fun).

%% @spec drag_n_drop(From :: #refX{}, To :: #refX{}) -> ok
%% @doc takes the formula and formats from a cell and drag_n_drops 
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%% 
%% (see also {@link hn_db_api:cut_n_paste/2} and {@link hn_db_api:copy_n_paste/2} - 
%% the difference between drag'n'drop and copy'n'paste or cut'n'paste is that 
%% drag'n'drop increments)
%% 
%% drag'n'drop has an interesting specification
%% (taken from Excel 2007 help)
%% currently excludes customer autofill
%% 
%% <code>Initial selection       Extended series</code>
%% 
%% <code>-----------------       ---------------</code>
%% 
%% <code>1, 2, 3                 4, 5, 6,... </code>
%% 
%% <code>9:00 10:00,             11:00, 12:00,... </code>
%% 
%% <code>Mon Tue,                Wed, Thu,... </code>
%% 
%% <code>Monday Tuesday,         Wednesday, Thursday,... </code>
%% 
%% <code>Jan Feb,                Mars, Apr,... </code>
%% 
%% <code>Jan, Apr                Jul, Oct, Jan,... </code>
%% 
%% <code>Jan-07, Apr-07          Jul-07, Oct-07, Jan-08,... </code>
%% 
%% <code>15-Jan, 15-Apr          15-Jul, 15-Oct,... </code>
%% 
%% <code>2007, 2008              2009, 2010, 2011,... </code>
%% 
%% <code>1-Jan, 1-Mar            1-May, 1-Jul, 1-Sep,... </code>
%% 
%% <code>Qtr3                    Qtr4, Qtr1, Qtr2,... </code>
%% 
%% <code>Q3                      Q4, Q1, Q2,... </code>
%% 
%% <code>Quarter3                Quarter4, Quarter1, Quarter2,... </code>
%% 
%% <code>text1, textA text2,     textA, text3, textA,... </code>
%% 
%% <code>1st Period              2nd Period, 3rd Period,... </code>
%% 
%% <code>Product 1               Product 2, Product 3,... </code>
%% 
%% <code>1 Product               2 Product, 3 Product</code>
%% 
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%% 
%% If a range is to be drag'n'dropped to a range then
%% one of the following criteria MUST be true:
%% <ul><li>the <b>from</b> range must be the same dimensions as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be the same width as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be the same height as the
%% <b>to</b> range</li></ul> 
drag_n_drop(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  case is_valid_d_n_d(From, To) of
                      {ok, single_cell, Incr}   -> ?copy(From, To, Incr);
                      {ok, 'onto self', _Incr}  -> {ok, ok};
                      {ok, cell_to_range, Incr} -> copy2(From, To, Incr)
                  end
          end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_page_vsn(RefX, Action) ->
    PageRefX = #refX{obj = {page, "/"}},
    NewVsn = ?counter(page_vsn_counters, PageRefX, 1),
    Record = #page_vsn{page_refX = PageRefX, action = Action,
                       action_refX = RefX, version = NewVsn},
    ok = mnesia:write(Record),
   {ok, ok}.

offset(#refX{obj = {cell, {X, Y}}} = RefX, {XO, YO}) ->
    RefX#refX{obj = {cell, {X + XO, Y + YO}}}.

dbsort(List, 'bottom-to-top') ->
    Fun = fun(#refX{obj = {cell, {_XA, YA}}},
              #refX{obj = {cell, {_XB, YB}}}) ->
                  if
                      (YA > YB)  -> true;
                      (YA =< YB) -> false
                  end
          end,
    lists:sort(Fun, List);
dbsort(List, 'right-to-left') ->
    Fun = fun(#refX{obj = {cell, {XA, _YA}}},
              #refX{obj = {cell, {XB, _YB}}}) ->
                  if
                      (XA > XB)  -> true;
                      (XA =< XB) -> false
                  end
          end,
    lists:sort(Fun, List).

copy_n_paste2(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> hn_db_wu:copy_cell(From, To, false);
        {ok, 'onto self'}    -> {ok, ok};
        {ok, cell_to_range}  -> copy2(From, To, false);
        {ok, range_to_cell}  -> To2 = cell_to_range(To),
                                copy3(From, To2, false);
        {ok, range_to_range} -> copy3(From, To, false)
    end.

cell_to_range(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {range, {X, Y, X, Y}}}.      

%% the last parameter returned is whether dates and integers should be 
%% incremented this can only be true for a vertical or horizontal drag
%% (returning 'y' and 'x') or is otherwise false
%% cell to cell drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, A}}, #refX{obj = {cell, A}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {cell, {X, _Y1}}}, #refX{obj = {cell, {X, _Y2}}}) ->
    {ok, single_cell, vertical};
is_valid_d_n_d(#refX{obj = {cell, {_X1, Y}}}, #refX{obj = {cell, {_X2, Y}}}) ->
    {ok, single_cell, horizontal};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {cell, _}}) ->
    {ok, single_cell, false};
%% cell to range drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, _}},
               #refX{obj = {range, {TX, _TY1, TX, _TY2}}}) ->
    {ok, cell_to_range, vertical};
is_valid_d_n_d(#refX{obj = {cell, _}},
               #refX{obj = {range, {_TX1, TY, _TX2, TY}}}) ->
    {ok, cell_to_range, horizontal};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range, false};
%% range to range drag'n'drop
is_valid_d_n_d(#refX{obj = {range, Range}}, #refX{obj = {range, Range}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {range, {FX, FY1, FX, FY2}}},
               #refX{obj = {range, TRange}}) ->
    {_TX1, TY1, _TX2, TY2} = TRange,
    case ((TY2 - TY1) - (FY2 - FY1)) of
        0    -> {ok, col_range_to_range};
        true -> {error, "target range is not the same height as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, {FX1, FY, FX2, FY}}},
               #refX{obj = {range, TRange}}) ->
    {TX1, _TY1, TX2, _TY2} = TRange,
    case ((TX2 - TX1) - (FX2 - FX1)) of
        0    -> {ok, row_range_to_range};
        true -> {error, "target range is not the same width as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

%% cell to range
copy2(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    lists:map(fun(X) -> ?copy(From, X, Incr) end, List),
    {ok, ok}.

%% range to range
copy3(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr).

copy3a(_From, [], _Incr)    -> {ok, ok};
copy3a(From, [H | T], Incr) -> FromRange = hn_util:range_to_list(From),
                               ToRange = hn_util:range_to_list(H),
                               {ok, ok} = copy3b(FromRange, ToRange, Incr),
                               copy3a(From, T, Incr).

copy3b([], [], _Incr) -> {ok, ok};
copy3b([FH | FT], [TH | TT], Incr) ->
    {ok, ok} = hn_db_wu:copy_cell(FH, TH, Incr),
    copy3b(FT, TT, Incr).

get_tiles(#refX{obj = {range, {X1F, Y1F, X2F, Y2F}}},
          #refX{obj = {range, {X1T, Y1T, X2T, Y2T}}} = To) ->

    % this is a bit messy. Excel does the following things:
    % * if both ranges are congruent - 1 tile
    % * if the To range is smaller than the From range it writes the whole
    %   From range as a block into the range whose top left is the same
    %   as the To range - 1 tile
    % * if the To range is an exact multipe of the from range it
    %   tiles it
    % * if the To range is one column wide and a the height is a multiple 
    %   of the From range then it tiles the From range vertically
    % * if the To range is one row high and the width is a multiple
    %   of the From range then it tiles the From range horizontally
    % * if the To range is not one of the above it writes the whole
    %   block into the range whose top left is the same as the To range
    %   
    %   First up rectify the ranges
    {FX1, FY1, FX2, FY2} = hn_util:rectify_range(X1F, Y1F, X2F, Y2F),
    {TX1, TY1, TX2, TY2} = hn_util:rectify_range(X1T, Y1T, X2T, Y2T),
    FWidth  = FX2 - FX1 + 1,
    FHeight = FY2 - FY1 + 1,
    TWidth  = TX2 - TX1 + 1,
    THeight = TY2 - TY1 + 1,
    WidthMultiple = TWidth/FWidth - erlang:trunc(TWidth/FWidth),
    HeightMultiple = THeight/FHeight - erlang:trunc(THeight/FHeight),
    % if the to range is an exact multiple of the From range in both
    % dimensions then tile it, otherwise just copy the From range into
    % a range of the same size whose top left cell is that of the To range
    % capice?
    {WTile, HTile} = case {WidthMultiple, HeightMultiple} of
                         {0.0, 0.0} -> {erlang:trunc(TWidth/FWidth),
                                        erlang:trunc(THeight/FHeight)} ;
                         _          -> {1, 1} 
                     end,
    get_tiles2(To, FWidth, FHeight, {WTile, HTile}).

get_tiles2(Ref, Width, Height, {WTile, HTile}) ->
    get_tiles2(Ref, Width, Height, {1, 1}, {WTile, HTile}, []).

%% has a special terminator for the single tile case
%% the algorith relies on you zigzagging over the body of the kirk:
%% * down the first column, increment the column, reset the row
%% * down the next column
%% * etc, etc,
%% which can't happen if you have 1 column and 1 row
get_tiles2(RefX, W, H, {WT, FT}, {WT, FT}, Acc) ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (WT - 1) * W,
    SY = Y + (FT - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    [NewAcc | Acc];
get_tiles2(RefX, W, H, {WT, M},  {WT, FT}, Acc) ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (WT - 1) * W,
    SY = Y + (M - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    get_tiles2(RefX, W, H, {1, M + 1}, {WT, FT}, [NewAcc | Acc]);
get_tiles2(RefX, W, H, {N, M},  {WT, FT}, Acc)  ->
    #refX{obj = {range, {X, Y, _, _}}} = RefX,
    SX = X + (N - 1) * W,
    SY = Y + (M - 1) * H,
    EX = SX + (W - 1),
    EY = SY + (H - 1),
    NewAcc = RefX#refX{obj = {range, {SX, SY, EX, EY}}},
    get_tiles2(RefX, W, H, {N + 1, M}, {WT, FT}, [NewAcc | Acc]).

is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {cell, _}})  ->
    {ok, single_cell};
is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {cell, _}}) ->
    {ok, range_to_cell};
is_valid_c_n_p(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {ok, range_to_range}.
