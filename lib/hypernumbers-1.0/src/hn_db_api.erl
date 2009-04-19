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
%%%            mnesia <em>MUST NOT</em> be called from any function in
%%%            this module.
%%%            
%%%            This module also handles the transaction management of
%%%            notification to the front-end. In order for this to work
%%%            each mnesia transaction construcuted here MUST begin with
%%%            a function call to initialise the notifications in the 
%%%            process dictionary using the function 'init_front_end_notify'
%%%            When the mnesia transaction returns the {@link hn_db_wu}
%%%            functions will have loaded the process dictionary with
%%%            the appropriate front end notifications which can then be
%%%            forwared using the function 'tell_front_end'
%%%            
%%%            Obviously this only needs to be done on functions that generate
%%%            notifications to the front-end (ie it is not required for any 
%%%            reads but also for some writes - like page version updates)
%%%            
%%%            Security operations (like checking if the right biccie
%%%            has been supplied for a remote update) <em>MUST</em> be
%%%            performed in these API functions using the 
%%%            {@link hn_db_wu:verify_biccie_in/2} and 
%%%            {@link hn_db_wu:verify_biccie_out/3} functions - they
%%%            <em>WILL NOT</em> be done in {@link hn_db_wu}.
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
%%% @TODO need to write a function to clear attributes 
%%% <code>clear_attributes(#refX{}, [Key1, Key2...])</code>
%%% @TODO should we have a subpages #refX egt {subpages, "/"}
%%% which would alllow you to specify operations like delete and copy
%%% on whole subtrees?
%%% @TODO in tell_front_end we gen_srv each change in one at a time, but
%%% we should bung 'em all in a oner...
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_api).

-include("spriki.hrl").
-include("hypernumbers.hrl").

-define(wu, hn_db_wu).
-define(copy, copy_cell).
-define(wr_rem_link, write_remote_link).
-define(shift_ch, shift_children).
-define(make_rec, hn_util:dirty_not_bk_in).
-define(init, initialise_remote_page_vsn).
-define(u_inc_hn, update_inc_hn).
-define(to_xml, simplexml:to_xml_string).
-define(create(Name,Type,Storage),
        fun() ->
                Attr = [{attributes, record_info(fields, Name)},
                        {type,Type},{Storage, [node()]}],
                {atomic,ok} = mnesia:create_table(Name, Attr)
        end()).
-define(not_ch, remoting_reg:notify_change).

-export([
         write_attributes/2,
         write_last/1,
         % write_permission/2,
         % write_style/2,
         read_attributes/2,
         read/1,
         read_whole_page/1,
         read_inherited_list/2,
         read_inherited_value/3,
         read_styles/1,
         read_page_structure/1,
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
         notify_back_create/1,
         read_incoming_hn/2,
         write_remote_link/3,
         notify_from_web/5,
         notify_back_from_web/4,
         handle_dirty_cell/1,
         handle_dirty/1,
         register_hn_from_web/4,
         check_page_vsn/2,
         initialise_remote_page_vsn/2,
         incr_remote_page_vsn/3,
         resync/2,
         create_db/0
        ]).

%% Upgrade functions that were applied at upgrade_REV
-export([upgrade_1519/0, upgrade_1556/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
upgrade_1519() ->
    F = fun({hn_user, Name, Pass, Auth, Created}) ->
                {hn_user, Name, Pass, Auth, Created, dict:new()}
        end,
    mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).

upgrade_1556() ->
    F = fun({hn_user, Name, Pass, Auth, Created, _Dict}) ->
                {hn_user, Name, Pass, Auth, Created, dict:new()}
        end,
    mnesia:transform_table(hn_user, F, record_info(fields, hn_user)).


%% @doc reads pages
%% @todo fix up api
read_page_structure(RefX) when is_record(RefX, refX) ->
    Fun = fun() ->
                  hn_db_wu:read_page_structure(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec initialise_remote_page_vsn(Site, Version) -> ok
%% @doc intialises the page version for a 'newly discovered' remote page.
%% This function is only called the first time that a remote pages makes a
%% request and the function {@link read_page_vsn_raw/2} returns 
%% <code>undefined</code>
initialise_remote_page_vsn(Site, Version) when is_record(Version, version) ->
    #version{page = Page, version = V} = Version,
    RefX = hn_util:url_to_refX(Page),
    Fun = fun() ->
                  ok = ?wu:initialise_remote_page_vsn(Site, RefX, V)
          end,
    mnesia:activity(transaction, Fun).

%% @spec resync(Site, List) -> ok
%% @doc triggers a resync of the current site to the remote pages.
%% This function is called when version numbers are out of sync between local 
%% and remote pages and forces a resynch
%% @TODO write me, ya bas!
resync(Site, #version{page = Page, version = Vsn}) ->
    bits:log("RESYNC £" ++ pid_to_list(self()) ++ "£" ++ Site ++ "£ for  £" ++
             Page ++ "£ version £" ++ tconv:to_s(Vsn)),
    ok.

%% @spec create_db() -> ok
%% @doc  Creates the database for hypernumbers
create_db()->
    % Seems sensible to keep this restricted
    % to disc_copies for now
    Storage = disc_copies,
    application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic, ok} = ?create(hn_item,               set, Storage),
    {atomic, ok} = ?create(remote_cell_link,      bag, Storage),
    {atomic, ok} = ?create(local_cell_link,       bag, Storage),
    {atomic, ok} = ?create(hn_user,               set, Storage),
    {atomic, ok} = ?create(dirty_cell,            set, Storage),
    {atomic, ok} = ?create(dirty_notify_in,       set, Storage),
    {atomic, ok} = ?create(dirty_inc_hn_create,   set, Storage),
    {atomic, ok} = ?create(dirty_notify_back_in,  set, Storage),
    {atomic, ok} = ?create(dirty_notify_out,      set, Storage),
    {atomic, ok} = ?create(dirty_notify_back_out, set, Storage),
    {atomic, ok} = ?create(incoming_hn,           set, Storage),
    {atomic, ok} = ?create(outgoing_hn,           set, Storage),
    {atomic, ok} = ?create(template,              set, Storage),
    {atomic, ok} = ?create(styles,                bag, Storage), 
    {atomic, ok} = ?create(style_counters,        set, Storage),
    {atomic, ok} = ?create(page_vsn,              set, Storage),
    {atomic, ok} = ?create(page_history,          bag, Storage),

    % now add appropriate indices
    {atomic, ok} = mnesia:add_table_index(dirty_cell, timestamp),
    ok.

%% @spec incr_remote_page_vsn(Site, Version::#version{}, Payload) -> 
%% {ok, NewVersion} | {error, pages_out_of_synch}
%% @doc increments the local storage of a page version number for a page on
%% a remote site if the new increment is one above the old one. If the increment
%% is greater than 1 returns an error which should trigger a resynch.
%% Incrementation of page versions for local pages should be done with 
%% {@link get_new_local_page_vsn/2}
%% @todo break up payload in hn_mochi.erl
incr_remote_page_vsn(Site, Version, Payload) when is_record(Version, version) ->
    Fun = fun() ->
                  ?wu:incr_remote_page_vsn(Site, Version, Payload)
          end,
    mnesia:activity(transaction, Fun).

%% @spec check_page_vsn(Site, Version::#version{}) 
%% -> synched | unsynched | not_yet_synched
%% @doc checks the page verion numbers for a set of pages
check_page_vsn(Site, Version) when is_record(Version, version) ->
    F = fun() ->
                #version{page = Page, version = NewV} = Version,
                PageX = hn_util:url_to_refX(Page),
                OldV = ?wu:read_page_vsn(Site, PageX),
                bits:log("CHECK PAGE VSN £" ++ pid_to_list(self()) ++
                         "£" ++ Site ++ "£ of £" ++ Page ++
                         "£ New Version £" ++ tconv:to_s(NewV) ++
                         "£ Old Version £"++ tconv:to_s(OldV)),
                case {NewV, OldV} of
                    {Vsn, Vsn}          -> synched;
                    {"undefined", OldV} -> ok = ?wu:?init(Site, PageX, OldV),
                                           synched;
                    {NewV, "undefined"} -> not_yet_synched;
                    {NewV, OldV}        -> unsynched
                end
        end,
    mnesia:activity(transaction, F).

%% @spec register_hn_from_web(Parent::#refX{}, Child::#refX{}, Proxy, Biccie) -> 
%% list()
%% @doc handles the registeration of a new hypernumber from the web server.
%% The Parent and Child references must both point to a cell.
%% This function returns a list with two tuples, one for the current value
%% and one for the current dependency tree
register_hn_from_web(Parent, Child, Proxy, Biccie)
  when is_record(Parent, refX), is_record(Child, refX) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ?wu:write_remote_link(Parent, Child, outgoing),
                  ?wu:register_out_hn(Parent, Child, Proxy, Biccie),
                  List = ?wu:read_attrs(Parent, ["value", "dependency-tree"]),
                  List2 = extract_kvs(List),
                  #refX{site = Site} = Parent,
                  Version = ?wu:read_page_vsn(Site, Parent),

                  % get the value (if there is one)
                  V = case lists:keysearch("value", 1, List2) of
                          false           -> "blank";
                          {_, {_, Value}} -> Value
                      end,

                  % get the dependency-tree (if there is one)
                  D = case lists:keysearch("dependency-tree", 1, List2) of
                          false             -> {xml, []};
                          {_, {_, DepTree}} -> DepTree
                      end,

                  % now package it up for the site that is registering
                  % the hypernumber
                  PPage = Parent#refX{obj = {page, "/"}},
                  PUrl = hn_util:refX_to_url(PPage),
                  Vsn = #version{page = PUrl, version = Version},
                  VsnJson = json_util:jsonify(Vsn),

                  % now return a JSON ready structure...
                  {struct, [{"value",           V},
                            {"dependency-tree", ?to_xml(D)},
                            {"parent_vsn",      VsnJson}]}
          end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("register_hn_from_web").

%% @spec handle_dirty_cell(Timestamp) -> ok
%% @doc handles a dirty cell.
%% Timestamp is the timestamp of the dirty cell - the actual
%% record itself may have been rewritten before it is processed
%% Silently fails if the cell is part of a shared range
%% @todo extend this to a dirty shared formula
%% @todo stop the silent fail!
handle_dirty_cell(TimeStamp)  ->
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Logging code                                                      %
    % #index{path = Path, row = Row, column = Col} = Index,             %
    % Str=string:join(Path,"/")++" Row "++integer_to_list(Row)++" Col " %
    %     ++integer_to_list(Col),                                       %
    % bits:log(Str),                                                    %
    % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                [DirtyCell] = ?wu:read_dirty_cell(TimeStamp),
                % io:format("in hn_db_api:handle_dirty_cell~n-DirtyCell is ~p~n",
                %          [DirtyCell]),
                CellIndex = DirtyCell#dirty_cell.index,
                Cell = hn_util:refX_from_index(CellIndex),
                ok = case ?wu:read_attrs(Cell, ["__shared"]) of
                         [] -> [{C, KV}] = ?wu:read_attrs(Cell, ["formula"]),
                               ?wu:write_attr(C, KV);
                         _  -> ok
                     end,
                ok = ?wu:clear_dirty_cell(Cell)
        end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("handle dirty").

%% @spec handle_dirty(Record) -> ok
%% Record = #dirty_notify_in{}
%% @doc handles a dirty .
%% The reference must be to a cell
%% @todo implement dirty ranges/functions/queries and stuff
%% %@TODO list the full set of possible change messages and how they should
handle_dirty(Record) when is_record(Record, dirty_notify_in) ->
    #dirty_notify_in{parent = Parent} = Record,
    % read the incoming remote children and mark them dirty
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                Cells = ?wu:read_remote_children(Parent, incoming),
                Fun2 =
                    fun(X) ->
                            [{RefX, KV}] = ?wu:read_attrs(X, ["formula"]),
                            ?wu:write_attr(RefX, KV)
                    end,
                [ok = Fun2(X)  || X <- Cells],
                ?wu:clear_dirty(Record)
        end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("handle dirty");
handle_dirty(Record) when is_record(Record, dirty_notify_out) ->
    ok = horiz_api:notify(Record),
    % now delete the dirty outgoing hypernumber
    Fun = fun() ->
                  mnesia:delete_object(Record)
          end,
    mnesia:activity(transaction, Fun);
handle_dirty(Record) when is_record(Record, dirty_notify_back_in) ->
    ok = horiz_api:notify_back(Record),
    Fun = fun() ->
                  ?wu:clear_dirty(Record)
          end,
    mnesia:activity(transaction, Fun);
handle_dirty(Record) when is_record(Record, dirty_notify_back_in) ->
    ok = horiz_api:notify_back(Record),
    Fun = fun() ->
                  ?wu:clear_dirty(Record)
          end,
    mnesia:activity(transaction, Fun);
handle_dirty(Record) when is_record(Record, dirty_notify_back_in) ->
    ok = horiz_api:notify_back(Record),
    Fun = fun() ->
                  ?wu:clear_dirty(Record)
          end,
    mnesia:activity(transaction, Fun);
handle_dirty(Record)
  when is_record(Record, dirty_notify_back_out) ->
    #dirty_notify_back_out{parent = P, child = C, change = Type} = Record,
    Fun =
        fun() ->
                case Type of
                    "unregister" ->
                        ok = ?wu:unregister_out_hn(P, C),
                        ok = ?wu:clear_dirty(Record);
                    "new child" ->
                        ok = ?wu:write_remote_link(P, C, outgoing),
                        ok = ?wu:clear_dirty(Record)
                end
        end,
    mnesia:activity(transaction, Fun).

%% @spec notify_from_web(Parent::#refX{}, Child::#refX{}, Type, Payload, 
%% Biccie) -> ok
%% @doc handles a notify message from the web server.
%% The parent reference must be cell references.
%% If this value doesn't match the Biccie on record this update will be logged and
%% will silently fail
%% Version is the page version of the remote page. If this version is newer that the
%% page version stored here it will trigger a structural replay of page updates
%% @todo log the biccie failures
%% @todo this code assumes that the child will notify all dependency tree
%% changes consequent to the insert/deletes and that we don't need to care
%% about it - page syncs will 'take care of it' BE SURE THAT THEY DO!
notify_from_web(P, C, "new_value", Payload, Bic)
  when is_record(P, refX), is_record(C, refX) ->
    #refX{site = ChildSite} = C,
    {_, Value, {array, DepTree}} = json_util:json_to_payload(Payload),
    DepTree2 = convertdep(C, DepTree),
    F = fun() ->
                ok = init_front_end_notify(),
                case ?wu:verify_biccie_in(ChildSite, P, Bic) of
                    true  -> ?wu:update_inc_hn(P, C, Value, DepTree2, Bic);
                    false -> ok
                end
        end,
    ok = mnesia:activity(transaction, F),
    ok = tell_front_end("notify from web");
notify_from_web(P, C, "insert", Payload, Bic)
  when is_record(P, refX), is_record(C, refX) ->

    #refX{site = ChildSite} = C,

    F = fun() ->
                ok = init_front_end_notify(),
                case ?wu:verify_biccie_in(ChildSite, P, Bic) of
                    true  -> notify_from_web2(P, C, Payload, Bic);
                    false -> ok
                end
        end,
    ok = mnesia:activity(transaction, F),
    ok = tell_front_end("notify from web").

notify_from_web2(Parent, Child, Payload, _Biccie) ->
    % this function is called when a insert/delete has been made on a remote page
    % this function does the following
    % * read all the #incoming_hn's that are from the page that has had
    %   the insert and then adjust them
    % * read all the cells that use the incoming_hn's, rewrite their formulae
    %   and resave them
    % * read all the remote_cell_link's that use those incoming_hn's and
    %   adjust them

    % do some housekeeping
    % first up unpack the payload
    {Type, Ref, Displacement} = json_util:json_to_payload(Payload),
    % now calculate the offset
    {XOff, YOff} = get_offset(Type, Displacement, Ref),
    % make the Parent a page parent because multiple notifications
    % will have been suppresed...
    PageParent = Parent#refX{obj = {page, "/"}},
    #refX{site = ChildSite} = Child,
    % start by reading the incoming_hn's
    case ?wu:read_incoming_hn(ChildSite, PageParent) of
        []   ->
            io:format("in hn_db_api:notify_from_web no incoming "++
                      "hypernumbers - should log this!~n"),
            ok;
        Inc_Hns ->
            % now shorten the list to the relevant ones
            % (ie those 'below' a vertical displacement or
            % to the 'right' of a horizontal displacement)

            Inc_Hns2 = shorten(Inc_Hns, Ref, Displacement),

            % now sort out the incoming_hn's
            Fun3 = fun(I) ->
                           #incoming_hn{site_and_parent = {_S, P}} = I,
                           NewP = shift(P, XOff, YOff),
                           ok = ?wu:shift_inc_hns(I, NewP)
                   end,
            lists:foreach(Fun3, Inc_Hns2),

            % convert the short list of incoming_hn's to refXs
            Fun4 = fun(#incoming_hn{site_and_parent = {_S, P}}) ->
                           hn_util:refX_from_index(P)
                   end,
            HnsXs = lists:map(Fun4, Inc_Hns2),

            % io:format("In hn_db_api:notify_from_web~n-"++
            %           "Inc_Hns  is ~p~n-"++
            %          "Inc_Hns2 is ~p~n-"++
            %          "HnsXs    is ~p~n",
            %          [Inc_Hns, Inc_Hns2, HnsXs]),

            % Lets process this list of cells
            % Start by rewriting them

            % now get the cells that use these incoming hypernumbers
            % Swap this out, read all the children of an item and then
            % move that item, then onto the next item...
            Fun5 =
                fun(P) ->
                        case ?wu:read_remote_children(P, incoming) of
                            [] ->
                                io:format("in hn_db_api:notify_from_web "++
                                          "no children of an incoming "++
                                          "hypernumber - should log "++
                                          "this!~n"),
                                ok;
                            Children ->
                                NewP = shift(P, XOff, YOff),
                                % io:format("in hn_db_api:notify_from_web~n"++
                                %          "-Children is ~p~n", [Children]),
                                ok = ?wu:?shift_ch(Children, P, NewP),
                                ok = ?wu:shift_remote_links(parent, P, NewP,
                                                            incoming)
                        end
                end,
            lists:foreach(Fun5, HnsXs),
            ok
    end.

%% @spec notify_back_from_web(Parent::#refX{}, Child::#refX{}, Biccie, Type) -> ok
%% @doc handles a notify_back message from the web server.
%% The parent and child references must be cell references.
notify_back_from_web(P, C, B, Type)
  when is_record(P, refX), is_record(C, refX) ->
    Fun =
        fun() ->
                case ?wu:verify_biccie_out(P, C, B) of
                    true -> Rec = #dirty_notify_back_out{child = C, parent = P,
                                                         change = Type},

                            ?wu:mark_dirty(Rec);
                    _    -> ok
                end
        end,
    mnesia:activity(transaction, Fun).

%% @spec write_remote_link(Parent::#refX{}, Child::#refX{}, Type) -> ok
%% @doc writes a remote link
write_remote_link(Parent, Child, Type)
  when is_record(Parent, refX), is_record(Child, refX) ->
    Fun = fun() ->
                  ?wu:write_remote_link(Parent, Child, Type)
          end,
    mnesia:activity(transaction, Fun).

%% @spec read_incoming_hn(Parent::#refX{}, Child::#refX{}) -> {Value, DepTree}
%% @doc gets the value of an incoming hypernumber
%% Parent is a reference to a remote cell only
%% Child is a reference to a local cell only
%% If the hypernumber requested hasn't been set up yet, this function will
%% trigger a creation process and return 'blank' for the moment... when the
%% hypernumber is set up the 'correct' value will come through as per normal...
read_incoming_hn(P, C) when is_record(P, refX), is_record(C, refX) ->
    #refX{site = CSite} = C,
    P2 = P#refX{obj = {page, "/"}},
    C2 = C#refX{obj = {page, "/"}},
    CUrl = hn_util:refX_to_url(C2),
    PUrl = hn_util:refX_to_url(P2),
    PVsn = #version{page = PUrl, version = ?wu:read_page_vsn(CSite, P)},
    CVsn = #version{page = CUrl, version = ?wu:read_page_vsn(CSite, C)},
    F = fun() ->
                case ?wu:read_incoming_hn(CSite, P) of
                    []   ->
                        Rec = #dirty_inc_hn_create{parent = P, child = C,
                                                   parent_vsn = PVsn, child_vsn = CVsn},
                        ok = ?wu:mark_dirty(Rec),
                        % need to write a link
                        ok = ?wu:?wr_rem_link(P, C, incoming),
                        {blank, []};
                    [Hn] ->
                        #incoming_hn{value = Val, biccie = B,
                                     'dependency-tree' = DepTree} = Hn,
                        % check if there is a remote cell
                        RPs = ?wu:read_remote_parents(C, incoming),
                        ok =
                            case lists:keymember(P, 1, RPs) of
                                false ->
                                    R = #dirty_notify_back_in{parent = P, child = C,
                                                              change = "new child",
                                                              biccie = B,
                                                              parent_vsn = PVsn,
                                                              child_vsn = CVsn},
                                    ?wu:mark_dirty(R);
                                true  -> ok
                            end,
                        {Val, DepTree}
                end
        end,
    mnesia:activity(transaction, F).

%% @spec notify_back_create(Record::#dirty_inc_hn_create{}) -> ok
%% @doc sets up a hypernumbers
%% @todo expand the paradigm to include ranges, columns, rows references and 
%% queries as things that be remote parents.
%% @TODO handle verions
notify_back_create(Record) when is_record(Record, dirty_inc_hn_create) ->

    #dirty_inc_hn_create{parent = P, child = C, parent_vsn = PVsn} = Record,
    _ParentPage = P#refX{obj = {page, "/"}},
    #refX{site = CSite} = C,

    Return = horiz_api:notify_back_create(Record),

    Fun =
        fun() ->
                ok = init_front_end_notify(),
                case Return of % will have to add NewCVsn in line below...
                    {Value, DepT, Biccie, _NewPVsn} ->
                        ok = ?wu:?u_inc_hn(P, C, Value, DepT, Biccie);
                    {error, unsynced, PVsn} ->
                        resync(CSite, PVsn);
                    {error, permission_denied} ->
                        exit("need to fix permission handling in "++
                             "hn_db_api:notify_back_create")
                end,
                ok = ?wu:clear_dirty(Record)
        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("notify back create").

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
write_attributes(RefX, List) when is_record(RefX, refX), is_list(List) ->
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  F = fun(X) -> ?wu:write_attr(RefX, X) end,
                  lists:foreach(F, List)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("write attributes").

%% @spec write_last(List) -> ok
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
                ok = init_front_end_notify(),
                {LastColumnRefX, LastRowRefX} = ?wu:get_last_refs(RefX),
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
                            ?wu:write_attr(RefX2, {"formula", Val})
                    end,
                [Fun1(X) || X <- List]

        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("write last").

%% @spec read_inherited_list(#refX{}, Attribute) -> {#refX{}, Val}
%% Attribute = string()
%% @doc Scans the tree and returns a list of value stored against
%%      Key 
read_inherited_list(RefX, Key) when is_record(RefX, refX) ->
    F = fun hn_db_wu:read_inherited_list/2,
    mnesia:activity(transaction, F, [RefX, Key]).

%% @spec read_inherited_value(#refX{}, Attibute, Default) -> {#refX{}, Val}
%% @doc  This function searches the tree for the first occurence of a value
%%       stored at a given reference, if not found it returns the supplied
%%       default value
%%       
read_inherited_value(RefX, Key, Default) when is_record(RefX, refX) ->
    F = fun hn_db_wu:read_inherited/3,
    mnesia:activity(transaction, F, [RefX, Key, Default]).

%% @spec read_attributes(#refX{}, AttrList) -> {#refX{}, Val}
%% AttrList = [list()]
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
read_attributes(RefX, AttrList) when is_record(RefX, refX), is_list(AttrList) ->
    Fun = fun hn_db_wu:read_attrs/2,
    mnesia:activity(transaction, Fun, [RefX, AttrList]).

%% @spec read_whole_page(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc read takes a page refererence and returns all the attributes under that
%% refernce including column, row, page, permission and user attributes
%% 
%% The <code>refX{}</code> must only be a page refX
read_whole_page(#refX{obj = {page, "/"}} = RefX) ->
    Fun = fun() ->    
              hn_db_wu:read_whole_page(RefX)
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
                  ?wu:read_cells(RefX)
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
                  hn_db_wu:read_styles(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec recalculate(#refX{}) -> ok
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
                  ok = init_front_end_notify(),
                  Cells = ?wu:read_attrs(RefX, ["formula"]),
                  [ok = ?wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("recalculate").

%% @spec reformat(#refX{}) -> ok
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
                  ok = init_front_end_notify(),
                  Cells = ?wu:read_attrs(RefX, ["format"]),
                  [ok = ?wu:write_attr(X, Y) || {X, Y} <- Cells]
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("reformat").

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
    move(RefX, insert, horizontal);
insert(#refX{obj = {row, _}} = RefX)  ->
    move(RefX, insert, vertical);
insert(#refX{obj = R} = RefX) when R == cell orelse R == range  ->
    move(RefX, insert, vertical).

%% @spec insert(RefX :: #refX{}, Type) -> ok 
%% Type = [horizontal | vertical]
%% @doc inserts a cell or range
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% </ul>
%% 
%% The Type variable determines how the insert displaces the existing cases...
insert(#refX{obj = {R, _}} = RefX, Disp)
  when is_record(RefX, refX), (R == cell orelse R == range),
       (Disp == horizontal orelse Disp == vertical)->
    move(RefX, insert, Disp).

%% @spec delete(Ref :: #refX{}) -> ok
%% @doc deletes a column or a row or a page
%% 
%% @todo this is all bollocks - should be row, column then cell/range as 
%% per insert/2.
%% This needs to check if it intercepts a shared formula
%% and if it does it should fail...
delete(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->
    % io:format("in hn_db_api:delete (row/col) ~n-RefX is ~p~n-R is ~p",
    %          [RefX, R]),
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    move(RefX, delete, Disp);
delete(#refX{obj = {page, _}} = RefX) ->
    % io:format("in hn_db_api:delete (page)~n-RefX is ~p~n", [RefX]),
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  ?wu:delete(RefX)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("delete").

%% @spec delete(RefX :: #refX{}, Type) -> ok
%% Type = [horizontal | vertical]
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
delete(#refX{obj = {R, _}} = RefX, Disp)
  when R == cell orelse R == range orelse R == row orelse R == column ->
    % io:format("in hn_db_api:delete/2~n-RefX is ~p~n-R is ~p",
    %          [RefX, R]),
    move(RefX, delete, Disp).

move(RefX, Type, Disp)
  when (Type == insert orelse Type == delete)
       andalso (Disp == vertical orelse Disp == horizontal) ->
    #refX{site = Site, obj = Obj} = RefX,
    % io:format("In hn_dp_api:move~n-RefX is ~p~n-Type is ~p~n-Disp is ~p~n",
    %          [RefX, Type, Disp]),
    Fun =
        fun() ->
                ok = init_front_end_notify(),
                % if the Type is delete we first delete the original cells
                Disp2 = atom_to_list(Disp),
                _NewVsn = ?wu:get_new_local_page_vsn(RefX, {insert, Disp2}),
                % io:format("in hn_db_api:move~n-Site is ~p~n-RefX is ~p~n-"++
                %          "NewVsn is ~p~n",
                %          [Site, RefX, NewVsn]),
                Off = get_offset(Type, Disp, Obj),
                % io:format("in hn_db_api:move~n-Off is ~p~n", [Off]),
                % when the move type is DELETE the cells that are moved
                % DO NOT include the cells described by the reference
                % but when the move type is INSERT the cells that are
                % move DO include the cells described by the reference
                % To make this work we shift the RefX up 1, left 1 
                % before getting the cells to shift for INSERT
                {Sort, RefXs} =
                    case {Type, Disp} of
                        {insert, horizontal} -> RefX2 = insert_shift(RefX, Disp),
                                                List = ?wu:get_refs_right(RefX2),
                                                {'right-to-left', List};
                        {insert, vertical}   -> RefX2 = insert_shift(RefX, Disp),
                                                List = ?wu:get_refs_below(RefX2),
                                                {'bottom-to-top', List};
                        {delete, horizontal} -> List = ?wu:get_refs_right(RefX),
                                                {'left-to-right', List};
                        {delete, vertical}   -> List = ?wu:get_refs_below(RefX),
                                                {'bottom-to-top', List}
                    end,
                % if this is a delete - we need to actually delete the cells
                ok = case Type of
                         delete -> ?wu:clear_cells(RefX);
                         insert -> ok
                     end,
                % we sort the cells so that 
                % * if we are INSERTING we DONT overwrite cells...
                % * if we are DELETING we DO overwrite cells...
                RefXs2 = dbsort(RefXs, Sort),
                % io:format("in hn_db_api:move~n-Sort is ~p~n-RefXs2 is ~p~n",
                %           [Sort, RefXs2]),
                [ok = ?wu:shift_cell(F, offset(F, Off)) || F <- RefXs2],
                % now notify all parents and children of all cells on
                % this page
                PageRef = RefX#refX{obj = {page, "/"}},

                % OK all our local stuff is sorted, now lets deal with the remote
                % children
                {R, Rest} = Obj,
                Change = {insert, {R, Rest}, Disp},
                % set the delay to zero
                ok = ?wu:mark_notify_out_dirty(PageRef, Change, 0),

                % Jobs a good'un, now for the remote parents
                %io:format("in hn_db_api:move do something with Parents...~n"),
                _Parents =  ?wu:find_incoming_hn(Site, PageRef),
                %io:format("in hn_db_api:move Parents are ~p~n", [Parents]),
                ok
        end,
    ok = mnesia:activity(transaction, Fun),
    ok = tell_front_end("move").

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
                ok = init_front_end_notify(),
                ?wu:clear_cells(RefX, Type)
        end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("clear").

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
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To),
                  ok = ?wu:clear_cells(From, all)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("cut n paste").

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
                  ok = init_front_end_notify(),
                  ok = copy_n_paste2(From, To)
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("copy n paste").

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
    % io:format("in drag_n_drop~nFrom is ~p~nTo is ~p~n", [From, To]),
    Fun = fun() ->
                  ok = init_front_end_notify(),
                  case is_valid_d_n_d(From, To) of
                      {ok, single_cell, Incr}   -> ?wu:?copy(From, To, Incr);
                      {ok, 'onto self', _Incr}  -> ok;
                      {ok, cell_to_range, Incr} -> copy2(From, To, Incr)
                  end
          end,
    mnesia:activity(transaction, Fun),
    ok = tell_front_end("drag n drop").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_front_end_notify() ->
    _Return = put('front_end_notify', []),
    ok.

tell_front_end(_X) ->
    List = get('front_end_notify'),
    % io:format("in tell_front_end~n-for ~p~n-List is ~p~n", [X, List]),
    Fun = fun({Key, A, B}) ->
                  case Key of
                      {S, P} -> remoting_reg:notify_style(S, P, A, B);
                      _     -> #ref{name = N, site = S1, path = P1,
                                    ref = Rf} = Key,
                               case N of
                                   "__"++_ -> ok; 
                                   _Else   -> ?not_ch(S1, P1, B, Rf, N, A)
                               end
                  end
          end,
    [ok = Fun(X) || X <- List],
    ok.

% normal change
%    #ref{name=Name, site=Site, path=Path, ref=Rf} = Ref,
%    case Name of
%        "__"++_ -> ok; 
%        _Else   -> % io:format("in tell_front_end~n-Ref is ~p~n-Val is ~p~n",
%                   %          [Ref, Val]),
%                   remoting_reg:notify_change(Site, Path, Type, Rf, Name, Val)
%    end.
% Style
%    remoting_reg:notify_style(Site, Path, Index, Style).


get_offset(insert, D, {cell,     _})              -> g_o1(D, 1, 1);
get_offset(insert, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, Y2 - Y1 + 1); 
get_offset(insert, D, {column, {X1, X2}})         -> g_o1(D, X2 - X1 + 1, 0); 
get_offset(insert, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, X2 - X1 + 1,
                                                          Y2 - Y1 + 1);
get_offset(delete, D, {cell,    _})               -> g_o1(D, -1, -1);
get_offset(delete, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, -(Y2 - Y1 + 1)); 
get_offset(delete, D, {column, {X1, X2}})         -> g_o1(D, -(X2 - X1 + 1), 0); 
get_offset(delete, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, -(X2 - X1 + 1),
                                                          -(Y2 - Y1 + 1)). 

g_o1(vertical, _X, Y)   -> {0, Y};
g_o1(horizontal, X, _Y) -> {X, 0}.

shift(RefX, XOff, YOff) ->
    #refX{obj = {cell, {XF, YF}}} = RefX,
    % finally make the new RefX record
    XT = XF + XOff,
    YT = YF + YOff,
    RefX#refX{obj = {cell, {XT, YT}}}.

%% takes a list of incoming hypernumbers and an displacement around a reference
%% and then only returns those hypernumbers which need to be changed
shorten(List, {cell, {X, Y}}, Displacement) ->
    shorten(List, {range, {X, Y, X, Y}}, Displacement);
shorten(List, {range, {X1, Y1, X2, Y2}} = _Range, "vertical") ->
    {X1a, Y1a, X2a, _Y2a} = hn_util:rectify_range(X1, Y1, X2, Y2),
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, MyY}}} = Parent,
                  ((MyX >= X1a) and (MyX =< X2a) and (MyY > Y1a))
          end,
    lists:filter(Fun, List);
shorten(List, {range, {X1, Y1, X2, Y2}} = _Range, "horizontal") ->
    {X1a, Y1a, _X2a, Y2a} = hn_util:rectify_range(X1, Y1, X2, Y2),
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, MyY}}} = Parent,
                  ((MyY >= Y1a) and (MyY =< Y2a) and (MyX > X1a))
          end,
    lists:filter(Fun, List);
shorten(List, {column, {X1, _X2}}, "horizontal") ->
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {MyX, _MyY}}} = Parent,
                  (MyX > X1)
          end,
    lists:filter(Fun, List);
shorten(List, {row, {Y1, _Y2}}, "vertical") ->
    Fun = fun(#incoming_hn{site_and_parent = {_Site, Parent}}) ->
                  #refX{obj = {cell, {_MyX, MyY}}} = Parent,
                  (MyY >= Y1)
          end,
    lists:filter(Fun, List).

insert_shift(#refX{obj = {cell, {X, Y}}} = RefX, vertical) ->
    RefX#refX{obj = {cell, {X, Y - 1}}};
insert_shift(#refX{obj = {cell, {X, Y}}} = RefX, horizontal) ->
    RefX#refX{obj = {cell, {X - 1, Y}}};
insert_shift(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {range, {X1, Y1 - 1, X2, Y2 - 1}}};
insert_shift(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX, horizontal) ->
    RefX#refX{obj = {range, {X1 - 1, Y1, X2 - 1, Y2}}};
insert_shift(#refX{obj = {row, {Y1, Y2}}} = RefX, vertical) ->
    RefX#refX{obj = {row, {Y1 - 1, Y2 -1}}};
insert_shift(#refX{obj = {column, {X1, X2}}} = RefX, horizontal) ->
    RefX#refX{obj = {column, {X1 - 1, X2 -1}}};
insert_shift(RefX, _Disp) -> RefX.


%% converts the dependency tree from the 'wire format' to the 'database format'
convertdep(Child, DepTree) when is_record(Child, refX) ->
    #refX{site = Site} = Child,
    convertdep1(DepTree, Site, []).

convertdep1([], _Site, Acc)      -> Acc;
convertdep1([H | T ], Site, Acc) ->
    {ok, Ref} = hn_util:parse_url(H),
    #ref{site = Site1} = Ref,
    case Site of
        Site1 -> convertdep1(T, Site, [{url, [{type, "local"}], [H]} | Acc]);
        _     -> convertdep1(T, Site, [{url, [{type, "remote"}],[H]} | Acc])
    end.

% extracts the key value pairs
extract_kvs(List) -> extract_kvs1(List, []).

extract_kvs1([], Acc)             -> Acc;
extract_kvs1([{_R, KV} | T], Acc) -> extract_kvs1(T, [KV | Acc]).

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
dbsort(List, 'top-to-bottom') ->
    Fun = fun(#refX{obj = {cell, {_XA, YA}}},
              #refX{obj = {cell, {_XB, YB}}}) ->
                  if
                      (YA < YB)  -> true;
                      (YA >= YB) -> false
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
    lists:sort(Fun, List);
dbsort(List, 'left-to-right') ->
    Fun = fun(#refX{obj = {cell, {XA, _YA}}},
              #refX{obj = {cell, {XB, _YB}}}) ->
                  if
                      (XA < XB)  -> true;
                      (XA >= XB) -> false
                  end
          end,
    lists:sort(Fun, List).

copy_n_paste2(From, To) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> ?wu:copy_cell(From, To, false);
        {ok, 'onto self'}    -> ok;
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
is_valid_d_n_d(#refX{obj = {range, {X, _FY1, X, _FY2}}},
               #refX{obj = {range, {X, _TY1, X, _TY2}}}) -> {ok, col_range_to_range};
is_valid_d_n_d(#refX{obj = {range, {_FX1, Y, _FX2, Y}}},
               #refX{obj = {range, {_TX1, Y, _TX2, Y}}}) -> {ok, row_range_to_range};
is_valid_d_n_d(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

%% cell to range
copy2(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    lists:map(fun(X) -> ?wu:?copy(From, X, Incr) end, List),
    ok.

%% range to range
copy3(From, To, Incr) when is_record(From, refX), is_record(To, refX) ->
    % range to range copies are 'tiled'
    TileList = get_tiles(From, To),
    copy3a(From, TileList, Incr).

copy3a(_From, [], _Incr)    -> ok;
copy3a(From, [H | T], Incr) -> FromRange = hn_util:range_to_list(From),
                               ToRange = hn_util:range_to_list(H),
                               ok = copy3b(FromRange, ToRange, Incr),
                               copy3a(From, T, Incr).

copy3b([], [], _Incr)              -> ok;
copy3b([FH | FT], [TH | TT], Incr) -> ok = ?wu:copy_cell(FH, TH, Incr),
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
