%%%-----------------------------------------------------------------------------
%%% File        : manager.erl
%%% Author      : Dale Harvey <dale@arandomurl.com>
%%% Description : Module which provides management to the spriki
%%%               application
%%%
%%% Created     : 19 Sep 2007 by Dale Harvey <dale@arandomurl.com>
%%%-----------------------------------------------------------------------------
-module(manager).

-export([out/1]).

-include_lib("mnemosyne/include/mnemosyne.hrl").
-include("yaws_api.hrl").
-include("spriki.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This section contains the handler for the input from Yaws                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

out(Arg) ->

    Url = yaws_api:request_url(Arg),
    Port = case {Url#url.port,Url#url.scheme} of
           {undefined,"http"} -> "80";
           {undefined,"https"} -> "443";
           {Else,_} -> integer_to_list(Else)
       end,

    Site   = lists:concat([Url#url.scheme,"://",Url#url.host,":",Port]),
    Path   = Url#url.path,
    SectList = util2:rev_chop(Path),
    FileName = Arg#arg.docroot++Path,

    case filelib:is_file(FileName) and not filelib:is_dir(FileName) of
        true  -> {page,Path};
        false ->
            case process_post(Arg,Site,Path) of
                {postresponse, Response}->  {html,Response};
                {ok,_Reason}->
                case SectList of
                    ["node","@erl"|_T] ->        {content,"text",io_lib:print(node())};
                    ["nodes","@erl"|_T] ->       {content,"text",io_lib:print(nodes())};
                    ["memory",Node,"@erl"|_T]->  {content,"text",get_memory(Node)};
                    %% Now get Mnesia information
                    %%
                    ["@mnesia"|_T] ->            {html,get_mnesia_info_page()};
                    ["access_module","@mnesia"|_T] -> {content,"text",get_mnesia_info(access_module)};
                    ["auto_repair","@mnesia"|_T] ->   {content,"text",get_mnesia_info(auto_repair)};
                    ["backup_module","@mnesia"|_T] -> {content,"text",get_mnesia_info(backup_module)};
                    ["checkpoints","@mnesia"|_T] ->   {content,"text",get_mnesia_info(checkpoints)};
                    ["db_nodes","@mnesia"|_T] ->      {content,"text",get_mnesia_info(db_nodes)};
                    ["debug","@mnesia"|_T] ->         {content,"text",get_mnesia_info(debug)};
                    ["directory","@mnesia"|_T] ->     {content,"text",mnesia:system_info(directory)};
                    ["dump_log_load_regulation","@mnesia"|_T] ->  {content,"text", get_mnesia_info(dump_log_load_regulation)};
                    ["dump_log_time_threshold","@mnesia"|_T] ->   {content,"text", get_mnesia_info(dump_log_time_threshold)};
                    ["dump_log_update_in_place","@mnesia"|_T] ->  {content,"text", get_mnesia_info(dump_log_update_in_place)};
                    ["dump_log_write_threshold","@mnesia"|_T] ->  {content,"text", get_mnesia_info(dump_log_write_threshold)};
                    ["embedded_mnemosyne","@mnesia"|_T] ->        {content,"text",get_mnesia_info(embedded_mnemosyne)};
                    ["event_module","@mnesia"|_T] -> {content,"text",get_mnesia_info(event_module)};
                    ["extra_db_nodes","@mnesia"|_T] -> {content,"text",get_mnesia_info(extra_db_nodes)};
                    ["fallback_activated","@mnesia"|_T] -> {content,"text",get_mnesia_info(fallback_activated)};
                    ["held_locks","@mnesia"|_T] -> {content,"text",get_mnesia_info(held_locks)};
                    ["ignore_fallback_at_startup","@mnesia"|_T] -> {content,"text",get_mnesia_info(ignore_fallback_at_startup)};
                    ["fallback_error_function","@mnesia"|_T] -> {content,"text",get_mnesia_info(fallback_error_function)};
                    ["is_running","@mnesia"|_T] -> {content,"text",get_mnesia_info(is_running)};
                    ["local_tables","@mnesia"|_T] -> {content,"text",get_mnesia_info(local_tables)};
                    ["lock_queue","@mnesia"|_T] -> {content,"text",get_mnesia_info(lock_queue)};
                    ["log_version","@mnesia"|_T] -> {content,"text",mnesia:system_info(log_version)};
                    ["master_node_tables","@mnesia"|_T] -> {content,"text",get_mnesia_info(master_node_tables)};
                    ["max_wait_for_decision","@mnesia"|_T] -> {content,"text", get_mnesia_info(max_wait_for_decision)};
                    ["protocol_version","@mnesia"|_T] -> {content,"text",get_mnesia_info(protocol_version)};
                    ["running_db_nodes","@mnesia"|_T] -> {content,"text",get_mnesia_info(running_db_nodes)};
                    ["schema_location","@mnesia"|_T] -> {content,"text",get_mnesia_info(schema_location)};
                    ["schema_version","@mnesia"|_T] -> {content,"text",get_mnesia_info(schema_version)};
                    ["subscribers","@mnesia"|_T] -> {content,"text",get_mnesia_info(subscribers)};
                    ["tables","@mnesia"|_T] -> {content,"text",get_mnesia_info(tables)};
                    ["transaction_commits","@mnesia"|_T] -> {content,"text",get_mnesia_info(transaction_commits)};
                    ["transaction_failures","@mnesia"|_T] -> {content,"text",get_mnesia_info(transaction_failures)};
                    ["transaction_log_writes","@mnesia"|_T] -> {content,"text", get_mnesia_info(transaction_log_writes)};
                    ["transaction_restarts","@mnesia"|_T] -> {content,"text",get_mnesia_info(transaction_restarts)};
                    ["transactions","@mnesia"|_T] -> {content,"text",get_mnesia_info(transactions)};
                    ["use_dir","@mnesia"|_T] ->  {content,"text",get_mnesia_info(use_dir)};
                    ["core_dir","@mnesia"|_T] -> {content,"text",get_mnesia_info(core_dir)};
                    ["no_table_loaders","@mnesia"|_T] -> {content,"text",get_mnesia_info(no_table_loaders)};
                    ["version","@mnesia"|_T] -> {content,"text",mnesia:system_info(version)};
                    ["details",Table,"@mnesia_table"|_T]-> {html,get_table_details(Table)};
                    ["all",Table,"@mnesia_table"|_T]->     {html,get_tab_info1(Table)};
                    ["access_mode",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(access_mode,Table)};
                    ["active_replicas",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(active_replicas,Table)};
                    ["arity",Table,"@mnesia_table"|_T]->   {content,"text",get_tab_info(arity,Table)};
                    ["attributes",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(attributes,Table)};
                    ["checkpoints",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(checkpoints,Table)};
                    ["commit_work",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(commit_work,Table)};
                    ["cookie",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(cookie,Table)};
                    ["cstruct",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(cstruct,Table)};
                    ["disc_copies",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(disc_copies,Table)};
                    ["disc_only_copies",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(disc_only_copies,Table)};
                    ["frag_properties",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(frag_properties,Table)};
                    ["info",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info_page(Table)};
                    ["index",Table,"@mnesia_table"|_T]->  {content,"text",get_tab_info(index,Table)};
                    ["load_by_force",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(load_by_force,Table)};
                    ["load_node",Table,"@mnesia_table"|_T]->  {content,"text",get_tab_info(load_node,Table)};
                    ["load_order",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(load_order,Table)};
                    ["load_reason",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(load_reason,Table)};
                    ["local_content",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(local_content,Table)};
                    ["master_nodes",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(master_nodes,Table)};
                    ["memory",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(memory,Table)};
                    ["ram_copies",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(ram_copies,Table)};
                    ["record_name",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(record_name,Table)};
                    ["record_validation",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(record_validation,Table)};
                    ["type",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(type,Table)};
                    ["size",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(size,Table)};
                    ["snmp",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(snmp,Table)};
                    ["storage_type",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(storage_type,Table)};
                    ["subscribers",Table,"@mnesia_table"|_T]->  {content,"text",get_tab_info(subscribers,Table)};
                    ["user_properties",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(user_properties,Table)};
                    ["version",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(version,Table)};
                    ["where_to_read",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(where_to_read,Table)};
                    ["where_to_write",Table,"@mnesia_table"|_T]-> {content,"text",get_tab_info(where_to_write,Table)};
                    ["wild_pattern",Table,"@mnesia_table"|_T]->  {content,"text",get_tab_info(wild_pattern,Table)};
                    ["?admin"|_T] -> {page,?HTML_ROOT++"admin.html"};
                    _Other    -> {page,?HTML_ROOT++"madmin.html"}
              end;
           {error, Error} ->
              {html,old_html:show_error_page(Error)}
        end
    end.

process_post(Arg,_Site,_Path)->
    case (Arg#arg.req)#http_request.method of
    'POST' ->
        Ref = case yaws_api:postvar(Arg,"ref") of
            {ok,R} -> R; E -> E end,

        case Ref of
        "ping" ->
            {ok,Value}=yaws_api:postvar(Arg,"value"),
            Node=list_to_atom(Value),
            case net_adm:ping(Node) of
            pong  -> {postresponse,"ok\n"++Value++" pinged"};
            Other -> {postresponse,"error\n"++Value++
                  " ping failed with message "++Other}
            end;
        "add table copy" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,Node}=yaws_api:postvar(Arg,"node"),
            {ok,Type}=yaws_api:postvar(Arg,"type"),
            {postresponse, add_table_copy(Table,Node,Type)};
        "add table index" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,Attribute}=yaws_api:postvar(Arg,"attribute"),
            {postresponse, add_table_index(Table,Attribute)};
        "change table access mode" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,AccessMode}=yaws_api:postvar(Arg,"accessmode"),
            {postresponse, change_table_access_mode(Table,AccessMode)};
        "change table copy type" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,Node}=yaws_api:postvar(Arg,"node"),
            {ok,Type}=yaws_api:postvar(Arg,"type"),
            {postresponse, change_table_copy_type(Table,Node,Type)};
        "del table copy" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,Node}=yaws_api:postvar(Arg,"node"),
            {postresponse, del_table_copy(Table,Node)};
        "del table index" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {ok,Attribute}=yaws_api:postvar(Arg,"attribute"),
            {postresponse, del_table_index(Table,Attribute)};
        "delete table" ->
            {ok,Table}=yaws_api:postvar(Arg,"table"),
            {postresponse, delete_table(Table)};
        _Other -> {ok,ok}
        end;
    _Other  ->
        {ok, ok} % We don't care except only about POSTs
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions manage all the above                                     %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_table(Table)->
    io:format("in flemnesia:delete_table with Table of ~p~n",[Table]),
    case mnesia:delete_table(list_to_atom(Table)) of
    {atomic, ok} ->
        "ok\ntable "++Table++" deleted";
    {aborted, Reason} ->
        "error\ndeleting table "++Table++
        " failed with error "++io_lib:print(Reason);
    Other -> io:format("in flemnesia:delete_table returned Other ~p~n",
               [Other])
    end.

del_table_index(Table,Attribute)->
    case mnesia:del_table_index(list_to_atom(Table),list_to_atom(Attribute)) of
    {atomic, ok} ->
        "ok\nindex "++Attribute++" deleted from table "++Table;
    {aborted, Reason} ->
        "error\ndeleting index "++Attribute++" from table "++Table++
        " failed with error "++io_lib:print(Reason)
    end.

del_table_copy(Table,Node)->

    case mnesia:del_table_copy(list_to_atom(Table),list_to_atom(Node)) of
    {atomic, ok} ->
        "ok\n table copy "++Table++" on "++Node++" deleted";
    {aborted, Reason} ->
        "error\ndelete of table copy "++Table++" on "++Node++
        " failed with error "++io_lib:print(Reason)
    end.

change_table_copy_type(Table,Node,Type)->
    case mnesia:change_table_copy_type(list_to_atom(Table),list_to_atom(Node),
                       list_to_atom(Type)) of
    {atomic, ok} ->
        "ok\n table copy "++Table++" on "++Node++
        " had type changed to"++Type;
    {aborted, Reason} ->
        "error\ntable copy "++Table++" on "++Node++
        " type change to "++Type++" failed with error "++
        io_lib:print(Reason)
    end.

change_table_access_mode(Table,AccessMode)->
    case mnesia:change_table_access_mode(list_to_atom(Table),
                     list_to_atom(AccessMode)) of
    {atomic, ok} ->
        "ok\nindex "++AccessMode++" changed on table "++Table;
    {aborted, Reason} ->
        "error\naccess mode change to "++AccessMode++" on table "++Table++
        " failed with error "++io_lib:print(Reason)
    end.

add_table_index(Table,Attribute)->
    case mnesia:add_table_index(list_to_atom(Table),list_to_atom(Attribute)) of
    {atomic, ok} ->
        "ok\nindex "++Attribute++" added to table "++Table;
    {aborted, Reason} ->
        "error\nadding index "++Attribute++" to table "++Table++
        " failed with error "++io_lib:print(Reason)
    end.

add_table_copy(Table,Node,Type)->
    case mnesia:add_table_copy(list_to_atom(Table),list_to_atom(Node),
                   list_to_atom(Type)) of
    {atomic, ok} ->
        "ok\n table copy "++Table++" added to "++Node++" with type "++Type;
    {aborted, Reason} ->
        "error\ntable copy "++Table++" added to "++Node++
        " with type "++Type++" failed with error "++
        io_lib:print(Reason)
    end.

get_table_details(Table)->
    Table2=list_to_existing_atom(string:strip(Table)),
    "TABLE STRUCTURE\n"++
    "Attributes: "++io_lib:print(mnesia:table_info(Table2,
                       attributes))++"\n"++
    "Record Name: "++io_lib:print(mnesia:table_info(Table2,
                       record_name))++"\n"++
    "Type: "++io_lib:print(mnesia:table_info(Table2,
                       type))++"\n"++
    "Storage Type: "++io_lib:print(mnesia:table_info(Table2,
                       storage_type))++"\n"++
    "Index: "++io_lib:print(mnesia:table_info(Table2,
                       index))++"\n"++
    "Access Mode:"++io_lib:print(mnesia:table_info(Table2,
                       access_mode))++"\n"++
    "\nTABLE DISTRIBUTION:\n"++
    "Active Replicas: "++io_lib:print(mnesia:table_info(Table2,
                       active_replicas))++"\n"++
    "Disc Copies: "++io_lib:print(mnesia:table_info(Table2,
                       disc_copies))++"\n"++
    "Disc Only Copies: "++io_lib:print(mnesia:table_info(Table2,
                       disc_only_copies))++"\n"++
    "Ram Copies: "++io_lib:print(mnesia:table_info(Table2,
                       ram_copies))++"\n".

get_memory(Node)->
    case atom_to_list(node()) of
    Node  -> flatten(erlang:memory());
    _Other-> flatten(rpc:call(list_to_atom(Node),erlang,memory,[]))
    end.

get_tab_info1(Table)->
    List=mnesia:table_info(list_to_existing_atom(string:strip(Table)),all),
    flatten(List).

flatten(List)->
    flatten(List,[]).

flatten([],Residuum)->
    Return=lists:reverse(Residuum),
    Return;
flatten([{Key,Value}|T],Residuum) ->
    NewResiduum = [io_lib:write(Key)++","++io_lib:write(Value)++"\n"|Residuum],
    flatten(T,NewResiduum).

get_tab_info(Details,Table)->
    Page=lists:flatten([mnesia:table_info(list_to_existing_atom(string:strip(Table)),
                      Details)]),
    io_lib:print(Page).

get_tab_info_page(Table)->
    Contents=lists:flatten([mnesia:table_info(list_to_existing_atom(string:strip(Table)),
                          all)]),
    FlatContent=io_lib:print(Contents),
    get_html_top()++"\n"++
    "<h3>Information about Mnesia Table "++Table++
    " obtained by running mnesia:table_info:"++
    "</h3>"++
    FlatContent++"\n"++
    "<br><h3>Each of these can be individually queried "++
    "using the links below:</h3>"++
    get_mnesia_table_links(Table)++
    get_html_bottom().

get_mnesia_info_page()->
    Contents=lists:flatten([mnesia:system_info(all)]),
    FlatContent=io_lib:print(Contents),
    get_html_top()++"\n"++
    "<h3>Information about Mnesia obtained by running mnesia:system_info:"++
    "</h3>"++
    FlatContent++"\n"++
    "<br><h3>Each of these can be individually queried "++
    "using the links below:</h3>"++
    get_mnesia_links()++
    get_html_bottom().

get_mnesia_info(Details)->
    io_lib:print(lists:flatten(mnesia:system_info(Details))).

get_html_top()->
    "<html><head><title>Flemnesia!</title></head><body>".

get_html_bottom()->
    "</body></html>".

get_mnesia_table_links(Table)->
    "<a href=\"@mnesia_table/"++Table++
    "/access_mode\">access_mode</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/active_replicas\">active_replicas</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/arity\">arity</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/attributes\">attributes</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/checkpoints\">checkpoints</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/commit_work\">commit_work</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/cookie\">cookie</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/cstruct\">cstruct</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/disc_copies\">disc_copies</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/disc_only_copies\">disc_only_copies</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/frag_properties\">frag_properties</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/index\">index</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/load_by_force\">load_by_force</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/load_node\">load_node</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/load_order\">load_order</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/load_reason\">load_reason</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/local_content\">local_content</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/master_nodes\">master_nodes</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/memory\">memory</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/ram_copies\">ram_copies</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/record_name\">record_name</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/record_validation\">record_validation</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/type\">type</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/size\">size</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/snmp\">snmp</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/storage_type\">storage_type</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/subscribers\">subscribers</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/user_properties\">user_properties</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/version\">version</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/where_to_read\">where_to_read</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/where_to_write\">where_to_write</a><br>"++
    "<a href=\"@mnesia_table/"++Table++
    "/wild_pattern\">wild_pattern</a><br>".

get_mnesia_links()->
    "<a href=\"/@mnesia/access_module\">access_module</a><br>"++
    "<a href=\"/@mnesia/auto_repair\">auto_repair</a><br>"++
    "<a href=\"/@mnesia/backup_module\">backup_module</a><br>"++
    "<a href=\"/@mnesia/checkpoints\">checkpoints</a><br>"++
    "<a href=\"/@mnesia/db_nodes\">db_nodes</a><br>"++
    "<a href=\"/@mnesia/debug\">debug</a><br>"++
    "<a href=\"/@mnesia/directory\">directory</a><br>"++
    "<a href=\"/@mnesia/dump_log_load_regulation\">"++
    "dump_log_load_regulation</a><br>"++
    "<a href=\"/@mnesia/dump_log_time_threshold\">"++
    "dump_log_time_threshold</a><br>"++
    "<a href=\"/@mnesia/dump_log_update_in_place\">"++
    "dump_log_update_in_place</a><br>"++
    "<a href=\"/@mnesia/dump_log_write_threshold\">"++
    "dump_log_write_threshold</a><br>"++
    "<a href=\"/@mnesia/embedded_mnemosyne\">embedded_mnemosyne</a><br>"++
    "<a href=\"/@mnesia/event_module\">event_module</a><br>"++
    "<a href=\"/@mnesia/extra_db_nodes\">extra_db_nodes</a><br>"++
    "<a href=\"/@mnesia/fallback_activated\">fallback_activated</a><br>"++
    "<a href=\"/@mnesia/held_locks\">held_locks</a><br>"++
    "<a href=\"/@mnesia/ignore_fallback_at_startup\">"++
    "ignore_fallback_at_startup</a><br>"++
    "<a href=\"/@mnesia/fallback_error_function\">"++
    "fallback_error_function</a><br>"++
    "<a href=\"/@mnesia/is_running\">is_running</a><br>"++
    "<a href=\"/@mnesia/local_tables\">local_tables</a><br>"++
    "<a href=\"/@mnesia/lock_queue\">lock_queue</a><br>"++
    "<a href=\"/@mnesia/log_version\">log_version</a><br>"++
    "<a href=\"/@mnesia/master_node_tables\">master_node_tables</a><br>"++
    "<a href=\"/@mnesia/max_wait_for_decision\">"++
    "max_wait_for_decision</a><br>"++
    "<a href=\"/@mnesia/protocol_version\">protocol_version</a><br>"++
    "<a href=\"/@mnesia/running_db_nodes\">running_db_nodes</a><br>"++
    "<a href=\"/@mnesia/schema_location\">schema_location</a><br>"++
    "<a href=\"/@mnesia/schema_version\">schema_version</a><br>"++
    "<a href=\"/@mnesia/subscribers\">subscribers</a><br>"++
    "<a href=\"/@mnesia/tables\">tables</a><br>"++
    "<a href=\"/@mnesia/transaction_commits\">transaction_commits</a><br>"++
    "<a href=\"/@mnesia/transaction_failures\">"++
    "transaction_failures</a><br>"++
    "<a href=\"/@mnesia/transaction_log_writes\">"++
    "transaction_log_writes</a><br>"++
    "<a href=\"/@mnesia/transaction_restarts\">"++
    "transaction_restarts</a><br>"++
    "<a href=\"/@mnesia/transactions\">transactions</a><br>"++
    "<a href=\"/@mnesia/use_dir\">use_dir</a><br>"++
    "<a href=\"/@mnesia/core_dir\">core_dir</a><br>"++
    "<a href=\"/@mnesia/no_table_loaders\">no_table_loaders</a><br>"++
    "<a href=\"/@mnesia/version\">version</a><br>".
