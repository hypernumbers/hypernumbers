%%% File    : move_tables.erl
%%% Author  : Tom McNulty 
%%% Created : 30 Nov 2009 by Tom McNulty

-module(migrate_tables).

-export([clone/2]).
-export([load_clone/1, load_clone/2]).
-export([delete/1]).

-define (IS_STRING(Term), (is_list(Term) andalso 
                           Term /= [] andalso 
                           is_integer(hd(Term)))).
-define(BACKUP_SRC,   "movetables_source").


clone("http://"++From=FromSite, "http://"++To=ToSite) ->
    [FromHost, FromPort | _Rest] = string:tokens(From, ":/"),
    [ToHost, ToPort | _Rest] = string:tokens(To, ":/"),

    %% String replacer
    {ok, Pat} = re:compile(FromSite),
    Replacer = fun(S1) ->
                       re:replace(S1, Pat, ToSite, [{return, list}])
               end,

    %% Create a checkpoint covering target tables.
    Tables = candidates(FromHost, FromPort),
    {ok, CP, _Nodes} = mnesia:activate_checkpoint([{max, Tables}]),

    %% Dump check point to backup file.
    ok = mnesia:backup_checkpoint(CP, ?BACKUP_SRC),
    ok = mnesia:deactivate_checkpoint(CP),

    %% Apply transformations to the backup.
    {ok,_} = mnesia:traverse_backup(?BACKUP_SRC, list_to_atom(ToHost++"_trans"),
                                    fun convert/2, 
                                    {ToHost, ToPort, Replacer}),
    ok.


load_clone(ToSite) ->
    load_clone(ToSite, false).

load_clone("http://"++To=ToSite, Fresh) ->
    [ToHost, _ToPort | _Rest] = string:tokens(To, ":/"),

    %% Create tables if fresh
    ok = case Fresh of
             true  -> hn_db_api:create_db(ToSite);
             false -> ok
         end,
    {atomic, Restored} = mnesia:restore(
                           list_to_atom(ToHost++"_trans"),
                           [{default_op, recreate_tables}]),
    io:format("Cloned tables: ~p~n", [Restored]).

    
delete("http://"++Delete) ->
    [DelHost, DelPort | _Rest] = string:tokens(Delete, ":/"),
    Tables = candidates(DelHost, DelPort),    
    [{atomic, ok} = mnesia:delete_table(T) || T <- Tables],
    ok.
    
convert({schema, db_nodes, _}=X, Acc) ->
    {[X], Acc}; 
convert({schema, version, _}=X, Acc) ->
    {[X], Acc}; 
convert({schema, cookie, _}=X, Acc) ->
    {[X], Acc}; 
convert({schema, OldTab, CreateList}, {Host, Port, _}=Acc) ->
    Tab = rename(OldTab, Host, Port),
    CreateList2 = lists:keyreplace(name, 1, CreateList, 
                                   {name, Tab}),
    CreateList3 = lists:keyreplace(ram_copies, 1, CreateList2, 
                                   {ram_copies, []}),
    CreateList4 = lists:keyreplace(disc_copies, 1, CreateList3, 
                                   {disc_copies, []}),
    CreateList5 = lists:keyreplace(disc_only_copies, 1, CreateList4, 
                                   {disc_only_copies, [node()]}),
    {[{schema, Tab, CreateList5}], Acc};
convert(Rec0, {Host, Port, Replacer}=Acc) ->
    %% Convert record to normal form, and transform
    Rec1 = convert_record(Rec0, Replacer),
    %% Convert record to table persisted form.
    Tab = rename(element(1, Rec0), Host, Port),
    Rec3 = setelement(1, Rec1, Tab),
    {[Rec3], Acc}.

convert_record(S, Replacer) when ?IS_STRING(S) ->
    Replacer(S);
convert_record(Ls, RF) when is_list(Ls) ->
    [convert_record(L, RF) || L <- Ls];
convert_record(R, RF) when is_tuple(R) ->
    Ls1 = erlang:tuple_to_list(R),
    Ls2 = [convert_record(L, RF) || L <- Ls1],
    erlang:list_to_tuple(Ls2);
convert_record(R, _RF) ->
    R.

candidates(Host, Port) ->
    Prefix = Host ++ "&" ++ Port,
    [T || T <- mnesia:system_info(tables),
          lists:prefix(Prefix, atom_to_list(T))].
           
rename(OldName, NewHost, NewPort) ->
    [_Host, _Port | Rest] = string:tokens(atom_to_list(OldName), "&"),
    list_to_atom(string:join([NewHost, NewPort] ++ Rest, "&")).
