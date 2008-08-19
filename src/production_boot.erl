%%%-------------------------------------------------------------------
%%% File        : production_boot.erl
%%% Author      : Gordon Guthrie
%%% Description :
%%%
%%% Created     : 11th March 2005 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------
-module(production_boot).

-export([
    root/0,
    setup_paths/0,
    start/0,
    start/1,
    stop/0,
    stop_halt/0,
    remote_stop/1,
    read_conf/0
    ]).

root() ->
    [_File,_Ebin|Rest] = lists:reverse(
        string:tokens(code:which(production_boot),"/")),
    Pre = case os:type() of
        {win32,_} -> ""; 
        _ ->         "/" 
    end,
    Pre++implode(lists:reverse(Rest),"/")++"/".
    
start(_Toolbar) ->
    Conf = read_conf(),
    setup_paths(Conf),
    start_apps(Conf,true).

start() ->
    Conf = read_conf(),
    setup_paths(Conf),
    start_apps(Conf,false).

read_conf() ->
    readxml_file(root()++"conf/hypernumbers.xml").

setup_paths() ->
    setup_paths(read_conf()).
    
setup_paths(Conf) ->

    Abs = values([],xmlsearch([],
        xmlsearch([],Conf,includes),absdir)),
    Rel = values([],xmlsearch([],
        xmlsearch([],Conf,includes),reldir)),
    Erl = values([],xmlsearch([],
        xmlsearch([],Conf,includes),erldir)),

    lists:map(fun(X) -> code:add_patha(X) end,Abs),
    lists:map(fun(X) -> code:add_patha(root()++X) end,Rel),
    lists:map(fun(X) -> code:add_patha(code:which(list_to_atom(X))) end,Erl),

    ok.

start_apps(Conf,Toolbar)->
    %% Start our applications
    application:start(read_excel),
    ok = application:start(engine),
    application:start(random_app),

    file:set_cwd("../lib/starling"),
    application:start(starling_app),
    file:set_cwd("../../../src"),
    
    code:add_path("../priv"),
    case Toolbar of
        true -> toolbar:start(); _ -> ok
    end.

stop() ->
    toolbar:quit(),
    application:stop(yaws),
    application:stop(mnesia),
    application:stop(mnemosyne),
    application:stop(remoting),
    application:stop(read_excel),
    application:stop(inets),
    application:stop(starling_app).

stop_halt() ->
    stop(),
    halt().

%% Call this from another node to stop the app
%% this code nicked wholesale from wiki.erl in the Jungerl
remote_stop([Node]) ->
    {ok, HostName} = inet:gethostname(),
    FQNode=list_to_atom(lists:concat([Node, "@", HostName])),
    io:format("Stop:~p~n",[FQNode]),
    case net_adm:ping(FQNode) of
	pong -> ok;
	pang ->
	    io:format("There is no node with this name~n")
    end,
    rpc:cast(FQNode, production_boot, stop_halt, []),
    init:stop().

%% These utilities usually reside in lib/engine/src/hn_util, but since I use
%% them to bootstrap, there are copies here, not be be exported though
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% XML Utils                                                                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
readxml_string(String) ->
    {Xml,_Misc} = xmerl_scan:string(String,[{space,normalize}]),
    SimpleXml = xmerl_lib:simplify_element(Xml),
    clear_whitespace(SimpleXml).

readxml_file(File) ->
    {ok,String} = read(File),
    readxml_string(String).

%%--------------------------------------------------------------------
%% Function:    clear_whitespace/1
%% Description: Clears the empty xml elements created by
%%              Insignificant whitespace
%%--------------------------------------------------------------------
clear_whitespace(Children) when is_list(Children) ->
    case io_lib:deep_char_list(Children) of
    true  -> lists:flatten(Children);
    false ->
        Clear = lists:filter(
            fun(X) ->
                case X of 
                    " " -> false; 
                    _   -> true 
                end
            end,Children),

        lists:map(fun(X) ->
                clear_whitespace(X) 
            end, Clear)
    end;

clear_whitespace({Name,Attr,Children}) ->
    {Name,Attr,clear_whitespace(Children)}.

%%--------------------------------------------------------------------
%% Function:    xmlsearch/3
%% Description: Searchs an xml tree for elements with name Name,
%%              returns a list of matching children or []
%%--------------------------------------------------------------------
xmlsearch(Rtn,[],_Name) -> Rtn;
xmlsearch(Rtn,{_Root,_Attr,Children},Name) ->
    xmlsearch(Rtn,Children,Name);
xmlsearch(Rtn,[{Root,Attr,Children}|T],Name) ->
    AddChild = case io_lib:char_list(Children) of
        true ->  Rtn;
        false -> Rtn ++ xmlsearch(Rtn,Children,Name)
    end,
    Self = case Root of
        Name -> [{Name,Attr,Children}];
        _ ->    Rtn
    end,
    AddChild ++ Self ++ xmlsearch(Rtn,T,Name).

%%--------------------------------------------------------------------
%% Function:    values/3
%% Description: Create a list of values from an xml tree
%%--------------------------------------------------------------------
values(Rtn,[]) -> Rtn;
values(Rtn,{_Root,_Attr,Children}) ->
    values(Rtn,Children);
values(Rtn,[{_Root,_Attr,Children}|T]) ->
    AddChild = case io_lib:char_list(Children) of
        true ->  lists:append(Rtn,[Children]);
        false -> lists:append(Rtn,values(Rtn,Children))
    end,
    lists:append(AddChild,values(Rtn,T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% File Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% Function:    read/1
%% Description: Reads a file into a string
%%--------------------------------------------------------------------
read(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    {ok,binary_to_list(Binary)}.

%%--------------------------------------------------------------------
%% Function:    implode/2
%%
%% Description: Add all the items in the list together into a string
%%              interspace with 'Token' (number converted to strings).
%%              (Token doesn't get added on to the last element).
%%--------------------------------------------------------------------
implode(Data, Seperator) when is_list(Data) andalso is_list(Seperator) ->
    lists:foldr(fun
        (X,[])  -> X; 
        (X,Acc) -> X++Seperator++Acc 
    end, "", Data).



