%%%-----------------------------------------------------------------------------
%%% File        : hn_yaws.erl
%%% Author      : Dale Harvey <dale@hypernumbers.com>
%%% Description : this handles incoming http requests
%%%-----------------------------------------------------------------------------
-module(hn_yaws).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yaws_api.hrl").
-include("spriki.hrl").
-include("regexp.hrl").
-include("handy_macros.hrl").

%%%-----------------------------------------------------------------
%%% Exported Functions
%%%-----------------------------------------------------------------
-export([ out/1 ]).

%%%-----------------------------------------------------------------
%%% Imported Functions
%%%-----------------------------------------------------------------
-import(tconv, [to_l/1, to_s/1, to_b26/1]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Yaws handler for all websheet requests
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(Arg) -> try

    Url      = yaws_api:request_url(Arg),
    Page     = hn_util:parse_url(Url),
    FileName = Arg#arg.docroot++Url#url.path,

    %% If the request is for an actual file on the server, just serve it
    %% ie (images, css, javascript etc etc) 
    case filelib:is_file(FileName) and not filelib:is_dir(FileName) of
    true -> {page,Url#url.path};
    false ->

        PostData = get_post_data(Page#page.format,Arg,Page#page.vars),
        User     = hn_util:get_cookie((Arg#arg.headers)#headers.cookie),
        Perms    = ok,

        %% Handle POST request, users need admin or edit permissions to make
        %% a post request, unless they are trying to login
        PostResult = case PostData of
        [] -> ok;
        _  ->
            case true of
            true -> process_POST(Arg,PostData,User,Page);
            _    ->
                case Perms of 
                N when N == admin ; N == edit ->
                    process_POST(Arg,PostData,User,Page);
                _ -> {ok,[{"post",[],"error"}]}
                end
            end
        end,

        %% If POST has nothing to return, handle GET
        case PostResult of
        ok                  -> process_GET(Arg,{User,Perms},Page);
        {ok,Content}        -> format_output(Page#page.format,Content);
        {ok,Content,Cookie} -> [format_output(Page#page.format,Content),Cookie]
        end
    end

    %% Globally catch any errors during processing
    catch
    _:Err ->
        error_logger:error_msg(
            "~p~n~p",[Err,erlang:get_stacktrace()]),
        {status,400}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% GET handlers, 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return HTML pages, for "/" , "?admin" and "?import"
process_GET(_Arg,_User,#page{ref={page,_},vars=[]}) ->
    {page,"/html/index.html"};

process_GET(Arg,User,Page) ->
    
    {Format,Data} = case request_type(Page) of
    
    attribute ->
        Addr = page_to_ref(Page),
        
        %% Convert 'hnxml' into xml, leave strings alone
        F = fun(Val) ->
            ?COND(io_lib:char_list(Val) == true, 
                Val, hn_util:hnxml_to_xml(Val))
        end,

        %% Switch to filter on the db api later
        Items = lists:filter(
            fun(#hn_item{addr=A}) -> 
                case (A)#ref.name of
                "__"++_ -> false;
                _ -> true
                end
            end,
            hn_db:get_item(Addr)),

        List = lists:map(fun hn_util:item_to_xml/1 , Items),

        {Page#page.format,{attr,[],List}};
        
    pages ->
        {Page#page.format,{dir,[],[]}};
        
    hypernumber ->
    
        Addr = page_to_ref(Page),
        
        Val = fun() ->
            case hn_db:get_item_val(Addr#ref{name=value}) of
            []    -> {blank,[],[]};
            [Tmp] -> hn_util:hnxml_to_xml(Tmp)
            end
        end,
          
        {Page#page.format,{hypernumber,[],[
            {value,[], [Val()]},
            {'dependancy-tree',[],
                hn_db:get_item_val(Addr#ref{name='dependancy-tree'})
            }
        ]}};
        
                
    reference -> 
        case hn_db:get_item(page_to_ref(Page)) of
        []   -> {{plain}, "blank"};
        List -> 
        
            F = fun(X) -> 
                ?COND((X#hn_item.addr)#ref.name == value,true,false)
            end,
            
            [Val] = case lists:filter(F,List) of
            [] -> [0];
            [#hn_item{val=Value}] -> Value
            end,
            
            {{plain}, hn_util:text(hn_util:xml_to_val(Val))}
        end
    end,
    
    format_output(Format,Data).

%% Utility GET functions
%%--------------------------------------------------------------------
request_type(Page) ->
    Vars = Page#page.vars,
    case lists:member({attr},Vars) of
    true  -> attribute;
    false ->
        case lists:member({hypernumber},Vars) of
        true  -> hypernumber;
        false ->
            case lists:member({pages},Vars) of
            true  -> pages;
            false -> reference
            end
        end
    end.        

page_to_ref(#page{site=Site, path=Path, ref=Ref}) ->
    #ref{site=Site, path=Path, ref=Ref}.

%% Format the output depending on the requested format.
format_output(Format,Data) ->
    case Format of
    {xml}   -> {content,"text/xml",simplexml:to_xml_string(Data)};
    {json}  -> {content,"text/plain",simplexml:to_json_string(Data)};
    {plain} -> {content,"text/plain",Data};
    {json,nocallback} -> 
        {content,"text/plain","hn("++simplexml:to_json_string(Data)++");"}
    end.

%% Takes an unfiltered list of spriki records, extracts the path
%% they are from and constructs a tree
create_pages_tree(List) -> 

    Trees = lists:map(
        fun(X) ->
            create_tree(string:tokens(X,"/"))
        end,
    path_list(List,[])),

    {dir,[{path,"/"}],merge_trees(Trees)}.

merge_trees([])         -> [];
merge_trees([H|T])      -> merge_trees([H],T).

merge_trees(Tree,[])    -> Tree;
merge_trees(Tree,[[]])  -> Tree;
merge_trees(Tree,[H|T]) ->

    {dir,[{path,P}],C1} = H,

    {Match,Rest} = lists:partition(fun(X) ->
        case X of 
        {dir,[{path,P}],_} -> true;
        _ -> false
        end 
    end,Tree),

    case Match of
    %% No Matches, add entire tree
    [] -> merge_trees([H|Tree],T);

    %% Generate a new Tree on current path and 
    %% Add it to siblings
    [{dir,[{path,_}],C2}] ->
        merge_trees([{dir,[{path,P}],merge_trees(C1,C2)}]++Rest,T)
    end.

create_tree([]) -> [];
create_tree([H|T]) ->
    {dir,[{path,H}],[create_tree(T)]}.    

%% Filter records and remove duplicates
path_list([],PathList) -> PathList.
%path_list([#spriki{index=#index{path=Path}}|T],List) ->
%    case lists:member(Path,List) of
%    true  -> path_list(T,List);
%    false -> path_list(T,[Path|List])
%    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% POST handlers, 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_post_data(_,#arg{req=#http_request{method='GET'}},_Dec) ->
    [];
get_post_data(_,_Arg,[{import}]) -> 
    [import];
get_post_data({json,nocallback},Arg,Dec) -> 
    get_post_data({json},Arg,Dec);
get_post_data({json},Arg,_Dec) ->
    simplexml:from_json_string(binary_to_list(Arg#arg.clidata));
get_post_data({xml},Arg,_Dec) ->
    simplexml:from_xml_string(binary_to_list(Arg#arg.clidata)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This section handles the POST request of the REST API
%% calls, Read the API spcification for what further
%% functionality needs to be supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filter the API call (action)
process_POST(Arg,PostData,_User,Page) ->
    A = "action", 
    case PostData of
    [import]             -> api_import(Arg,Page);
    {create,[],Data}     -> api_create(Data,Page);
    [{A,"login"}|R]      -> api_login(R);
    [{A,"logout"}]       -> api_logout();
    [{A,"clear"}|R]      -> api_clear(R,Page);
    [{A,"insert"}|R]     -> api_insert(R,Page);
    [{A,"delete"}|R]     -> api_delete(R,Page);
    {register,[],Data}   -> api_reg(Data,Page);
    {unregister,[],Data} -> api_unreg(Data,Page);
    {notify,[],Data}     -> api_notify(Data,Page)
    end.

%%% API call - IMPORT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% import an excel file, the import and log txt probably needs 
%% cleaned up
api_import(Arg,Page) -> 
    F = fun(X) -> 
        filefilters:read(excel,X,fun(A) -> import(A,Page) end) 
    end,
    case hn_util:upload(Arg,"Filedata",F) of
    ok ->    {ok,{create,[],["success"]}};
    error -> {ok,{create,[],["error"]}}
    end.

import([],_)-> ok;
import([{_Name,Tid}|T],Page)->
    Fun = fun(X,_Y) ->
        case X of
        {{_,{row_index,Row},{col_index,Col}},[_,Val]} ->
            Req = lists:flatten([Page#page.site,Page#page.path,
                util2:make_b26(Col+1),hn_util:text(Row+1)]),
            V = case Val of
                {value,number,Num} -> Num;
                {formula,Form}     -> Form;
                {string,Str}       -> Str
            end,
            hn_util:post(Req,"<create><value>"++
                hn_util:text(V)++"</value></create>","text/xml");
        _ -> false
        end
    end,
    ets:foldl(Fun,[],Tid),
    import(T,Page).

%%% API call - CREATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_create(Data, Page) ->

    #page{site=Site,path=Path,ref=Ref,vars=_Vars} = Page,
    
    lists:map
    (
        fun({Attr,[],Val}) ->
            V = case Val of
                []    -> [];
                [Tmp] -> Tmp
            end,
            Addr = #ref{site=Site,path=Path,ref=Ref,name=Attr},
            hn_main:set_attribute(Addr,V)
        end,
        Data
    ),
    ok.

%%% API call - CLEAR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_clear(_R,_Page) -> ok.

%%% API call - INSERT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_insert(_R,_Page) -> ok.

%%% API call - DELETE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_delete(_R,_Page) -> ok.

%%% API call - UNREGISTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_unreg(Data,Page) -> 

    [{biccie,[],[Bic]},{url,[],[Url]}] = Data,
    hn_db:del_remote_link(#remote_cell_link{ 
        parent = hn_util:page_to_index(Page),
        child  = hn_util:page_to_index(hn_util:parse_url(Url)),
        type   = outgoing }),

    {ok,"thanks"}.

%%% API call - REGISTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_reg(Data,Page)->

    [{biccie,[],[Bic]},{proxy,[],[Proxy]},{url,[],[Reg]}] = Data,
    
    hn_db:register_hn(
        hn_util:page_to_index(Page),
        hn_util:page_to_index(hn_util:parse_url(Reg)),
        Bic, Proxy, Reg).

%%% API call - NOTIFY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_notify(Data,Page)->
    case lists:keysearch(type,1,Data) of
    {value,{type,[],["change"]}} -> api_change(Data,Page)
    end.

api_change(Data,_Page)->

    [   {biccie,[],     [Bic]},
        {cell,[],       [Cell]},
        {type,[],       ["change"]},
        {value,[],      Val},
        {version,[],    [Version]}] = Data,
        
    V = case Val of 
        []    -> [];
        [Tmp] -> Tmp
    end,
  
    hn_db:update_hn(Cell,Bic,V,Version),
    
    {ok,"thanks"}.

%%% API call - LOGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_login([{"password",Pass},{"user",User}]) ->
    case users:login(User,Pass) of
        {error,invalid_user} -> {ok,{"login","invalid_user"}};
        {ok,Record} ->
            M = #user{name = User,loggedin = true,state = Record},
            NewCookie = yaws_api:new_cookie_session(M),
            {ok,{"login","success"},yaws_api:setcookie("user",NewCookie,"/")}
    end.

%%% API call - LOGOUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_logout() ->
    M = #user{loggedin = false},
    NewCookie = yaws_api:new_cookie_session(M),
    {ok,{"logout","success"},yaws_api:setcookie("user",NewCookie,"/")}.
    
