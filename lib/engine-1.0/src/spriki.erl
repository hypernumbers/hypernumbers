%%%-----------------------------------------------------------------------------
%%% File        : spriki.erl
%%% Author      : Gordon Guthrie <gordonguthrie@vixo.com>
%%% Description : this is the base handling module of the spriki
%%%               It is demo quality only
%%%
%%% Created     : 10 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%%-----------------------------------------------------------------------------

-module(spriki).

-include("yaws_api.hrl").
-include("spriki.hrl").
-include("regexp.hrl").
-include("handy_macros.hrl").



-export([ out/1, calc/4, recalc/1, process_input/4, get_hypernumber/9 ]).

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
        ok -> process_GET(Arg,{User,Perms},Page);
        {ok,Content} ->        format_output(Page#page.format,Content);
        {ok,Content,Cookie} -> [format_output(Page#page.format,Content),Cookie]
        end
    end

    %% Globally catch any errors during processing
    catch
    _:Err -> ?F("Error:~p~n~p",[Err,erlang:get_stacktrace()]),{html,"error"}
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
    
    case lists:member({attr},Page#page.vars) of
    true ->
    
        #page{site=Site,path=Path,ref=Ref} = Page,
        Addr = #attr_addr{site=Site,path=Path,ref=Ref},
        
        List = lists:map
        (
            fun(#hypnum_item{addr=R, val=Val}) ->
                {RefType,_} = R#attr_addr.ref,
                RefVal = hn_util:ref_to_str(R#attr_addr.ref),
                Attr = [{type,hn_util:text(RefType)},{ref,RefVal}],
                {ref,Attr,[{R#attr_addr.name,[],[hn_util:text(Val)]}]}
            end,
            db:get_attr(Addr)
        ),
        
        format_output(Page#page.format,{attr,[],List});
        
    false ->
        process_GET2(Arg,User,Page)
    end.


%% REST queries to a page
process_GET2(_Arg,{User,_},Page) when element(1,Page#page.ref) == page ->
    Data = case Page#page.vars of
    [{pages}] ->
        {dir,[],[]}
        %create_pages_tree(db:read_pages(Page#page.site))
    end,
    format_output(Page#page.format,Data);

%% REST calls to a reference
process_GET2(_Arg,_User,Page) ->
    format_output(Page#page.format,show_ref(Page)).

show_ref(#page{site=Site,path=Path,ref={cell,{X,Y}},vars=Vars}) ->

    %% first read the cell
    {Val,RefTree,Form,Errors,Refs} = 
        case db:read_spriki(Site,Path,X,Y) of
        [] -> {0,[],[],[],[]};
        [#spriki{value=Value,status=
                 #status{formula=Formula, reftree=RT,
                         errors=Errs, refs=Refns}}] -> 
            {Value,RT,Formula,Errs,Refns}
    end,

    FValue = case Errors of
    [] -> util2:make_text(Val);
    _  -> "Error!"
    end,

    case Vars of
    [{hypernumber}] ->
        {cell,[],[
            {value,[],[FValue]},
            {references,[],[util2:pad_list(hn_util:text(Refs))]},
            {reftree,[],[util2:pad_list(hn_util:text(RefTree))]},
            {errors,[],[util2:pad_list(hn_util:text(Errors))]}]}
    end.


%% Utility GET functions
%%--------------------------------------------------------------------
%% Format the output depending on the requested format.
format_output(Format,Data) ->
    case Format of
    {xml}  -> {content,"text/xml",simplexml:to_xml_string(Data)};
    {json} -> {content,"text/plain",simplexml:to_json_string(Data)};
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
path_list([],PathList) -> PathList;
path_list([#spriki{index=#index{path=Path}}|T],List) ->
    case lists:member(Path,List) of
    true  -> path_list(T,List);
    false -> path_list(T,[Path|List])
    end.
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
    [{A,"unregister"}|R] -> api_unreg(R,Page);
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

    #page{site=Site,path=Path,ref=Ref,vars=Vars} = Page,
    
    lists:map
    (
        fun({value,[],[Val]}) ->
            process_input(Site,Path,Ref,Val);
            
        ({Attr,[],[Val]}) ->
            Addr = #attr_addr{site=Site,path=Path,ref=Ref,name=Attr},
            db:add_attr(Addr,Val)
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
api_unreg(_R,_Page) -> ok.

%%% API call - REGISTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_reg(PostData,#page{site=Site,path=Path,ref={cell,{X,Y}}})->

    case PostData of
    [{biccie,[],[Bic]},{proxy,[],[Proxy]},{url,[],[Reg]}] ->
        case hn_util:parse_url(Reg) of
        #page{site=Site2,path=Path2,ref={cell,{X2,Y2}}} ->
            db:write_ref(
                #index{site=Site,path=Path,column=X,row=Y},
                #index{site=Site2,path=Path2,column=X2,row=Y2},
                #details_from{},
                #details_to{proxy_URL=Proxy,reg_URL=Reg,biccie=Bic})
        end
    end,
    ok.

%%% API call - NOTIFY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
api_notify(PostData,Page)->
    case lists:keysearch(type,1,PostData) of
    {value,{type,[],["change"]}} -> api_change(PostData,Page)
    end,
    ok.

api_change(PostData,Page)->
    case get_changed_data(PostData,Page) of
        {ok,{Val,{Site,Path,X,Y}}}-> 
            Index = #index{site=Site,path=Path,column=X,row=Y},            
            Hyp = #hypernumbers{value=Val,ref_from=Index},
            db:update_hypnum(Hyp),
            ok;
        _Other -> {ok,error}
    end.

get_changed_data(PostData,#page{site=Site,path=Path,ref={cell,{X,Y}}})->

    [{biccie,[],[Bic]},{notifyurl,[],[Notif_URL]},
        _Reg,_Type,{value,[],[Value]},_Ver] = PostData,

    #page{site=FSite,path=FPath,ref={cell,{FX,FY}}} 
        = hn_util:parse_url(Notif_URL),

    [Res]=db:read_ref(
        #index{site=FSite,path=FPath,column=FX,row=FY},
        #index{site=Site, path=Path, column=X, row=Y}),

    case (Res#ref.details_from)#details_from.biccie of
    Bic -> {ok,{Value,{FSite,FPath,FX,FY}}};
    _   -> {error, invalid_credentials}
    end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Rest of the POST functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_input(Site, Path, {cell, {X, Y}}, Val) ->
    case superparser:process(Val) of
        {formula, Fla} ->
            process_formula(Site, Path, X, Y, Fla);
        {value, Value} ->
            process_value(Site, Path, X, Y, Value)
    end.

process_value(Site, Path, X, Y, Value) ->

    {Type, Val} = case regexp:match(Value, ?RG_num) of
        
        {match, _, _} ->
            {number, util2:make_num(Value)};
        nomatch ->
            muin_util:fdbg(Value, "V"),
            {string, Value}
        end,
        
    Addr = #attr_addr{site=Site, path=Path, ref={cell,{X,Y}}},
    db:add_attr(Addr#attr_addr{name=value},Val),
    
    write_cell("", Val, Type, {Site, Path, X, Y}, {[], [], []}, []),
    ok.

process_formula(Site, Path, X, Y, Formula) ->

    Addr = #attr_addr{site=Site, path=Path, ref={cell,{X,Y}}},

    case muin:compile(Formula) of
    
    {ok, Ast} ->
        case muin:run(Ast, [{site, Site}, {path, Path}, {x, X}, {y, Y}]) of 

        {ok, {Value, RefTree, Errors, References}} ->
            ?F("RefTree ~p ~n",[RefTree]),
            ?F("References ~p ~n",[References]),
        
            db:add_attr(Addr#attr_addr{name=formula},Formula),
            db:add_attr(Addr#attr_addr{name=value},Value),
            write_cell(Formula, Value, number, {Site, Path, X, Y},{RefTree, Errors, References}, []);
        
        ErrVal = {error, Reason} ->
            ErrMsg = append([atom_to_list(Reason), " ", Site, Path,util2:make_b26(X), to_l(Y)]),
            db:add_attr(Addr#attr_addr{name=value},ErrMsg)
        end;

    {error, error_in_formula} ->
        db:add_attr(Addr#attr_addr{name=value},"Invalid Formula")
        
    end,
    ok.

write_cell(Formula, Value, Type, Bindings, Dependencies, Spec) ->

    {RefTree, Errors, References} = Dependencies,
    {Site, Path, X, Y} = Bindings,
    
    db:write(Site, Path, X, Y, Value, Type,
        #status{formula = Formula,
            reftree = RefTree, errors = Errors, refs = References}).


get_hypernumber(TSite,TPath,TX,TY,URL,FSite,FPath,FX,FY)->

    From = #index{site  =FSite,path=FPath,column=FX,row=FY},
    To   = #index{site=TSite,path=TPath,column=TX,row=TY},

    #hypernumbers{value = Val, reftree = Tree, errors = Err, 
        refs = Refs} = db:get_hypnum(URL,To,From),

    {Val,Tree,Err,Refs}.


calc(Site,Path,X,Y)->
    case db:read_spriki(Site,Path,X,Y) of
    [] -> {0,[{Site,Path,X,Y}],[],[{Site,Path,X,Y}]};
    [#spriki{value=Val,status=#status{reftree=Tree,errors=Err,refs=Refs}}] ->
        NewRefs=lists:umerge([[{Site,Path,X,Y}],Refs]),
        NewRefTree=lists:umerge(NewRefs,Tree),
        {Val,NewRefTree,Err,NewRefs}
    end.
‬
recalc(#index{site=Site,path=Path,column=X,row=Y}) ->
    
    case db:read_spriki(Site,Path,X,Y) of
    []   -> ok;‎
    [#spriki{status=#status{formula=Formula}}] ->
        process_formula(Site,Path,X,Y,Formula)
    end.

get_value(Site,Path,X,Y) ->
    case db:read_spriki(Site, Path, X, Y) of
        [] ->
            blank;
        [#spriki{value = Value, status = Stat}] ->
            case Stat#status.errors of
                []   ->
                    ?COND(Value == [], blank, Value);
                Else ->
                    Else
            end
    end.
