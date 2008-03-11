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

-export([ out/1, calc/4, recalc/1, process_input/5, get_hypernumber/9 ]).

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

        PostData    = get_post_data(Page#page.format,Arg,Page#page.vars),
        User        = hn_util:get_cookie((Arg#arg.headers)#headers.cookie),
        Permissions = users:get_permissions(User,Page),

        %% Handle POST request, users need admin or edit permissions to make
        %% a post request, unless they are trying to login
        PostResult = case PostData of
        [] -> ok;
        _  ->
            case true of
            true -> process_POST(Arg,PostData,User,Page);
            _    ->
                case Permissions of 
                N when N == admin ; N == edit ->
                    process_POST(Arg,PostData,User,Page);
                _ -> {ok,[{"post",[],"error"}]}
                end
            end
        end,

        %% If POST has nothing to return, handle GET
        case PostResult of
        ok -> process_GET(Arg,{User,Permissions},Page);
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
process_GET(#arg{docroot=Root},_,#page{ref={page,_},vars=[]}) ->
    {page,"/html/index.html"};

%% unauthorised users can get access to the pages and swf's. but not
%% the data
process_GET(_Arg,{_User,unauthorised},Page) ->
    format_output(Page#page.format,{"error","Error: unauthorised"});

%% REST queries to a page
process_GET(_Arg,{User,_},Page) when element(1,Page#page.ref) == page ->
    Data = case Page#page.vars of
    [{pages}] ->  
        create_pages_tree(db:read_pages(Page#page.site));
    [{info}] ->
        {ok,W} = db:get_websheet(Page),
        {"info",[{"name",W#websheet.name},{"gui",W#websheet.gui},
            {"public",atom_to_list(W#websheet.public)}]}
    end,

    format_output(Page#page.format,Data);

%% REST calls to a reference
process_GET(_Arg,_User,Page) ->
    format_output(Page#page.format,show_ref(Page)).

%% Column
show_ref(#page{site=Site,path=Path,ref={column,Y}}) ->
    
    Vals = lists:map(fun(A) -> 
        Row = to_s((A#spriki.index)#index.row),
        {row,[{label,Row}],[to_s(A#spriki.value)]}
    end,db:read_column(Site,Path,Y)),

    {column,[{label,to_s(tconv:to_b26(Y))}],Vals};

%% Row
show_ref(#page{site=Site,path=Path,ref={row,X}}) ->

    Vals = lists:map(fun(A) -> 
        Col = to_s(to_b26((A#spriki.index)#index.column)),
        {column,[{label,Col}],[to_s(A#spriki.value)]}
    end,db:read_row(Site,Path,X)),

    {row,[{label,to_s(X)}],Vals};

%% Cell
show_ref(#page{site=Site,path=Path,ref={cell,{X,Y}},vars=[]}) ->
    {cell,[],[{value,[],[hn_util:text(get_value(Site,Path,X,Y))]}]};

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
            {errors,[],[util2:pad_list(hn_util:text(Errors))]}]};
    [{toolbar}] ->
        {toolbar,[],[{value,[],[FValue]},
            {formula,[],[util2:make_text(Form)]},
            {reftree,[],[util2:pad_list(util2:make_text(RefTree))]},
            {errors,[],util2:pad_list(util2:make_text(Errors))}]}
    end;

%% Range
show_ref(#page{site=Site,path=Path,ref={range,{X1,Y1,X2,Y2}}})->
  
    Data = db:read_range(Site,Path,{X1,Y1,X2,Y2}),

    {range,[],
        [{row,[{label,to_s(X)}],
            [{column,[{label,to_s(to_b26(Y))}],
                [get_element(X,Y,Data)]
            } || Y <- lists:seq(X1,X2)]   
         } || X <- lists:seq(Y1,Y2)]
    }.

get_element(X,Y,[]) -> "";
get_element(X,Y,[H|T]) ->
    Index = H#spriki.index,
    case {Index#index.row,Index#index.column} of
    {X,Y} ->
        case (H#spriki.status)#status.errors of
        [] -> hn_util:text(H#spriki.value);
        E -> E
        end;
    _ ->
        get_element(X,Y,T)
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
    [import]                -> api_import(Arg,Page);
    {create,[],Data}     -> api_create(Data,Page);
    [{A,"login"}|R]        -> api_login(R);
    [{A,"logout"}]         -> api_logout();
    [{A,"clear"}|R]        -> api_clear(R,Page);
    [{A,"insert"}|R]       -> api_insert(R,Page);
    [{A,"delete"}|R]      -> api_delete(R,Page);
    {register,[],Data}    -> api_reg(Data,Page);
    [{A,"unregister"}|R] -> api_unreg(R,Page);
    {notify,[],Data}      ->  api_notify(Data,Page)
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
%% create is the only one fully implemented for row/col/range/cell
%% pattern matching on the head is a bit verbose
api_create([{value,[],Value}],#page{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    process_input(Site,Path,X,Y,lists:flatten(Value));

api_create(Value,#page{site=Site,path=Path,ref={range,{X1,Y1,X2,Y2}}})->
    lists:foldl(fun({row,[],Children},X) ->
        lists:foldl(fun({value,[],[V]},Y) ->
            process_input(Site,Path,Y+Y1,X+X1,V), Y+1
        end,0,Children), X+1
    end,0,Value),
    ok;

api_create(Value,#page{site=Site,path=Path,ref={column,Y},vars=Vars})->
    Values = lists:map(fun({value,[],[X]}) -> X end,Value),
    case {Vars,length(Values)} of
    {[{last}],1} ->
        Index = get_last_val(Site,Path,{column,Y}),
        process_input(Site,Path,Y,Index+1,lists:last(Values));
    {[{last}],_} -> 
        put_col(Site,Path,get_last_col(Site,Path)+1,1,Values);
    _ ->            
        put_col(Site,Path,Y,1,Values)
    end;

api_create(Value,#page{site=Site,path=Path,ref={row,X},vars=Vars})->
    Values = lists:map(fun({value,[],[X]}) -> X end,Value),
    case {Vars,length(Values)} of
    {[{last}],1} -> 
        Index = get_last_val(Site,Path,{row,X}),
        process_input(Site,Path,Index+1,X,lists:last(Values));
    {[{last}],_} -> 
        put_row(Site,Path,1,get_last_row(Site,Path)+1,Values);
    _ ->            
        put_row(Site,Path,1,X,Values)
    end.

put_row(_,_,_,_,[]) -> ok;
put_row(Site,Path,Index,Col,[H|T]) ->
    process_input(Site,Path,Index,Col,H),
    put_row(Site,Path,Index+1,Col,T).

put_col(_,_,_,_,[]) -> ok;
put_col(Site,Path,Row,Index,[H|T]) ->
    process_input(Site,Path,Row,Index,H),
    put_col(Site,Path,Row,Index+1,T).

get_last_col(Site,Path) ->
    List = lists:sort(fun(A,B) -> (A#spriki.index)#index.column <
      (B#spriki.index)#index.column end,db:read_site(Site,Path)),
    case List of
        [] -> 0;
        List -> ((lists:last(List))#spriki.index)#index.column
    end.

get_last_row(Site,Path) ->
    List = lists:sort(fun(A,B) -> (A#spriki.index)#index.row <
      (B#spriki.index)#index.row end,db:read_site(Site,Path)),
    case List of
        [] -> 0;
        List -> ((lists:last(List))#spriki.index)#index.row    
    end.

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
process_input(Site, Path, X, Y, Val) ->
    case Val of
        [$= | Formula] ->
            process_formula(Site,Path,X,Y,Formula);
        _Other ->
            process_value(Site,Path,X,Y,Val)
    end.

process_value(Site, Path, X, Y, Value) ->
    V = case Value of 
    undefined -> [];
    _ -> Value
    end,

    {Type, Val} = case regexp:match(V, ?RG_num) of
                      {match, _, _} ->
                          io:format("got a number~n"),
                          {number, util2:make_num(V)};
                      nomatch ->
                          io:format("got a string~n"),
                          muin_util:fdbg(V, "V"),
                          {string, V}
                  end,

    write_cell("", Val, Type, {Site, Path, X, Y}, {[], [], []}, []),
    ok.

process_formula(Site, Path, X, Y, Formula) ->
    case muin:parse("=" ++ Formula) of
        {ok, Ast} ->
            case muin:run(Ast, [{site, Site}, {path, Path}, {x, X}, {y, Y}]) of 
                {ok, {Value, RefTree, Errors, References}} ->
                    write_cell(Formula, Value, number, {Site, Path, X, Y},
                               {RefTree, Errors, References}, []);
                %% Self-reference or circular reference errors ONLY.
                %% {error, div0} and the rest are handled in the clause above.
                ErrVal= {error, Reason} ->
                    ErrMsg = append([atom_to_list(Reason), " ", Site, Path,
                                     util2:make_b26(X), to_l(Y)]),
                    write_cell(Formula, ErrVal, error, {Site, Path, X, Y},
                               {[], [ErrMsg], []}, [])
            end;
        ErrVal = {error, error_in_formula} ->
            write_cell(Formula, ErrVal, error, {Site, Path, X, Y},
                       {[], ["Invalid formula."], []}, [])
    end,
    ok.

write_cell(Formula, Value, Type, Bindings, Dependencies, Spec) ->
    {RefTree, Errors, References} = Dependencies,
    {Site, Path, X, Y} = Bindings,
    db:write(Site, Path, X, Y, Value, Type, make_bind_list(Site, Path, Spec),
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

recalc(#index{site=Site,path=Path,column=X,row=Y}) ->
    
    case db:read_spriki(Site,Path,X,Y) of
    []   -> ok;
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

get_last_val(Site,Path,{row,Ref}) ->
    case lists:filter(fun(X) -> (X#spriki.index)#index.row == Ref 
        end,db:read_site(Site,Path)) of
        [] -> 0;    
        List ->
            Tmp = lists:sort(fun(A,B) -> (A#spriki.index)#index.column <
            (B#spriki.index)#index.column end,List),
            ((lists:last(Tmp))#spriki.index)#index.column
    end;
get_last_val(Site,Path,{column,Ref}) ->
    case lists:filter(fun(X) -> (X#spriki.index)#index.column == Ref 
        end,db:read_site(Site,Path)) of
        [] -> 0;    
        List ->
            Tmp = lists:sort(fun(A,B) -> (A#spriki.index)#index.row <
            (B#spriki.index)#index.row end,List),
            ((lists:last(Tmp))#spriki.index)#index.row
    end.


make_bind_list(Site,Path,SpecBindings)->
    make_bind_list(Site,Path,SpecBindings,[]).

%% make_bind_list is the opposite of make_bindings
%% This function is a mess - it relies on the fact that "Site" and "Path"
%% are not valid values for variables of the type 'Site' or 'Path'
make_bind_list(_Site,_Path,[],Residuum)->
    Residuum.
