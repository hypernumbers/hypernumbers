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

-define(NOCACHE, {header,"Cache-control: no-cache"}).

-export([ out/1, calc/4, recalc/1, process_input/5, get_hypernumber/9 ]).

%% Exported until we refactor db.erl which is a mess
-export([ to_list/1, to_xml/1, to_post/1, to_json/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Yaws handler for all websheet requests
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(Arg) -> 

    Url      = yaws_api:request_url(Arg),
    Page     = hn_util:parse_url(Url),
    FileName = Arg#arg.docroot++Url#url.path,

    %% If the request is for an actual file on the server, just serve it
    %% ie (images, css, javascript etc etc) 
    try case filelib:is_file(FileName) and not filelib:is_dir(FileName) of
	true -> {page,{[?NOCACHE],Url#url.path}};
	false ->

        PostData    = get_post_data(Page#page.format,Arg,Page#page.vars),
        User        = hn_util:get_cookie((Arg#arg.headers)#headers.cookie),
        Permissions = users:get_permissions(User,Page),
    
	    case Page#page.ref of
        %% if requesting @swf, pick the swf to load from page admin
        {noref,X} when X == "@swf" ; X ==  "@swfadmin" ->
            case string:str(Url#url.path,"@swfadmin") of
            0 -> {page,{[?NOCACHE],"/swf/"++get_swf(Page)}};
            _ -> {page,{[?NOCACHE],"/swf/Admin.swf"}}
            end;
        
		_ ->
            %% Handle POST request, users need admin or edit permissions to make
            %% a post request, unless they are trying to login
            PostResult = case PostData of
            [] -> ok;
            _  ->
                case lists:member({"action","login"},PostData) of
                true -> process_POST(Arg,PostData,User,Page);
                _    ->
                    case Permissions of 
                    N when N == admin ; N == edit ->
                    process_POST(Arg,PostData,User,Page);
                    _ -> {ok,[{"post","error"}]}
                    end
                end
            end,

            %% If POST has nothing to return, handle GET
            case PostResult of
            ok ->                  process_GET(Arg,{User,Permissions},Page);
            {ok,Content} ->        format_output(Page#page.format,Content);
            {ok,Content,Cookie} -> [format_output(Page#page.format,Content),Cookie];
            {header,Content} ->    {header,Content}
		    end
        end
    end

    %% Globally catch any errors during processing
    catch
        throw ->     format_output(Page#page.format,{"error","undefined"});
        exit:Err ->  format_output(Page#page.format,{"error",Err});
        error:Err -> 
            ?F("Error:~p~n",[erlang:get_stacktrace()]),
            format_output(Page#page.format,{"error",Err})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% GET handlers, 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return HTML pages, for "/" , "?admin" and "?import"
process_GET(#arg{docroot=Root},_User,#page{ref={page,_},vars=[]}) ->
    {html,element(2,hn_util:read(Root++"/html/page.html"))};
process_GET(#arg{docroot=Root},_User,#page{ref={page,_},vars=[{admin}]}) ->
    {html,element(2,hn_util:read(Root++"/html/admin.html"))};
process_GET(#arg{docroot=Root},_User,#page{ref={page,_},vars=[{import}]}) ->
    {html,element(2,hn_util:read(Root++"/html/upload.html"))};

%% unauthorised users can get access to the pages and swf's. but not
%% the data
process_GET(_Arg,{_User,unauthorised},Page) ->
    format_output(Page#page.format,{"error","Error: unauthorised"});

%% REST queries to a page
process_GET(_Arg,{User,_},Page) when element(1,Page#page.ref) == page ->
    Data = case Page#page.vars of
    [{loggedin}] -> {"loggedin",atom_to_list(User#user.loggedin)};
    [{incoming}] -> transform_links(db:read_link_to(Page),"incoming");
    [{outgoing}] -> transform_links(db:read_link_from(Page),"outgoing");
    [{pages}] ->
        {"pages",lists:map(fun(I) -> {"page",I#index.path} end,
            db:read_websheets_from_page(Page))};
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
    {"row",[{"label","a"}],
        lists:map(fun(A) -> 
            {"row",[{"label",[(A#spriki.index)#index.column+96]}],
                A#spriki.value} 
        end,
        db:read_column(Site,Path,Y))};

%% Row
show_ref(#page{site=Site,path=Path,ref={row,X}}) ->
    {"row",[{"label","a"}],
        lists:map(fun(A) -> 
            {"column",[{"label",i((A#spriki.index)#index.column)}],
                A#spriki.value} 
        end,
        db:read_row(Site,Path,X))};

%% Cell
show_ref(#page{site=Site,path=Path,ref={cell,{X,Y}},vars=[]}) ->
    {"cell",{"value",get_value(Site,Path,X,Y)}};

show_ref(#page{site=Site,path=Path,ref={cell,{X,Y}},vars=Vars}) ->

    Ref = #index{site=Site,path=Path,column=X,row=Y},

    %% first read the cell
    {Val,RefTree,Form,_Src,Errors,Refs} = 
        case db:read_spriki(Site,Path,X,Y) of
        [] -> {0,[],[],[],[],[]};
        [#spriki{value=Value,status=
                 #status{formula=Formula, reftree=RT,
                         errors=Errs, refs=Refns}}] -> 
            {Value,RT,Formula,Errs,Refns}
	end,

    FValue = case Errors of
	[] -> util2:make_text(Val);
	_  -> "Error!"
	end,

    %% now read the link_to and link_from lists
    F = fun(A) -> {A#ref.ref_to,A#ref.ref_from} end,
    To   = transform_links(lists:map(F,db:read_ref(to,Ref)),"incoming"),
    From = transform_links(lists:map(F,db:read_ref(from,Ref)),"outgoing"),

    case Vars of
	[{hypernumber}] ->
	    {"cell",[{"value",FValue},
			 {"references",util2:pad_list(hn_util:text(Refs))},
			 {"reftree",util2:pad_list(hn_util:text(RefTree))},
			 {"errors",util2:pad_list(hn_util:text(Errors))}]};
	[{toolbar}] ->
	    {"toolbar",[{"cell","a"},{"value",FValue},
			{"formula",util2:make_text(Form)},
			{"reftree",util2:pad_list(util2:make_text(RefTree))},
			{"errors",util2:pad_list(util2:make_text(Errors))},
			To,From]};
	[{incoming}] -> To;
	[{outgoing}] -> From
    end;

%% Range
show_ref(#page{site=Site,path=Path,ref={range,Range}})->
    Fun = fun(Record)->
        Index  = Record#spriki.index,
        Value = case (Record#spriki.status)#status.errors of
            [] -> hn_util:text(Record#spriki.value);
            _E -> "Error!"
        end,
        {{Index#index.column,Index#index.row},Value} end,

    format_range(Range,lists:map(Fun,db:read_range(Site,Path,Range))).

%% Utility GET functions
%%--------------------------------------------------------------------
%% creates a list of marked up links for incoming/outgoing links
transform_links([],Root) -> 
    {Root,"empty"};
transform_links(Rcd,Root) when is_list(Rcd) ->
    {Root,lists:map((fun({To,From}) -> 
        {"link",[{"to",[{"site",To#index.site},{"path",To#index.path},
            {"row",To#index.row},{"column",To#index.column}]},{"from",[
            {"site",From#index.site},{"path",From#index.path},
            {"row",From#index.row},{"column",From#index.column}]}]}
        end),Rcd)}.

%% Get the swf associated with this url, if swf is "default" go one
%% step back until a swf is defined, or just serve spriki.swf
get_swf(Page) ->
    {ok,WebSheet} = db:get_websheet(Page),
    case WebSheet#websheet.gui of
    "default" ->
        case Page#page.path of
        "/" -> ?DEF_SWF;
        _   ->
            [_|T] = lists:reverse(string:tokens(Page#page.path,"/")),
            get_swf(Page#page{path = util2:repath(T)})
        end;
    Else -> Else
    end.

%% Format the output depending on the requested format.
format_output(Format,Data) ->
    case Format of
    {xml}  -> {content,"text/xml",to_xml(Data)};
    {list} -> {content,"text/plain", to_list(Data)};
    {json} -> {content,"application/json",to_json(Data)};
    {json,nocallback} -> 
        {content,"application/json","hn("++to_json(Data)++");"}
    end.

%% Cleanup
%%--------------------------------------------------------------------
format_range({X1,Y1,X2,Y2},List)->
    format_range(X1,Y1,X2,Y2,X2,Y2,List,[],[]).

format_range(X1,Y1,_X2,_Y2,X1,Y1,List,RowResiduum,Residuum)->
    %% recurse has gone from top to bottom so stop
    NewRowResiduum={"row",[{"label",integer_to_list(Y1)}],[get_element(X1,Y1,List),RowResiduum]},
    NewResiduum=[NewRowResiduum|Residuum],
    {"range",NewResiduum};
format_range(X1,Y1,X2,Y2,X1,Y,List,RowResiduum,Residuum)->
    NewRowResiduum={"row",[{"label",integer_to_list(Y)}],
		    [get_element(X1,Y,List),RowResiduum]},
    NewResiduum=[NewRowResiduum|Residuum],
    format_range(X1,Y1,X2,Y2,X2,Y-1,List,[],NewResiduum);
format_range(X1,Y1,X2,Y2,X,Y,List,RowResiduum,Residuum) ->
    NewRowResiduum=[get_element(X,Y,List)|RowResiduum],
    format_range(X1,Y1,X2,Y2,X-1,Y,List,NewRowResiduum,Residuum).

get_element(X,Y,List)->
    El = case lists:keysearch({X,Y},1,List) of
    false -> ""; 
    {value,{_,Value}} -> Value 
    end,
    {"column",[{"label",[X+96]}],El}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% POST handlers, 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear_post({post,[],Children}) -> clear_post(Children);
clear_post(Children) ->
    lists:map(fun({Name,[],Child}) ->
        H = lists:flatten(Child),
        Str = case io_lib:char_list(H) of
            true -> H;
            false ->
                F = fun({_C,[],Val}) -> Val end,
                lists:flatten(hn_util:implode(lists:map(F,Child),"\n"))
        end,
        {atom_to_list(Name),Str}
    end,Children).

flatten_json([]) -> "";
flatten_json(Value) when is_integer(Value) ->  integer_to_list(Value) ++ "\n";
flatten_json({_Object,Value}) -> flatten_json(Value);
flatten_json(Value) when is_list(Value) ->
    case io_lib:char_list(Value) of
    true -> Value ++ "\n";
    false ->
        [H|T] = Value,
        flatten_json(H) ++ flatten_json(T)
    end.

get_post_data(_,#arg{req=#http_request{method='GET'}},_Dec) -> [];
get_post_data(_,_Arg,[{import}]) -> [import];
get_post_data({json},Arg,_Dec) ->

    {ok,JSon} = json:decode_string(binary_to_list(Arg#arg.clidata)),
    {struct,[{post,{array,PostData}}]} = JSon,

    lists:map(fun({struct,[{Key,Val}]}) ->
        {atom_to_list(Key),string:strip(flatten_json(Val),both,$\n)}

    end,PostData);

get_post_data({json,nocallback},_Arg,_Dec)  -> todo;
get_post_data({list},Arg,_Dec) -> lists:keysort(1,yaws_api:parse_post(Arg));
get_post_data({xml},Arg,_Dec) ->
      
    lists:keysort(1,clear_post(hn_util:readxml_string(
        binary_to_list(Arg#arg.clidata)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This section handles the POST request of the REST API
%% calls, Read the API spcification for what further
%% functionality needs to be supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% No Post Data, Dont care
process_POST(_Arg,[],_User,_Page) -> ok;

%% Filter the API call (action)
process_POST(Arg,PostData,_User,Page) ->
    A = "action", 
    case PostData of
	[import] ->             api_import(Arg,Page);
	[{A,"login"}|R] ->      api_login(R);
	[{A,"logout"}] ->       api_logout();
	[{A,"create"}|R] ->     api_create(R,Page);
	[{A,"clear"}|R] ->      api_clear(R,Page);
	[{A,"insert"}|R] ->     api_insert(R,Page);
	[{A,"delete"}|R] ->     api_delete(R,Page);
	[{A,"register"}|R] ->   api_reg(R,Page);
	[{A,"unregister"}|R] -> api_unreg(R,Page);
	[{A,"notify"}|R] ->     api_notify(R,Page)
    end.

%%% API call - IMPORT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% import an excel file, the import and log txt probably needs 
%% cleaned up
api_import(Arg,Page) -> 
    F = fun(X) -> 
        filefilters:read(excel,X,"/tmp/log.txt",
            fun(A) -> import(A,Page) end) 
    end,
    case hn_util:upload(Arg,"Filedata",F) of
    ok ->    {ok,{"create","success"}};
    error -> {ok,{"create","error"}}
    end.

import([],_)-> ok;
import([{_Name,Tid}|T],Page)->
    Fun = fun(X,_Y) ->
        case X of
        {{{row_index,Row},{col_index,Col}},[_,{value,number,Val}]} ->
            Req = lists:flatten([Page#page.site,Page#page.path,
                util2:make_b26(Col+1),hn_util:text(Row+1)]),
            hn_util:post(Req,"action=create&value="++hn_util:text(Val))
        end
    end,
    ets:foldl(Fun,[],Tid),
    import(T,Page).

%%% API call - CREATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create is the only one fully implemented for row/col/range/cell
%% pattern matching on the head is a bit verbose
api_create([{"gui",Gui},{"name",Name},{"public",Private}],Page) ->
    WebSheet = #websheet{page=Page,
        public=list_to_atom(Private),gui=Gui,name=Name},
    db:save_websheet(WebSheet),
    {ok,{"create","success"}};

api_create([{"ref",Ref},{"user",User}],Page) ->
    {ok,WebSheet} = db:get_websheet(Page),
    Ws = WebSheet#websheet{permissions = WebSheet#websheet.permissions ++ {User,Ref}},
    db:save_websheet(Ws),
    {ok,{"create","success"}};

api_create([{"value",V}],#page{site=S,path=P,ref={range,{X1,Y1,X2,Y2}}})->
    put_range(S,P,{X1,X2,Y2},X1,Y1,string:tokens(V,"\n"));

api_create([{"value",Value}],#page{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    process_input(Site,Path,X,Y,Value);

api_create([{"value",Value}],#page{site=Site,path=Path,ref={column,Y},vars=Vars})->
    Values = string:tokens(Value,"\n"),
    case {Vars,length(Values)} of
    {[{last}],1} ->
        Index = get_last_val(Site,Path,{column,Y}),
        process_input(Site,Path,Y,Index+1,lists:last(Values));
    {[{last}],_} -> put_col(Site,Path,get_last_col(Site,Path)+1,1,Values);
    _ ->            put_col(Site,Path,Y,1,Values)
    end;

api_create([{"value",Value}],#page{site=Site,path=Path,ref={row,X},vars=Vars})->
    Values = string:tokens(Value,"\n"),
    case {Vars,length(Values)} of
    {[{last}],1} -> 
        Index = get_last_val(Site,Path,{row,X}),
        process_input(Site,Path,Index+1,X,lists:last(Values));

    {[{last}],_} -> put_row(Site,Path,1,get_last_row(Site,Path)+1,Values);
    _ ->            put_row(Site,Path,1,X,Values)
    end.

put_range(_,_,_,_,_,[]) -> ok;
put_range(Site,Path,{StartX,EndX,EndY},CurX,CurY,[H|T]) ->
    process_input(Site,Path,CurX,CurY,H),
    case {CurX+1 > EndX,CurY+1 > EndY} of
    {false,_} -> put_range(Site,Path,{StartX,EndX,EndY},CurX+1,CurY,T);
    {true,false} -> put_range(Site,Path,{StartX,EndX,EndY},StartX,CurY+1,T);
    {true,true} -> ok
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
	[{"biccie",Bic},{"proxy_URL",Proxy},{"registered_URL",Reg}] ->
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
    case lists:keysearch("type",1,PostData) of
	{value,{"type","change"}} -> api_change(PostData,Page)
    end,
    ok.

api_change(PostData,Page)->
    case get_changed_data(PostData,Page) of
        {ok,{Val,{Site,Path,X,Y}}}-> 
            Index = #index{site=Site,path=Path,column=X,row=Y},
            HNumber = util2:parse_hypns(Val,Index),
            Hyp = HNumber#hypernumbers{ref_from=Index},
            db:update_hypnum(Hyp),
            ok;
        _Other -> {ok,error}
    end.

get_changed_data(PostData,#page{site=Site,path=Path,ref={cell,{X,Y}}})->

    [{"biccie",Bic},{"notifying_URL",Notif_URL},
        _Reg,_Type,{"value",Value},_Ver] = PostData,

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
process_input(Site,Path,X,Y,Val) ->
    case Val of
	[$=|Formula] -> process_formula(Site,Path,X,Y,Formula);
	_Other       -> process_value(Site,Path,X,Y,Val)
    end.

process_value(Site, Path, X, Y, Value) ->
    V = case Value of 
            undefined -> [];
            _ -> Value
        end,

    {Type, Val} = case regexp:match(V, ?RG_num) of
                      {match, _, _} -> {number, util2:make_num(V)};
                      nomatch -> {string, V}
                  end,

    write_cell("", Val, Type, {Site, Path, X, Y}, {[], [], []}, []),
    ok.

%% TODO: Put error handling back in (proper).
process_formula(Site, Path, X, Y, Formula) ->
    %% Parse & run the formula.
    {ok, Ast} = muin:parse("=" ++ Formula),
    {ok, {Value, RefTree, Errors, References}} =
        muin:run(Ast, [{site, Site}, {path, Path}, {x, X}, {y, Y}]),
    
    %% Got any circular references?
    case lists:member({Site, Path, X, Y}, RefTree) of
        false ->
            write_cell(Formula, Value, number, {Site, Path, X, Y}, {RefTree, Errors, References}, []);
        true ->
            %% Write a duff abstract form that returns a value without references, and mark references
            %% as null to break the link.
            ErrMsg = {"Circular Reference: ",
                      Site ++ Path ++ util2:make_b26(X) ++ integer_to_list(Y)},
            
            write_cell(Formula, 0, error, {Site, Path, X, Y}, {RefTree, [ErrMsg | Errors]}, [])
    end,
    ok.

write_cell(Formula, Value, Type, Bindings, Dependencies, Spec) ->
    {RefTree, Errors, References} = Dependencies,
    {Site, Path, X, Y} = Bindings,
    db:write(Site, Path, X, Y, Value, Type, make_bind_list(Site, Path, Spec),
             #status{formula = Formula,
                     reftree = RefTree, errors = Errors, refs = References}).

%% OLD VERSION
%%write_cell(Site,Path,X,Y,Val,Type,Formula,Fun,Abs,Tree,Spec,Err,Refs) ->
%%    db:write(Site,Path,X,Y,Val,Type,make_bind_list(Site,Path,Spec),
%%		#status{formula=Formula,src=Fun,absform=Abs,
%%			reftree=Tree,errors=Err,refs=Refs}).

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
    ?F("spriki:recalc~n"),
    
    case db:read_spriki(Site,Path,X,Y) of
        []   -> ok;
        [#spriki{status=#status{formula=Formula}}] ->
            process_formula(Site,Path,X,Y,Formula)
    end.

get_value(Site,Path,X,Y)->

    case db:read_spriki(Site,Path,X,Y) of
    [] -> 0;
    [#spriki{value=Value,status=Stat}] ->
        case Stat#status.errors of
        []   -> Value;
        Else -> Else
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

bind([],Binding)->
    Binding;
bind([{_,{_,{_Type,Var,Value}}}|T],Binding) ->
    NewBinding=erl_eval:add_binding(list_to_atom(Var),Value,Binding),
    bind(T,NewBinding).

make_bind_list(Site,Path,SpecBindings)->
    make_bind_list(Site,Path,SpecBindings,[]).

%% make_bind_list is the opposite of make_bindings
%% This function is a mess - it relies on the fact that "Site" and "Path"
%% are not valid values for variables of the type 'Site' or 'Path'
make_bind_list(_Site,_Path,[],Residuum)->
    Residuum;
make_bind_list(Site,Path,[{_,{{"Site","Path"},{Type,VarName,Value}}}|T],
	       Residuum) ->
    Binding={Site,Path,Type,VarName,Value},
    make_bind_list(Site,Path,T,[Binding|Residuum]);
make_bind_list(Site,Path,[{_,{{"Site",Path2},{Type,VarName,Value}}}|T],
	       Residuum) ->
    Binding={Site,Path2,Type,VarName,Value},
    make_bind_list(Site,Path,T,[Binding|Residuum]);
make_bind_list(Site,Path,[{_,{{Site2,"Path"},{Type,VarName,Value}}}|T],
	       Residuum) ->
    Binding={Site2,Path,Type,VarName,Value},
    make_bind_list(Site,Path,T,[Binding|Residuum]);
make_bind_list(Site,Path,[{_,{{Site2,Path2},{Type,VarName,Value}}}|T],
	       Residuum) ->
    Binding={Site2,Path2,Type,VarName,Value},
    make_bind_list(Site,Path,T,[Binding|Residuum]).

i(Int) -> integer_to_list(Int).

flatten_post(List) when is_list(List) ->
    case io_lib:deep_char_list(List) of
    true -> yaws_api:url_encode(lists:flatten(List))++"\n";
    false ->
        [H|T] = List,
        flatten_post(H)++flatten_post(T)
    end;
flatten_post({_Key,Val}) ->
    flatten_post(Val).

to_post({_Key,Val}) ->
    Rtn = lists:map(fun({X,Y}) ->
        X++"="++string:strip(flatten_post(Y),both,$\n)++"&"
    end,Val),

    string:strip(lists:flatten(Rtn),both,$&).

to_json([]) -> "";
to_json(Test) when is_list(Test) ->
    case io_lib:char_list(Test) of
    true -> [$"]++Test++[$"];
    _ ->
        Str = lists:flatten(lists:map(fun(X) -> to_json(X)++"," end,Test)),
        "["++string:substr(Str, 1, string:len(Str)-1)++"]"
    end;
to_json({Tag,X}) -> "{"++[$"]++Tag++[$"]++":"++to_json(X)++"}";
to_json({Tag,_Att,X}) -> "{"++[$"]++Tag++[$"]++":"++to_json(X)++"}";
to_json(X) when is_integer(X) -> integer_to_list(X).

to_xml([]) -> "";
to_xml(Test) when is_list(Test) ->
    case io_lib:char_list(Test) of
    true -> Test;
    _    -> lists:flatten(lists:map(fun(X) -> to_xml(X) end,Test))
    end;
to_xml({Tag,X}) -> "<"++Tag++">"++to_xml(X)++"</"++Tag++">";
to_xml({Tag,Attributes,X}) ->
    F = fun({K,V}) -> K++"=\""++V++"\" " end,
    lists:flatten("<"++Tag++" "++lists:map(F,Attributes)++">"++to_xml(X)++"</"++Tag++">");
to_xml(X) when is_integer(X) -> integer_to_list(X).

to_list(Data) ->
    string:strip(lists:flatten(to_xlist(Data)),both,$\n).

to_xlist([]) -> "";
to_xlist(Test) when is_list(Test) ->
    case io_lib:char_list(Test) of
    true -> Test++"\n";
    _    -> lists:map(fun(X) -> to_xlist(X) end,Test)
    end;

to_xlist({_Tag,X})             -> to_xlist(X);
to_xlist({_Tag,_,X})           -> to_xlist(X);
to_xlist(X) when is_integer(X) -> integer_to_list(X)++"\n";
to_xlist(X) when is_float(X)   -> float_to_list(X)++"\n";
to_xlist(X) when is_atom(X)    -> atom_to_list(X).
