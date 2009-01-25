%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Handle Hypernumbers HTTP requests

-module(hn_yaws).

-include("yaws_api.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-define(NOCACHE,[{header,{cache_control,"no-cache"}},
                 {status,200}]).

-import(simplexml,[to_xml_string/1,to_json_string/1]).
-export([ out/1, get_page_attributes/1 ]).

-record(upload, {fd,filename, last}).

%% @spec out(Arg) -> YawsReturn
%% @doc Yaws handler for all incoming HTTP requests
out(Arg) ->
    Url      = yaws_api:request_url(Arg),
    FileName = Arg#arg.docroot++Url#url.path,

    %% Serve static files, can move to a different server later
    case filelib:is_file(FileName) and not filelib:is_dir(FileName) of
        true  ->
            [{page,Url#url.path},{header,{cache_control,"max-age=3600"}}];
        false ->
            {ok,Ret} = case catch(do_request(Arg,Url)) of
                           {ok,Resp} ->
                               {ok,Resp};
                           Else      ->
                               Stack = erlang:get_stacktrace(),
                               ?ERROR("~p~n~p",[Else,Stack]),
                               {ok,[{status,400}]}
                       end,
            Ret
    end.

%% @spec do_request(Arg,Url) -> {ok,Responce}
%% @doc handle incoming requests
do_request(Arg, Url) ->
    
    RV = fun({X,undefined}) -> X;
            (Else)          -> Else
         end,

    Method = (Arg#arg.req)#http_request.method,
    {ok,Ref} = hn_util:parse_url(Url),
    Vars = lists:map(RV,yaws_api:parse_query(Arg)),
    {ok,Type} = hn_util:get_req_type(Vars),
    {ok,PostData} = get_post_data(Arg,Type,Vars),
    
    %% Verify AuthToken, authtoken can be passed via url
    %% or cookies, url takes precedence
    {ok,AuthCode} = get_var_or_cookie(auth,Vars,Arg),
    {ok,Token}    = get_var_or_cookie(token,Vars,Arg),
    User = case string:tokens(AuthCode,":") of
               [] ->
                   anonymous;
               [UserId,_AuthToken] ->
                   %% TODO Verify Token
                   UserId
           end,
    
    %% Dodgy expenses system stuff
    put(username,User),
    
    {ok,Access} = case hn_users:get_permissions(User,Ref) of
                      {ok,{protected_read,Token}}  -> {ok,read};
                      {ok,{protected_write,Token}} -> {ok,write};
                      {ok,{_Write,_Tok}}           -> {ok,require_token};
                      Else                         -> Else
                  end,

    Return = case req(Method,PostData,Vars,Access,Ref) of
                 {return,Data} ->
                     Data;
                 {ok,Data} ->
                     case Type of
                         xml ->
                             ?NOCACHE++
                             [{content,"text/xml",to_xml_string(Data)}];
                         json ->
                             [{content,"text/plain",to_json_string(Data)}]
                     end
             end,
    
    {ok,Return}.

req('GET', [], _, no_access, #ref{ref={page,"/"}}) ->
    {return,{page,"/html/login.html"}};
req('GET', [], _, no_access, _Ref)  ->
    {return,{status,503}};
req('GET',[],_,require_token,#ref{ref={page,"/"}}) ->
    {return,{page,"/html/token.html"}};
req('GET',[],_,require_token,_Page)  ->
    {return,{status,503}};
req('GET',[],[],_,#ref{ref={page,"/"},path=["admin"]}) ->
    {return,{page,"/html/admin.html"}};
req('GET',[],["save"],_,Ref=#ref{ref={page,"/"},path=Path}) ->
    F = fun(X) ->
                {page,[{path,hn_util:list_to_path(X)}],
                 [get_page_attributes(Ref#ref{path=Path++X})]}
        end,
    Pages = lists:map(F,hn_main:get_pages_under(Path)),
    Xml = {root,[{domain,Ref#ref.site++hn_util:list_to_path(Path)}],Pages},
    Name = case Path of
               []   -> "hypernumbers";
               Else -> lists:last(Else)
           end,
    {return,
     [{header, "Content-Type: application/xml"},
      {header, "Content-Disposition: attachment; "
                ++"filename=\""++Name++".xml\""},
      {content,"text/xml",to_xml_string(Xml)}]};

req('GET',[],[],_,Ref=#ref{ref={page,"/"}}) ->
    HTML = case hn_db:get_item_val(Ref#ref{name=gui}) of
               []    -> "/html/index.html";
               Else  -> "/apps/"++Else++".html"
           end,
    {return,{page,HTML}};
req('GET',[],[{"gui",GUI}],_,#ref{ref={page,"/"}}) ->
    {return,?NOCACHE++[{page,"/apps/"++GUI++".html"}]};

req('GET',[],["new"],_,Ref=#ref{site=Site,ref={page,"/"}}) ->
    Tpl = case hn_db:get_item_val(Ref#ref{name='__template'}) of
              [] -> 
                  Path = hn_util:list_to_path(Ref#ref.path)++"{auto,incr}/",
                  hn_templates:make_path(Path);
              Else ->
                  Else
          end,
    NPage=hn_templates:get_next2(#ref{site=Site},Tpl,"Blank"),
    hn_main:copy_pages_below(Ref,NPage),
    {return,{redirect, Ref#ref.site++NPage}};

%% deprecated
req('GET',[],[{"new",Template}],_,Ref=#ref{site=Site,ref={page,"/"}}) ->
    NPage=hn_templates:get_next(#ref{site=Site},Template,"BlankUsername"),
    Tpl="@"++Template,
    io:format("in hn_yaws:req add the gui to the beast~n"),
    hn_main:copy_pages_below(Ref#ref{path=[Tpl]},NPage),
    {return,{redirect, Ref#ref.site++NPage}};

req('GET',[],["templates"],_,_Ref=#ref{ref={page,"/"}}) ->
    hn_templates:get_templates();

req('GET',[],["port"],_,_Ref) ->
    Port = gen_server:call(hn_config,{get,remoting_port}),
    {ok,{port,[],[integer_to_list(Port)]}};

req('GET',[],["attr"],_,Ref) ->
    Attr = get_page_attributes(Ref),
    {ok,Attr};

req('GET',[],[{"attr",Val}],_,Ref) ->
    Name = list_to_existing_atom(Val),
    NRef  = Ref#ref{name=Name},
    Def  = get_default(Name),
    {ok,V} = hn_db:get_item_inherited(NRef,Def),
    {ok,{attr,[],[V]}};

%% ?pages
req('GET',[],["pages"],_,_Ref) ->
    Items = mnesia:dirty_match_object(#hn_item{_ = '_'}),
    {ok,create_pages_tree(Items)};

%% ?hypernumber
req('GET',[],["hypernumber"],_,Ref) ->
    Val = fun() ->
                  case hn_db:get_item_val(Ref#ref{name=rawvalue}) of
                      []  -> [{blank,[],[]}];
                      Tmp -> hn_util:to_xml(Tmp)
                  end
          end,
    
    DepTree = case hn_db:get_item_val(Ref#ref{name='dependancy-tree'}) of
                  {xml,Tree} -> Tree;
                  []         -> []
              end,
    {ok,{hypernumber,[],[{value,[], Val()},
                         {'dependancy-tree',[], DepTree}]}};

req('GET',[],[],_,Ref) ->
    
    case hn_db:get_item(Ref) of
        []   ->
            {return,{content,"text/plain","blank"}};
        List ->
            F = fun(#hn_item{addr=A}) -> 
                        A#ref.name == rawvalue
                end,
            
            Val = case lists:filter(F,List) of
                      [] -> 0;
                      [#hn_item{val=Value}] -> Value
                  end,

            {return,{content,"text/plain",hn_util:text(Val)}}
    end;

req('POST',{login,[],[{email,[],[Email]},
                      {password,[],[Pass]}]},[],_User,_Ref) ->
     case hn_users:login(Email,Pass) of
        {error,invalid_user} ->
            {ok,{auth,[],[{unauthorised,[],[]}]}};
        {ok,Record} ->
            {IP,_Port} = {{127,0,0,1},80},
            Token = hn_users:gen_authtoken(Record,IP),
            Sent  = Email++":"++hn_util:bin_to_hexstr(Token),
            {ok,{auth,[],[{token,[],[Sent]}]}}
    end;

req('POST',[],_,X,_) when X == no_access; X == read  ->
     {return,{status,503}};

req('POST', {create, [], [{Name, [], [Value]}]}, _Attr, _User, Ref = #ref{ref = {cell, _, _}}) ->
     hn_main:set_attribute(Ref#ref{name=Name},Value),
    {ok,{success,[],[]}};

req('POST', {create, [], Data}, _Attr, _User, Ref = #ref{ref = {range, _}}) ->
     case Data of
        [{formula, [], [Formula]}] ->
            hn_main:formula_to_range(Formula, Ref);
        _ ->
            hn_main:attributes_to_range(Data, Ref)
    end,
    {ok, {success, [], []}};

req('POST',{create,[],Data},Vars,_User,Ref = #ref{auth=Auth}) ->
    LastRow = get_last_index(Ref#ref.site,Ref#ref.path,row)+1,
    lists:foldl
      (
      fun({Attr,[],Val},Sum) ->
              NewAddr = case lists:member("lastrow",Vars) of
                            true  ->
                                Ref#ref{ref={cell,{Sum,LastRow}},name=Attr};
                            false ->
                                Ref#ref{name=Attr}
                        end,
              case Val of
                  [] ->
                      throw({empty_ref});
                  [Value]  ->
                      hn_main:set_attribute(NewAddr#ref{auth=Auth},Value)
              end,
              Sum+1
      end,
      1,
      Data
     ),
    {ok,{success,[],[]}};

req('POST',{delete,[],Recurse},_Attr,_User,Ref = #ref{ref={page,"/"}}) ->
 
    F = fun(X) ->
                Path = Ref#ref.path,
                hn_db:remove_item(Ref#ref{path=Path++X,ref='_'})
        end,
    Pages = case Recurse of
                [] -> hn_main:get_pages_under(Ref#ref.path);
                [{nochildren,[],[]}] -> []
            end,
    lists:foreach(F,Pages++[[]]),
    {ok,{success,[],[]}};

req('POST',{delete,[],Data},_Vars,_User,Ref) ->
    % TODO when the API is set up this function should be swapped out for
    % hn_db:delete_cells()
    lists:map
      (
      fun({Attr,[],[]}) ->
              hn_db:remove_item(Ref#ref{name=Attr})
      end,
      Data
     ),
    {ok,{success,[],[]}};

req('POST',{unregister,[],[{biccie,[],[_Bic]},{url,[],[Url]}]},_Attr,_User,Ref) ->
    hn_db:del_remote_link(#remote_cell_link{
        parent = hn_util:ref_to_index(Ref),
        child  = hn_util:ref_to_index(hn_util:parse_url(Url)),
        type   = outgoing }),

    {ok,{success,[],[]}};

req('POST',{register,[],[{biccie,[],[Bic]},{proxy,[],[Proxy]},{url,[],[Reg]}]},_Attr,_User,Ref) ->
    {ok,RegRef}=hn_util:parse_url(Reg),
    hn_db:register_hn(
      hn_util:ref_to_index(Ref),
      hn_util:ref_to_index(RegRef),
      Bic, Proxy, Reg),
    {ok,{success,[],[]}};

req('POST',{notify,[],Data},_Attr,_User,Ref) ->
    case lists:keysearch(type,1,Data) of
        {value,{type,[],["change"]}} ->
            api_change(Data,Ref)
    end;

%% deprecated
req('POST',{template,[],[{name,[],[Name]},
                         {url,[],[Url]},
                         {gui,[],[Gui]},
                         {formurl,[],[Form]}]},_Attr,_User,Ref) ->
    Tpl = "/@"++Name++"/",
    ok = hn_main:copy_pages_below(Ref,Tpl),
    {ok,NRef} = hn_util:parse_url(Ref#ref.site++Tpl),
    {ok,ok}=hn_templates:write_def(Name,Url,Gui,Form),
    ok=hn_main:set_attribute(NRef#ref{name=template},"@"++Name),
    ok=hn_main:set_attribute(NRef#ref{name=gui},Gui),
    ok=hn_main:set_attribute(NRef#ref{name=form},Form),
    {ok,{success,[],[]}};

req('POST',{template,[],[{url,[],[Path]}]},_Attr,_User,Ref) ->
    [_Head|Rest] = lists:reverse(string:tokens(Path,"/")),
    NRef = Ref#ref{path=lists:reverse(Rest)},
    Tpl = [{tpl,Ref#ref.path},{url,hn_templates:make_path(Path)}],
    ok=hn_main:set_attribute(NRef#ref{name='__template'},Tpl),
    ok=hn_main:set_attribute(NRef#ref{name=dynamic},"true"),
    {ok,{success,[],[]}};

req('POST',Data,["import"],_User,_Page) ->
    F = fun() ->
                {ok,_Stuff} = hn_import:hn_xml("/tmp/"++Data)
        end,
    F(),
    %TODO: GET RID OF  - should also import permissions
    hypernumbers_app:set_def_perms(),
    {ok,{success,[],[]}};

req(Method,Data,Vars,User,Page) ->
    throw({unmatched_request,Method,Data,Vars,User,Page}).


api_change([{biccie,[],     [Bic]},
            {cell,[],       [Cell]},
            {type,[],       ["change"]},
            {value,[],      [Val]},
            {version,[],    [Version]}], _Page)->

    {_,_,[Val2]}=Val,
    hn_db:update_hn(Cell,Bic,Val2,Version),

    {ok,{success,[],[]}}.

%% Utility functions
%%--------------------------------------------------------------------
get_page_attributes(Ref) ->
    F = fun(#hn_item{addr=A}) ->
                case atom_to_list((A)#ref.name) of
                    "__"++_ -> false;
                    _       -> true
                end
        end,
    Items = lists:filter(F,hn_db:get_item(Ref)),
    List = lists:map(fun hn_util:item_to_xml/1,Items),
    {attr,[],List}.

get_post_data(Arg,_,_) when (Arg#arg.req)#http_request.method == 'GET' ->
    {ok,[]};
get_post_data(Arg,_,["import"]) ->
    {ok,upload(Arg)};
get_post_data(Arg,json,_) ->
    {ok,simplexml:from_json_string(binary_to_list(Arg#arg.clidata))};
get_post_data(Arg,xml,_) ->
    {ok,simplexml:from_xml_string(binary_to_list(Arg#arg.clidata))}.

get_default(public) -> "public";
get_default(_) -> "".

%% Get the index of the last populated row or column
get_last_index(Site,Path,RowCol) ->
    case hn_db:get_item(#ref{site=Site,path=Path,ref={page,"/"}}) of
        []   -> 0;
        Else ->
            
            %% Only count cell value attributes
            F = fun(#hn_item{addr=Ref}) ->
                        case Ref of
                            #ref{name=value, ref = {cell,_}} -> true;
                            _ -> false
                        end 
                end,
            CellList = lists:filter(F,Else),
            
            List = lists:sort(
                     fun(#hn_item{addr=#ref{ref = {cell,{X1,Y1}}}},
                         #hn_item{addr=#ref{ref = {cell,{X2,Y2}}}}) ->
                             ?COND(RowCol == column,X1 < X2,Y1 < Y2)
                     end,CellList),
            
            case List of
                []    -> 0;
                _Else ->  
                    #hn_item{addr=#ref{ref={cell,{X,Y}}}} = lists:last(List),
                    ?COND(RowCol == column,X,Y)
	    end
    end.

%% Takes an unfiltered list of spriki records, extracts the path
%% they are from and constructs a tree
create_pages_tree(List) ->

    %%?INFO("~p",[dh_tree:create(path_list(List,[]))]),

    TmpTrees = lists:filter(
                 fun([]) -> false; (_) -> true end,
                 path_list(List,[])),
    
    Trees = lists:map(
              fun(X) -> create_tree(X) end,
              TmpTrees),
    
    {dir,[{path,"/"}],merge_trees(Trees)}.

merge_trees([])         -> [];
merge_trees([H|T])      -> merge_trees([H],T).

merge_trees(Tree,[])    -> Tree;
merge_trees(Tree,[[]])  -> Tree;
merge_trees(Tree,[[]|T])-> merge_trees(Tree,T);
merge_trees(Tree,[H|T]) ->
    {dir,[{path,P}],C1} = H,

    F = fun(X) ->
                case X of
                    {dir,[{path,P}],_} -> true;
                    _ -> false
                end
        end,
    {Match,Rest} = lists:partition(F,Tree),
    
    case Match of
        %% No Matches, add entire tree
        [] ->
            merge_trees([H|Tree],T);
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
path_list([#hn_item{addr = #ref{path=Path}}|T],List) ->
    case lists:member(Path,List) of
        true  -> path_list(T,List);
        false -> path_list(T,[Path|List])
    end.

get_var_or_cookie(Name,Vars,Arg) ->
    case lists:keysearch(Name,1,Vars) of
        false ->
            {ok,yaws_api:find_cookie_val(atom_to_list(Name),Arg)};
        {value,{auth,Auth}} ->
            {ok,Auth}
    end.


%% Urm, fix all this
upload(A) when A#arg.state == undefined ->
    State = #upload{},
    multipart(A, State);
upload(A) ->
    multipart(A, A#arg.state).

err() ->
    throw(oops).

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    err()
            end
    end.

addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last==true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->
    file:close(State#upload.fd),
    %%file:delete([?DIR,State#upload.filename]),
    {done, State#upload.filename};

addFileChunk(_A, [], State) when State#upload.last==true ->
    throw(incomplete);

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {"restorefile", Opts}}|Res], State ) ->
    case lists:keysearch(filename, 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),
            case file:open(["/tmp/", Fname] ,[write]) of
                {ok, Fd} ->
                    S2 = State#upload{filename = Fname, fd = Fd},
                    addFileChunk(A, Res, S2);
                _Err ->
                    {done, err()}
            end;
        false ->
            {done, err()}
    end;

addFileChunk(A, [{body, Data}|Res], State) 
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(A, Res, State);
        _Err ->
            {done, err()}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.



%%  upload(A,Name,Fun) when A#arg.state == undefined ->
%%      multipart(A, #upload{},Name,Fun);
%%  upload(A,Name,Fun) ->
%%      multipart(A, A#arg.state,Name,Fun).

%%  multipart(A,State,Name,Fun) ->
%%      Parse = yaws_api:parse_multipart_post(A),
%%      case Parse of
%%          {cont, Cont, Res} ->
%%              case addFileChunk(A, Res, State, Name,Fun) of
%%                  {done, Result} ->   Result;
%%                  {cont, NewState} -> {get_more, Cont, NewState}
%%              end;
%%          {result, Res} ->
%%              case addFileChunk(A, Res, State#upload{last=true},Name,Fun) of
%%                  {done, Result}  -> Result;
%%                  {cont, _}       -> error
%%   	        end
%%      end.

%% addFileChunk(A, [{part_body, Data}|Res], State,Name,Fun) ->
%%     addFileChunk(A, [{body, Data}|Res], State,Name,Fun);
%% addFileChunk(_A, [], S,_Name,Fun) when S#upload.last==true,
%% S#upload.filename /= undefined, S#upload.fd /= undefined ->
%%     file:close(S#upload.fd),
%%     Fun(["/tmp/",S#upload.filename]),
%%     {done, ok};
%% addFileChunk(_A, [], S,_Name,_Fun) when S#upload.last==true ->
%%     {done, ok};
%% addFileChunk(_A, [], State,_Name,_Fun) -> {cont, State};
%% addFileChunk(A, [{head, {Name, Opts}}|Res], State,Name,Fun) ->
%%     case lists:keysearch(filename, 1, Opts) of
%%         {value, {_, Fname0}} ->
%%             Fname = yaws_api:sanitize_file_name(basename(Fname0)),
%%             case file:open(["/tmp/", Fname] ,[write]) of
%%                 {ok, Fd} ->
%%                     S2 = State#upload{filename = Fname,fd = Fd},
%%                     addFileChunk(A, Res, S2,Name,Fun);
%%                 _ -> {done, ok}
%%             end;
%%         false -> {done, ok}
%%     end;
%% addFileChunk(A, [{body, Data}|Res], State,Name,Fun)
%%   when State#upload.filename /= undefined ->
%%     case file:write(State#upload.fd, Data) of
%%         ok -> addFileChunk(A, Res, State, Name,Fun);
%%         _  -> {done, ok}
%%     end;
%% addFileChunk(A, [_N|Res], State,Name,Fun) ->
%%     addFileChunk(A, Res, State,Name,Fun).

%% basename(FilePath) ->
%%     case string:rchr(FilePath, $\\) of
%%         0 ->% probably not a DOS name
%%             filename:basename(FilePath);
%%         N ->% probably a DOS name, remove everything after last \
%%             basename(string:substr(FilePath, N+1))
%%     end.
