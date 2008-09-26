%% @author Dale Harvey
%% @copyright 2008 Hypernumbers Ltd
%% @doc Handle Hypernumbers HTTP requests

-module(hn_yaws).

-include("yaws_api.hrl").
-include("spriki.hrl").
-include("handy_macros.hrl").

-import(simplexml,[to_xml_string/1,to_json_string/1]).
-export([ out/1, get_page_attributes/1 ]).

-record(upload, {fd,filename, last}).

%% @spec out(Arg) -> YawsReturn.
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
                               error_logger:error_msg("~p~n~p",[Else,Stack]),
                               {ok,[{status,400}]}
                       end,
            Ret
    end.

%% @spec do_request(Arg,Url) -> {ok,Responce}
%% @doc handle incoming requests
do_request(Arg,Url) ->

    RV = fun({X,undefined}) -> X;
            (Else) -> Else
         end,
    Method = (Arg#arg.req)#http_request.method,
    {ok,Ref} = hn_util:parse_url(Url),
    Vars = lists:map(RV,yaws_api:parse_query(Arg)),
    {ok,Type} = hn_util:get_req_type(Vars),
    {ok,PostData} = get_post_data(Arg,Type),

%% Verify AuthToken, authtoken can be passed via url
%% or cookies, url takes precedence
    {ok,AuthCode} = get_var_or_cookie(auth,Vars,Arg),
    {ok,Token} = get_var_or_cookie(token,Vars,Arg),
    User = case string:tokens(AuthCode,":") of
               [] ->
                   anonymous;
               [UserId,_AuthToken] ->
%% TODO Verify Token
                   UserId
           end,

    {ok,Access} = case get_permissions(User,Ref) of
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
                             [{header,{cache_control,"no-cache"}},
                              {status,200},
                              {content,"text/xml",to_xml_string(Data)}];
                         json ->
                             [{content,"text/plain",to_json_string(Data)}]
                     end
             end,

    {ok,Return}.

req('GET',[],_,no_access,#ref{ref={page,"/"}}) ->
    {return,{page,"/html/login.html"}};
req('GET',[],_,no_access,_Ref)  ->
    {return,{status,503}};
req('GET',[],_,require_token,#ref{ref={page,"/"}}) ->
    {return,{page,"/html/token.html"}};
req('GET',[],_,require_token,_Page)  ->
    {return,{status,503}};
req('GET',[],[],_,Ref=#ref{ref={page,"/"}}) ->
    {ok,V} = hn_db:get_item_inherited(Ref,"index"),
    {return,{page,"/html/"++V++".html"}};
req('GET',[],[{"gui",GUI}],_,#ref{ref={page,"/"}}) ->
    {return,{page,"/html/"++GUI++".html"}};

%%req('GET',[],[{"new",Template}],_,Ref=#ref{ref={page,"/"}}) ->
%%    {ok,Tpl} = hn_db:get_item_inherited(Ref#ref{name=template},blank),
%%    Items = hn_db:get_item(Ref#ref{path=lists:append(Ref#ref.path,'_')}),
%%    F = fun(#hn_item{addr=R}) ->
%%                Last = lists:last(R#ref.path),
%%                ?COND(hn_util:is_numeric(Last),
%%                      list_to_integer(Last),0)
%%        end,
%%    Ind = case Items of
%%              [] -> 1;
%%              _Else -> lists:max(lists:map(F,Items))+1
%%          end,
%%    NPage = "/"++string:join(Ref#ref.path,"/")++"/"++integer_to_list(Ind)++"/",

%%    hn_main:copy_page(Ref#ref{path=[Tpl]},NPage),
%%    {return,{redirect, Ref#ref.site++NPage}};

req('GET',[],[{"new",Template}],_,Ref=#ref{site=Site,ref={page,"/"}}) ->
    NPage=hn_templates:get_next(#ref{site=Site},Template,"BlankUsername"),
    Tpl="@"++Template,
    io:format("in hn_yaws:req add the gui to the beast~n"),
    hn_main:copy_page(Ref#ref{path=[Tpl]},NPage),
    {return,{redirect, Ref#ref.site++NPage}};

req('GET',[],["templates"],_,Ref=#ref{ref={page,"/"}}) ->
    hn_templates:get_templates();

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
                      []  -> {blank,[],[]};
                      Tmp -> hn_util:to_xml(Tmp)
                  end
          end,

    DepTree = case hn_db:get_item_val(Ref#ref{name='dependancy-tree'}) of
                  {xml,Tree} -> Tree;
                  []         -> []
              end,
    {ok,{hypernumber,[],[{value,[], Val()},{'dependancy-tree',[], DepTree}]}};

%% /a1
req('GET',[],[],_,Ref) ->
    case hn_db:get_item(Ref) of
        []   ->
            {return,{content,"text/plain","blank"}};
        List ->
            F = fun(X) ->
                        (X#hn_item.addr)#ref.name == rawvalue
                end,

            Val = case lists:filter(F,List) of
                      [] -> 0;
                      [#hn_item{val=Value}] -> Value
                  end,

            {return,{content,"text/plain",hn_util:text(Val)}}
    end;

req('POST',{login,[],[{email,[],[Email]},{password,[],[Pass]}]},[],_User,_Ref) ->
    case users:login(Email,Pass) of
        {error,invalid_user} ->
            {ok,{auth,[],[{unauthorised,[],[]}]}};
        {ok,Record} ->
            {IP,_Port} = {{127,0,0,1},80},
            Token = users:gen_authtoken(Record,IP),
            Sent  = Email++":"++hn_util:bin_to_hexstr(Token),
            {ok,{auth,[],[{token,[],[Sent]}]}}
    end;

req('POST',[],_,X,_) when X == no_access; X == read  ->
    {return,{status,503}};

req('POST',{create,[],[{Name,[],[Value]}]},_Attr,_User,Ref) ->
    hn_main:set_attribute(Ref#ref{name=Name},Value),
    {ok,{success,[],[]}};

req('POST',{create,[],Data},_Attr,_User,Ref = #ref{ref={range,{Y1,X1,Y2,X2}}}) ->
    F = fun(X,Y,Z) ->
                case lists:nth(Z,Data) of
                    {_Name,[],[]} ->
                        ok;
                    {Name,[],[Val]} ->
                        NewRef = Ref#ref{ref={cell,{Y,X}},name=Name},
                        hn_main:set_attribute(NewRef,Val)
                end
        end,

    [[ F(X,Y,((X-X1)*(Y2-Y1+1))+(Y-Y1)+1)
       || Y <- lists:seq(Y1,Y2)] || X <- lists:seq(X1,X2)],

    {ok,{success,[],[]}};

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

req('POST',{delete,[],[]},_Attr,_User,Ref = #ref{ref={page,"/"}}) ->
    hn_db:remove_item(Ref#ref{ref='_'}),
    {ok,{success,[],[]}};

req('POST',{delete,[],Data},_Vars,_User,Ref) ->
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
    hn_db:register_hn(
      hn_util:ref_to_index(Ref),
      hn_util:ref_to_index(hn_util:parse_url(Reg)),
      Bic, Proxy, Reg),
    {ok,{success,[],[]}};

req('POST',{notify,[],Data},_Attr,_User,Ref) ->
    case lists:keysearch(type,1,Data) of
        {value,{type,[],["change"]}} ->
            api_change(Data,Ref)
    end;

req('POST',{template,[],[{name,[],[Name]},
                         {url,[],[Url]},
                         {gui,[],[Gui]},
                         {formurl,[],[Form]}]},_Attr,_User,Ref) ->
    Tpl = "/@"++Name++"/",
    ok = hn_main:copy_page(Ref,Tpl),
    {ok,NRef} = hn_util:parse_url(Ref#ref.site++Tpl),
    #ref{path=Path}=NRef,
    {ok,ok}=hn_templates:write_def(Name,Url,Gui,Form),
    ok=hn_main:set_attribute(NRef#ref{name=template},"@"++Name),
    ok=hn_main:set_attribute(NRef#ref{name=gui},Gui),
    ok=hn_main:set_attribute(NRef#ref{name=form},Form),
    {ok,{success,[],[]}};

req(Method,Data,Vars,User,Page) ->
    throw({unmatched_request,Method,Data,Vars,User,Page}).


api_change([{biccie,[],     [Bic]},
            {cell,[],       [Cell]},
            {type,[],       ["change"]},
            {value,[],      [Val]},
            {version,[],    [Version]}], _Page)->

    hn_db:update_hn(Cell,Bic,Val,Version),

    {ok,{success,[],[]}}.


%%% API call - IMPORT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% import an excel file, the import and log txt probably needs
%% cleaned up
%% api_import(Arg,Page) ->
%%     F = fun(X) ->
%%         filefilters:read(excel,X,fun(A) -> import(A,Page) end)
%%     end,
%%     case hn_util:upload(Arg,"Filedata",F) of
%%     ok ->    {ok,{create,[],["success"]}};
%%     error -> {ok,{create,[],["error"]}}
%%    end.

%% import([],_)-> ok;
%% import([{_Name,Tid}|T],Page)->
%%     Fun = fun(X,_Y) ->
%%         case X of
%%         {{_,{row_index,Row},{col_index,Col}},[_,Val]} ->
%%             Req = lists:flatten([Page#page.site,Page#page.path,
%%                 util2:make_b26(Col+1),hn_util:text(Row+1)]),
%%             V = case Val of
%%                 {value,number,Num} -> Num;
%%                 {formula,Form}     -> Form;
%%                 {string,Str}       -> Str
%%             end,
%%             hn_util:post(Req,"<create><formula>"++
%%                 hn_util:text(V)++"</formula></create>","text/xml");
%%         _ -> false
%%         end
%%     end,
%%     ets:foldl(Fun,[],Tid),
%%    import(T,Page).


%% Utility functions
%%--------------------------------------------------------------------
get_page_attributes(Ref) ->
    Items = lists:filter(
	      fun(#hn_item{addr=A}) ->
                      case (A)#ref.name of
                          "__"++_ ->
                              false;
                          _ ->
                              true
                      end
              end,
              hn_db:get_item(Ref)),
    List =  lists:map(fun hn_util:item_to_xml/1,Items),
    {attr,[],List}.

get_post_data(Arg,_) when (Arg#arg.req)#http_request.method == 'GET' ->
    {ok,[]};
get_post_data(Arg,json) ->
    {ok,simplexml:from_json_string(binary_to_list(Arg#arg.clidata))};
get_post_data(Arg,xml) ->
    {ok,simplexml:from_xml_string(binary_to_list(Arg#arg.clidata))}.

get_default(public) -> "public";
get_default(_) -> "".

%% Get the index of the last populated row or column
get_last_index(Site,Path,RowCol) ->
    case hn_db:get_item(#ref{site=Site,path=Path,ref={page,"/"}}) of
        []   -> 0;
        Else ->
            %% Only count cell value attributes
            CellList = lists:filter( fun(#hn_item{addr=Ref}) ->
                                             case Ref of
                                                 #ref{name=value, ref = {cell,_}} -> true;
                                                 _ -> false
                                             end end,Else),

            List = lists:sort(
                     fun(#hn_item{addr=#ref{ref = {cell,{X1,Y1}}}},
                         #hn_item{addr=#ref{ref = {cell,{X2,Y2}}}}) ->
                             ?COND(RowCol == column,X1 < X2,Y1 < Y2)
                     end,CellList),

	    case List of
		[]    -> 0;
                _Else ->  #hn_item{addr=#ref{ref={cell,{X,Y}}}} = lists:last(List),
			  ?COND(RowCol == column,X,Y)
	    end
    end.

%% Takes an unfiltered list of spriki records, extracts the path
%% they are from and constructs a tree
create_pages_tree(List) ->

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
		    {dir,[{path,P}],_} ->
                        true;
		    _ ->
                        false
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

%% Given a user find the most elevated permission
%% granted to that user
get_permissions(User,Ref=#ref{site=Site}) ->
    Groups = hn_db:get_item_val(#ref{site=Site,ref={page,"/"},name="__groups"}),
    {ok,Perms}   = hn_db:get_item_list(Ref#ref{name="__permissions"}),
    {ok,UGroups} = find_groups(Groups,User),
    {ok,Access}  = find_perms(User,UGroups,[no_access],Perms),
    %% Sort permissions in order of precedence then pick
    %% the highest
    Sort = fun(X,Y) -> perm_index(X) > perm_index(Y) end,
    [H|_Rest] = lists:sort(Sort,Access),
    {ok,H}.

%% Rank permission atoms in order of precedence
perm_index(no_access)           -> 0;
perm_index({protected_read,_X}) -> 1;
perm_index({protected_write,_X})-> 2;
perm_index(read)                -> 3;
perm_index(write)               -> 4;
perm_index(admin)               -> 5.

%% Find the permissions pertaining to a user
find_perms(_User,_Groups,Perms,[]) ->
    {ok,Perms};
find_perms(User,Groups,Perms,[{user,anonymous,Perm}|T]) ->
    find_perms(User,Groups,[Perm|Perms],T);
find_perms(User,Groups,Perms,[{user,User,Perm}|T]) ->
    find_perms(User,Groups,[Perm|Perms],T);
find_perms(User,Groups,Perms,[{group,Group,Perm}|T]) ->
    P = case lists:member(Group,Groups) of
            true ->
                [Perm|Perms];
            false ->
                Perms
        end,
    find_perms(User,Groups,P,T);
find_perms(User,Groups,Perms,[_H|T]) ->
    find_perms(User,Groups,Perms,T).

%% Create a list of groups that a user is in
%% expanded into nested groups
find_groups(Groups,User) ->
    find_groups(Groups,User,Groups,[]).

find_groups(_GList,_User,[],Groups) ->
    {ok,Groups};
find_groups(GList,User,[{Name,List}|T],Groups) ->
    NGroups = group_list(GList,User,[Name],List,Groups),
    find_groups(GList,User,T,NGroups).

group_list(_GList,_User,_Name,[],Acc) ->
    hslists:uniq(lists:flatten(Acc));
group_list(GList,User,Name,[{user,User}|T],Acc) ->
    group_list(GList,User,Name,T,[Name|Acc]);
group_list(GList,User,Name,[{group,Group}|_T],Acc) ->
    case lists:keysearch(Group,1,GList) of
        false ->
            Acc;
        {value,{Group,List}} ->
            group_list(GList,User,[Group|Name],List,Acc)
                %++ group_list(GList,User,Name,T,Acc)
    end;
group_list(GList,User,Name,[{user,_A}|T],Acc) ->
    group_list(GList,User,Name,T,Acc).

get_var_or_cookie(Name,Vars,Arg) ->
    case lists:keysearch(Name,1,Vars) of
        false ->
            {ok,yaws_api:find_cookie_val(atom_to_list(Name),Arg)};
        {value,{auth,Auth}} ->
            {ok,Auth}
    end.


upload(A,Name,Fun) when A#arg.state == undefined ->
    multipart(A, #upload{},Name,Fun);
upload(A,Name,Fun) ->
    multipart(A, A#arg.state,Name,Fun).

multipart(A,State,Name,Fun) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State, Name,Fun) of
                {done, Result} ->   Result;
                {cont, NewState} -> {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true},Name,Fun) of
                {done, Result}  -> Result;
                {cont, _}       -> error
 	        end
    end.

addFileChunk(A, [{part_body, Data}|Res], State,Name,Fun) ->
    addFileChunk(A, [{body, Data}|Res], State,Name,Fun);
addFileChunk(_A, [], S,_Name,Fun) when S#upload.last==true,
S#upload.filename /= undefined, S#upload.fd /= undefined ->
    file:close(S#upload.fd),
    Fun(["/tmp/",S#upload.filename]),
    {done, ok};
addFileChunk(_A, [], S,_Name,_Fun) when S#upload.last==true ->
    {done, ok};
addFileChunk(_A, [], State,_Name,_Fun) -> {cont, State};
addFileChunk(A, [{head, {Name, Opts}}|Res], State,Name,Fun) ->
    case lists:keysearch(filename, 1, Opts) of
    {value, {_, Fname0}} ->
        Fname = yaws_api:sanitize_file_name(basename(Fname0)),
        case file:open(["/tmp/", Fname] ,[write]) of
        {ok, Fd} ->
            S2 = State#upload{filename = Fname,fd = Fd},
            addFileChunk(A, Res, S2,Name,Fun);
        _ -> {done, ok}
        end;
    false -> {done, ok}
    end;
addFileChunk(A, [{body, Data}|Res], State,Name,Fun)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
    ok -> addFileChunk(A, Res, State, Name,Fun);
    _  -> {done, ok}
    end;
addFileChunk(A, [_N|Res], State,Name,Fun) ->
    addFileChunk(A, Res, State,Name,Fun).

basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
    0 ->%% probably not a DOS name
        filename:basename(FilePath);
    N ->%% probably a DOS name, remove everything after last \
        basename(string:substr(FilePath, N+1))
    end.
