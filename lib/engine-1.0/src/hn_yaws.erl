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
-include("handy_macros.hrl").

-import(simplexml,[to_xml_string/1,to_json_string/1]).
%%%-----------------------------------------------------------------
%%% Exported Functions
%%%-----------------------------------------------------------------
-export([ out/1 ]).

out(Arg) ->
    
    Url      = yaws_api:request_url(Arg),
    FileName = Arg#arg.docroot++Url#url.path,
    
    %% Serve static files, can move to a different server later
    case filelib:is_file(FileName) and not filelib:is_dir(FileName) of
        true  -> 
            [{page,Url#url.path},{header,{cache_control,"max-age=3600"}}];
        false ->
            
            {ok,Ret} = case catch(do_request(Arg,Url)) of
                           {ok,Resp} -> {ok,Resp};
                           Else      -> 
                               Stack = erlang:get_stacktrace(),
                               error_logger:error_msg("~p~n~p",[Else,Stack]),
                               {ok,[{status,400}]}
                       end,

            Headers = [{header,{cache_control,"no-cache"}}],
            lists:append(Ret,Headers)              
    end.

do_request(Arg,Url) ->  
    
    Page   = hn_util:parse_url(Url),
    Ref    = page_to_ref(Page),
    Method = (Arg#arg.req)#http_request.method,
    
    %% Verify AuthToken, authtoken can be passed via url 
    %% or cookies, url takes precedence
    {ok,AuthCode} = get_var_or_cookie(auth,Page,Arg),
    
    User = case string:tokens(AuthCode,":") of 
               [] ->        
                   anonymous;
               [UserId,_AuthToken] -> 
                   %% Verify Token
                   UserId
           end,
        
    {ok,Token} = get_var_or_cookie(token,Page,Arg),
    {ok,Access} = case get_permissions(User,Ref) of
                      {ok,{protected_read,Token}}  -> {ok,read};
                      {ok,{protected_write,Token}} -> {ok,write}; 
                      {ok,{_Write,_Tok}} -> {ok,require_token};
                      Else -> Else
                  end,               

    Return = case req(Arg,Method,Access,Page) of
                 {ok,{page,Path}} -> 
                     [{page,Path}];
                 {ok,{status,Status}} -> 
                     [{status,Status}];
                 {ok,{text,Text}} -> 
                     [{content,"text/plain",Text}];
                 {ok,Data} -> 
                     case Page#page.format of
                         {xml} -> 
                             [{content,"text/xml",to_xml_string(Data)}];
                         {json} -> 
                             [{content,"text/plain",to_json_string(Data)}]
                     end
             end,
    
    {ok,Return}.

get_var_or_cookie(Name,Page,Arg) ->
    case lists:keysearch(Name,1,Page#page.vars) of
        false -> 
            {ok,yaws_api:find_cookie_val(atom_to_list(Name),Arg)};
        {value,{auth,Auth}} ->  
            {ok,Auth}
    end.
    
%% call to a page without access redirects to login
req(_Arg,'GET',no_access,#page{ref={page,"/"}})  -> 
    {ok,{page,"/html/login.html"}};
%% GET api call with no_access
req(_Arg,'GET',no_access,_Page)  -> 
    {ok,{status,503}};

%% call to a page without access redirects to login
req(_Arg,'GET',require_token,#page{ref={page,"/"}})  -> 
    {ok,{page,"/html/token.html"}};
%% GET api call with no_access
req(_Arg,'GET',require_token,_Page)  -> 
    {ok,{status,503}};

%% Index page "/"
req(_Arg,'GET',_,#page{ref={page,"/"},vars=[]}) -> 
    {ok,{page,"/html/index.html"}};

%% ?attr
req(_Arg,'GET',_,Page = #page{vars = [{attr}]}) -> 
    Items = lists:filter(
              fun(#hn_item{addr=A}) -> 
                      case (A)#ref.name of
                          "__"++_ -> 
                              false;
                          _ -> 
                              true
                      end
              end,
              hn_db:get_item(page_to_ref(Page))),
    List = lists:map(fun hn_util:item_to_xml/1 , Items),
    {ok,{attr,[],List}};    

%% ?attr=value
req(_Arg,'GET',_,Page = #page{vars = [{attr,Val}]}) -> 
    Name   = list_to_existing_atom(Val),
    Ref    = (page_to_ref(Page))#ref{name=Name},
    {ok,V} = hn_db:get_item_inherited(Ref,get_default(Name)),
    {ok,{attr,[],[V]}};

%% ?pages
req(_Arg,'GET',_,#page{vars = [{pages}]}) -> 
    Items = mnesia:dirty_match_object(#hn_item{_ = '_'}),
    {ok,create_pages_tree(Items)};

%% ?hypernumber
req(_Arg,'GET',_,Page = #page{vars = [{hypernumber}]}) -> 
    Addr = page_to_ref(Page),   
    Val = fun() ->
                  case hn_db:get_item_val(Addr#ref{name=rawvalue}) of
                      []  -> {blank,[],[]};
                      Tmp -> hn_util:to_xml(Tmp)
                  end
          end,
    
    DepTree = case hn_db:get_item_val(Addr#ref{name='dependancy-tree'}) of
                  {xml,Tree} -> Tree;
                  []         -> []
              end,
    {ok,{hypernumber,[],[{value,[], Val()},{'dependancy-tree',[], DepTree}]}};

%% /a1 
req(_Arg,'GET',_,Page = #page{vars = []}) -> 
    case hn_db:get_item(page_to_ref(Page)) of
        []   -> {ok,{text,"blank"}};
    List ->      
            F = fun(X) ->                        
                        (X#hn_item.addr)#ref.name == rawvalue
                end,
            
            Val = case lists:filter(F,List) of
                  [] -> 0;
                      [#hn_item{val=Value}] -> Value
                  end,  
            
            {ok,{text,hn_util:text(Val)}}
    end;

req(Arg,'POST', _User, Page = #page{format=Format,vars=Vars}) ->
    {ok,PostData} = get_post_data(Format,Arg,Vars),
    post(Arg,_User,Page,PostData);
    
req(_Arg,_Method,_User,_Page) ->
    throw(unmatched_get_request).

post(Arg,_User,_Page,{login,[],[{email,[],[Email]},{password,[],[Pass]}]}) ->
    case users:login(Email,Pass) of
        {error,invalid_user} -> 
            {ok,{auth,[],[{unauthorised,[],[]}]}};
        {ok,Record} ->
            {IP,_Port} = Arg#arg.client_ip_port,
            Token = users:gen_authtoken(Record,IP),
            Sent  = Email++":"++hn_util:bin_to_hexstr(Token),
            {ok,{auth,[],[{token,[],[Sent]}]}}
    end;

post(_Arg,X,_,_) when X == no_access; X == read  ->
    {ok,{status,503}};

post(Arg,_User,Page,{create,[],Data}) ->
    
    Ref = page_to_ref(Page),    
    {ok,Auth} = get_var_or_cookie(auth,Page,Arg),
    
    lists:foldl
      (
      fun({Attr,[],[Val]},Sum) ->
              
              NewAddr = case lists:member({lastrow},Page#page.vars) of
                            true  -> 
                                {row,X} = Ref#ref.ref,
                                Ref#ref{ref={cell,{Sum,X+1}},name=Attr};
                            false -> 
                                Ref#ref{name=Attr}
                        end,
              
              case Val of
                  [] -> throw(empty_val); 
                  _  -> hn_main:set_attribute(NewAddr#ref{auth=Auth},Val)
              end,
              Sum+1
      end,
      1,
      Data
     ),
    {ok,{success,[],[]}};

post(_Arg,_User,Page,{delete,[],Data}) ->    
    lists:map
      (
      fun({Attr,[],[]}) ->
              hn_db:remove_item((page_to_ref(Page))#ref{name=Attr})
      end,
      Data
     ),
    {ok,{success,[],[]}};

%%% UNREGISTER
post(_Arg,_User,Page,{unregister,[],[{biccie,[],[_Bic]},{url,[],[Url]}]}) -> 
    hn_db:del_remote_link(#remote_cell_link{ 
        parent = hn_util:page_to_index(Page),
        child  = hn_util:page_to_index(hn_util:parse_url(Url)),
        type   = outgoing }),
    
    {ok,{success,[],[]}};

%%% REGISTER
post(_Arg,_User,Page,
     {register,[],[{biccie,[],[Bic]},{proxy,[],[Proxy]},{url,[],[Reg]}]}) ->
    hn_db:register_hn(
      hn_util:page_to_index(Page),
      hn_util:page_to_index(hn_util:parse_url(Reg)),
      Bic, Proxy, Reg),
    {ok,{success,[],[]}};

%%% NOTIFY
post(_Arg,_User,Page,{notify,[],Data}) ->
    case lists:keysearch(type,1,Data) of
        {value,{type,[],["change"]}} -> 
            api_change(Data,Page)
    end;

post(_Arg,_User,_Page,Data) ->
    error_logger:error_msg("~p~n",[Data]),
    throw(unmatched_post_request).

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
            hn_util:post(Req,"<create><formula>"++
                hn_util:text(V)++"</formula></create>","text/xml");
        _ -> false
        end
    end,
    ets:foldl(Fun,[],Tid),
    import(T,Page).


%% Utility functions
%%--------------------------------------------------------------------
get_post_data(_,_Arg,[{import}]) -> 
    {ok,[import]};
get_post_data({json,nocallback},Arg,Dec) -> 
    get_post_data({json},Arg,Dec);
get_post_data({json},Arg,_Dec) ->
    {ok,simplexml:from_json_string(binary_to_list(Arg#arg.clidata))};
get_post_data({xml},Arg,_Dec) ->
    {ok,simplexml:from_xml_string(binary_to_list(Arg#arg.clidata))}.

get_default(public) -> "public";
get_default(_) -> "". 

page_to_ref(#page{site=Site, path=Path, ref=Ref,vars=Vars}) ->
    NRef = case lists:member({lastrow},Vars) of
               true  -> {row,get_last_index(Site,Path,row)};
               false -> 
                   case lists:member({lastcol},Vars) of
                       true  -> {column,get_last_index(Site,Path,column)};
                       false -> Ref
                   end
           end,
    #ref{site=Site, path=Path, ref=NRef}. 

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
            
            #hn_item{addr=#ref{ref={cell,{X,Y}}}} = lists:last(List),
            ?COND(RowCol == column,X,Y)
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
perm_index({protected_read,_X})  -> 1;
perm_index({protected_write,_X}) -> 2;
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
group_list(GList,User,Name,[{group,Group}|T],Acc) ->
    case lists:keysearch(Group,1,GList) of
        false -> 
            Acc;
        {value,{Group,List}} ->
            group_list(GList,User,[Group|Name],List,Acc)
                %++ group_list(GList,User,Name,T,Acc)
    end;
group_list(GList,User,Name,[{user,_A}|T],Acc) ->
    group_list(GList,User,Name,T,Acc).
