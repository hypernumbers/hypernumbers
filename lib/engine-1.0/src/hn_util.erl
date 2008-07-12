%%%-----------------------------------------------------------------------------
%%% File        : hn_util.erl
%%% Author      : Dale Harvey
%%% Description : Utilities for hypernumbers application
%%%-----------------------------------------------------------------------------
-module(hn_util).

-include("spriki.hrl").
-include("regexp.hrl").
-include("yaws.hrl").
-include("yaws_api.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

-record(upload, {fd,filename, last}).

-export([
    %% HyperNumbers Utils
    index_to_url/1,     page_to_index/1,    ref_to_str/1,
    xml_to_val/1,
    item_to_xml/1,
    in_range/2,
    to_xml/1,
    %% HTTP Utils
    req/1,              post/2,             post/3,
    parse_url/1,
    %% XML Utils
    xmlsearch/3,        values/2,
    %% File Utils
    read/1,
    %% List Utils
    add_uniq/2,         implode/2,          is_alpha/1,
    is_numeric/1,       str_replace/2,      text/1,
    trim/1,
    %% Yaws Utils
    create_conf/1,      upload/3,           get_cookie/1
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% HyperNumbers Utils                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index_to_url(#index{site=Site,path=Path,column=X,row=Y}) ->
    lists:append([Site, string:join([Path], "/"), "/", tconv:to_b26(X), text(Y)]).
    
page_to_index(#page{site=Site,path=Path,ref={cell,{X,Y}}}) ->
    #index{site=Site,path=Path,column=X,row=Y}.

ref_to_str({page,Path})  -> Path;   
ref_to_str({cell,{X,Y}}) -> tconv:to_b26(X)++text(Y);
ref_to_str({row,Y})      -> text(Y);
ref_to_str({column,X})   -> tconv:to_b26(X);
ref_to_str({range,{X1,Y1,X2,Y2}}) ->
    tconv:to_b26(X1)++text(Y1)++":"++tconv:to_b26(X2)++text(Y2).


xml_to_val({boolean,[],[true]})  -> true;
xml_to_val({boolean,[],[false]}) -> false;
xml_to_val({error,[],[Ref]})     -> Ref;
xml_to_val({float,[],[Ref]})     -> list_to_float(Ref);
xml_to_val({int,[],[Ref]})       -> list_to_integer(Ref);
xml_to_val({string,[],[Ref]})    -> Ref;
xml_to_val({datetime, [], [Ref]}) -> Ref;
xml_to_val(Else)                 -> Else.

%% Turn a hn_item record into its xml <ref> display
item_to_xml(#hn_item{addr=A,val=V}) ->
    Type = hn_util:text(element(1,A#ref.ref)),
    Str  = hn_util:ref_to_str(A#ref.ref),
    
    Value = case A#ref.name of
    value    -> to_xml(V);
    rawvalue -> to_xml(V);
    Else     -> to_val(V)
    end,
    
    {ref,[{type,Type},{ref,Str}],[{A#ref.name,[], Value}]}.
    
in_range({range,{X1,Y1,X2,Y2}},{cell,{X,Y}}) ->
    Y >= Y1 andalso Y =< Y2 andalso X >= X1 andalso X =< X2.

to_val({xml,Xml}) -> Xml;
to_val(Else) ->
    case io_lib:char_list(Else) of
    true  -> [Else];
    false -> throw({unmatched_type,Else})
    end.

to_xml(true)  -> [{bool,[],["true"]}];
to_xml(false) -> [{bool,[],["false"]}];    
to_xml(Val) when is_integer(Val) -> [{int,[],[integer_to_list(Val)]}];
to_xml(Val) when is_float(Val)   -> [{float,[],[float_to_list(Val)]}];
to_xml(Val) when is_atom(Val)    -> [{error,[],[atom_to_list(Val)]}];
to_xml(Else) ->
    case io_lib:char_list(Else) of
    true  -> [{string,[],[Else]}];
    false -> throw({unmatched_type,Else})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Http Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
req(Url) ->
    {ok,{{_V,_Status,_R},_H,Body}} = http:request(get,{Url,[]},[],[]),
    Body.

post(Url,Data) ->
    {ok, {{_V,_Status,_R},_H,Body}} = 
        http:request(post,{Url,[],"text/plain",Data},[],[]),
    Body.

post(Url,Data,Format) ->
    {ok, {{_V, _Status,_R},_H,Body}} =
        http:request(post,{Url,[],Format,Data},[],[]),
    Body.

parse_url(Url) when is_list(Url) ->
    parse_url(yaws_api:parse_url(Url));

parse_url(Url) when is_record(Url,url) ->

    Port = case {Url#url.port,Url#url.scheme} of
        {undefined,http} -> "80";
        {undefined,https} -> "443";
        {Else,_} -> integer_to_list(Else)
    end,

    Site = lists:concat([Url#url.scheme,"://",Url#url.host,":",Port]),

    {Format,Vars} = format_vars(Url#url.querypart),

    {Ref,Path} = case lists:last(Url#url.path) of
        $/ -> {"/",string:tokens(Url#url.path,"/")};
        _  ->
            Tokens = string:tokens(Url#url.path,"/"),
            [TmpRef|T] = lists:reverse(Tokens),
            {TmpRef,lists:reverse(T)}
    end,

    RefType = parse_reference(Ref),

    RefVal = case RefType of
    page ->   "/";
    cell ->   util2:strip_ref(Ref);
    range ->  util2:parse_range(Ref);
    column -> element(1,util2:strip_ref(Ref++"1"));
    row ->    element(2,util2:strip_ref("a"++Ref));
    noref ->  Ref
    end,
    
    #page{
        site=Site,
        path=Path,
        ref={RefType,RefVal},
        vars=Vars,
        format=Format}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% XML Utils                                                                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    Return = file:read_file(FileName),
    %%io:format("in hn_util:read FileName is ~p and Return is ~p~n",[FileName,Return]),
    {ok,Binary} = Return,
    {ok,binary_to_list(Binary)}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% List Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% Function:    add_uniq/2
%%
%% Description: 
%%--------------------------------------------------------------------
add_uniq(List,Item) -> 

    [Item] ++ lists:filter(
        fun(X) -> ?COND(X == Item,false,true) end,
        List).
    
%%--------------------------------------------------------------------
%% Function:    trim/2
%%--------------------------------------------------------------------
trim(String) -> strip_ws(lists:reverse(strip_ws(lists:reverse(String)))).

strip_ws([H|Rest]) when H == 10; H == 13; H == 32 -> strip_ws(Rest);
strip_ws(String) -> String.
%%--------------------------------------------------------------------
%% Function:    str_replace/2
%%
%% Description: Takes a list of key/value pairs, and a string, and
%%              replaces {key} with value in the string
%%              cleans {key} pairs that arent replaces out of the
%%              string
%%--------------------------------------------------------------------
str_replace(Template,[]) ->
    {ok,Temp,_} = regexp:gsub(Template,"\{\[A-Za-z]*\}",""),
    Temp;

str_replace(Template,[{K,V}|T]) ->
    {ok,Temp,_} = regexp:gsub(Template,"{"++K++"}",V),
    str_replace(Temp,T).

%%--------------------------------------------------------------------
%% Function:    implode/2
%%
%% Description: Add all the items in the list together into a string
%%              interspace with 'Token' (number converted to strings).
%%              (Token doesn't get added on to the last element).
%%--------------------------------------------------------------------
implode(Data, Sep) when is_list(Data) andalso is_list(Sep) ->
   lists:foldr(fun(X,[]) -> X; (X,Acc) -> X++Sep++Acc end, "", Data).

%%--------------------------------------------------------------------
%% Function:    is_alpha/1
%% Description: Returns true if a string is a list of a-z
%%--------------------------------------------------------------------
is_alpha(Str) ->
    Fun = fun(XX) ->         
        if XX < 97  -> false;  
           XX > 122 -> false;
           true     -> true      
        end                  
    end,
    case is_list(Str) of
        false -> false;
        true -> lists:all(Fun, Str)
    end.

%%--------------------------------------------------------------------
%% Function:    is_numeric/1
%% Description: Returns true if a string is a list of digits
%%--------------------------------------------------------------------
is_numeric([]) -> false;
is_numeric(Str) ->
    Fun = fun(XX) ->         
        if XX < 48 -> false;  
           XX > 57 -> false;
           true    -> true      
        end                  
    end,
    case is_list(Str) of
        false -> false;
        true -> lists:all(Fun, Str)
    end.

%%--------------------------------------------------------------------
%% Function:    text/1
%% Description: Returns a string representation of the parameter
%%--------------------------------------------------------------------
text(X) when is_integer(X) -> integer_to_list(X);
text(X) when is_float(X)   -> float_to_list(X);
text(X) when is_list(X)    -> lists:flatten(X);
text(X) when is_atom(X)    -> atom_to_list(X);
text(_X) -> "". %% quick fix for the "plain" api

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Yaws Utils                                                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_conf(Xml) ->
    [{_,_,Conf}] = xmlsearch([],Xml,yawsconf),
    Sconf = lists:map(fun(X) -> create_sconf(X) end,xmlsearch([],Xml,group)),
    Gconf = gconf_vals(#gconf{},Conf),
    {Gconf,Sconf}.

upload(A,Name,Fun) when A#arg.state == undefined -> 
    multipart(A, #upload{},Name,Fun);
upload(A,Name,Fun) -> 
    multipart(A, A#arg.state,Name,Fun).

%%--------------------------------------------------------------------
%% Function:    get_cookie/1
%% Description: reads the cookie and session data from request header
%%              returns nice record of state information
%%--------------------------------------------------------------------
get_cookie(ClientHeaders) ->

    case yaws_api:find_cookie_val("user",ClientHeaders) of

    [] -> #user{loggedin = false};

    UserCookie ->
        case yaws_api:cookieval_to_opaque(UserCookie) of
        {ok, Cookie}       -> Cookie;
        {error,no_session} -> #user{loggedin = false}
        end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_reference("/") -> page;
parse_reference(Cell) ->
    case string:chr(Cell,$:) of
    0 ->
        case hn_util:is_numeric(Cell) of
        true  -> row;
        false ->
            case hn_util:is_alpha(Cell) of
            true  -> column;
            false -> 
                case regexp:match(Cell,?RG_cell) of
                {match,_,_} -> cell;
                nomatch     -> noref
                end
            end
        end;
    _ ->
        range
    end.

get_format([{format,"json"}|_],_)-> json;
get_format([{format,"xml"}|_],_)-> xml;
get_format([_N|Tail],Def)-> get_format(Tail,Def);
get_format([],Def)-> Def.

format_vars([]) -> {{xml},[]};
format_vars(Query) ->

    %% list of valid vars
    Valid = ["links","format","lastrow","nocallback","loggedin","info","attr",
         "toolbar","last","hypernumber","pages","login","admin","import"],

    F = fun({K,V}) ->
        case lists:member(K,Valid) of
        true ->
            case V of
            undefined -> {list_to_atom(K)};
            _         -> {list_to_atom(K),V}
            end;
        _   -> {invalid}
        end
    end,

    Split = fun(X) -> 
        case string:chr(X,$=) of
        0 -> {X,undefined};
        _ -> [Key,Val] = string:tokens(X,"="),
             {Key,Val}
        end
    end,
    
    Vars = lists:map(F,lists:map(Split,string:tokens(Query,"&"))),

    Format = case lists:member(nocallback,Vars) of
         true -> {get_format(Vars,xml),nocallback};
         _    -> {get_format(Vars,xml)}
         end,

    %% Remove the invalid posts and format
    Tmp2 = lists:filter(fun(X) ->
        case X of 
        {invalid}    -> false;
        {nocallback} -> false;
        {format,_}   -> false;
        _ -> true end end,Vars),
    {Format,lists:keysort(1,Tmp2)}.


create_sconf({group,[{port,Port}],Groups}) ->
    lists:map(
        fun(X) -> 
            sconf_servers(X,list_to_integer(Port)) 
        end,
        xmlsearch([],Groups,server)).

sconf_servers({server,[{name,Name}],Vals},Port) ->
    sconf_vals(#sconf{port=Port,servername=Name},Vals).

sconf_vals(C,[]) -> C;
sconf_vals(C,[{auth,[], [{dir,[],Dir},{realm,[],Realm},{users,[],Auth}]}|T]) ->
    {ok,Users} = file:consult(production_boot:root()++Auth),
    sconf_vals(C#sconf{authdirs=lists:append(C#sconf.authdirs,[{Dir,{auth,[Dir],Realm,"Basic",Users,[],false}}])},T);
sconf_vals(C,[{port,[],V}|T]) ->
    sconf_vals(C#sconf{port = list_to_integer(V)},T);
sconf_vals(C,[{docroot,[],[{absdir,[],Dir}]}|T]) ->
    sconf_vals(C#sconf{docroot = Dir},T);
sconf_vals(C,[{docroot,[],[{reldir,[],Dir}]}|T]) ->
    sconf_vals(C#sconf{docroot = production_boot:root()++Dir},T);
sconf_vals(Conf,[{appmods,[],V}|T]) ->
    List = lists:map(fun({mod,[{path,Path},{module,Mod}],[]}) ->
        {Path,list_to_atom(Mod)} end,V),
    sconf_vals(Conf#sconf{appmods = List},T);
sconf_vals(Conf,[{listen,[],Val}|T]) ->
    [N1,N2,N3,N4] = lists:map(fun(X) ->
        list_to_integer(X) end, string:tokens(Val,".")),
    sconf_vals(Conf#sconf{listen = {N1,N2,N3,N4}},T).

%% ew, TODO: CLEANUP
gconf_vals(C,[]) -> C;
gconf_vals(C,[{log,[],[{absdir,[],Dir}]}|T]) ->      gconf_vals(C#gconf{logdir = Dir},T);
gconf_vals(C,[{log,[],[{reldir,[],Dir}]}|T]) ->      gconf_vals(C#gconf{logdir = production_boot:root()++Dir},T);
gconf_vals(C,[{yaws_dir,[],[{absdir,[],Dir}]}|T]) -> gconf_vals(C#gconf{yaws_dir = Dir},T);
gconf_vals(C,[{yaws_dir,[],[{reldir,[],Dir}]}|T]) -> gconf_vals(C#gconf{yaws_dir = production_boot:root()++Dir},T);
gconf_vals(C,[{yaws,[],V}|T]) ->                     gconf_vals(C#gconf{yaws = V},T);
gconf_vals(C,[{phpexe,[],V}|T]) ->                   gconf_vals(C#gconf{phpexe = V},T);
gconf_vals(C,[{trace,[],V}|T]) ->                    gconf_vals(C#gconf{trace = list_to_atom(V)},T);
gconf_vals(C,[{ebin,[],V}|T]) ->                     gconf_vals(C#gconf{ebin_dir = values([],V)},T);
gconf_vals(C,[{include,[],V}|T]) ->                  gconf_vals(C#gconf{include_dir = values([],V)},T);
gconf_vals(C,[{servers,[],_V}|T]) ->                 gconf_vals(C,T).

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
