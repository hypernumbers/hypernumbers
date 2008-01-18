%%%-----------------------------------------------------------------------------
%%% File        : hypernumbers.erl
%%% Author      : Dale Harvey <dale@arandomurl.com>
%%% Description : The Corporate Website
%%%
%%% Created     : 16 Oct 2007 by Dale Harvey <dale@arandomurl.com>
%%%-----------------------------------------------------------------------------
-module(hypernumbers).

-export([out/1]).
-import(xmerl_xs, [xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

-include("spriki.hrl").
-include("yaws_api.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This section contains the handler for the input from Yaws                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

out(Arg) ->

    %% System Globals
    R = Arg#arg.req,
    Root = Arg#arg.docroot,

    %% Global Variables
    {_,EncPath} = R#http_request.path,
    DecodedPath = yaws_api:url_decode(EncPath),
    Path=string:tokens(yaws_api:url_decode(EncPath),"/"),

    case filelib:is_file(Root++DecodedPath) and
            not filelib:is_dir(Root++DecodedPath) of

        %% Call to an existing file, css / images etc
        true  -> {page,DecodedPath};
        
        %% Page on the site
        false ->
           
            %% Get the content
            Ret = case Path of
                
                %% Home Page
                [] ->
                    {ok,Beta} = handle_post(beta,Arg),
    
                    {news,[],PostList} = hn_util:readxml_file(Root++"/news.xml"),

                    News = lists:map(
                        fun({post,[],[{title,[],Title},{date,[],Date},
                            {user,[],_User},{text,[],Text}]}) ->
                        T = hn_util:implode(string:tokens(Title," "),"-"),
                        "<div class='post'><h2><a href='/news/"++T++"'>"++Title++"</a></h2>"
                        ++hn_util:xml_to_string(Text)++"<a href='/news/"++T++"'> continue "
                        ++"reading ...</a><span class='date'>"++Date++"</span></div>"
                        end,PostList),

                    {ok,Home} = hn_util:read(Root++"/tpl/home.tpl"),
                    NewsRtn = hn_util:str_replace(Home,
                        [{"BETA",Beta},{"NEWS",News}]),

                    [{"CLASS","news"},{"CONTENT",NewsRtn}];

                %% Specific news Item
                ["news",Title] ->
                    {ok,Beta} = handle_post(beta,Arg),

                    MTitle = hn_util:implode(string:tokens(Title,"-")," "),
                    {ok,Home} = hn_util:read(Root++"/tpl/home.tpl"),

                    {news,[],Posts} = hn_util:readxml_file(Root++"/news.xml"),
                    News = hn_util:str_replace(Home,[{"BETA",Beta},
                        {"NEWS",get_news(MTitle,Posts)}]),

                    [{"CLASS","news"},{"CONTENT",News}];

                ["contact"] ->
                    {ok,Cont} = handle_post(mail,Arg),
                    {ok,Contact} = hn_util:read(Root++"/tpl/contact.tpl"),
                    [{"CLASS","contact"},{"CONTENT",Contact},{"CONTACT",Cont}];

                %% This handles any other url, it glues the path together with _
                %% and looks for a related tpl file in the template folder
                %% else 404
                Else ->
                    F = hn_util:implode(Else,"_")++".tpl",
                    case filelib:is_file(Root++"/tpl/"++F) of
                    true -> 
                        {ok,Tpl} = hn_util:read(Root++"/tpl/"++F),
                        [{"CLASS",F},{"CONTENT",Tpl}];
                    false ->
                        {ok,FourohFour} = hn_util:read(Root++"/tpl/404.tpl"),
                        [{"CONTENT",FourohFour},{"CLASS","404"}]
                    end
            end,

            %% Master Templace
            {ok,Index} = hn_util:read(Root++"/tpl/index.tpl"),
            {html,hn_util:str_replace(Index,Ret)}
    end.

get_news(Title,[H|T]) ->
    case H of
    {post,[],[{title,[],Title},{date,[],Date},{user,[],User},{text,[],Text}]} ->
        "<div class='mainpost'><h2>"++Title++"</h2><span class='small'>Posted on: "++Date
        ++"</span><br /><span class='small'>by <i>"++User++"</i></span><p>"
        ++hn_util:xml_to_string(Text)++"</p></div>";
    _ -> get_news(Title,T)
    end.

handle_post(mail,Arg) ->
    case (Arg#arg.req)#http_request.method of
        'POST' ->
            Email = case yaws_api:postvar(Arg,"email") of
                {ok,E} -> yaws_api:url_encode(E); _ -> undefined end,
            Name = case yaws_api:postvar(Arg,"name") of
                {ok,N} -> yaws_api:url_encode(N); _ -> undefined end,
            Msg = case yaws_api:postvar(Arg,"msg") of
                {ok,M} -> yaws_api:url_encode(M); _ -> undefined end,

            case {io_lib:char_list(Email),io_lib:char_list(Name),
                    io_lib:char_list(Msg)} of

                {true,true,true} ->

                    hn_util:post(?HN_URL1++"/contact/1?last",
                        lists:flatten(["action=create&value=",Name,"\n",Email,"\n",Msg])),

                    {ok,"Thanks, We will try to "
                         ++"reply as soon as possible"};
                _ ->
                    {ok,"There was an error sending "
                         ++"your mail"}
            end;
        _  ->
            {ok,[]}
    end;

handle_post(beta,Arg) ->
    case (Arg#arg.req)#http_request.method of
        'POST' ->
            Email = case yaws_api:postvar(Arg,"email") of
                {ok,E} -> E; _ -> undefined end,
            Name = case yaws_api:postvar(Arg,"name") of
                {ok,N} -> N; _ -> undefined end,

            case {Email,Name} of
                {A,B} when A /= undefined, B /= undefined ->
                    hn_util:post(?HN_URL1++"/alpha/1?last",
                        lists:flatten(["action=create&value=",A,"\n",B])),

                    {ok,"<span class='notice'>Thank You for your interest, "
                        ++"we will contact you as soon as possible</span>"};
                _ ->
                    {ok,"<span class='notice'>You must enter all details</span>"}
            end;
        _  ->
            {ok,[]}
    end.