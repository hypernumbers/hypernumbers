%%% @copyright Hypernumbers Ltd
%%% @doc Utilities for hypernumbers application

-module(hn_util).

-include("spriki.hrl").
-include("regexp.hrl").
-include("hypernumbers.hrl").
-include("muin_records.hrl").

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         parse_zpath/1,
         valid_email/1,
         extract_name_from_email/1,
         capitalize_name/1,

         transform_site/1,
         transform_perms/1,
         add_views/0,

         esc_regex/1,     
         recursive_copy/2,

         site_to_atom/2,
         site_to_fs/1,
         site_from_fs/1,

         % HyperNumbers Utils
         delete_directory/1,
         compile_html/2,
         delete_gen_html/0,
         generate_po/1,
         %generate_po_CHEATING/1,
         jsonify_attrs/1,
         jsonify_val/1,
         is_older/2,

         strip80/1,
         refX_to_url/1,
         index_to_url/1,
         obj_to_change_msg/1,
         in_range/2,
         range_to_list/1,
         rectify_range/4,
         rectify_row_or_col/2,
         path_to_json_path/1,

         % HTTP Utils
         %parse_url/1,
         parse_ref/1,
         parse_attr/1,
         parse_attr/2,

         % List Utils
         is_alpha/1,
         is_numeric/1,
         text/1,
         list_to_path/1,

         % Just some record conversion utilities
         refX_from_index/1,
         url_to_refX/1,

         % general utilities
         get_offset/3,
         js_to_utf8/1,

         % Path Utilities
         abs_path/2,
         just_path/1,
         drop_last/1,

         % Formula utilities
         make_formula/2
        ]).

%%% turns a path into a z-path
parse_zpath(List) -> parse_z(List, []).

parse_z([], Acc) -> lists:reverse(Acc);
parse_z([H | T], Acc) ->
    {ok, Re} = re:compile("(^\\[.+\\]$)"), %" for emacs syntact highlighting
    case re:run(H, Re) of
        {match, _} -> parse_z(T, [{zseg, H} | Acc]);
        nomatch    -> parse_z(T, [ {seg, H} | Acc])
    end.

%% make formula creates a new formula, but also returns a status.
%% Status can be [clean | dirty]
%% Formulae that return dirty should be marked dirty at recalc
%% time as they will not recalc to the real value
%% The function 'INDIRECT' is an example of such a function
make_formula(Status, Toks) ->
    mk_f(Toks, {Status, []}).

%% this function needs to be extended...
mk_f([], {St, A}) ->
    {St, "="++lists:flatten(lists:reverse(A))};

mk_f([{errval, _, '#REF!'} | T], {St, A}) -> 
    mk_f(T, {St, ["#REF!" | A]});

mk_f([{deref, Text} | T], {_St, A}) ->
    mk_f(T, {dirty, [Text | A]});

%% special infering of division
mk_f([{cellref, _, C1}, {cellref, _, C2} | T], {St, A}) ->
    mk_f(T, {St, [C2#cellref.text, "/", C1#cellref.text | A]});

mk_f([{int, _, I}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", integer_to_list(I) | A]});

mk_f([{float, _, {F, _}}, {cellref,_,C} | T], {St, A}) -> 
    mk_f(T, {St, [C#cellref.text, "/", float_to_list(F) | A]});

mk_f([{')',_}, {cellref,_,C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text, "/", ")" | A]});

%% order matters - now detecting 'root' cells
mk_f([{cellref, _, #cellref{path="/", text=Text}} | T], {St, A}) ->
    mk_f(T, {St, ["/" ++ Text | A]});

mk_f([{cellref, _, C} | T], {St, A}) ->
    mk_f(T, {St, [C#cellref.text | A]});

mk_f([{rangeref, _, R} | T], {St, A}) ->
    mk_f(T, {St, [R#rangeref.text | A]});

mk_f([{namedexpr, _, N} | T], {St, A}) ->
    mk_f(T, {St, [N#namedexpr.path ++ N#namedexpr.text | A]});

mk_f([{bool, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{atom, _, H} | T], {St, A}) ->
    mk_f(T, {St, [atom_to_list(H) | A]});

mk_f([{int, _, I} | T], {St, A}) ->
    mk_f(T, {St, [integer_to_list(I) | A]});

mk_f([{float, _, {F, _OrigStr}} | T], {St, A}) ->
    mk_f(T, {St, [float_to_list(F) | A]});

mk_f([{formula, _, S} | T], {St, A}) ->
    mk_f(T, {St, [S | A]});

mk_f([{str, _, S} | T], {St, A}) ->
    mk_f(T, {St, [$", S, $" | A]});

mk_f([{recalc, S} | T], {_St, A}) ->
                         mk_f(T, {dirty, [S | A]});

                  mk_f([{name, _, "INDIRECT"} | T], {_St, A}) ->
                         mk_f(T, {dirty, ["INDIRECT" | A]});

                  mk_f([{name, _, S} | T], {St, A}) ->
                         mk_f(T, {St, [S | A]});

                  mk_f([{H, _} | T], {St, A}) ->
                         mk_f(T, {St, [atom_to_list(H) | A]}).


-spec extract_name_from_email(string()) -> string(). 
extract_name_from_email(Email) ->
    [Name | _Rest] =  string:tokens(Email, ".+@"),
    capitalize_name(Name).

-spec capitalize_name(string()) -> string(). 
capitalize_name([X|Rest]) -> [string:to_upper(X)|Rest].

-spec valid_email(string()) -> boolean(). 
valid_email(Email) ->
    NewE = string:strip(Email),
    EMail_regex = "^(?<name>([a-z0-9!#$%&'*+/=?^_`{|}~\\-]+)"
        ++ "(\\.[a-z0-9!#\$%&'*+/=?^_`{|}~\\-]+)*)"
        ++ "(@)(?<dom>([a-z0-9](([a-z0-9\\-\\.]*[a-z0-9]+)*))+)"
        ++ "(?<tld>(\\.[a-zA-Z]{2})|\\.com|\\.org|\\.net|\\.gov|\\.mil"
        ++ "|\\.biz|\\.info|\\.mobi|\\.name|\\.aero|\\.jobs|\\.museum|\\.edu|"
        ++"\\.asia|\\.cat|\\.coop|\\.int|\\.jobs|\\.pro|\\.arpa|\\.tel|\\.travel)$",
        %" for syntax highighting
    % io:format("Name is ~p~nDom is ~p~nnTld is ~p~n",
    %          [re:run(Email, EMail_regex, [{capture, [name]}]),
    %           re:run(Email, EMail_regex, [{capture, [dom]}]),
    %           re:run(Email, EMail_regex, [{capture, [tld]}])
    %         ]),
    case re:run(NewE, EMail_regex) of
        nomatch    -> false;
        {match, _} -> true
    end.

%% used to turn sites into atoms for process names, etc
-spec site_to_atom(list(), list()) -> atom().
site_to_atom(Site, PostFix) when is_list(Site) andalso is_list(PostFix) ->
    "http://" ++ S = Site,
    list_to_atom([case X of $: -> $&; X -> X end || X <- S ++ PostFix]).

site_to_fs("http://"++Site) ->
    [case S of $: -> $&; S  -> S end 
     || S <- Site].

site_from_fs(Site) ->
    "http://" ++ [case S of $& -> $:; S  -> S end 
                  || S <- Site].

%% Escape the replacement part of the regular expression
%% so no spurious replacements
-spec esc_regex(list() | binary()) -> list().
esc_regex(List) when is_binary(List) ->
    esc_regex(binary_to_list(List));
esc_regex(List) ->
    esc_regex(List, []).

esc_regex([], Acc) ->
    lists:flatten(lists:reverse(Acc));

esc_regex([$&   | Rest], Acc) ->
    esc_regex(Rest, ["\\&" | Acc]);
esc_regex([Else | Rest], Acc) ->
    esc_regex(Rest, [Else | Acc]).


%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.                            
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy1(From, To, X) || X <- Files],
    ok.

% ignore hidden
rec_copy1(_From, _To, [$. | _T]) ->
    ok; 
rec_copy1(From, To, File) ->

    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),

    case filelib:is_dir(NewFrom) of

        true  ->
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);

        false ->
            case filelib:is_file(NewFrom) of                
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok            
            end
    end.


%% Delete a directory (and all its children)
-spec delete_directory(string()) -> ok.
delete_directory(From) ->
    case file:list_dir(From) of
        {ok, Files} -> 
            file:list_dir(From),
            [ok = delete_dir(filename:join(From, File)) || File <- Files],
            ok = file:del_dir(From);
        _Else ->
            ok
    end.

delete_dir(File) ->
    case filelib:is_dir(File) of
        true  -> delete_directory(File);
        false -> file:delete(File)
    end.


%% is string a list of alpha(a-z) characters
-spec is_alpha(string()) -> true | false.
is_alpha(Str) ->
    Fun = fun(XX) ->         
                  if XX < 97  -> false;  
                     XX > 122 -> false;
                     true     -> true      
                  end                  
          end,
    case is_list(Str) of
        false -> false;
        true  -> lists:all(Fun, Str)
    end.


%% is string a list of digits
-spec is_numeric(string()) -> true | false.
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
        true  -> lists:all(Fun, Str)
    end.

get_offset(insert, D, {cell,     _})              -> g_o1(D, 1, 1);
get_offset(insert, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, Y2 - Y1 + 1); 
get_offset(insert, D, {column, {X1, X2}})         -> g_o1(D, X2 - X1 + 1, 0); 
get_offset(insert, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, X2 - X1 + 1,
                                                          Y2 - Y1 + 1);
get_offset(delete, D, {cell,    _})               -> g_o1(D, -1, -1);
get_offset(delete, D, {row,    {Y1, Y2}})         -> g_o1(D, 0, -(Y2 - Y1 + 1)); 
get_offset(delete, D, {column, {X1, X2}})         -> g_o1(D, -(X2 - X1 + 1), 0); 
get_offset(delete, D, {range,  {X1, Y1, X2, Y2}}) -> g_o1(D, -(X2 - X1 + 1), -
                                                          (Y2 - Y1 + 1)).

g_o1(vertical, _X, Y)   -> {0, Y};
g_o1(horizontal, X, _Y) -> {X, 0}.


compile_html(Html, Lang) ->
    {ok, Bin} = file:read_file(code:lib_dir(hypernumbers)++"/po/"++Lang++".po"),
    gettext:store_pofile(Lang, Bin),
    {ok, C} = sgte:compile_file(Html),
    sgte:render(C, [{options, [{gettext_lc, Lang}]}]).

is_older(File1, File2) ->
    {ok, Info1} = file:read_file_info(File1),
    {ok, Info2} = file:read_file_info(File2),
    Info2#file_info.mtime > Info1#file_info.mtime.

%% generate_po_CHEATING(Ref) ->
%%     Needs a user object.... 
%%     Body = hn_mochi:page_attributes(make_refX(Ref)),
%%     generate_po1(Body).

generate_po(Url) ->
    delete_gen_html(),
    {ok,{{_V,_Status,_R},_H,Body}} = httpc:request(get,{Url++"?attr",[]},[],[]),
    generate_po1(Body).

generate_po1(Body) ->
    {struct, Json} = mochijson2:decode(Body),
    {struct, Cells} = pget(<<"cell">>, Json),
    {struct, Pos} = pget(<<"2">>, Cells),
    Files = lists:map(fun po_files/1, Pos),
    lists:map(fun(X) -> po_row(Files, X) end, Cells),
    lists:map(fun({_I, F}) -> file:close(F) end, Files),
    ok.

po_files({Index, {struct, List}}) ->
    Lang = binary_to_list(pget(<<"value">>, List)),
    Path = code:lib_dir(hypernumbers)++"/po/"++Lang++".po",
    {ok, File} = file:open(Path, [write]),
    {Index, File}.

po_row(_File, {<<"1">>, _Children}) -> ok;
po_row(_File, {<<"2">>, _Children}) -> ok;
po_row(File, {_Row, {struct, Children}}) ->
    {struct, Attr} = pget(<<"1">>, Children),
    Id = pget(<<"value">>, Attr),
    lists:map(fun(X) -> po_val(File, Id, X) end, Children),
    ok.

po_val(Files, Id, {Col, {struct, Cell}}) ->
    Str = "msgid \"~s\"\nmsgstr \"~s\"\n\n",
    io:format(pget(Col, Files), Str, [Id, pget(<<"value">>, Cell)]).

delete_gen_html() ->
    Dir = code:lib_dir(hypernumbers)++"/priv/docroot/hypernumbers/",
    [file:delete(X) || X <- filelib:wildcard(Dir++"*.html.*")].


jsonify_attrs(Attrs) ->
    [jsonify_val(A) || A <- Attrs].

jsonify_val({[$_,$_|_]=K, _}) ->
    {K, "bleh"};
jsonify_val({"parents", _}) ->
    {"parents", "bleh"};    
jsonify_val({Name, {errval, Error}}) ->
    {Name, atom_to_list(Error)};
jsonify_val({Name, {datetime, {1,1,1}=Date, Time}}) ->
    {Name, dh_date:format("g:i A", {Date, Time})};
jsonify_val({Name, {datetime, Date, Time}}) ->
    {Name, muin_date:to_rfc1123_string({datetime, Date, Time})};
jsonify_val({"value", true}) ->
    {"value", "true"};
jsonify_val({"value", false}) ->
    {"value", "false"};
jsonify_val({"preview", {Text, Height, Width}}) ->
    {"preview", {struct, [{"txt", Text}, {"h", Height}, {"w", Width}]}};
%% TODO: fix names
jsonify_val({Name, {namedexpr, _Path, Nm}}) ->
    {Name, Nm};
jsonify_val({K, V}) ->
    try (mochijson:encoder([{input_encoding, utf8}]))(V),
        {K, V}
    catch
        error: _Err ->
            error_logger:error_msg("#MOCHIJSON! error ~p ~p~n", [K, V]),
            {K, {errval, '#MOCHIJSON!'}};
        exit: _Err ->
            error_logger:error_msg("#MOCHIJSON! exit ~p ~p~n", [K, V]),
            {K, {errval, '#MOCHIJSON!'}};
        throw: _Err ->
            error_logger:error_msg("#MOCHIJSON! throw ~p ~p~n", [K, V]),
            {K, {errval, '#MOCHIJSON!'}}
    end.                

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions convert to and from #refX and  #index records and Urls   %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
url_to_refX(Url) -> url_parser:make_refX(Url).

refX_from_index(#index{site = S, path = P, column = X, row = Y}) ->
    #refX{site = S, path = P, obj = {cell, {X, Y}}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% HyperNumbers Utils                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rectify_range(X1, Y1, X2, Y2) ->
    % in case the range is passed in arsey-backwards
    {X1a, X2a} = rectify_row_or_col(X1, X2),
    {Y1a, Y2a} = rectify_row_or_col(Y1, Y2),
    {X1a, Y1a, X2a, Y2a}.

rectify_row_or_col(Z1, Z2) ->
    if
        (Z1 >  Z2) -> {Z2, Z1};
        (Z1 =< Z2) -> {Z1, Z2}
    end.    

range_to_list(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    {X1a, Y1a, X2a, Y2a} = rectify_range(X1, Y1, X2, Y2),
    range_to_list1(RefX, X1a, X1a, Y1a, X2a, Y2a, []).

range_to_list1(RefX, _Reset, X, Y, X, Y, Acc) ->
    [RefX#refX{obj = {cell, {X, Y}}} | Acc];
range_to_list1(RefX, Reset, X2, Y1, X2, Y2, Acc) ->
    range_to_list1(RefX, Reset, Reset, Y1 + 1, X2, Y2,
                   [RefX#refX{obj = {cell, {X2, Y1}}} | Acc]);
range_to_list1(RefX, Reset, X1, Y1, X2, Y2, Acc) ->
    range_to_list1(RefX, Reset, X1 + 1, Y1, X2, Y2,
                   [RefX#refX{obj = {cell, {X1, Y1}}} | Acc]).

refX_to_url(#refX{site = Site, path = Path, obj = {cell, {X, Y}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X), text(Y)]);
refX_to_url(#refX{site = Site, path = Path, obj = {column, {X1, zero, X2, inf}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X1), ":",
                  tconv:to_b26(X2)]);
refX_to_url(#refX{site = Site, path = Path, obj = {row, {zero, Y1, inf, Y2}}}) ->
    lists:append([Site, list_to_path(Path), text(Y1), text(Y2)]);
refX_to_url(#refX{site = Site, path = Path, obj = {range, {X1, Y1, X2, Y2}}}) ->
    lists:append([Site, list_to_path(Path), tconv:to_b26(X1), text(Y1), ":",
                  tconv:to_b26(X2), text(Y2)]);
refX_to_url(#refX{site = Site, path = Path, obj = {page, "/"}}) ->
    lists:append([Site, list_to_path(Path)]).

index_to_url(#index{site=Site,path=Path,column=X,row=Y}) ->
    lists:append([Site, list_to_path(Path),tconv:to_b26(X), text(Y)]).

-spec strip80(string()) -> string(). 
strip80(S) -> strip80(S, []). 
strip80([], Acc) -> lists:reverse(Acc);
strip80(":80"++_, Acc) -> lists:reverse(Acc);
strip80([H|T], Acc) -> strip80(T, [H|Acc]).

list_to_path([])   -> "/";
list_to_path(Path) -> "/" ++ string:join(Path, "/") ++ "/".

path_to_json_path([])                -> "path.json";
path_to_json_path(P) when is_list(P) -> "path." ++ string:join(P, ".")
                                            ++ ".json".

obj_to_change_msg({page,Path}) ->
    Path;   
obj_to_change_msg({cell,{X,Y}}) ->
    tconv:to_b26(X)++text(Y);
% file import seems to create old-fashioned rows
obj_to_change_msg({row, {Y, Y}}) ->
    text(Y); % change msgs only get a singleton ref
obj_to_change_msg({row, {range, {zero, Y, inf, Y}}}) ->
    text(Y); % change msgs only get a singleton ref
obj_to_change_msg({column, {range, {X, zero, X, inf}}}) ->
    tconv:to_b26(X); % change msgs only get a singleton ref
% file import seems to create old-fashioned columns
obj_to_change_msg({column, {X, X}}) ->
    tconv:to_b26(X); % change msgs only get a singleton ref
obj_to_change_msg({range,{X1,Y1,X2,Y2}}) ->
    tconv:to_b26(X1)++text(Y1)++":"++tconv:to_b26(X2)++text(Y2).

in_range({range,{X1,Y1,X2,Y2}}, {cell,{X,Y}}) ->
    Y >= Y1 andalso Y =< Y2 andalso X >= X1 andalso X =< X2.

%% parse_url("http://"++Url) ->
%%     {Host, Path, NUrl} = prs(Url),
%%     Type = has_conds(Path),
%%     case lists:last(NUrl) of
%%         $/ -> #refX{site="http://"++Host, type = Type,
%%                     path=Path, obj={page, "/"}};
%%         _  -> [Addr | P] = lists:reverse(Path),
%%               Obj = parse_attr(cell, Addr),
%%               Type2 = case {Obj, Type} of
%%                           {{row, _},    false} -> gurl;
%%                           {{column, _}, false} -> gurl;
%%                           {_,           false} -> url;
%%                           {_,           true}  -> gurl
%%                       end,
%%               #refX{site="http://"++Host, type = Type2,
%%                     path=lists:reverse(P),
%%                     obj = Obj}
%%     end.

%% %% needs to be fixed for validating page paths
%% has_conds([]) -> false;
%% has_conds(Path) ->
%%     Re = "^[a-zA-Z0-9_\-~]$", %",
%%     Fun = fun(X, Acc) ->
%%                   NewAcc = case re:run(X, Re) of
%%                                {match, _} -> true;
%%                                nomatch    -> false
%%                            end,
%%                   case {Acc, NewAcc} of
%%                       {true, true} -> true;
%%                       _            -> false
%%                   end
%%           end,
%%     lists:foldl(Fun, true, Path).

%% prs(Url) ->
%%     case string:tokens(Url, "/") of
%%         [Host]        -> {Host, [], "/"};
%%         [Host | Path] -> {Host, Path, Url}
%%     end.

parse_attr(Addr) ->
    parse_attr(cell, Addr).

parse_attr(cell, Addr) ->
    case re:run(Addr,?RG_cell) of
        {match,_} -> {cell, util2:strip_ref(Addr)};
        _         -> parse_attr(range, Addr)
    end;

parse_attr(range, Addr) ->
    case re:run(Addr,?RG_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {X1, Y1} = util2:strip_ref(Cell1),
            {X2, Y2} = util2:strip_ref(Cell2),
            {XX1, YY1, XX2, YY2} = hn_util:rectify_range(X1, Y1, X2, Y2),
            {range, {XX1, YY1, XX2, YY2}};
        _ -> 
            parse_attr(col, Addr)
    end;

parse_attr(col, Addr) ->
    case re:run(Addr,?RG_col_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {col, {tconv:b26_to_i(Cell1), tconv:b26_to_i(Cell2)}};
        _ -> 
            parse_attr(row, Addr)
    end;

parse_attr(row, Addr) ->
    case re:run(Addr,?RG_row_range) of
        {match,_} -> 
            [Cell1, Cell2] = string:tokens(Addr, ":"),
            {row, {ltoi(Cell1), ltoi(Cell2)}};
        _ ->
            % this is where references will be typed as names
            % (when we get them in)
            {name, Addr}
            %throw(invalid_reference)
    end.

ltoi(X) ->
    list_to_integer(X).

parse_ref(Ref) ->
    RefType = type_reference(Ref),
    RefVal  = case RefType of
                  page     -> "/";
                  cell     -> util2:strip_ref(undollar(Ref));
                  range    -> util2:parse_range(undollar(Ref));
                  column   -> [First, Last] = string:tokens(Ref, ":"),
                              {tconv:to_i(undollar(First)),
                               tconv:to_i(undollar(Last))};
                  row      -> [First, Last] = string:tokens(Ref, ":"),
                              {tconv:to_i(undollar(First)),
                               tconv:to_i(undollar(Last))};
                  filename -> Ref
              end,
    {RefType, RefVal}.

undollar(A) ->
    re:replace(A, "\\$", "", [{return, list}, global]). %"

%%--------------------------------------------------------------------
%% Function:    text/1
%% Description: Returns a string representation of the parameter
%%--------------------------------------------------------------------
               text(X) when is_integer(X) -> integer_to_list(X);
               text(X) when is_float(X)   -> float_to_list(X);
               text(X) when is_list(X)    -> lists:flatten(X);
               text({errval, Errval})     -> atom_to_list(Errval);
               text(X) when is_boolean(X) -> atom_to_list(X);
               text(Dt) when is_record(Dt, datetime) -> muin_date:to_rfc1123_string(Dt);
               text(_X) -> "". %% quick fix for the "plain" api

js_to_utf8({struct, Val}) -> {struct, lists:map(fun js_to_utf8/1, Val)};
js_to_utf8({array, Val})  -> {array,  lists:map(fun js_to_utf8/1, Val)};
js_to_utf8({Key, Val})    -> {xmerl_ucs:to_utf8(Key), js_to_utf8(Val)};
js_to_utf8(X) when is_integer(X); is_float(X); is_atom(X) -> X;
js_to_utf8(X)             -> xmerl_ucs:to_utf8(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Path Utilities                                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec just_path(string()) -> string().
just_path([]) -> [];
just_path(Url) ->
    case lists:last(Url) of
        $/ -> Url;
        _Or ->
            Lead = if hd(Url) == $/ -> "/";
                      true -> [] end,
            Lead ++ case string:tokens(Url, "/") of
                        [] -> [];
                        [_S] -> [];
                        L -> string:join(drop_last(L), "/") ++ "/"
                    end
    end.

-spec drop_last(list()) -> list().
drop_last([_]) -> []; 
drop_last([X | Rest]) -> [X | drop_last(Rest)].

-spec abs_path([string()], string()) -> string().
abs_path(_, [$/ | _] = Path2) -> Path2;
abs_path(Path, Path2) -> 
    Path2Toks = string:tokens(Path2, "/"),
    PathR = lists:reverse(Path),
    "/" ++ string:join(abs_path2(PathR, Path2Toks), "/").

abs_path2(P1, P2) ->
    abs_path2(P1, P2, queue:new()).
abs_path2([], [], Q) ->
    queue:to_list(Q);
abs_path2(Left, ["." | RestR], Q) ->
    abs_path2(Left, RestR, Q);
abs_path2(Left, [".." | RestR], Q) ->
    case {queue:is_empty(Q), Left} of
        {false, _} -> abs_path2(Left, RestR, queue:drop_r(Q));
        {true, []} -> abs_path2([".."], RestR, Q); 
        {true, [".."|_]} -> abs_path2([".." | Left], RestR, Q);
        {true, [_|RestL]} -> abs_path2(RestL, RestR, Q)
    end;
abs_path2([], [X | RestR], Q) ->
    abs_path2([], RestR, queue:in(X, Q));
abs_path2([".." | RestL], [_ | RestR], Q) ->
    abs_path2(RestL, RestR, Q);
abs_path2([".." | _], [], _Q) ->
    "";
abs_path2([X | RestL], Right, Q) ->
    abs_path2(RestL, Right, queue:in_r(X, Q)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Internal Functions                                                       %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_reference("/") -> page;
type_reference(Cell) ->
    case string:chr(Cell, $:) of
        0 ->
            case re:run(Cell, ?RG_cell) of
                {match, _} -> cell;
                _          -> filename
                    
            end;
        _ -> type_r2(Cell)
    end.

type_r2(Range) ->
    case re:run(Range, ?RG_range) of
        {match, _} -> range;
        _          -> type_r3(Range)
    end.

type_r3(Col) ->
    case re:run(Col, ?RG_col_range) of
        {match, _} -> column;
        _          -> type_r4(Col)
    end.

type_r4(Row) ->
    case re:run(Row, ?RG_row_range) of
        {match, _} -> row;
        _          -> filename
    end.

pget(Key, List) ->
    proplists:get_value(Key, List, undefined).

transform_site(Dest) ->
    MigrateDir = code:lib_dir(hypernumbers) ++ "/../../"++Dest++"/",
    Sites      = filelib:wildcard(MigrateDir ++ "*"),
    [ transform_perms(Site) || Site <- Sites ],
    ok.

transform_perms(Path) ->

    file:copy(Path ++ "/permissions.export",
              Path ++ "/permissions.export.bak"),

    {ok, Terms} = file:consult(Path ++ "/permissions.export"),

    NTerms = [ tmptr(Term) || Term <- Terms ],
    Perms    = make_script_terms(NTerms,[]),

    ok = file:write_file(Path ++ "/permissions.export", Perms).

tmptr({add_view,[{path,Path}, {perms, Perms},
                 {view,"_g/core/spreadsheet"}]}) ->
    tmptr({add_view,[{path,Path}, {perms, Perms}, {view,"spreadsheet"}]});
tmptr({add_view,[{path,Path}, {perms, Perms}, {view,"_g/core/webpage"}]}) ->
    {add_view,[{path,Path}, {perms, Perms}, {view,"webpage"}]};
tmptr({set_champion,[{path,Path},{view,"_g/core/spreadsheet"}]}) ->
    {set_champion,[{path,Path},{view,"spreadsheet"}]};
tmptr({set_champion,[{path,Path},{view,"_g/core/webpage"}]}) ->
    {set_champion,[{path,Path},{view,"webpage"}]};
tmptr(Tmp) ->
    Tmp.

make_script_terms([], Acc) -> 
    FirstLine = io_lib:format("~s~n",["%%-*-erlang-*-"]),
    lists:flatten([FirstLine | lists:reverse(Acc)]);
make_script_terms([H | T], Acc) ->
    NewAcc = lists:flatten(io_lib:format("~p.~n", [H])),
    make_script_terms(T, [NewAcc | Acc]).

add_views() ->
    [ begin
          auth_srv:add_view(Site, [], ["admin"], "table"),
          auth_srv:add_view(Site, ["[**]"], ["admin"], "table"),
          auth_srv:add_view(Site, [], ["admin"], "webpage"),
          auth_srv:add_view(Site, ["[**]"], ["admin"], "webpage")
      end || Site <- hn_setup:get_sites()].


%%%
%%% Unit Tests
%%%

valid_email_test_() ->
    [
     ?_assertEqual(true, valid_email("a@b.cc")),
     ?_assertEqual(true, valid_email("a1@b.cc")),
     ?_assertEqual(true, valid_email("d_a@b.cc")),
     ?_assertEqual(true, valid_email("e+a@b.cc")),
     ?_assertEqual(true, valid_email("x.a@b.cc")),
     ?_assertEqual(true, valid_email("a@b.d.e.cc")),
     ?_assertEqual(true, valid_email("a@b.com")),
     ?_assertEqual(true, valid_email("a@b1.com")),
     ?_assertEqual(true, valid_email("a@b.mil")),
     ?_assertEqual(true, valid_email("a@b.aero")),
     ?_assertEqual(true, valid_email("a@b.co.uk")),
     ?_assertEqual(true, valid_email("aaaaa@bbbbb.cc")),
     ?_assertEqual(true, valid_email("aa11aa22@bbbbb.cc")),
     ?_assertEqual(true, valid_email("dddd_aa__dd__aa@bbbb.cc")),
     ?_assertEqual(true, valid_email("eeee+aa++fff@bbbbb.cc")),
     ?_assertEqual(true, valid_email("xx.aa.ddd@bbbb.cc")),
     ?_assertEqual(true, valid_email("aaaaa@bbbb.dddd.eee.cc")),
     ?_assertEqual(true, valid_email("aaaaa@bbbbbbb.com")),
     ?_assertEqual(true, valid_email("aaaaa@b111bbbbbb11.com")),
     ?_assertEqual(true, valid_email("aaaaa@b111-bb-bb--bb11.com")),
     ?_assertEqual(true, valid_email("aaaaa@bbbbb.mil")),
     ?_assertEqual(true, valid_email("aaaaa@bbbb.aero")),
     ?_assertEqual(true, valid_email("aaaa@bbbbb.co.uk")),
     ?_assertEqual(false, valid_email("a")),
     ?_assertEqual(false, valid_email("a@b")),
     ?_assertEqual(false, valid_email("aaaa")),
     ?_assertEqual(false, valid_email("aaaa@bbbbb")),
     ?_assertEqual(false, valid_email("a@b-.com")),
     ?_assertEqual(false, valid_email("aaaa@bb'bbb.com")),
     ?_assertEqual(false, valid_email("a@b.rhino"))
    ].

abspath_test_() ->
    Base = ["first", "second", "third"],
    [
     ?_assertEqual("/already/absolute", abs_path(Base, "/already/absolute")),
     ?_assertEqual("/first/second/third/d1", abs_path(Base, "d1")),
     ?_assertEqual("/first/second/third/d1", abs_path(Base, "./d1")),
     ?_assertEqual("/first/second/d1", abs_path(Base, "../d1")),
     ?_assertEqual("/d1", 
                   abs_path(Base, "../third/../../ok/.././../././first//../d1")),
     ?_assertEqual("/first/second/d1", abs_path(Base, "../something/../d1"))
    ].

just_path_test_() ->
    [
     ?_assertEqual("", just_path("")),
     ?_assertEqual("/", just_path("/")),
     ?_assertEqual("/", just_path("/blah")),
     ?_assertEqual("", just_path("..")),
     ?_assertEqual("../", just_path("../")),
     ?_assertEqual("../", just_path("../A1")),
     ?_assertEqual("./", just_path("./A1")),
     ?_assertEqual("", just_path("blah")),
     ?_assertEqual("/", just_path("/blah")),
     ?_assertEqual("/blah/", just_path("/blah/")),
     ?_assertEqual("/blah/", just_path("/blah/A:A")),
     ?_assertEqual("/blah/more/path/", just_path("/blah/more/path/A:A"))
    ].
