%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Functions to handle file uploads.

-module(hn_file_upload).
-compile(export_all).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% @doc Handles a file upload request from a user. Imports the file etc and
%% returns response data to be sent back to the client.

handle_upload(Req, User) ->
    Username  = hn_users:name(User),
    Filestamp = Username ++ "__" ++ dh_date:safe_now(),
    io:format("In handle_upload Filestamp is ~p~n", [Filestamp]),
    
    Callback = fun(N) ->
                       %% Passing filestamp (time & username) for file name in state record here.
                       file_upload_callback(N, #file_upload_state{filename = Filestamp})
               end,
    
    {_, _, State} = mochiweb_multipart:parse_multipart_request(Req, Callback),
    RawPath = Req:get(raw_path),
    Basename = filename:basename(State#file_upload_state.original_filename, ".xls"),
    {ok, Safename, _N} = regexp:gsub(Basename, "\\s+", "_"),
    ParentPage = RawPath ++ Safename ++ "/",
    {value, {'Host', Host}} = mochiweb_headers:lookup('Host', Req:get(headers)),

    try 
        import(State#file_upload_state.filename, Host, ParentPage),
        {struct, [{"location", ParentPage}]}
    catch
        Error:_Reason ->
            ?ERROR("Error Importing ~p~n",[Error]),
            {struct, [{"error", "error reading sheet"}]}
    end.
           
file_upload_callback({headers, Headers}, S) ->
    ContentDisposition = hd(Headers),
    NewState = case ContentDisposition of
                   {"content-disposition", {"form-data", [{"name", _}, {"filename", Filename}]}} ->
                       Filestamp = S#file_upload_state.filename,
                       FullFilename = filename:join([code:lib_dir(hypernumbers),
                                                     "..", "..",
                                                     "priv", "uploads",
                                                     Filestamp ++ "__" ++ string:to_lower(Filename)]),
                       #file_upload_state{original_filename = string:to_lower(Filename),
                                          filename = FullFilename};
                   _ ->
                       S
               end,
    fun(N) -> file_upload_callback(N, NewState) end;
file_upload_callback({body, Data}, S) ->
    if  S#file_upload_state.filename =/= undefined ->
            if S#file_upload_state.file =/= undefined ->
                    file:write(S#file_upload_state.file, Data),
                    NewState = S;
               true ->
                    case file:open(S#file_upload_state.filename, [raw, write]) of
                        {ok, File} ->
                            file:write(File, Data),
                            NewState = S#file_upload_state{file = File};
                        {error, Error} ->
                            ?ERROR("Could not open ~p for writing, error: ~p",
                                   [S#file_upload_state.filename, Error]),
                            NewState = S,
                            exit({error, Error})
                    end
            end;
        true ->
            NewState = S
    end,

    fun(N) -> file_upload_callback(N, NewState) end;
file_upload_callback(body_end, S) ->
    if S#file_upload_state.file =/= undefined -> file:close(S#file_upload_state.file);
       true -> ok
    end,
    
    fun(N) -> file_upload_callback(N, S) end;
file_upload_callback(_, State) ->
    State.

%%% interfacing with the reader
%%% TODO: some of this needs to be part of the reader

%% @doc Writes contents of an XLS file into Hypernumbers pages.
%% Filename is a full name, parent page is path to page under which pages for
%% individual sheets will be created.

import(Filename, Host, ParentPage) ->
    {Celldata, _Names, _Formats, CSS, Warnings} = readxls(Filename),
    Site = "http://" ++ Host,
    F = fun(X, {Ls, Fs}) ->
                {SheetName, Target, V} = read_reader_record(X),
                Sheet = excel_util:esc_tab_name(SheetName),
                Postdata = conv_for_post(V),
                Path = ParentPage++Sheet++"/",
                Ref = case Target of
                          {{Fr, Fc}, {Lr, Lc}} -> {rc_to_a1(Fr, Fc), rc_to_a1(Lr, Lc)};
                          {Row, Col}           -> rc_to_a1(Row, Col)
                      end,
                Datatpl = {Path, Ref, Postdata},
                case Postdata of
                    [$=|_] -> {Ls, [Datatpl|Fs]};
                    _      -> {[Datatpl|Ls], Fs}
                end               
        end,
    {Lits, Flas} = lists:foldl(F,{[], []}, Celldata),
        Dopost = fun({Path, Ref, Postdata}) when is_list(Ref) -> % single cell
                         Url = string:to_lower(Site ++ Path ++ Ref),
                         {ok, RefX} = hn_util:parse_url(Url),
                         ok = hn_db_api:write_attributes(RefX, [{"formula", Postdata}]);
                    ({Path, {Tl, Br}, Postdata}) -> % array formula
                         Url = string:to_lower(Site ++ Path ++ Tl ++ ":" ++ Br),
                         {ok, RefX} = hn_util:parse_url(Url),
                         hn_main:formula_to_range(RefX, Postdata)
                 end,

    lists:foreach(Dopost, Lits),
    lists:foreach(Dopost, Flas),

    io:format("going into writing css with CSS of ~p~n", [CSS]),
    %% Now fire in the CSS and formats
    WriteCSS = fun(X) ->
                       io:format("Got to 1~n"),
                       {{{sheet, SheetName}, {row_index, Row}, {col_index, Col}},
                        [CSSItem]} = X,
                       io:format("Got to 2~n"),
                       Sheet = excel_util:esc_tab_name(SheetName),
                       io:format("Got to 3~n"),
                       Path = ParentPage++Sheet++"/",
                       io:format("Got to 4~n"),
                       Ref = rc_to_a1(Row,Col),
                       io:format("Got to 5~n"),
                       Url = string:to_lower(Site ++ Path ++ Ref),
                       io:format("Got to 6~n"),
                       {ok, RefX} = hn_util:parse_url(Url),
                       io:format("Got to 7~n"),
                       #refX{path = P2} = RefX,
                       io:format("Got to 8~n"),
                       RefX2 = #refX{site = Site, path = P2, obj = {page, "/"}},
                       io:format("Got to 9~n"),
                       Return = hn_db_api:write_style_IMPORT(RefX2, CSSItem),
                       io:format("Return is ~p~n", [Return]),
               end,
    lists:foreach(WriteCSS, CSS),

    %% write parent page information
    PathComps = string:tokens(ParentPage, "/"),
    ok = write_warnings(Warnings, Site, PathComps),
    write_to_cell("Your data has been imported and is under sheet1/, sheet2/ etc.", 1, 1, Site, PathComps),
    write_to_cell("The list of warnings is in column F.", 1, 2, Site, PathComps),
    
    loop(),
    ok.

write_to_cell(Str, Col, Row, Site, Path) ->
    ok = hn_db_api:write_attributes(#refX{site = Site,
                                          path = Path,
                                          obj = {cell, {Col, Row}}},
                                    [{"formula", Str}]).
                                          

write_warnings(Warnings, Site, Path) ->
    write_warnings(Warnings, 1, Site, Path).
write_warnings([], _, _, _) ->
    ok;
write_warnings([W|Ws], Idx, Site, Path) -> % Idx is the index of W in the original list
    WarningString = case W of
                        {{index, _N}, S} -> S;
                        {S, []}          -> S
                    end,
    write_to_cell(WarningString, 7, Idx, Site, Path),
    write_warnings(Ws, Idx+1, Site, Path).


readxls(Fn) ->
    filefilters:read(excel, Fn, fun decipher_ets_tables/1).


read_table(Name, TableDescriptors) ->
    {value, {Name, Id}} = lists:keysearch(Name, 1, TableDescriptors),
    Records = ets:foldl(fun(X, Acc) -> [X|Acc] end, [], Id),
    Records.
                             

%% Input: list of {key, id} pairs where key is an ETS table holding info
%% about some part of the XLS file.
%% @TODO: formats, names, styles.
decipher_ets_tables(Tids) ->
    CellRecs = read_table(cell, Tids),
    CellInfo = lists:map(fun({Index, [_, Body]}) -> {Index, Body} end, CellRecs),
    AFRecs = read_table(array_formulae, Tids),
    Celldata = CellInfo ++ AFRecs,
    Names = read_table(names, Tids),
    Formats = read_table(formats, Tids),
    CSS = read_table(css,Tids),
    Warnings = read_table(warnings, Tids),
    {Celldata, Names, Formats, CSS, Warnings}.

loop()->
    case mnesia:table_info(dirty_cell,size) of
        0 -> ok;
        _ -> timer:sleep(250),
              loop()
    end.
read_reader_record({{{sheet, SheetName}, {row_index, Row}, {col_index, Col}}, Val}) ->
    {SheetName, {Row, Col}, Val};
read_reader_record({{{sheet, SheetName}, {firstrow, Fr}, {firstcol, Fc}, {lastrow, Lr}, {lastcol, Lc}}, Fla}) ->
    {SheetName, {{Fr, Fc}, {Lr, Lc}}, Fla}.

%% @doc Convert row and column pair to A1-style ref
%% @spec rc_to_a1(Row :: integer(), Col :: integer()) -> string()
rc_to_a1(Row, Col) ->
    tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1).

%% @TODO: Some of these conversion need to be done inside the reader itself.
conv_for_post(Val) ->
    case Val of
        {_, boolean, true}        -> "true";
        {_, boolean, false}       -> "false";
        {_, date, {datetime,D,T}} -> make_date_string({D,T});
        {_, number, N}            -> tconv:to_s(N);
        {_, error, E}             -> E;
        {string, X}               -> X;
        {formula, F}              -> F
    end.

fix_integers(X) ->
    case make_float(X) of
        "not float" -> X;
        X2          -> if
                           (X2-round(X2)) == 0.0 -> integer_to_list(round(X2));
                           true                  -> X
                       end
    end.

make_date_string({Days,Time}) ->
     make_day_string(Days)++" "++make_time_string(Time).

make_day_string({Year,Month,Day}) ->
    integer_to_list(Day)++"/"++pad(integer_to_list(Month))++"/"++pad(integer_to_list(Year)).

make_time_string({Hour,Minute,Second})->
    pad(integer_to_list(Hour))++":"++pad(integer_to_list(Minute))++":"++pad(integer_to_list(Second)).

pad(X) when is_list(X) ->
    case length(X) of
	1 -> "0"++X;
	_ -> X
    end.

make_float(List) ->
    Return = try
                list_to_float(List)
              catch
                exit:_Reason   -> "not float";
                error:_Message -> "not float";
                throw:_Term     -> "not float"
            end,
    case Return of
      "not float" -> make_float2(List);
      _           -> Return
    end.

make_float2(List)->
    try
      list_to_integer(List)*1.0
    catch
      exit:_Reason   -> "not float";
      error:_Message -> "not float";
      throw:_Term    -> "not float"
    end.
