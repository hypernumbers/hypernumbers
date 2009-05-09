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
    Filestamp = Username ++ "__" ++ dh_date:format("Y_m_d_h_i_s"),
    
    Callback = fun(N) ->
                       %% Passing filestamp (time & username) for file name in state record here.
                       file_upload_callback(N, #file_upload_state{filename = Filestamp})
               end,
    
    {_, _, State} = mochiweb_multipart:parse_multipart_request(Req, Callback),
    RawPath = Req:get(raw_path),
    Orig = State#file_upload_state.original_filename,
    Basename = filename:basename(Orig, ".xls"),
    {ok, Safename, _N} = regexp:gsub(Basename, "\\s+", "_"),
    ParentPage = RawPath ++ Safename ++ "/",
    {value, {'Host', Host}} = mochiweb_headers:lookup('Host', Req:get(headers)),

    try
        import(State#file_upload_state.filename, Host, ParentPage, Username, Orig),
        {struct, [{"location", ParentPage}]}
    catch
        _Error:_Reason ->
            ?ERROR("Error Importing ~p ~n User:~p~n Reason:~p~n Stack:~p~n",
                   [ParentPage, hn_users:name(User), _Reason, erlang:get_stacktrace()]),
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
                                                     Filestamp ++ "__" ++ Filename]),
                       #file_upload_state{original_filename = Filename,
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
                    ?INFO("~p",[S#file_upload_state.filename]),
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

import(Filename, Host, ParentPage, Username, OrigFilename) ->
    {Celldata, _Names, _Formats, CSS, Warnings, Sheetnames} = readxls(Filename),
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
                         Url = Site ++ Path ++ Ref,
                         RefX = hn_util:parse_url(Url),
                         ok = hn_db_api:write_attributes(RefX, [{"formula", Postdata}]);
                    ({Path, {Tl, Br}, Postdata}) -> % array formula
                         Url = Site ++ Path ++ Tl ++ ":" ++ Br,
                         RefX = hn_util:parse_url(Url),
                         hn_main:formula_to_range(RefX, Postdata)
                 end,

    lists:foreach(Dopost, Lits),
    lists:foreach(Dopost, Flas),

    %% Now fire in the CSS and formats
    WriteCSS = fun(X) ->
                       {{{sheet, SheetName}, {row_index, Row}, {col_index, Col}},
                        [CSSItem]} = X,
                       Sheet = excel_util:esc_tab_name(SheetName),
                       Path = ParentPage++Sheet++"/",
                       Ref = rc_to_a1(Row,Col),
                       Url = Site ++ Path ++ Ref,
                       RefX = hn_util:parse_url(Url),
                       #refX{path = P2} = RefX,
                       RefX2 = #refX{site = Site, path = P2, obj = {page, "/"}},
                       ok = hn_db_api:write_style_IMPORT(RefX2, CSSItem)
               end,
    lists:foreach(WriteCSS, CSS),

    %% write parent page information
    PathComps = string:tokens(ParentPage, "/"),
    GetSheets = fun(X, Acc) ->
                        {_, [NewSheet]} = X,
                        [Site ++ ParentPage ++ NewSheet ++"/" | Acc]
                end,
    Sheetnames2 = lists:foldl(GetSheets, [], Sheetnames),
    HeaderStyle = [{"font-weight", "bold"},
                   {"font-size", "16px"},
                   {"color", "#E36C0A"}],
    WarningStyle = [{"font-weight", "bold"},
                   {"font-size", "12px"},
                   {"color", "#FF0000"}],
    write_to_cell("File Import Details",
                  2, 2, Site, PathComps, HeaderStyle),
    write_to_cell("File " ++ OrigFilename ++ " imported by " ++ Username ++ " on " ++
                  dh_date:format("r"),
                  2, 3, Site, PathComps, []),
    write_to_cell("Each sheet in your Excel file has been written as a page. "++
                  "The sheetname may have been rewritten to make them valid URL's. ",
                  2, 5, Site, PathComps,[]),
    write_to_cell("(Don't worry all your formulae that refer to cells on a renamed "++
                  "sheet have been rewritten to take this into account.)", 
                  2, 6, Site, PathComps, []),
    write_to_cell("Imported Pages",
                  2, 8, Site, PathComps, HeaderStyle),
    WriteSheets = fun(X, Int) ->
                          write_to_cell(X, 2, Int, Site, PathComps, []),
                          Int + 1
                  end,
    Index = lists:foldl(WriteSheets, 9, Sheetnames2),

    write_to_cell("Warnings",
                  2, Index + 1, Site, PathComps, HeaderStyle),
    write_to_cell("(remember this is an early BETA product!)",
                  2, Index + 2, Site, PathComps, WarningStyle),
    write_to_cell("Not all Excel functions are fully supported at the moment.",
                  2, Index + 3, Site, PathComps, []),

    ok = write_warnings(Warnings, Index + 5, Site, PathComps),
    loop(),
    ok.

write_to_cell(Str, Col, Row, Site, Path, Attrs) ->
    Attrs2 = [{"formula", Str} | Attrs],
    ok = hn_db_api:write_attributes(#refX{site = Site,
                                          path = Path,
                                          obj = {cell, {Col, Row}}},
                                    Attrs2).
                                          
write_warnings([], _, _, _) ->
    ok;
write_warnings([W|Ws], Idx, Site, Path) -> % Idx is the index of W in the original list
    WarningString = case W of
                        {S, []}          -> S;
                        {_, S}           -> S
                    end,
    write_to_cell(WarningString, 2, Idx, Site, Path, []),
    write_warnings(Ws, Idx + 1, Site, Path).


readxls(Fn) ->
    filefilters:read(excel, Fn, fun decipher_ets_tables/1).


read_table(Name, TableDescriptors) ->
    {value, {Name, Id}} = lists:keysearch(Name, 1, TableDescriptors),
    Records = ets:foldl(fun(X, Acc) -> [X | Acc] end, [], Id),
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
    CSS = read_table(css, Tids),
    Warnings = read_table(warnings, Tids),
    Sheetnames = read_table(sheetnames, Tids),
    {Celldata, Names, Formats, CSS, Warnings, Sheetnames}.

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
