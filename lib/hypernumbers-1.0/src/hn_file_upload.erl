%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Functions to handle file uploads.
-module(hn_file_upload).

-export([
         handle_upload/3,
         import/5
        ]).

-export([test_import/0,
         test_import/2,
         test_import/1 ]).

-include("spriki.hrl").
-include("hypernumbers.hrl").

% TODO - rest if chunking and sleep is still needed
-define(CHUNK, 1000).
-define(SLEEP, 500).

%% holds upload state for callback function in the hn_file_upload module.
-record(file_upload_state, {
          ref,               % The RefX for the request
          original_filename, % file name as it comes from the user
          filename,          % file name as saved on the server
          file,
          currentdata,
          data = []
         }).


%% @doc Handles a file upload request from a user. Imports the file etc and
%% returns response data to be sent back to the client.
handle_upload(Mochi, Ref, UserName) ->
    stream_to_file(Mochi, Ref, UserName).

%%% interfacing with the reader
%%% TODO: some of this needs to be part of the reader

%% @doc Writes contents of an XLS file into Hypernumbers pages.
%% Filename is a full name, parent page is path to page under which pages for
%% individual sheets will be created.
convert_addr({{Fr, Fc}, {Lr, Lc}}) ->
    {rc_to_a1(Fr, Fc), rc_to_a1(Lr, Lc)};
convert_addr({Row, Col}) ->
    rc_to_a1(Row, Col).

split_css(X, Acc) ->
    {SheetName, Target, V} = read_reader_record(X),
    Sheet = excel_util:esc_tab_name(SheetName),
    [{Sheet, convert_addr(Target), V} | Acc].

split_sheets(X, {Ls, Fs}) ->
    {SheetName, Target, V} = read_reader_record(X),
    Sheet = excel_util:esc_tab_name(SheetName),

    Postdata = conv_for_post(V),
    Datatpl = {Sheet, convert_addr(Target), Postdata},

    case Postdata of
        [$=|_] -> {Ls, [Datatpl|Fs]};
        _      -> {[Datatpl|Ls], Fs}
    end.

write_data(Ref, {Sheet, Target, Data}, Uid) when is_list(Target) ->
    NRef = Ref#refX{path = Ref#refX.path ++ [Sheet],
                    obj  = hn_util:parse_attr(Target)},
    new_db_api:write_attributes([{NRef, [{"formula", Data}]}], Uid).

write_css(Ref, {Sheet, Target, CSS}, Uid) when is_list(Target) ->
    NRef = Ref#refX{path = Ref#refX.path ++ [Sheet],
                    obj  = hn_util:parse_attr(Target)},
    new_db_api:write_attributes([{NRef, defaultize(CSS)}], Uid).

%% Excel's default borders are
%% * no type of border
%% * no style of border
%% * black
%% Our default borders aer:
%% * no type of border
%% * no style of border
%% * no colour
%% This function 'makes it so' <-- super-ugelee n'est pas?
defaultize(CSS) -> case def1(["bottom", "top", "right", "left"], CSS) of
                       abort  -> CSS;
                       NewCSS -> NewCSS
                   end.

% only apply the default if it applies to all 4 borders
def1([], CSS)      -> CSS;
def1([H | T], CSS) ->
    Color = "border-" ++ H ++ "-color",
    Style = "border-" ++ H ++ "-style",
    Width = "border-" ++ H ++ "-width",
    Lk1 = lists:keysearch(Color,  1, CSS),
    Lk2 = lists:keysearch(Style, 1, CSS),
    Lk3 = lists:keysearch(Width, 1, CSS),
    case {Lk1, Lk2, Lk3} of
        {{_, {_, "rgb(000,000,000)"}}, false, false} ->
            NewCSS = lists:keydelete(Color, 1, CSS),
            def1(T, NewCSS);
        _ ->
            abort
    end.

test_import() ->
     test_import("buildings.west-george-street.electricity.2011.mar.9.1800051976217.data", hn_util:url_to_refX("http://hypernumbers.dev:9000")).
    %test_import("c_year", hn_util:url_to_refX("http://hypernumbers.dev:9000")).

test_import(File) ->
    test_import(File, hn_util:url_to_refX("http://hypernumbers.dev:9000/")).

test_import(File, Ref) ->
    io:format("File is ~p~nRef is ~p~n", [File, Ref]),
    Path = code:lib_dir(hypernumbers)++"/../../tests/excel_files/"
        ++ "Win_Excel07_As_97/"++File++".xls",
    %Path = "/home/gordon/hypernumbers/tests/"++File++".xls",
    import(Path, "anonymous", Ref#refX{path=[File]}, File,nil).

import(File, User, Ref, Name, Uid) ->

    {Cells, _, _, CSS, Wrngs, Sheets} = filefilters:read(excel, File),
    {Literals, Formulas} = lists:foldl(fun split_sheets/2,
                                       {[], []}, Cells),
    CSS2 = lists:foldl(fun split_css/2, [], CSS),

    [ write_data(Ref, X, Uid) || X <- Literals ],
    [ write_data(Ref, X, Uid) || X <- Formulas ],
    [ write_css(Ref,  X, Uid) || X <- CSS2 ],

    ok = write_warnings_page(Ref, Sheets, User, Name, Wrngs, Uid).

write_warnings_page(Ref, Sheets, User, Name, Warnings, Uid) ->

    new_db_api:write_attributes([{Ref#refX{obj = {column, {2, 2}}},
                                 [{width, 400}]}], Uid),

    % write parent page information
    HeaderSt = [{"font-weight", "bold"},
                {"font-size", "16px"}, {"color", "#E36C0A"}],
    WarningSt = [{"font-weight", "bold"},
                 {"font-size", "12px"}, {"color", "#FF0000"}],

    Msg = ?FORMAT("File ~s imported by ~s on ~s",
                  [Name, User, dh_date:format("r")]),

    write_to_cell(Ref, "File Import Details", 2, 2, HeaderSt),
    write_to_cell(Ref, Msg, 2, 3, []),

    write_to_cell(Ref, "Each sheet in your Excel file has been written as "
                  "a page. The sheetname may have been rewritten to "
                  "make them valid URL's. ", 2, 5, []),
    write_to_cell(Ref, "(Don't worry all your formulae that refer to cells on "
                  "a renamed sheet have been rewritten to take this into "
                  "account.)", 2, 6, []),
    write_to_cell(Ref, "Imported Pages", 2, 8, HeaderSt),

    Root = hn_util:list_to_path(Ref#refX.path),
    F = fun({_X, [Sheet]}, Int) ->
                SheetPath = Root++Sheet++"/",
                Url       = "<a href='"++SheetPath++"'>"++SheetPath++"</a>",
                write_to_cell(Ref, Url, 2, Int, []),
                Int + 1
        end,
    Index = lists:foldl(F, 9, Sheets),

    write_to_cell(Ref, "Warnings", 2, Index + 1, HeaderSt),
    write_to_cell(Ref, "(remember this is an early BETA product!)",
                  2, Index + 2, WarningSt),
    write_to_cell(Ref, "Not all Excel functions are fully supported "
                  "at the moment. Any functions that you use which " ++
                  "are not supported will be listed here.", 2, Index + 3, []),

    ok = write_warnings(Ref, Warnings, Index + 5),

    ok.

write_to_cell(Ref, Str, Col, Row, Attrs) ->
    Attrs2 = [{"formula", Str} | Attrs],
    new_db_api:write_attributes([{Ref#refX{obj = {cell, {Col, Row}}}, Attrs2}]).

write_warnings(_Ref, [], _) ->
    ok;
% Idx is the index of W in the original list
write_warnings(Ref, [W|Ws], Idx) ->
    WarningString = case W of
                        {S, []}          -> S;
                        {_, S}           -> S
                    end,
    write_to_cell(Ref, WarningString, 2, Idx, []),
    write_warnings(Ref, Ws, Idx + 1).

%% Input: list of {key, id} pairs where key is an ETS table holding info
%% about some part of the XLS file.
%% @TODO: formats, names, styles.
read_reader_record({{{sheet, SheetName}, {row_index, Row},
                     {col_index, Col}}, Val}) ->
    {SheetName, {Row, Col}, Val};

read_reader_record({{{sheet, SheetName}, {firstrow, Fr}, {firstcol, Fc},
                     {lastrow, Lr}, {lastcol, Lc}}, Fla}) ->
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
        {_, date, {datetime,D,T}} -> dh_date:format("d/m/Y H:i:s",{D,T});
        {_, number, N}            -> tconv:to_s(N);
        {_, error, E}             -> E;
        {string, X}               -> X;
        {formula, F}              -> F
    end.

setup_state(S, {"content-disposition",
                {"form-data", [_Nm, {"filename", FileName}]}}) ->
    Stamp = S#file_upload_state.filename ++ "__" ++ FileName,
    Site  = (S#file_upload_state.ref)#refX.site,
    Name  = filename:join([hn_util:docroot(Site), "..", "uploads", Stamp]),
    filelib:ensure_dir(Name),
    {ok, File} = file:open(Name, [raw, write]),
    {file, S#file_upload_state{original_filename = FileName,
                              filename = Name,
                              file = File}};
setup_state(S, {"content-disposition",
                {"form-data", [{"name", Name}]}}) ->
    #file_upload_state{data = Data} = S,
    NewData = case lists:keysearch(Name, 1, Data) of
                  {value, _} -> exit("data should never exist for a header");
                  false      -> [{Name, []} | Data]
              end,
    {data, S#file_upload_state{currentdata = Name, data = NewData}}.

stream_to_file(Mochi, Ref, UserName) ->
    Stamp    = UserName ++ dh_date:format("__Y_m_d_h_i_s"),
    Rec      = #file_upload_state{filename=Stamp, ref=Ref},
    CallBack = fun(N) -> file_upload_callback(N, Rec, start) end,
    {_, _, State} = mochiweb_multipart:parse_multipart_request(Mochi, CallBack),
    {ok, State#file_upload_state.filename,
     State#file_upload_state.original_filename,
     State#file_upload_state.data}.

file_upload_callback({headers, Headers}, S, _) ->
    {Type, NewState} = setup_state(S, hd(Headers)),
    fun(N) -> file_upload_callback(N, NewState, Type) end;

file_upload_callback({body, Data}, S, file) ->
    file:write(S#file_upload_state.file, Data),
    fun(N) -> file_upload_callback(N, S, file) end;

file_upload_callback(body_end, S, file) ->
    file:close(S#file_upload_state.file),
    fun(N) -> file_upload_callback(N, S, file) end;

file_upload_callback({body, Data}, S, data) ->
    {value, {Key, Start}} = lists:keysearch(S#file_upload_state.currentdata, 1,
                                     S#file_upload_state.data),
    NewData = lists:keyreplace(Key, 1, S#file_upload_state.data,
                               {Key, Start ++ binary_to_list(Data)}),
    NewS = S#file_upload_state{data = NewData},
    fun(N) -> file_upload_callback(N, NewS, data) end;

file_upload_callback(body_end, S, data) ->
    fun(N) -> file_upload_callback(N, S, data) end;

file_upload_callback(eof, State, _) -> State.


