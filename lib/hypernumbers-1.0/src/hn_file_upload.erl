%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Functions to handle file uploads.

-module(hn_file_upload).
-compile(export_all).
-include("spriki.hrl").
-include("hypernumbers.hrl").

%% @doc Handles a file upload request from a user. Imports the file etc and
%% returns response data to be sent back to the client.

handle_upload(Req, User) ->
    Username = case User of
                   anonymous -> "anonymous";
                   U         -> U#hn_user.name
               end,
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_universal_time(erlang:now()),
    Timestamp = lists:flatten(io_lib:format("~p_~p_~p-~p_~p_~p", [Y, M, D, H, Min, S])),
    Filestamp = Username ++ "__" ++ Timestamp,
    Callback = fun(N) ->
                       %% Passing filestamp (time & username) for file name in state record here.
                       file_upload_callback(N, #file_upload_state{filename = Filestamp})
               end,
    {_, _, State} = mochiweb_multipart:parse_multipart_request(Req, Callback),
    "success". % TODO: URL of new page here.

file_upload_callback({headers, Headers}, S) ->
    ContentDisposition = hd(Headers),
    NewState = case ContentDisposition of
                   {"content-disposition", {"form-data", [{"name", _}, {"filename", Filename}]}} ->
                       Filestamp = S#file_upload_state.filename,
                       FullFilename = filename:join([code:lib_dir(hypernumbers),
                                                     "..", "..",
                                                     "priv", "uploads",
                                                     Filestamp ++ "__" ++ Filename]),
                       #file_upload_state{filename = FullFilename};
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
