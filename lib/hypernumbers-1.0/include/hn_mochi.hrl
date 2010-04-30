%% HTTP Environment 
-record(env, { accept,
               body :: multipart | list(),
               raw_body :: {upload, string()} | binary(),
               headers = [] :: list(), %% These are 'response' headers.
               method,
               mochi,
               uid :: string(),
               email = "anonymous" :: string()
             }).
