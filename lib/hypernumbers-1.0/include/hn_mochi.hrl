-record(req, { accept,
               body         :: multipart | list(),
               raw_body     :: {upload, string()} | binary(),
               headers = [] :: list(), %% These are 'response' headers.
               method,
               mochi,
               auth_req,
               uid,
               user }).
