-record(req, { accept,
               body,
               raw_body,
               headers = [], %% These are 'response' headers.
               method,
               mochi,
               auth_req,
               uid,
               user }).
