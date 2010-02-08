-record(req, { accept,
               body,
               headers = [], %% These are 'response' headers.
               method,
               mochi,
               auth_req,
               uid,
               user }).
