-record(req, { accept,
               body,
               headers = [], %% These are 'response' headers.
               method,
               mochi,
               pending = nil, 
               uid,
               user }).
