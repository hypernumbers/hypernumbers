-record(req, {mochi,
              headers = [], %% These are 'response' headers.
              body,
              method,
              user,
              uid,
              accept}).
