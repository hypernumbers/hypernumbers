-record(signon, {
          email,
          group,      % in WordPress a user can only be a member of one group
          return_url,
          timestamp
         }).
-define(DRIFT, 900000000). % 15 minutes in microseconds
