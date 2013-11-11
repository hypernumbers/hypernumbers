%%% records used in passport

-record(hypertag, {
          uid,
          email,
          expiry,
          data
         }).

-record(user, {
          uid,
          email,
          passMD5      = nil,
          validated    = false,
          created_on   = calendar:universal_time(),
          lastlogin_on = nil,
          data         = dict:new()
         }).

-record(reset, {
          age  = 0,
          hash = "",
          site = application:get_env(hypernumbers, norefer_url)
         }).
