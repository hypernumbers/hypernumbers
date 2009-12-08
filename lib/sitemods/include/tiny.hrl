-record(tiny_sub, {sub = [] :: string(),
                   allocated = false :: true | false}).

-record(run_details,
        {
          sub,
          domain,
          port,
          email,
          password
         }).
