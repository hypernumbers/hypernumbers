-record(log, {
          timestamp   = now(),
          tid         = [],
          pid         = [],
          type        = [],
          lock        = [],
          table       = [],
          dump        = []
         }).

-record(restart, {
          timestamp = now(),
          tid         = [],
          pid         = [],
          type        = [],
          lock        = [],
          table       = [],
          duration    = [],
          clashing_tid = [],
          clashing_pid = [],
          dump        = []
         }).

-record(api, {
          timestamp = now(),
          tid         = [],
          pid         = [],
          fn_id       = [],
          fn_instance = []
         }).

%% -record(aggregate, {
%%           fn_id,
%%           total_calls = 0,
%%           total_restarts = 0,
%%           total_duration = 0,
%%           average_duration = 0
%%          }).

-record(agg_stats, {
          fn_id          = undefined,
          total_calls    = 0,
          total_logs     = 0,
          total_duration = 0
         }).

-record(stats, {
          no_calls,
          no_logs
         }).

-record(api_call, {
          timestamp    = now(),
          fn_id        = [],
          fn_instance  = [],
          tid          = null,
          duration     = [],
          reports      = [],
          restarts     = [],
          logs         = [],
          stats        = #stats{}
         }).
