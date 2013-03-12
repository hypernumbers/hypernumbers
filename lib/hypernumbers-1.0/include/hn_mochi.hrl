%% HTTP Environment
-record(env, { accept,
               body :: multipart | list(),
               raw_body :: {upload, string()} | binary(),
               headers = [] :: list(), %% These are 'response' headers.
               method,
               mochi,
               uid :: string(),
               email = "anonymous" :: string(),
               auth = null
             }).

% useful macros

-define(SHEETVIEW,   "spreadsheet").
-define(WEBPAGE,     "webpage").
-define(WIKI,        "wikipage").
-define(LOGVIEW,     "logs").
-define(DEBUG,       "debug").
-define(RECALC,      "recalc").
-define(PHONE,       "phone").
-define(RECORDING,   "recording").

-define(NO_STAMP, undefined).
