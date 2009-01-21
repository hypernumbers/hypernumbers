-define(HN_NAME,    "HyperNumbers").

-define(TIMEOUT,    1000).
-define(HTML_ROOT,  "/html/").

-define(PORTNO,     1935).

-define(HTTP,       $h,$t,$t,$p).
-define(HTTPS,      $h,$t,$t,$p,$s).
-define(SLASH,      47).

-define(SALT,       "salt").

%% Test Macros
-define(HN_URL1,   "http://127.0.0.1:9000").
-define(HN_URL2,   "http://127.0.0.1:9001").

-record(index,
        {
          site,
          path,
          column,
          row
         }).

-record(ref,
        {
          site        = [],
          path        = [],
          ref         = null,
          name        = undef,
          auth        = []
         }).


-record(status,
        {
          formula     = [],
          reftree     = [],
          errors      = [],
          refs        = []
         }).

%% the details_from record is used by the site that has an outstanding request
%% for a hypernumber and is used to authenticate a notification message:
%% - where the remote site should have posted the notification (proxy_URL)
%% - how the remote site should authenticate itself
%%
%% the details of what the remote cell should be are held in the ref_from
%% field in the ref table
%%
%% the 'version' field is used to check that the page structures versions
%% are aligned and that one site is in synch with another
-record(details_from,
        {
          proxy_URL   = [],
          biccie      = [],
          version
         }).

%% the details_to record is used by a site that has to notify another one
% of a change in a hypernumber. These details are passed to authenticate
%% that request. They are posted to:
%% - the proxy URL (as specified by the registering site)
%%
%% the details that are sent are:
%% - reg_URL (the URL of *A* cell that actually made the request for
%%            a hypernumber - bear in mind many have but there will
%%            only be a single notification per remote site)
%% - biccie  (the authentication token)
%%
%% The record will be formatted according to the value of 'format'
-record(details_to,
        {
          proxy_URL   = [],
          reg_URL     = [],
          biccie      = [],
          format
         }).

-record(hn_item,
        {
          addr        = #ref{},
          val         = []
         }).

-record(dirty_cell,
        {
          index       = #index{},
          timestamp   = now()
         }).

-record(dirty_hypernumber,
        {
          index       = #index{},
          timestamp   = now()
         }).

-record(local_cell_link,   %% Link 2 cells on the same sheet together
        {
          parent      = #index{},
          child       = #index{}
         }).

-record(remote_cell_link,  %% Link a hypernumber with a cell,
        {                           %% is used to link both incoming and outgoing
          parent      = #index{}, %% hypernumbers
          child       = #index{},
          type        = null      %% incoming or outgoing
         }).

-record(outgoing_hn,
        {
          index       = {[],#index{}},
          biccie      = [],       %% A shared token
          url         = [],
          version     = 0         %% Version for structural updates
         }).

-record(incoming_hn,
        {
          remote     = #index{},  %% The address of the number
          value,
          deptree     = [],       %% Cells use in this numbers calculation
          biccie      = [],       %% A shared token
          version     = 0         %% Version for structural updates
         }).

-record(hn_user,
        {
          name        = [],
          password    = [],
          authtoken   = null,
          created     = calendar:local_time()
         }).

-record(template,
        {
          name        = [],
          temp_path   = [],
          gui         = index,
          form        = null
         }).


%% style is the container tuple for the table styles
-record(style,
        {
          'border-right'        = [],
          'border-left'         = [],
          'border-top'          = [],
          'border-bottom'       = [],
          'border-color'        = [],
          'border-right-style'  = [],
          'border-left-style'   = [],
          'border-top-style'    = [],
          'border-bottom-style' = [],
          color                 = [],
          'vertical-align'      = [],
          'background-color'    = [],
          'font-weight'         = [],
          'font-size'           = [],
          'font-family'         = [],
          'font-style'          = [],
          'text-decoration'     = [],
          'text-shadow'         = [],
          'font-stretch'        = [],
          'text-align'          = []
         }).        

-record(styles,
        {
          ref   = #ref{},
          index = 0,
          style = #style{}
         }).

%% this builds the counters for the style table
-record(style_counters,
        {
          ref,
          integer
         }).
