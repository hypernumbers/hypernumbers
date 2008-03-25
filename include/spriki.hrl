-define(HN_NAME,    "HyperNumbers").

-define(TIMEOUT,    1000).
-define(HTML_ROOT,  "/html/").

-define(PORTNO,     1935). 

-define(HTTP,       $h,$t,$t,$p).
-define(HTTPS,      $h,$t,$t,$p,$s).
-define(SLASH,      47).

%% Test Macros
-define(HN_URL1,   "http://127.0.0.1:9000").
-define(HN_URL2,   "http://127.0.0.1:9001").

-record( index,
{
    site,
    path,
    column,
    row
}).

-record( page,
{
    site,
    path,
    ref,
    vars,
    format,
    auth        = undefined
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
-record( details_from,
{
    proxy_URL   = [],
    biccie      = [],
    version
}).

%% the details_to record is used by a site that has to notify another one
%% of a change in a hypernumber. These details are passed to authenticate
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

-record( ref,
{
    site        = [],
    path        = [],
    ref         = null,
    name        = undef
}).

-record( hn_item,
{
    addr        = #ref{},
    val         = []
}).

-record( dirty_cell,
{
    index       = #index{},
    timestamp   = now()
}).

-record( dirty_hypernumbers,
{
    index       = #index{},
    timestamp   = now()
 }).

-record( link_cell,
{
    parent      = #index{}, 
    child       = #index{}
}).

-record( outgoing_hn,
{
    local       = #index{},
    remote      = #index{},
    biccie      = [],       %% A shared token
    proxy       = [],
    url         = [],
    version     = 0         %% Version for structural updates
}).

-record( incoming_hn,
{
    remote     = #index{},  %% The address of the number
    value,                  
    deptree     = [],       %% Cells use in this numbers calculation
    biccie      = [],       %% A shared token
    version     = 0,        %% Version for structural updates
    cells       = []        %% Cells that use this number
}).

-record( users,
{
    name        = [],
    password    = [],
    status      = user,
    created     = calendar:local_time()
}).

-record( websheet,
{
    page        = #page{},
    permissions = [],
    name        = [],
    version     = 0,
    change      = [],
    gui         = "default",
    public      = default
}).

%% Cookie Record for logged in users
-record( user,
{
    name,
    loggedin    = false,
    state       = #users{}
}).
