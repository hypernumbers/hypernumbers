-define(HN_NAME,    "HyperNumbers").

-define(TIMEOUT,     1000).
-define(HTML_ROOT,  "/html/").
-define(SWF_ROOT,   "/swf/").

-define(PORTNO, 1935). 
-define(F, io:format). % Handy for debugging

-define(REGISTER,   $r,$e,$g,$i,$s,$t,$e,$r).
-define(UNREGISTER, $u,$n,$r,$e,$g,$i,$s,$t,$e,$r).
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
    row,
    column
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

%% a hypnum_item is a value stored against a page , row , col , cell , range
%% the 'ref' is an attr_ref record that specified the full address of the
%% reference including the name of the attribute
-record( attr_addr,
{
    site        = [],
    path        = [],
    ref         = null,
    name        = undef
}).

-record( hypnum_item,
{
    addr        = #attr_addr{},
    val         = []
}).

-record(bindings,
{
    index       = #index{},
	page        = #page{},
    type,
	varname     = [],
	value
}).

-record(dirty_refs,
{
    index       = #index{},
    timestamp
}).

-record( dirty_hypernumbers,
{
    index       = #index{},
    timestamp
 }).

-record( ref,
{
    ref_from    = #index{},
    ref_to      = #index{},
    details_from= #details_from{},
    details_to  = #details_to{}
}).

-record( spriki,
{
    index       = #index{},
    value,
    val_type,
    status      = #status{},
    num_format  = [],
    disp_format = []
}).

-record( hypernumbers,
{
    ref_from    = #index{},
    value,
    reftree     = [],
    errors      = [],
    refs        = []
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
