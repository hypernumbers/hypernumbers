-define(EXPIRY,    259200). % 3 days in seconds = 3*24*60*60

-type now() :: {integer(),integer(),integer()}.
-type cellidx() :: pos_integer().
-type generator() :: fun(() -> string()).
-type resource_addr() :: {string(), integer(), atom()}. %% (IP, Port, Node).


-record(core_site, {site = [] :: string(),
                    type :: atom()}).

%% Zone Tables
-record(zone, { label :: string(),
                min_size :: integer(),
                ideal_size :: integer(),
                pool :: gb_tree(),
                generator :: generator(),
                zone_id :: integer() }).

-record(record, { name, % :: {string(), string()},
                  address,% :: resource_addr(),
                  %% Following two used for linode.
                  zone_id,% :: integer(),
                  resource_id }).% :: integer()}).

-record(resource, { address = [] :: resource_addr(),
                    weight = 0 :: integer() }).

%% render addons
-record(render,
        {
          css = [],
          js = [],
          js_reload = [],
          title = []
         }).

%% Site Tables
-record(index,
        {
          site,
          path,
          column,
          row
         }).

-record(xrefX,
        {
          idx,
          site        = [],
          path        = [],
          obj         = null
         }).

-record(refX,
        {
          site        = [],
          type,
          path        = [],
          obj         = null
         }).

-record(status,
        {
          formula     = [],
          reftree     = [],
          errors      = [],
          refs        = []
         }).

-record(user_fns, {
          name,
          ast,
          pagejson,
          wizardjson
         }).

-record(local_obj,
        {
          idx,
          type,
          path,
          obj,
          revidx
         }).

-record(revidx,
        {
          revidx,
          idx
         }).

-record(item, {idx, attrs}).

-record(relation,
        {
          cellidx                    :: cellidx(),
          children = ordsets:new()   :: ordsets:ordset(cellidx()),
          parents = ordsets:new()    :: ordsets:ordset(cellidx()),
          infparents = ordsets:new() :: ordsets:ordset(cellidx()),
          z_parents = ordsets:new()  :: ordsets:ordset(#refX{}),
          include = false
       }).

-record(dirty_queue,
        {
          id = now(),
          dirty = [],
          auth_req
         }).

% this record is for the table that gets all written cells and
% so that the zinf tree can determine if they are 'proper dirty' for dbsrv
-record(dirty_for_zinf,
        {
          id = now(),
          dirty :: #xrefX{}
         }).

% this record is for the table that gets changes to the zinf tree
-record(dirty_zinf,
        {
          id = now(),
          type,
          dirtycellidx :: cellidx(),
          old          :: ordsets:ordset(#refX{}),
          new          :: ordsets:ordset(#refX{}),
          processed = false
         }).

% this record is for the table that logs changes
-record(logging,
        {
          idx,
          timestamp = util2:get_timestamp(),
          uid        = [],
          action     = [],
          actiontype = [],
          type       = [],
          path,
          obj,
          log
         }).

-record(group,
        {
          name = [],
          members = gb_sets:empty()
         }).

% this is the general KV store for KV's that need to be under transations
% or distrubuted between servers
-record(kvstore,
        {
          key,
          value
         }).

-record(template,
        {
          name        = [],
          temp_path   = [],
          gui         = index,
          form        = null
         }).

%% magic_style is the container tuple for the table styles
%% NOTE the attribute 'overwrite-color' isn't in a magic style and shouldn't be
%%      it the overwrite text colour generated by the formats and, as the
%%      name suggests, overwrites the style colour

% magic_style commented out to make css styles work in the gui again!
% to switch back to using styles simply comment out and uncomment the
% one after

-record(magic_style,
        {
          'border-right-style'  = [],
          'border-left-style'   = [],
          'border-top-style'    = [],
          'border-bottom-style' = [],
          'border-right-color'  = [],
          'border-left-color'   = [],
          'border-top-color'    = [],
          'border-bottom-color' = [],
          'border-right-width'  = [],
          'border-left-width'   = [],
          'border-top-width'    = [],
          'border-bottom-width' = [],
          color                 = [],
          'vertical-align'      = [],
          'background-color'    = [],
          'font-weight'         = [],
          'font-size'           = [],
          'font-family'         = [],
          'font-style'          = [],
          'font-stretch'        = [],
          'text-decoration'     = [],
          'text-shadow'         = [],
          'text-align'          = [],
          'white-space'         = []
         }).

-record(style,
        {
          magic_style = #magic_style{},
          idx
         }).

-record(form,
        {
          key, % one form element per unit ref
          id, % {path, transaction, label}
          kind,
          restrictions = none,
          attrs = []
         }).

-record(webcontrol,
        {
          id,
          command = []
         }).

-record(help,
        {
          name,
          warning,
          arity,
          category,
          text,
          notes= []
         }).

%% HN Mochi Query Parameters. Leave as undefined.
-record(qry,
        {
          challenger,
          hypertag,
          mark,
          map,
          pages,
          paths,
          permissions,
          rawview,
          return,
          status,
          stamp,
          template,
          templates,
          updates,
          via,
          view,
          views
         }).

%% web controls records

-record(namedpage,
        {
          template,
          name
         }).

-record(numberedpage,
        {
          template,
          type,
          prefix
         }).

-record(datedpage,
        {
          template,
          format
          }).

-record(plainpath,
        {
          path
         }).

-record(destination,
        {
          type = false
         }).

-record(segment,
        {
          page,
          redirect = #destination{},
          addspreadsheetgroups = [],
          addwebpagegroups = [],
          addwikipagegroups = [],
          addtablegroups = []
         }).

% the internal api for include
-record(incs,
        {
          js         = [],
          js_reload  = [],
          css        = []
         }).

% the timer table
-record(timer,
        {
          idx,
          spec
         }).

% the css and javascript tables
-record(include,
        {
          idx,
          path,
          js,
          js_reload,
          css
         }).

% api table records
-record(api,
        {
          publickey,
          privatekey,
          urls = []
          }).

-record(api_urls,
        {
          path = undefined,    % unitialised records will cause a crash! Good!
          admin = false,       % by default not admin
          inclue_subs = false,
          append_only = true   % by default can only append
         }).

% records for data upload maps
-record(head,
        {type,
         filetype,
         template,
         overwrite
        }).

-record(validation,
        {sheet,
         cell,
         constraint
        }).

-record(mapping,
        {sheet,
         from,
         to
        }).
