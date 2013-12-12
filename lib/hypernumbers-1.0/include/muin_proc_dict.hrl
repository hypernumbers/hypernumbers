%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       Macros to manipulate the process dictionary in muin
%%%            May the Lord have mercy on my soul!
%%% @end
%%% Created : 29 Mar 2011 by Gordon Guthrie <>

-define(mx,            muin:pd_retrieve(x)).
-define(my,            muin:pd_retrieve(y)).
-define(mpath,         muin:pd_retrieve(path)).
-define(msite,         muin:pd_retrieve(site)).
-define(mar,           muin:pd_retrieve(auth_req)).
-define(array_context, muin:pd_retrieve(array_context)).

-define(error_in_formula, {errval, '#FORMULA!'}).
-define(syntax_err,       {error, syntax_error}).

-record(muin_context, {
          idx,
          site           = []    :: list(),
          path           = []    :: list(),
          row                    :: integer(),
          col                    :: integer(),
          x                      :: integer(),
          y                      :: integer(),
          auth_req       = []    :: list(),
          array_context  = []    :: list(),
          finite_refs    = []    :: list(),
          range_refs     = []    :: list(),
          infinite_refs  = []    :: list(),
          errors         = []    :: list(),
          recompile      = false :: boolean(),
          circref        = false :: boolean(),
          selfreference  = false :: boolean(),
          is_zcalc       = false :: boolean()
         }).

-record(old_muin_context, {
          path :: list(),
          row  :: integer(),
          col  :: integer()
         }).
