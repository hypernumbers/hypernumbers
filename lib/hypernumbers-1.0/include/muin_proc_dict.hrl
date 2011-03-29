%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Gordon Guthrie
%%% @doc       Macros to manipulate the process dictionary in muin
%%%            May the Lord have mercy on my sould
%%% @end
%%% Created : 29 Mar 2011 by Gordon Guthrie <>

-define(mx, get(x)).
-define(my, get(y)).
-define(mpath, get(path)).
-define(msite, get(site)).
-define(mar, get(auth_req)).
-define(array_context, get(array_context)).

-define(error_in_formula, {errval, '#FORMULA!'}).
-define(syntax_err, {error, syntax_error}).

