%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc break out list of modules
-module(fns).

-export([
         get_modules/0
        ]).

get_modules() ->
    [
     stdfuns_text,
     stdfuns_math,
     stdfuns_stats,
     stdfuns_date,
     stdfuns_financial,
     stdfuns_info,
     stdfuns_lookup_ref,
     stdfuns_eng,
     stdfuns_logical,
     stdfuns_db,
     hnfuns_graphs,
     hnfuns_graphs2,
     hnfuns_web,
     hnfuns_integration,
     hnfuns_html,
     hnfuns_forms,
     hnfuns_contacts,
     hnfuns_controls,
     hnfuns_z,
     hnfuns_special,
     hnfuns_site,
     hnfuns_bootstrap
    ].
