%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie 
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Records for web controls 
%%%
%%% @end
%%% Created :  1 Feb 2011 by <gordon@hypernumbers.com>
%%-------------------------------------------------------------------

-record(wcpagename,
        {
          template,
          name
         }).

-record(wcpagenumber,
        {
          template,
          type,
          prefix
         }).

-record(wcpagedate,
        {
          template,
          format
          }).

