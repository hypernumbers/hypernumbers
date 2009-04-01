%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       This document writes warnings for unsupported functions
%%%
%%% @end
%%% Created : 17 Mar 2009 by <gordonguthrie@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(hn_warnings).

-include("working_fns.hrl").

-export([warnings/3]).

warnings(FuncVar, [H | _T] , Tbl) -> warnings1(FuncVar, H, Tbl);
warnings(FuncVar, [], Tbl)        -> warnings1(FuncVar, [], Tbl).

warnings1(FuncVar, H, Tbl) ->

    % if FuncVar is 255 then this is not an Excel function but is a function
    % whose name is the head of the arguments list...
    Nm = if
             (FuncVar == 255) -> {string, Name} = H,
                                 Name;
             true             -> excel_rev_comp:macro_to_string_WARNING(FuncVar)
              end,
    
    Index = ms_util2:get_index(help, name),
    case lists:keymember(Nm, Index, ?WORKING_FNS) of
        false ->  Str = "Function "++Nm++
                                  " is used in your spreadsheet but "++
                                  "not supported in Hypernumbers "++
                                  "at the moment!",
                              excel_util:append(Tbl, warnings, Str);
        Help -> #help{warning = Warning} = Help,
                case Warning of
                    ""    -> ok;
                    Other -> excel_util:append(Tbl, warnings, Other)
                end
    end.
