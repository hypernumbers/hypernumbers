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
    Nm    = get_name(H, FuncVar),
    Index = ms_util2:get_index(help, name) + 1, % we are working with the raw tuple here
    
    case lists:keysearch(Nm, Index, ?WORKING_FNS) of
        false ->  
            Str = "Function " ++ Nm ++ " "
                "is used in your spreadsheet but "++
                "not supported in Hypernumbers "++
                "at the moment!",
            excel_util:write(Tbl, warnings, [Nm | Str]);
        
        {value, #help{warning = ""}} -> 
            ok;
        
        {value, #help{warning = Warning}} ->
            io:format("Warning is ~p~n", [Warning]),
            excel_util:write(Tbl, warnings, [Nm | Warning])

    end.

get_name(H, 255) ->
    {string, Name} = H,
    Name;
get_name(_H, FuncVar) ->
    excel_rev_comp:macro_to_string_WARNING(FuncVar). 

    
