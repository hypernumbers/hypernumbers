%%%-------------------------------------------------------------------
%%% @author     Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       This document writes warnings for unsupported functions
%%%
%%% @end
%%% Created : 17 Mar 2009 by <gordonguthrie@hypernumbers.com>
%%%-------------------------------------------------------------------
-module(hn_warnings).

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
    
    F1 = ["DAY",
          "HOUR",
          "MONTH",
          "SECOND",
          "YEAR"],

    F2 = ["NPER",
          "NPV",
          "PMT",
          "PROPER",
          "PV",
          "RATE",
          "REPLACE",
          "REPT",
          "ROWS",
          "SLN",
          "SYD",
          "T",
          "TIME",
          "ABS",
          "ACOS",
          "AND",
          "ASIN",
          "ATAN",
          "ATAN2",
          "AVERAGE",
          "COLUMNS",
          "COS",
          "COUNTBLANK",
          "DATE",
          "DAVERAGE",
          "DEGREES",
          "EVEN",
          "EXP",
          "FACT",
          "FALSE",
          "IF",
          "INT",
          "ISBLANK",
          "ISERR",
          "ISERROR",
          "ISLOGICAL",
          "ISNA",
          "ISNONTEXT",
          "ISNUMBER",
          "ISTEXT",
          "LEFT",
          "LEN",
          "LN",
          "LOG",
          "LOG10",
          "LOWER",
          "MAX",
          "MOD",
          "N",
          "NA",
          "NOT",
          "NOW",
          "ODD",
          "OR",
          "PI",
          "POWER",
          "RADIANS",
          "ROUND",
          "SIN",
          "SQRT",
          "STDEV",
          "SUM",
          "TAN",
          "TRUE",
          "TRUNC",
          "UPPER"],
    
    case lists:member(Nm, F1) of
        true ->  Str = "Function "++Nm++" is supported but dates and times "++
                     "are stored differently in Hypernumbers than in other "++
                     "spreadsheets, so take care!",
                     excel_util:append(Tbl, warnings, Str);
        false -> case lists:member(Nm, F2) of
                     true  -> ok;
                     false -> Str = "Function "++Nm++
                                  " is used in your spreadsheet but "++
                                  "not supported in Hypernumbers "++
                                  "at the moment!",
                                  excel_util:append(Tbl, warnings, Str)
                 end
    end.
