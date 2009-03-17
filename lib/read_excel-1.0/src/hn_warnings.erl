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
    FuncName = if
                  (FuncVar >  255) -> H;
                  (FuncVar =< 255) -> excel_rev_comp:macro_to_string_WARNING(FuncVar)
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
    
    case lists:member(FuncName, F1) of
        true ->  excel_util:append(Tbl, warnings, "Function "++FuncName++
                                   " is supported but dates and times are stored "++
                                   "differently in Hypernumbers than in other "++
                                   "spreadsheets, so take care!");
        false -> case lists:member(FuncName, F2) of
                     true  -> ok;
                     false -> excel_util:append(Tbl, warnings, "Function "++FuncName++
                                                " is used in your spreadsheet but "++
                                                "not supported in Hypernumbers"++
                                                "at the moment!")
                 end
    end.
