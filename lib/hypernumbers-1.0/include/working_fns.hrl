%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       The current supported function set
%%%
%%% @end
%%% Created :  1 Apr 2009 by 
%%%-------------------------------------------------------------------
-include("spriki.hrl").

-define(date_time_warning, "is supported but dates and times "++
        "are stored differently in Hypernumbers than in other "++
        "spreadsheets, so take care!").

-define(WORKING_FNS,
        [
         #help{name = "ABS",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052089781033.aspx"
              },
         #help{name = "ACOS",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052089811033.aspx"
              },
         #help{name = "AND",
               warning = "",
               arity = "infinite",
               category = "LOGICAL",
               text = "http://office.microsoft.com/en-us/excel/HP052089861033.aspx"
              },
         #help{name = "ASIN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052089881033.aspx"
              },
         #help{name = "ATAN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052089901033.aspx"
              },
         #help{name = "ATAN2",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052089911033.aspx"
              },
         #help{name = "AVERAGE",
               warning = "",
               arity = "1",
               category = "Statistical",
               text = "http://office.microsoft.com/en-us/excel/HP052089941033.aspx"
              },
         #help{name = "COLUMNS",
               warning = "",
               arity = "1",
               category = "Lookup",
               text = "http://office.microsoft.com/en-us/excel/HP052090171033.aspx"
              },
         #help{name = "COS",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052090241033.aspx"
              },
         #help{name = "COUNTBLANK",
               warning = "",
               arity = "1",
               category = "Statistical",
               text = "http://office.microsoft.com/en-us/excel/HP052090281033.aspx"
              },
         #help{name = "DATE",
               warning = "",
               arity = "2",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052090421033.aspx"
              },
         #help{name = "DAVERAGE",
               warning = "",
               arity = "",
               category = "",
               text ="http://office.microsoft.com/en-us/excel/HP052090451033.aspx"
              },
         #help{name = "DAY",
               warning = "Function DAY " ++ ?date_time_warning,
               arity = "1",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052090461033.aspx"
              },
         #help{name = "DEGREES",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052090561033.aspx"
              },
         #help{name = "EVEN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052090801033.aspx"
              },
         #help{name = "EXP",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052090821033.aspx"
              },
         #help{name = "FACT",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052090841033.aspx"
              },
         #help{name = "FALSE",
               warning = "",
               arity = "0",
               category = "Logical",
               text = "http://office.microsoft.com/en-us/excel/HP052090861033.aspx"
              },
         #help{name = "HOUR",
               warning = "Function HOUR " ++ ?date_time_warning,
               arity = "1",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052091151033.aspx"
              },
         #help{name = "IF",
               warning = "",
               arity = "2 or 3",
               category = "Logical",
               text = "http://office.microsoft.com/en-us/excel/HP052091181033.aspx"
              },
         #help{name = "INT",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052091421033.aspx"
              },
         #help{name = "ISBLANK",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISERR",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISERROR",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISLOGICAL",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISNA",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISNONTEXT",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISNUMBER",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "ISTEXT",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091471033.aspx"
              },
         #help{name = "LEFT",
               warning = "",
               arity = "1/2",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052091531033.aspx"
              },
         #help{name = "LEN",
               warning = "",
               arity = "1",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052091541033.aspx"
              },
         #help{name = "LN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052091561033.aspx"
              },
         #help{name = "LOG",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052091571033.aspx"
              },
         #help{name = "LOG10",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052091581033.aspx"
              },
         #help{name = "LOWER",
               warning = "",
               arity = "1",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052091671033.aspx"
              },
         #help{name = "MAX",
               warning = "",
               arity = "infinite",
               category = "Statistical",
               text = "http://office.microsoft.com/en-us/excel/HP052091701033.aspx"
              },
         #help{name = "MOD",
               warning = "",
               arity = "2",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052091821033.aspx"
              },
         #help{name = "MONTH",
               warning = "Function MONTH " ++ ?date_time_warning,
               arity = "1",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052091841033.aspx"
              },
         #help{name = "N",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091871033.aspx"
              },
         #help{name = "NA",
               warning = "",
               arity = "1",
               category = "Information",
               text = "http://office.microsoft.com/en-us/excel/HP052091881033.aspx"
              },
         #help{name = "NOT",
               warning = "",
               arity = "1",
               category = "Logical",
               text = "http://office.microsoft.com/en-us/excel/HP052091961033.aspx"
              },
         #help{name = "NOW",
               warning = "",
               arity = "0",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052091971033.aspx"
              },
         #help{name = "NPER",
               warning = "",
               arity = "3/4/5",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052091981033.aspx"
              },
         #help{name = "NPV",
               warning = "",
               arity = "infinite",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052091991033.aspx"
              },
         #help{name = "ODD",
               warning = "",
               arity = "",
               category = "",
               text = "http://office.microsoft.com/en-us/excel/HP052092031033.aspx"
              },
         #help{name = "OR",
               warning = "",
               arity = "infinite",
               category = "Logical",
               text = "http://office.microsoft.com/en-us/excel/HP052092091033.aspx"
              },
         #help{name = "PI",
               warning = "",
               arity = "0",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092141033.aspx"
              },
         #help{name = "PMT",
               warning = "",
               arity = "4/5/6",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052092151033.aspx"
              },
         #help{name = "POWER",
               warning = "",
               arity = "2",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092171033.aspx"
              },
         #help{name = "PROPER",
               warning = "",
               arity = "1",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052092241033.aspx"
              },
         #help{name = "PV",
               warning = "",
               arity = "3/4/5",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052092251033.aspx"
              },
         #help{name = "RADIANS",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092281033.aspx"
              },
         #help{name = "RATE",
               warning = "",
               arity = "3/4/5/6",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052092321033.aspx"
              },
         #help{name = "REPLACE",
               warning = "",
               arity = "",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052092351033.aspx"
              },
         #help{name = "REPT",
               warning = "",
               arity = "2",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052092361033.aspx"
              },
         #help{name = "ROUND",
               warning = "",
               arity = "2",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092391033.aspx"
              },
         #help{name = "ROWS",
               warning = "",
               arity = "1",
               category = "Lookup",
               text = "http://office.microsoft.com/en-us/excel/HP052092461033.aspx"
              },
         #help{name = "SECOND",
               warning = "Function SECOND " ++ ?date_time_warning,
               arity = "1",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052092511033.aspx"
              },
         #help{name = "SIN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092571033.aspx"
              },
         #help{name = "SLN",
               warning = "",
               arity = "3",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052092631033.aspx"
              },
         #help{name = "SQRT",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092691033.aspx"
              },
         #help{name = "STDEV",
               warning = "",
               arity = "infinite",
               category = "Statistical",
               text = "http://office.microsoft.com/en-us/excel/HP052092771033.aspx"
              },
         #help{name = "SUM",
               warning = "",
               arity = "infinite",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052092901033.aspx"
              },
         #help{name = "SYD",
               warning = "",
               arity = "4",
               category = "Financial",
               text = "http://office.microsoft.com/en-us/excel/HP052093021033.aspx"
              },
         #help{name = "T",
               warning = "",
               arity = "1",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052093041033.aspx"
              },
         #help{name = "TAN",
               warning = "",
               arity = "1",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052093061033.aspx"
              },
         #help{name = "TIME",
               warning = "",
               arity = "3",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052093151033.aspx"
              },
         #help{name = "TRUE",
               warning = "",
               arity = "0",
               category = "Logical",
               text = "http://office.microsoft.com/en-us/excel/HP052093231033.aspx"
              },
         #help{name = "TRUNC",
               warning = "",
               arity = "1/2",
               category = "Maths",
               text = "http://office.microsoft.com/en-us/excel/HP052093241033.aspx"
              },
         #help{name = "UPPER",
               warning = "",
               arity = "1",
               category = "Text And Data",
               text = "http://office.microsoft.com/en-us/excel/HP052093271033.aspx"
              },
         #help{name = "YEAR",
               warning = "Function YEAR " ++ ?date_time_warning,
               arity = "1",
               category = "Date And Time",
               text = "http://office.microsoft.com/en-us/excel/HP052093431033.aspx"
              }
        ]).
