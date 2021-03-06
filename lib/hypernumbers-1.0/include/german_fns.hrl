%%-------------------------------------------------------------------
%% @author    Gordon Guthrie
%% @copyright (C) 2009-2014, Hypernumbers Ltd
%% @doc       This file is generated by generate_terms.rb
%%
%% @end
%% Created :  2 Apr 2009 by <gordon@hypernumbers.com>
%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-define(german_fns,
    [
{'german', "IPMT", [90,73,78,83,90]},
{'german', "SMALL", [75,75,76,69,73,78,83,84,69]},
{'german', "PI", [80,73]},
{'german', "ISPMT", [73,83,80,77,84]},
{'german', "STDEVPA", [83,84,65,66,87,78,65]},
{'german', "MATCH", [86,69,82,71,76,69,73,67,72]},
{'german', "HYPERLINK", [72,89,80,69,82,76,73,78,75]},
{'german', "SECOND", [83,69,75,85,78,68,69]},
{'german', "ISERROR", [73,83,84,70,69,72,76,69,82]},
{'german', "ODD", [85,78,71,69,82,65,68,69]},
{'german', "SKEW", [83,67,72,73,69,70,69]},
{'german', "ISREF", [73,83,84,66,69,90,85,71]},
{'german', "LOWER", [75,76,69,73,78]},
{'german', "RIGHT", [82,69,67,72,84,83]},
{'german', "VLOOKUP", [83,86,69,82,87,69,73,83]},
{'german', "CODE", [67,79,68,69]},
{'german', "SIN", [83,73,78]},
{'german', "TRIMMEAN", [71,69,83,84,85,84,90,84,77,73,84,84,69,76]},
{'german', "LOG10", [76,79,71,49,48]},
{'german', "HOUR", [83,84,85,78,68,69]},
{'german', "DATE", [68,65,84,85,77]},
{'german', "POISSON", [80,79,73,83,83,79,78]},
{'german', "DDB", [71,68,65]},
{'german', "CELL", [90,69,76,76,69]},
{'german', "EXACT", [73,68,69,78,84,73,83,67,72]},
{'german', "SQRT", [87,85,82,90,69,76]},
{'german', "HLOOKUP", [87,86,69,82,87,69,73,83]},
{'german', "ISLOGICAL", [73,83,84,76,79,71]},
{'german', "FACT", [70,65,75,85,76,84,195,132,84]},
{'german', "LOGEST", [82,75,80]},
{'german', "DVAR", [68,66,86,65,82,73,65,78,90]},
{'german', "RTD", [82,84,68]},
{'german', "SLOPE", [83,84,69,73,71,85,78,71]},
{'german', "LOGINV", [76,79,71,73,78,86]},
{'german', "SLN", [76,73,65]},
{'german', "BINOMDIST", [66,73,78,79,77,86,69,82,84]},
{'german', "ROUNDUP", [65,85,70,82,85,78,68,69,78]},
{'german', "ROUND", [82,85,78,68,69,78]},
{'german', "ACOSH", [65,82,67,67,79,83,72,89,80]},
{'german', "GAMMAINV", [71,65,77,77,65,73,78,86]},
{'german', "COVAR", [75,79,86,65,82]},
{'german', "FTEST", [70,84,69,83,84]},
{'german', "IF", [87,69,78,78]},
{'german', "DGET", [68,66,65,85,83,90,85,71]},
{'german', "INDEX", [73,78,68,69,88]},
{'german', "WEEKDAY", [87,79,67,72,69,78,84,65,71]},
{'german', "GETPIVOTDATA", [71,69,84,80,73,86,79,84,68,65,84,65]},
{'german', "SEARCH", [83,85,67,72,69,78]},
{'german', "UNION", [85,78,73,79,78]},
{'german', "NPV", [78,66,87]},
{'german', "ATANH", [65,82,67,84,65,78,72,89,80]},
{'german', "ISNONTEXT", [73,83,84,75,84,69,88,84]},
{'german', "CHIINV", [67,72,73,73,78,86]},
{'german', "SUMXMY2", [83,85,77,77,69,88,77,89,50]},
{'german', "TRIM", [71,76,195,132,84,84,69,78]},
{'german', "N", [78]},
{'german', "ZTEST", [71,84,69,83,84]},
{'german', "VALUE", [87,69,82,84]},
{'german', "BETADIST", [66,69,84,65,86,69,82,84]},
{'german', "PV", [66,87]},
{'german', "TREND", [84,82,69,78,68]},
{'german', "AVEDEV", [77,73,84,84,69,76,65,66,87]},
{'german', "DSTDEV", [68,66,83,84,68,65,66,87]},
{'german', "NORMSDIST", [83,84,65,78,68,78,79,82,77,86,69,82,84]},
{'german', "YEAR", [74,65,72,82]},
{'german', "ERROR.TYPE", [70,69,72,76,69,82,46,84,89,80]},
{'german', "FIND", [70,73,78,68,69,78]},
{'german', "DVARP", [68,66,86,65,82,73,65,78,90,69,78]},
{'german', "SINH", [83,73,78,72,89,80]},
{'german', "SUM", [83,85,77,77,69]},
{'german', "COLUMNS", [83,80,65,76,84,69,78]},
{'german', "RAND", [90,85,70,65,76,76,83,90,65,72,76]},
{'german', "GROWTH", [86,65,82,73,65,84,73,79,78]},
{'german', "GEOMEAN", [71,69,79,77,73,84,84,69,76]},
{'german', "DMIN", [68,66,77,73,78]},
{'german', "T", [84]},
{'german', "ATAN", [65,82,67,84,65,78]},
{'german', "INTERCEPT", [65,67,72,83,69,78,65,66,83,67,72,78,73,84,84]},
{'german', "FIXED", [70,69,83,84]},
{'german', "TRUE", [87,65,72,82]},
{'german', "COMBIN", [75,79,77,66,73,78,65,84,73,79,78,69,78]},
{'german', "DCOUNT", [68,66,65,78,90,65,72,76]},
{'german', "FISHER", [70,73,83,72,69,82]},
{'german', "REPT", [87,73,69,68,69,82,72,79,76,69,78]},
{'german', "MODE", [77,79,68,65,76,87,69,82,84]},
{'german', "CLEAN", [83,195,132,85,66,69,82,78]},
{'german', "TDIST", [84,86,69,82,84]},
{'german', "FV", [90,87]},
{'german', "LN", [76,78]},
{'german', "COUNTIF", [90,195,132,72,76,69,78,87,69,78,78]},
{'german', "EXP", [69,88,80]},
{'german', "DAY", [84,65,71]},
{'german', "LOOKUP", [86,69,82,87,69,73,83]},
{'german', "CEILING", [79,66,69,82,71,82,69,78,90,69]},
{'german', "TTEST", [84,84,69,83,84]},
{'german', "MONTH", [77,79,78,65,84]},
{'german', "ADDRESS", [65,68,82,69,83,83,69]},
{'german', "OFFSET", [66,69,82,69,73,67,72,46,86,69,82,83,67,72,73,69,66,69,78]},
{'german', "TYPE", [84,89,80]},
{'german', "RANK", [82,65,78,71]},
{'german', "LOGNORMDIST", [76,79,71,78,79,82,77,86,69,82,84]},
{'german', "CORREL", [75,79,82,82,69,76]},
{'german', "CHIDIST", [67,72,73,86,69,82,84]},
{'german', "COSH", [67,79,83,72,89,80]},
{'german', "PROB", [87,65,72,82,83,67,72,66,69,82,69,73,67,72]},
{'german', "CHITEST", [67,72,73,84,69,83,84]},
{'german', "RATE", [90,73,78,83]},
{'german', "RADIANS", [82,65,68,73,65,78,84]},
{'german', "INFO", [73,78,70,79]},
{'german', "MINUTE", [77,73,78,85,84,69]},
{'german', "WEIBULL", [87,69,73,66,85,76,76]},
{'german', "AREAS", [66,69,82,69,73,67,72,69]},
{'german', "ASIN", [65,82,67,83,73,78]},
{'german', "NEGBINOMDIST", [78,69,71,66,73,78,79,77,86,69,82,84]},
{'german', "MEDIAN", [77,69,68,73,65,78]},
{'german', "MMULT", [77,77,85,76,84]},
{'german', "TANH", [84,65,78,72,89,80]},
{'german', "PRODUCT", [80,82,79,68,85,75,84]},
{'german', "REPLACE", [69,82,83,69,84,90,69,78]},
{'german', "BETAINV", [66,69,84,65,73,78,86]},
{'german', "HYPGEOMDIST", [72,89,80,71,69,79,77,86,69,82,84]},
{'german', "OR", [79,68,69,82]},
{'german', "ISNA", [73,83,84,78,86]},
{'german', "AVERAGEA", [77,73,84,84,69,76,87,69,82,84,65]},
{'german', "NOT", [78,73,67,72,84]},
{'german', "PERCENTILE", [81,85,65,78,84,73,76]},
{'german', "ISERR", [73,83,84,70,69,72,76]},
{'german', "ROMAN", [82,195,150,77,73,83,67,72]},
{'german', "PEARSON", [80,69,65,82,83,79,78]},
{'german', "CONFIDENCE", [75,79,78,70,73,68,69,78,90]},
{'german', "VDB", [86,68,66]},
{'german', "NOW", [74,69,84,90,84]},
{'german', "COS", [67,79,83]},
{'german', "NA", [78,86]},
{'german', "VARA", [86,65,82,73,65,78,90,65]},
{'german', "UPPER", [71,82,79,83,83]},
{'german', "FORECAST", [83,67,72,195,132,84,90,69,82]},
{'german', "FINV", [70,73,78,86]},
{'german', "TINV", [84,73,78,86]},
{'german', "PERCENTRANK", [81,85,65,78,84,73,76,83,82,65,78,71]},
{'german', "FREQUENCY", [72,195,132,85,70,73,71,75,69,73,84]},
{'german', "SIGN", [86,79,82,90,69,73,67,72,69,78]},
{'german', "POWER", [80,79,84,69,78,90]},
{'german', "DCOUNTA", [68,66,65,78,90,65,72,76,50]},
{'german', "HARMEAN", [72,65,82,77,73,84,84,69,76]},
{'german', "PROPER", [71,82,79,83,83,50]},
{'german', "LOG", [76,79,71]},
{'german', "FISHERINV", [70,73,83,72,69,82,73,78,86]},
{'german', "STDEVA", [83,84,65,66,87,65]},
{'german', "MAX", [77,65,88]},
{'german', "LARGE", [75,71,82,195,150,83,83,84,69]},
{'german', "DSTDEVP", [68,66,83,84,68,65,66,87,78]},
{'german', "INDIRECT", [73,78,68,73,82,69,75,84]},
{'german', "MDETERM", [77,68,69,84]},
{'german', "PPMT", [75,65,80,90]},
{'german', "TIME", [90,69,73,84]},
{'german', "NORMINV", [78,79,82,77,73,78,86]},
{'german', "RSQ", [66,69,83,84,73,77,77,84,72,69,73,84,83,77,65,83,83]},
{'german', "DPRODUCT", [68,66,80,82,79,68,85,75,84]},
{'german', "ISNUMBER", [73,83,84,90,65,72,76]},
{'german', "TODAY", [72,69,85,84,69]},
{'german', "DAYS360", [84,65,71,69,51,54,48]},
{'german', "CONCATENATE", [86,69,82,75,69,84,84,69,78]},
{'german', "NPER", [90,90,82]},
{'german', "LINEST", [82,71,80]},
{'german', "CRITBINOM", [75,82,73,84,66,73,78,79,77]},
{'german', "FDIST", [70,86,69,82,84]},
{'german', "FLOOR", [85,78,84,69,82,71,82,69,78,90,69]},
{'german', "DB", [71,68,65,50]},
{'german', "COUNTBLANK", [65,78,90,65,72,76,76,69,69,82,69,90,69,76,76,69,78]},
{'german', "DSUM", [68,66,83,85,77,77,69]},
{'german', "ASINH", [65,82,67,83,73,78,72,89,80]},
{'german', "CHAR", [90,69,73,67,72,69,78]},
{'german', "KURT", [75,85,82,84]},
{'german', "STDEV", [83,84,65,66,87]},
{'german', "ROWS", [90,69,73,76,69,78]},
{'german', "SUMPRODUCT", [83,85,77,77,69,78,80,82,79,68,85,75,84]},
{'german', "VAR", [86,65,82,73,65,78,90]},
{'german', "DATEVALUE", [68,65,84,87,69,82,84]},
{'german', "IRR", [73,75,86]},
{'german', "CHOOSE", [87,65,72,76]},
{'german', "TIMEVALUE", [90,69,73,84,87,69,82,84]},
{'german', "PERMUT", [86,65,82,73,65,84,73,79,78,69,78]},
{'german', "DEGREES", [71,82,65,68]},
{'german', "COUNT", [65,78,90,65,72,76]},
{'german', "MINVERSE", [77,73,78,86]},
{'german', "MID", [84,69,73,76]},
{'german', "ISTEXT", [73,83,84,84,69,88,84]},
{'german', "VARP", [86,65,82,73,65,78,90,69,78]},
{'german', "STANDARDIZE", [83,84,65,78,68,65,82,68,73,83,73,69,82,85,78,71]},
{'german', "LEFT", [76,73,78,75,83]},
{'german', "ROUNDDOWN", [65,66,82,85,78,68,69,78]},
{'german', "COUNTA", [65,78,90,65,72,76,50]},
{'german', "AVERAGE", [77,73,84,84,69,76,87,69,82,84]},
{'german', "STDEVP", [83,84,65,66,87,78]},
{'german', "COLUMN", [83,80,65,76,84,69]},
{'german', "SUMIF", [83,85,77,77,69,87,69,78,78]},
{'german', "STEYX", [83,84,70,69,72,76,69,82,89,88]},
{'german', "LEN", [76,195,132,78,71,69]},
{'german', "GAMMALN", [71,65,77,77,65,76,78]},
{'german', "SUMX2MY2", [83,85,77,77,69,88,50,77,89,50]},
{'german', "PMT", [82,77,90]},
{'german', "FALSE", [70,65,76,83,67,72]},
{'german', "INTERSECT", [73,78,84,69,82,83,69,67,84]},
{'german', "GAMMADIST", [71,65,77,77,65,86,69,82,84]},
{'german', "SUMSQ", [81,85,65,68,82,65,84,69,83,85,77,77,69]},
{'german', "EVEN", [71,69,82,65,68,69]},
{'german', "ATAN2", [65,82,67,84,65,78,50]},
{'german', "EXPONDIST", [69,88,80,79,78,86,69,82,84]},
{'german', "TRANSPOSE", [77,84,82,65,78,83]},
{'german', "VARPA", [86,65,82,73,65,78,90,69,78,65]},
{'german', "ABS", [65,66,83]},
{'german', "AND", [85,78,68]},
{'german', "ISBLANK", [73,83,84,76,69,69,82]},
{'german', "MOD", [82,69,83,84]},
{'german', "SUBSTITUTE", [87,69,67,72,83,69,76,78]},
{'german', "SYD", [68,73,65]},
{'german', "ACOS", [65,82,67,67,79,83]},
{'german', "NORMSINV", [83,84,65,78,68,78,79,82,77,73,78,86]},
{'german', "SUMX2PY2", [83,85,77,77,69,88,50,80,89,50]},
{'german', "SUBTOTAL", [84,69,73,76,69,82,71,69,66,78,73,83]},
{'german', "NORMDIST", [78,79,82,77,86,69,82,84]},
{'german', "MIN", [77,73,78]},
{'german', "DAVERAGE", [68,66,77,73,84,84,69,76,87,69,82,84]},
{'german', "TEXT", [84,69,88,84]},
{'german', "TAN", [84,65,78]},
{'german', "ROW", [90,69,73,76,69]},
{'german', "MAXA", [77,65,88,65]},
{'german', "MINA", [77,73,78,65]},
{'german', "DOLLAR", [68,77]},
{'german', "MIRR", [81,73,75,86]},
{'german', "TRUNC", [75,195,156,82,90,69,78]},
{'german', "DMAX", [68,66,77,65,88]},
{'german', "QUARTILE", [81,85,65,82,84,73,76,69]},
{'german', "DEVSQ", [83,85,77,81,85,65,68,65,66,87]},
{'german', "INT", [71,65,78,90,90,65,72,76]},
{'1234', '5678', '9999'} % apologies to the gods for this bodge! GG
]).
