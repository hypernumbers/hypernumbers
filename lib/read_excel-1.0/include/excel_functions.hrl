%%% This file contains the macro definitions used in parsing the structure
%%% of an Excel file

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% It contains the ID's of the built in sheet functions as defined in
%%% Section 3.11

-define(?COUNT,0).
-define(?IF,1).
-define(?ISNA,2).
-define(?ISERROR,3).
-define(?SUM,4).
-define(?AVERAGE,5).
-define(?MIN,6).
-define(?MAX,7).
-define(?ROW,8).
-define(?COLUMN,9).
-define(?NA,10).
-define(?NPV,11).
-define(?STDEV,12).
-define(?DOLLAR,13).
-define(?FIXED,14).
-define(?SIN,15).
-define(?COS,16).
-define(?TAN,17).
-define(?ARCTAN,18).
-define(?PI,19).
-define(?SQRT,20).
-define(?EXP,21).
-define(?LN,22).
-define(?LOG10,23).
-define(?ABS,24).
-define(?INT,25).
-define(?SIGN,26).
-define(?ROUND,27).
-define(?LOOKUP,28).
-define(?INDEX,29).
-define(?REPT,30).
-define(?MID,31).
-define(?LEN,32).
-define(?VALUE,33).
-define(?TRUE,34).
-define(?FALSE,35).
-define(?AND,36).
-define(?OR,37).
-define(?NOT,38).
-define(?MOD,39).
-define(?DCOUNT,40).
-define(?DSUM,41).
-define(?DAVERAGE,42).
-define(?DMIN,43).
-define(?DMAX,44).
-define(?DSTDEV,45).
-define(?VAR,46).
-define(?DVAR,47).
-define(?TEXT,48).
-define(?LINEST,49).
-define(?TREND,50).
-define(?LOGEST,51).
-define(?GROWTH,52).
%% 53 - 55 MISSING...
-define(?PV,56).
-define(?FV,57).
-define(?NPER,58).
-define(?PMT,59).
-define(?RATE,60).
-define(?MIRR,61).
-define(?IRR.62).
-define(?RAND,63).
-define(?MATCH,64).
-define(?DATE,65).
-define(?TIME,66).
-define(?DAY,67).
-define(?MONTH,68).
-define(?YEAR,69).
-define(?WEEKDAY,70).
-define(?HOUR,71).
-define(?MINUTE,72).
-define(?SECOND,73).
-define(?NOW,74).
-define(?AREAS,75).
-define(?ROWS,76).
-define(?COLUMNS,77).
-define(?OFFSET,78).
%% 79 TO 81 MISSING...
-define(?SEARCH,82).
-define(?TRANSPOSE,83).
%% 84 TO 85 MISSING...
-define(?TYPE,86).
%% 87 TO 96 MISSING...
-define(?ATAN2,97).
-define(?ASIN,98).
-define(?ACOS,99).
-define(?CHOOSE,100).
-define(?HLOOKUP,101).
-define(?VLOOKUP,102).
%% 103 TO 104 MISSING...
-define(?ISREF,105).
%% 106 TO 108 MISSING...
-define(?LOG,109).
%% 110 MISSING...
-define(?CHAR,111).
-define(?UPPER,112).
-define(?LOWER,113).
-define(?PROPER,114).
-define(?LEFT,115).
-define(?RIGHT,116).
-define(?EXACT,117).
-define(?TRIM,118).
-define(?REPLACE,119).
-define(?SUBSTITUTE,120).
-define(?CODE,121).
%% 122 TO 123 MISSING...
-define(?FIND,124).
-define(?CELL,125).
-define(?ISERR,126).
-define(?ISTEXT,127).
-define(?ISNUMBER,128).
-define(?ISBLANK,129).
-define(?T,130).
-define(?N,131).
%% 132 TO 139 MISSING...
-define(?DATEVALUE,140).
-define(?TIMEVALUE,141).
-define(?SLN,142).
-define(?SYD,143).
-define(?DDB,144).
%% 145 TO 147 MISSING...
-define(?INDIRECT,148).
%% 149 TO 161 MISSING...
-define(?CLEAN,162).
-define(?MDETERM,163).
-define(?MINVERSE,164).
-define(?MMULT,165).
%% 166 MISSING...
-define(?IPMT,167).
-define(?PPMT,168).
-define(?COUNTA,169).
%% 170 TO 182 MISSING...
-define(?PRODUCT,183).
-define(?FACT,184).
%% 185 TO 190 MISSING...
-define(?DPRODUCT,191).
-define(?ISNONTEXT,192).
-define(?STDEVP,193).
-define(?VARP,194).
-define(?DSDEVP,195).
-define(?DVARP,196).
-define(?TRUNC,197).
-define(?ISLOGICAL,198).
-define(?DCOUNTA,199).
%% 200 TO 203 MISSING...
-define(?USDOLLAR,204).
-define(?FINDB,205).
-define(?SEARCHB,206).
-define(?REPLACEB,207).
-define(?LEFTB,208).
-define(?RIGHTB,209).
-define(?MIDB,210).
-define(?LENB,211).
-define(?ROUNDUP,212).
-define(?ROUNDDOWN,213).
-define(?ASC,214).
-define(?DBSC,215).
-define(?RANK,216).
%% 217 TO 218 MISSING...
-define(?ADDRESS,219).
-define(?DAYS360,220).
-define(?TODAY,221).
-define(?VDB,222).
%% 223 TO 226 MISING...
-define(?MEDIAN,227).
-define(?SUMPRODUCT,228).
-define(?SINH,229).
-define(?COSH,230).
-define(?TANH,231).
-define(?ASINH,232).
-define(?ACOSH,233).
-define(?ATANH,234).
-define(?DFET,235).
%% 236 TO 243 MISSING...
-define(?INFO,244).
%% 245 TO 246 MISSING...
-define(?DB,247).
%% 248 TO 251 MISSING...
-define(?FREQUENCY,252).
%% 253 TO 260 MISSING...
-define(?ERRORTYPE,261).
%% 262 TO 268 MISSING...
-define(?AVEDEV,269).
-define(?BETADIST,270).
-define(?GAMMALN,271).
-define(?BETAINV,272).
-define(?BINOMDIST,273).
-define(?CHIDIST,274).
-define(?CHIINV,275).
-define(?COMBIN,276).
-define(?CONFIDENCE,277).
-define(?CRITBINOM,278).
-define(?EVEN,279).
-define(?EXPONDIST,280).
-define(?FDIST,281).
-define(?FINV,282).
-define(?FISHER,283).
-define(?FISHERINV,284).
-define(?FLOOR,285).
-define(?GAMMADIST,286).
-define(?GAMMAINV,287).
-define(?CEILING,288).
-define(?HYPGEOMVERT,289).
-define(?LOGNORMDIST,290).
-define(?LOGINV,291).
-define(?NEGBINOMDIST,292).
-define(?NORMDIST,293).
-define(?NORMSDIST,294).
-define(?NORMINV,295).
-define(?NORMSINB,296).
-define(?STANDARDIZE,297).
-define(?ODD,298).
-define(?PERMUT,299).
-define(?POISSON,300).
-define(?TDIST,301).
-define(?WEIBULL,302).
-define(?SUMXMY2,303).
-define(?SUMX2MY2,304).
-define(?SUMX2PY2,305).
-define(?CHITEST,306).
-define(?CORREL,307).
-define(?COVAR,308).
-define(?FORECAST,309).
-define(?FTEST,310).
-define(?INTERCEPT,311).
-define(?PEARSON,312).
-define(?RSQ,313).
-define(?STEYX,314).
-define(?SLOPE,315).
-define(?TTEST,316).
-define(?PROB,317).
-define(?DEVSQ,318).
-define(?GEOMEAN,319).
-define(?HARMEAN,320).
-define(?SUMSQ,321).
-define(?KURT,322).
-define(?SKEW,323).
-define(?ZTEST,324).
-define(?LARGE,325).
-define(?SMALL,326).
-define(?QUARTILE,327).
-define(?PERCENTILE,328).
-define(?MODE,330).
-define(?TRIMMEAN,331).
-define(?TINV,332).
%% 333 TO 335 MISSING...
-define(?CONCATENATE,336).
-define(?POWER,337).
%% 338 TO 341 MISSING...
-define(?RADIANS,342).
-define(?DEGREES,343).
-define(?SUBOTAL,344).
-define(?SUMIF,345).
-define(?COUNTIF,346).
-define(?COUNTBLANK,347).
%% 348 TO 349 MISSING...
-define(?ISPMT,350).
-define(?DATEDIF,351).
-define(?DATESTRING,352).
-define(?NUMBERSTRING,353).
-define(?ROMAN,354).
%% 355 TO 357 MISSING...
-define(?GETPIVOTDATA,358).
-define(?HYPERLINK,359).
-define(?PHONETIC,360).
-define(?AVERAGEA,361).
-define(?MAXA,362).
-define(?MINA,363).
-define(?STDEVPA,364).
-define(?VARPA,365).
-define(?STDEVA,366).
-define(?VARA,367).
