%%% This file contains macro definitions for various record costants
%%% in Microsoft Excel BIFF8 formats

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% They are descibed in Section 2.5

%%% The constants are listed here alphabetically by COMMON RECORD SUBSTRUCTURE
%%% that is to say for every record in microsoftbiff.hrl

%% RK Common Record Substructure BIFF8 Section 2.5.5 excelfileformat.pdf V1.40
-define(CRS_RK_UNCHANGED,0).
-define(CRS_RK_SHIFT_DOWN_BY_100,1).
-define(CRS_RK_FLOATING_POINT,0).
-define(CRS_RK_INTEGER,1).

%% UNICODE 16 Common Record Substructure BIFF8 Section 2.5.3
%% excelfileformat.pdf V1.40
-define(CRS_UNI16_UNCOMPRESSED,1).
-define(CRS_UNI16_ASIAN,4).
-define(CRS_UNI16_RICH_TEXT,8).
