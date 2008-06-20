%%% This file contains macro definitions for various record constants
%%% in Microsoft Excel BIFF8 formats

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% They are descibed in Section 6.

%%% The constants are listed here alphabetically by RECORD NAME
%%% that is to say for every record in microsoftbiff.hrl there
%%% may or may not be associated record costants - but if there
%%% are they will be grouped here by the microsoftbiff.hrl
%%% RECORD NAME

%% XF BIFF8 Section 4.61 of excelfileformat.pdf V1.41
-define(rc_CELL_XF,0).
-define(rc_STYLE_XF,1).

%% BOF BIFF8 Section 5.8.1 excelfileformat.pdf V1.40
-define(rc_BOF_WorkbookGlobals,5).
-define(rc_BOF_VBModule,6).
-define(rc_BOF_Worksheet,16).
-define(rc_BOF_Chart,32).
-define(rc_BOF_MacroSheet,64).
-define(rc_BOF_Workspace,256).

%% BOUNDSHEET BIFF8 B Section 5.12 excelfileformat.pdf V1.40
-define(rc_BOUNDSHEET_Visible,0).
-define(rc_BOUNDSHEET_Hidden,1).
-define(rc_BOUNDSHEET_StrongHidden,2).
-define(rc_BOUNDSHEET_Worksheet,0).
-define(rc_BOUNDSHEET_Chart,2).
-define(rc_BOUNDSHEET_VBModule,6).

%% FORMULA BIFF8 Section 5.47 excelfileformat.pdf V1.40
-define(rc_FORMULA_ALWAYS_RECALC_BITFLAG,1).
-define(rc_FORMULA_CALC_ON_OPEN_BITFLAG,2).
-define(rc_FORMULA_PART_SHARED_BITFLAG,8).
-define(rc_FORMULA_STRING,0).
-define(rc_FORMULA_BOOLEAN,1).
-define(rc_FORMULA_ERROR,2).
-define(rc_FORMULA_EMPTY_CELL,3).
