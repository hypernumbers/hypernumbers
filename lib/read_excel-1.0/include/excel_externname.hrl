%%% This file contains the macro definitions that pertain to
%%% EXTERNNAME records

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Section 5.38.4 of Version 1.40 of that document

%% these flags describe the various options
-define(STANDARD,0).
-define(BUILT_IN,1).
-define(MANUAL_DDE,8).
-define(AUTO_DDE,9).
-define(MANUAL_OLE,16).
-define(AUTO_OLE,17).

