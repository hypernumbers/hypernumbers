%%% This file contains the macro definitions of the various
%%% Excel file formats - the excel base tokens
%%% At the moment only BIFF8's are being encoded

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% These constants describe the type of elements in an array
%%% which is appended to the Reverse Polish Notation token stream
%%% in encoding a formula in an Excel file
%%% They are descibed in Section 2.5.7
-define(EmptyArrayEl,0).
-define(NumberArrayEl,1).
-define(StringArrayEl,2).
-define(BooleanArrayEl,4).
-define(ErrorArrayEl,16).