%%% This file contains the macro definitions of the various
%%% Excel file formats - the excel base tokens
%%% At the moment only BIFF8's are being encoded

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% These constants describe the type of errors in an array
%%% which is appended to the Reverse Polish Notation token stream
%%% in encoding a formula in an Excel file
%%% They are descibed in Section 2.5.6

%% 00H #NULL! Intersection of two cell ranges is empty
-define(NullError,0).
%% 07H #DIV/0! Division by zero
-define(DivZeroError,7).
%% 0FH #VALUE! Wrong type of operand
-define(ValueError,15).
%% 17H #REF! Illegal or deleted cell reference
-define(RefError,23).
%% 1DH #NAME? Wrong function or range name
-define(NameError,29).
%% 24H #NUM! Value range overflow
-define(NumError,36).
%% 2AH #N/A
-define(NAError,42).