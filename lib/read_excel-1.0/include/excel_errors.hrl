%%% Copyright (C) 2007-2014 Hypernumbers Ltd
%%% This module is licensed under the Erlang Public License V1.0
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

%% The records that manage references to sheets other than the
%% current one (both internal and external) uses 'FF' as a marker
%% of a deleted reference
-define(EXTERNALBOOK_REF_ERROR,-1).

%% Excel stores errors as negative integers
%% here is the def of them
-define(ErrDiv0Int,-2146826281).
-define(ErrNAInt,-2146826246).
-define(ErrNameInt,-2146826259).
-define(ErrNullInt,-2146826288).
-define(ErrNumInt,-2146826252).
-define(ErrRefInt,-2146826265).
-define(ErrValueInt,-2146826273).
