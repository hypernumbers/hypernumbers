%%% Copyright (C) 2007-2014 Hypernumbers Ltd
%%% This module is licensed under the Erlang Public License V1.0
%%% This file contains the constants for attributes as defined in
%%% Section 5.9.22 of excelfileformat.pdf V1.40

%% 01H 04H Section 5.99.2 of excelfileformatV1.40.pfd
-define(InternalReferences,1025).
%% 01H 3AH Section 5.99.3 of excelfileformatV1.40.pfd
-define(Add_In_Fns1,1).
-define(Add_In_Fns2,14849).
%% Section 5.99.4 of excelfileformatV1.40.pfd
-define(DDE_OLE,0).

%% These next constants are defined on Page 392 in the book
%% Microsoft Excel 97 Developers Kit
-define(chEmpty,0).
-define(chEncode,1).
-define(chSelf,2).

-define(chVolume,1).
-define(chSameVolume,2).
-define(chDownDir,3).
-define(chUpDir,4).
-define(chStartUpDir,6).
-define(chAltStartUpDir,7).
-define(chLibDir,8).

%% this is the marker for a name that refers to an external Excel file
-define(EXTERNALBOOK,-2).
