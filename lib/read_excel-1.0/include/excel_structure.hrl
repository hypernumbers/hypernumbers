%%% This file contains the macro definitions used in parsing the structure
%%% of an Excel file

%% Trial and error indicates that this is the uncompressed UTF-16 representation
%% of the name of the Workbook stream
-define(EXCEL_WORKBOOK,{'utf16-16',<<87,0,111,0,114,0,107,0,98,0,111,0,111,0,107,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}).

%% Trial and error indicates that this is the uncompressed UTF-16 representation
%% of the name of the Root Entry stream
-define(ROOT_ENTRY,{'utf16-16',<<82,0,111,0,111,0,116,0,32,0,69,0,110,0,116,0,114,0,121,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}).
