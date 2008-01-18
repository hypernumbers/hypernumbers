%%% This file contains the macro definitions of the various
%%% Excel file formats - the excel base tokens
%%% At the moment only BIFF8's are being encoded

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% They are descibed in Section 3.4.7
%% Token ID	Token name      BIFF2	BIFF3	BIFF4	BIFF5/7	BIFF8
%% 20H 40H 60H	tArray  	7	8	8	8	8
-define(tArrayR,32).
-define(tArrayV,64).
-define(tArrayA,96).
%% 21H 41H 61H	tFunc   	3	3	4	4	4
-define(tFuncR,33).
-define(tFuncV,65).
-define(tFuncA,97).
%% 22H 42H 62H	tFuncVar	4	4	5	5	5
-define(tFuncVarR,34).
-define(tFuncVarV,66).
-define(tFuncVarA,98).
%% 23H 43H 63H	tName   	8	11	11	15	5
-define(tNameR,35).
-define(tNameV,67).
-define(tNameA,99).
%% 24H 44H 64H	tRef    	4	4	4	4	5
-define(tRefR,36).
-define(tRefV,68).
-define(tRefA,100).
%% 25H 45H 65H	tArea   	7	7	7	7	9
-define(tAreaR,37).
-define(tAreaV,69).
-define(tAreaA,101).
%% 26H 46H 66H	tMemArea	5	7	7	7	7
-define(tMemAreaR,38).
-define(tMemAreaV,70).
-define(tMemAreaA,102).
%% 27H 47H 67H	tMemErr 	5	7	7	7	7
-define(tMemErrR,39).
-define(tMemErrV,71).
-define(tMemErrA,103).
%% 28H 48H 68H	tMemNoMem	5	7	7	7	7
-define(tMemNoMemR,40).
-define(tMemNoMemV,72).
-define(tMemNoMemA,104).
%% 29H 49H 69H	tMemFunc	2	3	3	3	3
-define(tMemFuncR,41).
-define(tMemFuncV,73).
-define(tMemFuncA,105).
%% 2AH 4AH 6AH	tRefErr 	4	4	4	4	5
-define(tRefErrR,42).
-define(tRefErrV,74).
-define(tRefErrA,106).
%% 2BH 4BH 6BH	tAreaErr	7	7	7	7	9
-define(tAreaErrR,43).
-define(tAreaErrV,75).
-define(tAreaErrA,107).
%% 2CH 4CH 6CH	tRefN   	4	4	4	4	5
-define(tRefNR,44).
-define(tRefNV,76).
-define(tRefNA,108).
%% 2DH 4DH 6DH	tAreaN  	7	7	7	7	9
-define(tAreaNR,45).
-define(tAreaNV,77).
-define(tAreaNA,109).
%% 2EH 4EH 6EH	tMemAreaN	2	3	3	3	3
%% -define(tMemAreaNR,46).
%% -define(tMemAreaNV,78).
%% -define(tMemAreaNA,110).
%% 2FH 4FH 6FH	tMemNoMemN	2	3	3	3	3
%% -define(tMemNoMemNR,47).
%% -define(tMemNoMemNV,79).
%% -define(tMemNoMemNA,111).
%% 30H 50H 70H	Not used	—	—	—	—	—
%% 31H 51H 71H	Not used	—	—	—	—	—
%% 32H 52H 72H	Not used	—	—	—	—	—
%% 33H 53H 73H	Not used	—	—	—	—	—
%% 34H 54H 74H	Not used	—	—	—	—	—
%% 35H 55H 75H	Not used	—	—	—	—	—
%% 36H 56H 76H	Not used	—	—	—	—	—
%% 37H 57H 77H	Not used	—	—	—	—	—
%% 38H 58H 78H	tFuncCE 	3	3	—	—	—
%%-define(tFuncCE,3692664).
%% 39H 59H 79H	tNameX  	—	—	—	25	7
-define(tNameXR,57).
-define(tNameXV,89).
-define(tNameXA,121).
%% 3AH 5AH 7AH	tRef3d  	—	—	—	18	7
-define(tRef3dR,58).
-define(tRef3dV,90).
-define(tRef3dA,122).
%% 3BH 5BH 7BH	tArea3d 	—	—	—	21	11
-define(tArea3dR,59).
-define(tArea3dV,91).
-define(tArea3dA,123).
%% 3CH 5CH 7CH	tRefErr3d	—	—	—	18	7
-define(tRefErr3dR,60).
-define(tRefErr3dV,92).
-define(tRefErr3dA,124).
%% 3DH 5DH 7DH	tAreaErr3d	—	—	—	21	11
-define(tAreaErr3dR,61).
-define(tAreaErr3dV,93).
-define(tAreaErr3dA,125).
%% 3EH 5EH 7EH	Not used	—	—	—	—	—
%% 3FH 5FH 7FH	Not used	—	—	—	—	—
