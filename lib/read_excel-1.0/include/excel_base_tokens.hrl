%%% This file contains the macro definitions of the various
%%% Excel file formats - the excel base tokens
%%% At the moment only BIFF8's are being encoded

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% They are descibed in Section 3.4.7

%% TID  Token name	BIFF2	BIFF3	BIFF4	BIFF5/7	BIFF8
%% 00H	Notused	—	—	—	—	—
%% 01H	tExp    	4	5	5	5	5
-define(tExp,1).
%% 02H	tTbl    	4	5	5	5	5
-define(tTbl,2).
%% 03H	tAdd    	1	1	1	1	1
-define(tAdd,3).
%% 04H	tSub    	1	1	1	1	1
-define(tSub,4).
%% 05H	tMul    	1	1	1	1	1
-define(tMul,5).
%% 06H	tDiv    	1	1	1	1	1
-define(tDiv,6).
%% 07H	tPower  	1	1	1	1	1
-define(tPower,7).
%% 08H	tConcat 	1	1	1	1	1
-define(tConcat,8).
%% 09H	tLT     	1	1	1	1	1
-define(tLT,9).
%% 0AH	tLE     	1	1	1	1	1
-define(tLE,10).
%% 0BH	tEQ     	1	1	1	1	1
-define(tEQ,11).
%% 0CH	tGE     	1	1	1	1	1
-define(tGE,12).
%% 0DH	tGT     	1	1	1	1	1
-define(tGT,13).
%% 0EH	tNE     	1	1	1	1	1
-define(tNE,14).
%% 0FH	tIsect  	1	1	1	1	1
-define(tIsect,15).
%% 10H	tList   	1	1	1	1	1
-define(tList,16).
%% 11H	tRange  	1	1	1	1	1
-define(tRange,17).
%% 12H	tUplus  	1	1	1	1	1
-define(tUplus,18).
%% 13H	tUminus 	1	1	1	1	1
-define(tUminus,19).
%% 14H	tPercent	1	1	1	1	1
-define(tPercent,20).
%% 15H	tParen  	1	1	1	1	1
-define(tParen,21).
%% 16H	tMissArg	1	1	1	1	1
-define(tMissArg,22).
%% 17H	tStr    	var.	var.	var.	var.	var.
-define(tStr,23).
%% 18H	tNlr    	—	—	—	—	var.
-define(tNlr,24).
%% 19H	tAttr   	var.	var.	var.	var.	var.
-define(tAttr,25).
%% 1AH	tSheet  	8	11	11	—	—
%%-define(tSheet,26).
%% 1BH	tEndSheet	4	5	5	—	—
%%-define(tEndSheet,27).
%% 1CH	tErr    	2	2	2	2	2
-define(tErr,28).
%% 1DH	tBool   	2	2	2	2	2
-define(tBool,29).
%% 1EH	tInt    	3	3	3	3	3
-define(tInt,30).
%% 1FH	tNum    	9	9	9	9	9
-define(tNum,31).
