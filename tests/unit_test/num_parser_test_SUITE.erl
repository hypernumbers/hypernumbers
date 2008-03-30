%%%-----------------------------------------------------------------------------
%%% File        : num_parser_test_SUITE.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests the parser for the numerical format
%%%
%%%
%%% Created     : 10 Mar 2008 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(num_parser_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

%% Tests 1 to 40 all come from the default custom formats in Excel
%% all tests furth of 40 are bespoke tests
%% all tests are generated from the Excel spreadsheet num_parser_master.xls
%% which is in Subversion alongside this test suite

%% Tests 1
-define(FORMAT1,"0").

-define(VALUE1a,33).
-define(OUTPUT1a,{black,"33"}).

-define(VALUE1b,-33).
-define(OUTPUT1b,{black,"-33"}).

-define(VALUE1c,33.333).
-define(OUTPUT1c,{black,"33"}).

-define(VALUE1d,-33.33).
-define(OUTPUT1d,{black,"-33"}).

-define(VALUE1e,0).
-define(OUTPUT1e,{black,"0"}).

-define(VALUE1f,0.0).
-define(OUTPUT1f,{black,"0"}).

-define(VALUE1g,"bob").
-define(OUTPUT1g,{black,"bob"}).

-define(VALUE1h,0.0003333).
-define(OUTPUT1h,{black,"0"}).

-define(VALUE1i,-0.000033).
-define(OUTPUT1i,{black,"0"}).

-define(VALUE1j,0.000000000000000000000000000003333).
-define(OUTPUT1j,{black,"0"}).

-define(VALUE1k,-0.00000000000000000000000000000033).
-define(OUTPUT1k,{black,"0"}).

-define(VALUE1l,33330000000000000000000000000000000000000000000).
-define(OUTPUT1l,{black,"33330000000000000000000000000000000000000000000"}).

-define(VALUE1m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT1m,{black,"-33330000000000000000000000000000000000000000000"}).

-define(VALUE1n,2.999).
-define(OUTPUT1n,{black,"3"}).

%% Test 2
-define(FORMAT2,"0.00").
-define(VALUE2a,33).
-define(OUTPUT2a,{black,"33.00"}).

-define(VALUE2b,-33).
-define(OUTPUT2b,{black,"-33.00"}).

-define(VALUE2c,33.333).
-define(OUTPUT2c,{black,"33.33"}).

-define(VALUE2d,-33.33).
-define(OUTPUT2d,{black,"-33.33"}).

-define(VALUE2e,0).
-define(OUTPUT2e,{black,"0.00"}).

-define(VALUE2f,0.0).
-define(OUTPUT2f,{black,"0.00"}).

-define(VALUE2g,"bob").
-define(OUTPUT2g,{black,"bob"}).

-define(VALUE2h,0.0003333).
-define(OUTPUT2h,{black,"0.00"}).

-define(VALUE2i,-0.000033).
-define(OUTPUT2i,{black,"0.00"}).

-define(VALUE2j,0.000000000000000000000000000003333).
-define(OUTPUT2j,{black,"0.00"}).

-define(VALUE2k,-0.00000000000000000000000000000033).
-define(OUTPUT2k,{black,"0.00"}).

-define(VALUE2l,33330000000000000000000000000000000000000000000).
-define(OUTPUT2l,{black,"33330000000000000000000000000000000000000000000.00"}).

-define(VALUE2m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT2m,{black,"-33330000000000000000000000000000000000000000000.00"}).

%% Test 3
-define(FORMAT3,"#,##0").

-define(VALUE3a,3333).
-define(OUTPUT3a,{black,"3,333"}).

-define(VALUE3b,-3333).
-define(OUTPUT3b,{black,"-3,333"}).

-define(VALUE3c,3333.333).
-define(OUTPUT3c,{black,"3,333"}).

-define(VALUE3d,-3333.33).
-define(OUTPUT3d,{black,"-3,333"}).

%% Test 4
-define(FORMAT4,"#,##0.00").

-define(VALUE4a,3333).
-define(OUTPUT4a,{black,"3,333.00"}).

-define(VALUE4b,-3333).
-define(OUTPUT4b,{black,"-3,333.00"}).

-define(VALUE4c,3333.33).
-define(OUTPUT4c,{black,"3,333.33"}).

-define(VALUE4d,-3333.33).
-define(OUTPUT4d,{black,"-3,333.33"}).

%% Test 5
-define(FORMAT5,"#,##0;-#,##0").

-define(VALUE5a,3333).
-define(OUTPUT5a,{black,"3,333"}).

-define(VALUE5b,-3333).
-define(OUTPUT5b,{black,"-3,333"}).

-define(VALUE5c,3333.333).
-define(OUTPUT5c,{black,"3,333"}).

-define(VALUE5d,-3333.33).
-define(OUTPUT5d,{black,"-3,333"}).

%% Test 6
-define(FORMAT6,"#,##0;[Red]-#,##0").

-define(VALUE6a,3333).
-define(OUTPUT6a,{black,"3,333"}).

-define(VALUE6b,-3333).
-define(OUTPUT6b,{red,"-3,333"}).

-define(VALUE6c,3333.333).
-define(OUTPUT6c,{black,"3,333"}).

-define(VALUE6d,-3333.33).
-define(OUTPUT6d,{red,"-3,333"}).

%% Test 7
-define(FORMAT7,"#,##0.00;-#,##0.00").

-define(VALUE7a,3333).
-define(OUTPUT7a,{black,"3,333.00"}).

-define(VALUE7b,-3333).
-define(OUTPUT7b,{black,"-3,333.00"}).

-define(VALUE7c,3333.333333).
-define(OUTPUT7c,{black,"3,333.33"}).

-define(VALUE7d,-3333.333333).
-define(OUTPUT7d,{black,"-3,333.33"}).

%% Test 8
-define(FORMAT8,"#,##0.00;[ReD]-#,##0.00").

-define(VALUE8a,3333).
-define(OUTPUT8a,{black,"3,333.00"}).

-define(VALUE8b,-3333).
-define(OUTPUT8b,{red,"-3,333.00"}).

-define(VALUE8c,3333.333333).
-define(OUTPUT8c,{black,"3,333.33"}).

-define(VALUE8d,-3333.333333).
-define(OUTPUT8d,{red,"-3,333.33"}).

%% Test 9
-define(FORMAT9,"£#,##0;-£#,##0").

-define(VALUE9a,3333).
-define(OUTPUT9a,{black,"£3,333"}).

-define(VALUE9b,-3333).
-define(OUTPUT9b,{black,"-£3,333"}).

-define(VALUE9c,3333.333333).
-define(OUTPUT9c,{black,"£3,333"}).

-define(VALUE9d,-3333.333333).
-define(OUTPUT9d,{black,"-£3,333"}).

%% Test 10
-define(FORMAT10,"£#,##0;[Red]-£#,##0").

-define(VALUE10a,3333).
-define(OUTPUT10a,{black,"£3,333"}).

-define(VALUE10b,-3333).
-define(OUTPUT10b,{red,"-£3,333"}).

-define(VALUE10c,3333.333333).
-define(OUTPUT10c,{black,"£3,333"}).

-define(VALUE10d,-3333.333333).
-define(OUTPUT10d,{red,"-£3,333"}).

%% Test 11
-define(FORMAT11,"£#,##0.00;-£#,##0.00").

-define(VALUE11a,3333).
-define(OUTPUT11a,{black,"£3,333.00"}).

-define(VALUE11b,-3333).
-define(OUTPUT11b,{black,"-£3,333.00"}).

-define(VALUE11c,3333.333333).
-define(OUTPUT11c,{black,"£3,333.33"}).

-define(VALUE11d,-3333.333333).
-define(OUTPUT11d,{black,"-£3,333.33"}).

%% Test 12
-define(FORMAT12,"£#,##0.00;[Red]-£#,##0.00").

-define(VALUE12a,3333).
-define(OUTPUT12a,{black,"£3,333.00"}).

-define(VALUE12b,-3333).
-define(OUTPUT12b,{red,"-£3,333.00"}).

-define(VALUE12c,3333.333333).
-define(OUTPUT12c,{black,"£3,333.33"}).

-define(VALUE12d,-3333.333333).
-define(OUTPUT12d,{red,"-£3,333.33"}).

%% Test 13
-define(FORMAT13,"0%").

-define(VALUE13a,3333).
-define(OUTPUT13a,{black,"333300%"}).

-define(VALUE13b,-3333).
-define(OUTPUT13b,{black,"-333300%"}).

-define(VALUE13c,3333.333333).
-define(OUTPUT13c,{black,"333333%"}).

-define(VALUE13d,-3333.333333).
-define(OUTPUT13d,{black,"-333333%"}).

%% Test 14
-define(FORMAT14,"0.00%").

-define(VALUE14a,3333).
-define(OUTPUT14a,{black,"333300.00%"}).

-define(VALUE14b,-3333).
-define(OUTPUT14b,{black,"-333300.00%"}).

-define(VALUE14c,3333.333333).
-define(OUTPUT14c,{black,"333333.33%"}).

-define(VALUE14d,-3333.333333).
-define(OUTPUT14d,{black,"-333333.33%"}).

%% Test 15
-define(FORMAT15,"0.00E+0").

-define(VALUE15a,3333).
-define(OUTPUT15a,{black,"3.33e+3"}).

-define(VALUE15b,-3333).
-define(OUTPUT15b,{black,"-3.33e+3"}).

-define(VALUE15c,3333.333333).
-define(OUTPUT15c,{black,"3.33e+3"}).

-define(VALUE15d,-3333.333333).
-define(OUTPUT15d,{black,"-3.33e+3"}).

%% Test 16
-define(FORMAT16,"##0.0E+00").

-define(VALUE16a,3333).
-define(OUTPUT16a,{black,"3.3e+3"}).

-define(VALUE16b,-3333).
-define(OUTPUT16b,{black,"-3.3e+3"}).

-define(VALUE16c,3333.333333).
-define(OUTPUT16c,{black,"3.3e+3"}).

-define(VALUE16d,-3333.333333).
-define(OUTPUT16d,{black,"-3.3e+3"}).

%% Test 17
-define(FORMAT17,"# ?/?").
-define(FORMAT18,"# ??/??").
%%%%%%%%%%%%%
%%         %%
%% Skipped %%
%%         %%
%%%%%%%%%%%%%

%% Test 19
-define(FORMAT19,"dd/mm/yyyy").

-define(VALUE19a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT19a,{black,"18/10/1933"}).

%% Test 20
-define(FORMAT20,"dd-mmm-yy").

-define(VALUE20a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT20a,{black,"18-Oct-33"}).

%% Test 21
-define(FORMAT21,"dd-mmm").

-define(VALUE21a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT21a,{black,"18-Oct"}).

%% Test 22
-define(FORMAT22,"mmm-yy").

-define(VALUE22a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT22a,{black,"Oct-33"}).

%% Test 23
-define(FORMAT23,"h:mm AM/PM").

-define(VALUE23a,round(0.5*86400)).
-define(OUTPUT23a,{black,"12:01 AM"}).

-define(VALUE23b,round(0.25*86400)).
-define(OUTPUT23b,{black,"6:01 AM"}).

-define(VALUE23c,round(0.75*86400)).
-define(OUTPUT23c,{black,"6:01 PM"}).

-define(VALUE23d,0).
-define(OUTPUT23d,{black,"0:01 AM"}).

-define(VALUE23e,86400).
-define(OUTPUT23e,{black,"0:01 AM"}).

%% Test 24
-define(FORMAT24,"h:mm:ss AM/PM").

-define(VALUE24a,round(0.5*86400)).
-define(OUTPUT24a,{black,"12:01:00 AM"}).

-define(VALUE24b,round(0.25*86400)).
-define(OUTPUT24b,{black,"6:01:00 AM"}).

-define(VALUE24c,round(0.75*86400)).
-define(OUTPUT24c,{black,"6:01:00 PM"}).

-define(VALUE24d,0).
-define(OUTPUT24d,{black,"0:01:00 AM"}).

-define(VALUE24e,86400).
-define(OUTPUT24e,{black,"0:01:00 AM"}).

%% Test 25
-define(FORMAT25,"hh:mm").

-define(VALUE25a,round(0.5*86400)).
-define(OUTPUT25a,{black,"12:01"}).

-define(VALUE25b,round(0.25*86400)).
-define(OUTPUT25b,{black,"06:01"}).

-define(VALUE25c,round(0.75*86400)).
-define(OUTPUT25c,{black,"18:01"}).

-define(VALUE25d,0).
-define(OUTPUT25d,{black,"00:01"}).

-define(VALUE25e,86400).
-define(OUTPUT25e,{black,"00:01"}).

%% Test 26
-define(FORMAT26,"hh:mm:ss").

-define(VALUE26a,round(0.5*86400)).
-define(OUTPUT26a,{black,"12:01:00"}).

-define(VALUE26b,round(0.25*86400)).
-define(OUTPUT26b,{black,"06:01:00"}).

-define(VALUE26c,round(0.75*86400)).
-define(OUTPUT26c,{black,"18:01:00"}).

-define(VALUE26d,0).
-define(OUTPUT26d,{black,"00:01:00"}).

-define(VALUE26e,86400).
-define(OUTPUT26e,{black,"00:01:00"}).

%% Test 27
-define(FORMAT27,"dd/mm/yyyy hh:mm").

-define(VALUE27a,round(0.5*86400)).
-define(OUTPUT27a,{black,"01/01/0000 12:01"}).

-define(VALUE27b,round(0.25*86400)).
-define(OUTPUT27b,{black,"01/01/0000 06:01"}).

-define(VALUE27c,round(0.75*86400)).
-define(OUTPUT27c,{black,"01/01/0000 18:01"}).

-define(VALUE27d,0).
-define(OUTPUT27d,{black,"01/01/0000 00:01"}).

-define(VALUE27e,86400).
-define(OUTPUT27e,{black,"02/01/0000 00:01"}).

%% Test 28
-define(FORMAT28,"mm:ss").

-define(VALUE28a,round(0.5*86400)).
-define(OUTPUT28a,{black,"01:00"}).

-define(VALUE28b,round(0.25*86400)).
-define(OUTPUT28b,{black,"01:00"}).

-define(VALUE28c,round(0.75*86400)).
-define(OUTPUT28c,{black,"01:00"}).

-define(VALUE28d,0).
-define(OUTPUT28d,{black,"01:00"}).

-define(VALUE28e,86400).
-define(OUTPUT28e,{black,"01:00"}).

%% Test 29
-define(FORMAT29,"mm:ss.0").

-define(VALUE29a,round(0.5*86400)).
-define(OUTPUT29a,{black,"01:00.0"}).

-define(VALUE29b,round(0.25*86400)).
-define(OUTPUT29b,{black,"01:00.0"}).

-define(VALUE29c,round(0.75*86400)).
-define(OUTPUT29c,{black,"01:00.0"}).

-define(VALUE29d,0).
-define(OUTPUT29d,{black,"01:00.0"}).

-define(VALUE29e,86400).
-define(OUTPUT29e,{black,"01:00.0"}).

%% Test 30
-define(FORMAT30,"@").

%% Test 31
-define(FORMAT31,"[h]:mm:ss").

-define(VALUE31a,round(0.5*86400)).
-define(OUTPUT31a,{black,"0:01:00"}).

-define(VALUE31b,round(0.25*86400)).
-define(OUTPUT31b,{black,"6:01:00"}).

-define(VALUE31c,round(0.75*86400)).
-define(OUTPUT31c,{black,"12:01:00"}).

-define(VALUE31d,0).
-define(OUTPUT31d,{black,"18:01:00"}).

-define(VALUE31e,86400).
-define(OUTPUT31e,{black,"24:01:00"}).

%% Test 32
-define(FORMAT32,"_-£* #,##0_-;-£* #,##0_-;_-£* \"-\"_-;_-@_-").
-define(FORMAT33,"_-* #,##0_-;-* #,##0_-;_-* \"-\"_-;_-@_-").
-define(FORMAT34,"_-£* #,##0.00_-;-£* #,##0.00_-;_-£* \"-\"??_-;_-@_-").
-define(FORMAT35,"_-* #,##0.00_-;-* #,##0.00_-;_-* \"-\"??_-;_-@_-").
-define(FORMAT36,"[>100][ReD]_-£* #,##0_-;[>=100][BLue]-£* #,##0_-;[>=100][GreEn]_-£* \"-\"_-;_-@_-").
-define(FORMAT37,"[=<100]_-* #,##0_-;[=<100]-* #,##0_-;[=100]_-* \"-\"_-;_-@_-").
-define(FORMAT38,"[>100.0][ReD]_-£* #,##0_-;[>=100.0][BLue]-£* #,##0_-;[>=100.0][GreEn]_-£* \"-\"_-;_-@_-").
-define(FORMAT39,"[=<100.0]_-* #,##0_-;[=<100.0]-* #,##0_-;[=100.0]_-* \"-\"_-;_-@_-").
-define(FORMAT40,"0;;").

%% Now some more tests
-define(FORMAT41,"0000").

-define(VALUE41a,33).
-define(OUTPUT41a,{black,"0033"}).

-define(VALUE41b,33.3).
-define(OUTPUT41b,{black,"0033"}).

-define(VALUE41c,-33).
-define(OUTPUT41c,{black,"-0033"}).

-define(VALUE41d,-33.3).
-define(OUTPUT41d,{black,"-0033"}).

-define(FORMAT42,"??00").

-define(VALUE42a,33).
-define(OUTPUT42a,{black,"  33"}).

-define(VALUE42b,33.3).
-define(OUTPUT42b,{black,"  33"}).

-define(VALUE42c,-33).
-define(OUTPUT42c,{black,"-  33"}).

-define(VALUE42d,-33.3).
-define(OUTPUT42d,{black,"-  33"}).

-define(FORMAT43,"##00").

-define(VALUE43a,33).
-define(OUTPUT43a,{black,"33"}).

-define(VALUE43b,33.3).
-define(OUTPUT43b,{black,"33"}).

-define(VALUE43c,-33).
-define(OUTPUT43c,{black,"-33"}).

-define(VALUE43d,-33.3).
-define(OUTPUT43d,{black,"-33"}).

-define(FORMAT44,"0#?0").

-define(VALUE44a,33).
-define(OUTPUT44a,{black,"033"}).

-define(VALUE44b,33.3).
-define(OUTPUT44b,{black,"033"}).

-define(VALUE44c,-33).
-define(OUTPUT44c,{black,"-033"}).

-define(VALUE44d,-33.3).
-define(OUTPUT44d,{black,"-033"}).

-define(FORMAT45,"0?#0").

-define(VALUE45a,33).
-define(OUTPUT45a,{black,"0 33"}).

-define(VALUE45b,33.3).
-define(OUTPUT45b,{black,"0 33"}).

-define(VALUE45c,-33).
-define(OUTPUT45c,{black,"-0 33"}).

-define(VALUE45d,-33.3).
-define(OUTPUT45d,{black,"-0 33"}).

-define(FORMAT46,"0,000").

-define(VALUE46a,3333).
-define(OUTPUT46a,{black,"3,333"}).

-define(VALUE46b,3333.33).
-define(OUTPUT46b,{black,"3,333"}).

-define(VALUE46c,-3333).
-define(OUTPUT46c,{black,"-3,333"}).

-define(VALUE46d,-3333.33).
-define(OUTPUT46d,{black,"-3,333"}).

-define(FORMAT47,"00,00").

-define(VALUE47a,3333).
-define(OUTPUT47a,{black,"3,333"}).

-define(VALUE47b,3333.33).
-define(OUTPUT47b,{black,"3,333"}).

-define(VALUE47c,-3333).
-define(OUTPUT47c,{black,"-3,333"}).

-define(VALUE47d,-3333.33).
-define(OUTPUT47d,{black,"-3,333"}).

-define(FORMAT48,"00\"tt\"00").

-define(VALUE48a,3333).
-define(OUTPUT48a,{black,"33tt33"}).

-define(VALUE48b,3333.33).
-define(OUTPUT48b,{black,"33tt33"}).

-define(VALUE48c,-3333).
-define(OUTPUT48c,{black,"-33tt33"}).

-define(VALUE48d,-3333.33).
-define(OUTPUT48d,{black,"-33tt33"}).

-define(FORMAT49,"0,0\"u\"0\"v\"0\"w\"0\"x\"0\"y\"0").

-define(VALUE49a,123456).
-define(OUTPUT49a,{black,"0,1u2v3,w4x5y6"}).

-define(VALUE49b,123456.78).
-define(OUTPUT49b,{black,"0,1u2v3,w4x5y7"}).

-define(VALUE49c,-123456).
-define(OUTPUT49c,{black,"-0,1u2v3,w4x5y6"}).

-define(VALUE49d,-123456.78).
-define(OUTPUT49d,{black,"-0,1u2v3,w4x5y7"}).

-define(FORMAT50,"00\"a\"0.0\"b\"0").

-define(VALUE50a,123456).
-define(OUTPUT50a,{black,"12345a6.0b0"}).

-define(VALUE50b,123456.78).
-define(OUTPUT50b,{black,"12345a6.7b8"}).

-define(VALUE50c,-123456).
-define(OUTPUT50c,{black,"-12345a6.0b0"}).

-define(VALUE50d,-123456.78).
-define(OUTPUT50d,{black,"-12345a6.7b8"}).

-define(FORMAT51,"00\"a\"0.\"b\"00").

-define(VALUE51a,123456).
-define(OUTPUT51a,{black,"12345a6.b00"}).

-define(VALUE51b,123456.78).
-define(OUTPUT51b,{black,"12345a6.b78"}).

-define(VALUE51c,-123456).
-define(OUTPUT51c,{black,"-12345a6.b00"}).

-define(VALUE51d,-123456.78).
-define(OUTPUT51d,{black,"-12345a6.b78"}).

-define(FORMAT52,"000\"a\".\"b\"00").

-define(VALUE52a,123456).
-define(OUTPUT52a,{black,"123456a.b00"}).

-define(VALUE52b,123456.78).
-define(OUTPUT52b,{black,"123456a.b78"}).

-define(VALUE52c,-123456).
-define(OUTPUT52c,{black,"-123456a.b00"}).

-define(VALUE52d,-123456.78).
-define(OUTPUT52d,{black,"-123456a.b78"}).

-define(FORMAT53,"00\"x\"0,00\"y\"0,000\"a\".\"b\"00").

-define(VALUE53a,123456).
-define(OUTPUT53a,{black,"00x0,12y3,456a.b00"}).

-define(VALUE53b,123456.78).
-define(OUTPUT53b,{black,"00x0,12y3,456a.b78"}).

-define(VALUE53c,-123456).
-define(OUTPUT53c,{black,"-00x0,12y3,456a.b00"}).

-define(VALUE53d,-123456.78).
-define(OUTPUT53d,{black,"-00x0,12y3,456a.b78"}).

-define(FORMAT54,"0,0\"a\"0.0\"b\"0e+2").

-define(VALUE54a,123456).
-define(OUTPUT54a,{black,"01a2.3b5e+2"}).

-define(VALUE54b,123456.78).
-define(OUTPUT54b,{black,"01a2.3b5e+2"}).

-define(VALUE54c,-123456).
-define(OUTPUT54c,{black,"-01a2.3b5e+2"}).

-define(VALUE54d,-123456.78).
-define(OUTPUT54d,{black,"-01a2.3b5e+2"}).

-define(FORMAT55,"0,0\"a\"0.0\"b\"0e-02").

-define(VALUE55a,123456).
-define(OUTPUT55a,{black,"01a2.3b5e42"}).

-define(VALUE55b,123456.78).
-define(OUTPUT55b,{black,"01a2.3b5e42"}).

-define(VALUE55c,-123456).
-define(OUTPUT55c,{black,"-01a2.3b5e42"}).

-define(VALUE55d,-123456.78).
-define(OUTPUT55d,{black,"-01a2.3b5e42"}).


%% Test 56
-define(FORMAT56,"h:mm am/pm").

-define(VALUE56a,round(0.5*86400)).
-define(OUTPUT56a,{black,"12:01 am"}).

-define(VALUE56b,round(0.25*86400)).
-define(OUTPUT56b,{black,"6:01 am"}).

-define(VALUE56c,round(0.75*86400)).
-define(OUTPUT56c,{black,"6:01 pm"}).

-define(VALUE56d,0).
-define(OUTPUT56d,{black,"0:01 am"}).

-define(VALUE56e,86400).
-define(OUTPUT56e,{black,"0:01 am"}).

%% Test 57
-define(FORMAT57,"h:mm A/P").

-define(VALUE57a,round(0.5*86400)).
-define(OUTPUT57a,{black,"12:01 A"}).

-define(VALUE57b,round(0.25*86400)).
-define(OUTPUT57b,{black,"6:01 A"}).

-define(VALUE57c,round(0.75*86400)).
-define(OUTPUT57c,{black,"6:01 P"}).

-define(VALUE57d,0).
-define(OUTPUT57d,{black,"0:01 A"}).

-define(VALUE57e,86400).
-define(OUTPUT57e,{black,"0:01 A"}).

%% Test 58
-define(FORMAT58,"h:mm a/p").

-define(VALUE58a,round(0.5*86400)).
-define(OUTPUT58a,{black,"12:01 a"}).

-define(VALUE58b,round(0.25*86400)).
-define(OUTPUT58b,{black,"6:01 a"}).

-define(VALUE58c,round(0.75*86400)).
-define(OUTPUT58c,{black,"6:01 p"}).

-define(VALUE58d,0).
-define(OUTPUT58d,{black,"0:01 a"}).

-define(VALUE58e,86400).
-define(OUTPUT58e,{black,"0:01 a"}).


%% Failing tests
-define(FORMAT_F1A,"00.00\"ttt\"00.00").
-define(FORMAT_F2A,"0.0.0").

%% Test server callback functions
%%------------------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:setup_paths(),
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initiation before each test case
%%------------------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%------------------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%------------------------------------------------------------------------------
all() -> 
    [
     num_parser_test1a,
     num_parser_test1b,
     num_parser_test1c,
     num_parser_test1d,
     num_parser_test1e,
     num_parser_test1f,
     num_parser_test1g,
     num_parser_test1h,
     num_parser_test1i,
     num_parser_test1j,
     num_parser_test1k,
     num_parser_test1l,
     num_parser_test1m,
     num_parser_test1n,
     num_parser_test2a,
     num_parser_test2b,
     num_parser_test2c,
     num_parser_test2d,
     num_parser_test2e,
     num_parser_test2f,
     num_parser_test2g,
     num_parser_test2h,
     num_parser_test2i,
     num_parser_test2j,
     num_parser_test2k,
     num_parser_test2l,
     num_parser_test2m,
     num_parser_test3a,
     num_parser_test3b,
     num_parser_test3c,
     num_parser_test3d,
     num_parser_test4a,
     num_parser_test4b,
     num_parser_test4c,
     num_parser_test4d,
     num_parser_test5a,
     num_parser_test5b,
     num_parser_test5c,
     num_parser_test5d,
     num_parser_test6a,
     num_parser_test6b,
     num_parser_test6c,
     num_parser_test6d,
     num_parser_test7a,
     num_parser_test7b,
     num_parser_test7c,
     num_parser_test7d,
     num_parser_test8a,
     num_parser_test8b,
     num_parser_test8c,
     num_parser_test8d,
     num_parser_test9a,
     num_parser_test9b,
     num_parser_test9c,
     num_parser_test9d,
     num_parser_test10a,
     num_parser_test10b,
     num_parser_test10c,
     num_parser_test10d,
     num_parser_test11a,
     num_parser_test11b,
     num_parser_test11c,
     num_parser_test11d,
     num_parser_test12a,
     num_parser_test12b,
     num_parser_test12c,
     num_parser_test12d,
     num_parser_test13a,
     num_parser_test13b,
     num_parser_test13c,
     num_parser_test13d,
     num_parser_test14a,
     num_parser_test14b,
     num_parser_test14c,
     num_parser_test14d,
     num_parser_test15a,
     num_parser_test15b,
     num_parser_test15c,
     num_parser_test15d,
     num_parser_test16a,
     num_parser_test16b,
     num_parser_test16c,
     num_parser_test16d,
     num_parser_test17,
     num_parser_test18,
     num_parser_test19a,
     num_parser_test20a,
     num_parser_test21a,
     num_parser_test22a,
     num_parser_test23a,
     num_parser_test23b,
     num_parser_test23c,
     num_parser_test23d,
     num_parser_test23e,
     num_parser_test24a,
     num_parser_test24b,
     num_parser_test24c,
     num_parser_test24d,
     num_parser_test24e,
     num_parser_test25a,
     num_parser_test25b,
     num_parser_test25c,
     num_parser_test25d,
     num_parser_test25e,
     num_parser_test26a,
     num_parser_test26b,
     num_parser_test26c,
     num_parser_test26d,
     num_parser_test26e,
     num_parser_test27a,
     num_parser_test27b,
     num_parser_test27c,
     num_parser_test27d,
     num_parser_test27e,
     num_parser_test28a,
     num_parser_test28b,
     num_parser_test28c,
     num_parser_test28d,
     num_parser_test28e,
     num_parser_test29a,
     num_parser_test29b,
     num_parser_test29c,
     num_parser_test29d,
     num_parser_test29e,
     num_parser_test30, % skipp
     num_parser_test31a,
     num_parser_test31b,
     num_parser_test31c,
     num_parser_test31d,
     num_parser_test31e,
     num_parser_test32,
     num_parser_test33,
     num_parser_test34,
     num_parser_test35,
     num_parser_test36,
     num_parser_test37,
     num_parser_test38,
     num_parser_test39,
     num_parser_test40,
     num_parser_test41a,
     num_parser_test41b,
     num_parser_test41c,
     num_parser_test41d,
     num_parser_test42a,
     num_parser_test42b,
     num_parser_test42c,
     num_parser_test42d,
     num_parser_test43a,
     num_parser_test43b,
     num_parser_test43c,
     num_parser_test43d,
     num_parser_test44a,
     num_parser_test44b,
     num_parser_test44c,
     num_parser_test44d,
     num_parser_test45a,
     num_parser_test45b,
     num_parser_test45c,
     num_parser_test45d,
     num_parser_test46a,
     num_parser_test46b,
     num_parser_test46c,
     num_parser_test46d,
     num_parser_test47a,
     num_parser_test47b,
     num_parser_test47c,
     num_parser_test47d,
     num_parser_test48a,
     num_parser_test48b,
     num_parser_test48c,
     num_parser_test48d,
     num_parser_test49a,
     num_parser_test49b,
     num_parser_test49c,
     num_parser_test49d,
     num_parser_test50a,
     num_parser_test50b,
     num_parser_test50c,
     num_parser_test50d,
     num_parser_test51a,
     num_parser_test51b,
     num_parser_test51c,
     num_parser_test51d,
     num_parser_test52a,
     num_parser_test52b,
     num_parser_test52c,
     num_parser_test52d,
     num_parser_test53a,
     num_parser_test53b,
     num_parser_test53c,
     num_parser_test53d,
     num_parser_test54a,
     num_parser_test54b,
     num_parser_test54c,
     num_parser_test54d,
     num_parser_test55a,
     num_parser_test55b,
     num_parser_test55c,
     num_parser_test55d,
     num_parser_test56a,
     num_parser_test56b,
     num_parser_test56c,
     num_parser_test56d,
     num_parser_test56e,
     num_parser_test57a,
     num_parser_test57b,
     num_parser_test57c,
     num_parser_test57d,
     num_parser_test57e,
     num_parser_test58a,
     num_parser_test58b,
     num_parser_test58c,
     num_parser_test58d,
     num_parser_test58e,
     num_parser_fail1a,
     num_parser_fail2a
    ].
    
%% Case executor
executor(Format,Value,Expected)->
    {erlang,Output}=format:get_src(Format),
    Got=format:run_format(Value,Output),
    test_util:expected(Expected,Got).

%% Case failer
failer(Format)->
  {error,_}=format:get_src(Format).

%% Test cases starts here.
%%------------------------------------------------------------------------------
num_parser_test1a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1a(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1a,?OUTPUT1a).

num_parser_test1b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1b(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1b,?OUTPUT1b).

num_parser_test1c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1c(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1c,?OUTPUT1c).

num_parser_test1d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1d(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1d,?OUTPUT1d).

num_parser_test1e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1e(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1e,?OUTPUT1e).

num_parser_test1f() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1f(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1f,?OUTPUT1f).

num_parser_test1g() ->- 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1g(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1g,?OUTPUT1g).

num_parser_test1h() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1h(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1h,?OUTPUT1h).

num_parser_test1i() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1i(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1i,?OUTPUT1i).

num_parser_test1j() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1j(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1j,?OUTPUT1j).

num_parser_test1k() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1k(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1k,?OUTPUT1k).

num_parser_test1l() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1l(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1l,?OUTPUT1l).

num_parser_test1m() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1m(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1m,?OUTPUT1m).

num_parser_test1n() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1n(Config) when is_list(Config) -> 
  executor(?FORMAT1,?VALUE1n,?OUTPUT1n).

num_parser_test2a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2a(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2a,?OUTPUT2a).

num_parser_test2b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2b(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2b,?OUTPUT2b).

num_parser_test2c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2c(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2c,?OUTPUT2c).

num_parser_test2d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2d(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2d,?OUTPUT2d).

num_parser_test2e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2e(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2e,?OUTPUT2e).

num_parser_test2f() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2f(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2f,?OUTPUT2f).

num_parser_test2g() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2g(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2g,?OUTPUT2g).

num_parser_test2h() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2h(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2h,?OUTPUT2h).

num_parser_test2i() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2i(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2i,?OUTPUT2i).

num_parser_test2j() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2j(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2j,?OUTPUT2j).

num_parser_test2k() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2k(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2k,?OUTPUT2k).

num_parser_test2l() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2l(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2l,?OUTPUT2l).

num_parser_test2m() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test2m(Config) when is_list(Config) -> 
  executor(?FORMAT2,?VALUE2m,?OUTPUT2m).

num_parser_test3a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3a(Config) when is_list(Config) -> 
  executor(?FORMAT3,?VALUE3a,?OUTPUT3a).

num_parser_test3b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3b(Config) when is_list(Config) -> 
  executor(?FORMAT3,?VALUE3b,?OUTPUT3b).

num_parser_test3c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3c(Config) when is_list(Config) -> 
  executor(?FORMAT3,?VALUE3c,?OUTPUT3c).

num_parser_test3d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3d(Config) when is_list(Config) -> 
  executor(?FORMAT3,?VALUE3d,?OUTPUT3d).

num_parser_test4a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4a(Config) when is_list(Config) -> 
  executor(?FORMAT4,?VALUE4a,?OUTPUT4a).

num_parser_test4b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4b(Config) when is_list(Config) -> 
  executor(?FORMAT4,?VALUE4b,?OUTPUT4b).

num_parser_test4c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4c(Config) when is_list(Config) -> 
  executor(?FORMAT4,?VALUE4c,?OUTPUT4c).

num_parser_test4d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test4d(Config) when is_list(Config) -> 
  executor(?FORMAT4,?VALUE4d,?OUTPUT4d).

num_parser_test5a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5a(Config) when is_list(Config) -> 
  executor(?FORMAT5,?VALUE5a,?OUTPUT5a).

num_parser_test5b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5b(Config) when is_list(Config) -> 
  executor(?FORMAT5,?VALUE5b,?OUTPUT5b).

num_parser_test5c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5c(Config) when is_list(Config) -> 
  executor(?FORMAT5,?VALUE5c,?OUTPUT5c).

num_parser_test5d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test5d(Config) when is_list(Config) -> 
  executor(?FORMAT5,?VALUE5d,?OUTPUT5d).

num_parser_test6a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6a(Config) when is_list(Config) -> 
  executor(?FORMAT6,?VALUE6a,?OUTPUT6a).

num_parser_test6b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6b(Config) when is_list(Config) -> 
  executor(?FORMAT6,?VALUE6b,?OUTPUT6b).

num_parser_test6c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6c(Config) when is_list(Config) -> 
  executor(?FORMAT6,?VALUE6c,?OUTPUT6c).

num_parser_test6d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test6d(Config) when is_list(Config) -> 
  executor(?FORMAT6,?VALUE6d,?OUTPUT6d).

num_parser_test7a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7a(Config) when is_list(Config) -> 
  executor(?FORMAT7,?VALUE7a,?OUTPUT7a).

num_parser_test7b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7b(Config) when is_list(Config) -> 
  executor(?FORMAT7,?VALUE7b,?OUTPUT7b).

num_parser_test7c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7c(Config) when is_list(Config) -> 
  executor(?FORMAT7,?VALUE7c,?OUTPUT7c).

num_parser_test7d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test7d(Config) when is_list(Config) -> 
  executor(?FORMAT7,?VALUE7d,?OUTPUT7d).

num_parser_test8a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8a(Config) when is_list(Config) -> 
  executor(?FORMAT8,?VALUE8a,?OUTPUT8a).

num_parser_test8b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8b(Config) when is_list(Config) -> 
  executor(?FORMAT8,?VALUE8b,?OUTPUT8b).

num_parser_test8c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8c(Config) when is_list(Config) -> 
  executor(?FORMAT8,?VALUE8c,?OUTPUT8c).

num_parser_test8d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test8d(Config) when is_list(Config) -> 
  executor(?FORMAT8,?VALUE8d,?OUTPUT8d).

num_parser_test9a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9a(Config) when is_list(Config) -> 
  executor(?FORMAT9,?VALUE9a,?OUTPUT9a).

num_parser_test9b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9b(Config) when is_list(Config) -> 
  executor(?FORMAT9,?VALUE9b,?OUTPUT9b).

num_parser_test9c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9c(Config) when is_list(Config) -> 
  executor(?FORMAT9,?VALUE9c,?OUTPUT9c).

num_parser_test9d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test9d(Config) when is_list(Config) -> 
  executor(?FORMAT9,?VALUE9d,?OUTPUT9d).

num_parser_test10a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10a(Config) when is_list(Config) -> 
  executor(?FORMAT10,?VALUE10a,?OUTPUT10a).

num_parser_test10b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10b(Config) when is_list(Config) -> 
  executor(?FORMAT10,?VALUE10b,?OUTPUT10b).

num_parser_test10c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10c(Config) when is_list(Config) -> 
  executor(?FORMAT10,?VALUE10c,?OUTPUT10c).

num_parser_test10d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test10d(Config) when is_list(Config) -> 
  executor(?FORMAT10,?VALUE10d,?OUTPUT10d).

num_parser_test11a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11a(Config) when is_list(Config) -> 
  executor(?FORMAT11,?VALUE11a,?OUTPUT11a).

num_parser_test11b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11b(Config) when is_list(Config) -> 
  executor(?FORMAT11,?VALUE11b,?OUTPUT11b).

num_parser_test11c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11c(Config) when is_list(Config) -> 
  executor(?FORMAT11,?VALUE11c,?OUTPUT11c).

num_parser_test11d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test11d(Config) when is_list(Config) -> 
  executor(?FORMAT11,?VALUE11d,?OUTPUT11d).

num_parser_test12a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12a(Config) when is_list(Config) -> 
  executor(?FORMAT12,?VALUE12a,?OUTPUT12a).

num_parser_test12b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12b(Config) when is_list(Config) -> 
  executor(?FORMAT12,?VALUE12b,?OUTPUT12b).

num_parser_test12c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12c(Config) when is_list(Config) -> 
  executor(?FORMAT12,?VALUE12c,?OUTPUT12c).

num_parser_test12d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test12d(Config) when is_list(Config) -> 
  executor(?FORMAT12,?VALUE12d,?OUTPUT12d).

num_parser_test13a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13a(Config) when is_list(Config) -> 
  executor(?FORMAT13,?VALUE13a,?OUTPUT13a).

num_parser_test13b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13b(Config) when is_list(Config) -> 
  executor(?FORMAT13,?VALUE13b,?OUTPUT13b).

num_parser_test13c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13c(Config) when is_list(Config) -> 
  executor(?FORMAT13,?VALUE13c,?OUTPUT13c).

num_parser_test13d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test13d(Config) when is_list(Config) -> 
  executor(?FORMAT13,?VALUE13d,?OUTPUT13d).

num_parser_test14a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14a(Config) when is_list(Config) -> 
  executor(?FORMAT14,?VALUE14a,?OUTPUT14a).

num_parser_test14b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14b(Config) when is_list(Config) -> 
  executor(?FORMAT14,?VALUE14b,?OUTPUT14b).

num_parser_test14c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14c(Config) when is_list(Config) -> 
  executor(?FORMAT14,?VALUE14c,?OUTPUT14c).

num_parser_test14d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test14d(Config) when is_list(Config) -> 
  executor(?FORMAT14,?VALUE14d,?OUTPUT14d).

num_parser_test15a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15a(Config) when is_list(Config) -> 
  executor(?FORMAT15,?VALUE15a,?OUTPUT15a).

num_parser_test15b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15b(Config) when is_list(Config) -> 
  executor(?FORMAT15,?VALUE15b,?OUTPUT15b).

num_parser_test15c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15c(Config) when is_list(Config) -> 
  executor(?FORMAT15,?VALUE15c,?OUTPUT15c).

num_parser_test15d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test15d(Config) when is_list(Config) -> 
  executor(?FORMAT15,?VALUE15d,?OUTPUT15d).

num_parser_test16a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16a(Config) when is_list(Config) -> 
  executor(?FORMAT16,?VALUE16a,?OUTPUT16a).

num_parser_test16b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16b(Config) when is_list(Config) -> 
  executor(?FORMAT16,?VALUE16b,?OUTPUT16b).

num_parser_test16c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16c(Config) when is_list(Config) -> 
  executor(?FORMAT16,?VALUE16c,?OUTPUT16c).

num_parser_test16d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test16d(Config) when is_list(Config) -> 
  executor(?FORMAT16,?VALUE16d,?OUTPUT16d).

%%%%%%%%%%%%%
%%         %%
%% Skipped %%
%%         %%
%%%%%%%%%%%%%
num_parser_test17() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test17(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT17).

num_parser_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test18(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT18).





num_parser_test19a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test19a(Config) when is_list(Config) -> 
  executor(?FORMAT19,?VALUE19a,?OUTPUT19a).

num_parser_test20a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test20a(Config) when is_list(Config) -> 
  executor(?FORMAT20,?VALUE20a,?OUTPUT20a).

num_parser_test21a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test21a(Config) when is_list(Config) -> 
  executor(?FORMAT21,?VALUE21a,?OUTPUT21a).

num_parser_test22a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test22a(Config) when is_list(Config) -> 
  executor(?FORMAT22,?VALUE22a,?OUTPUT22a).

num_parser_test23a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23a(Config) when is_list(Config) -> 
  executor(?FORMAT23,?VALUE23a,?OUTPUT23a).

num_parser_test23b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23b(Config) when is_list(Config) -> 
  executor(?FORMAT23,?VALUE23b,?OUTPUT23b).
  
num_parser_test23c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23c(Config) when is_list(Config) -> 
  executor(?FORMAT23,?VALUE23c,?OUTPUT23c).

num_parser_test23d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23d(Config) when is_list(Config) -> 
  executor(?FORMAT23,?VALUE23d,?OUTPUT23d).

num_parser_test23e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test23e(Config) when is_list(Config) -> 
  executor(?FORMAT23,?VALUE23e,?OUTPUT23e).

num_parser_test24a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24a(Config) when is_list(Config) -> 
  executor(?FORMAT24,?VALUE24a,?OUTPUT24a).

num_parser_test24b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24b(Config) when is_list(Config) -> 
  executor(?FORMAT24,?VALUE24b,?OUTPUT24b).
  
num_parser_test24c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24c(Config) when is_list(Config) -> 
  executor(?FORMAT24,?VALUE24c,?OUTPUT24c).

num_parser_test24d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24d(Config) when is_list(Config) -> 
  executor(?FORMAT24,?VALUE24d,?OUTPUT24d).

num_parser_test24e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test24e(Config) when is_list(Config) -> 
  executor(?FORMAT24,?VALUE24e,?OUTPUT24e).

num_parser_test25a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25a(Config) when is_list(Config) -> 
  executor(?FORMAT25,?VALUE25a,?OUTPUT25a).

num_parser_test25b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25b(Config) when is_list(Config) -> 
  executor(?FORMAT25,?VALUE25b,?OUTPUT25b).
  
num_parser_test25c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25c(Config) when is_list(Config) -> 
  executor(?FORMAT25,?VALUE25c,?OUTPUT25c).

num_parser_test25d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25d(Config) when is_list(Config) -> 
  executor(?FORMAT25,?VALUE25d,?OUTPUT25d).

num_parser_test25e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test25e(Config) when is_list(Config) -> 
  executor(?FORMAT25,?VALUE25e,?OUTPUT25e).

num_parser_test26a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26a(Config) when is_list(Config) -> 
  executor(?FORMAT26,?VALUE26a,?OUTPUT26a).

num_parser_test26b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26b(Config) when is_list(Config) -> 
  executor(?FORMAT26,?VALUE26b,?OUTPUT26b).
  
num_parser_test26c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26c(Config) when is_list(Config) -> 
  executor(?FORMAT26,?VALUE26c,?OUTPUT26c).

num_parser_test26d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26d(Config) when is_list(Config) -> 
  executor(?FORMAT26,?VALUE26d,?OUTPUT26d).

num_parser_test26e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test26e(Config) when is_list(Config) -> 
  executor(?FORMAT26,?VALUE26e,?OUTPUT26e).

num_parser_test27a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27a(Config) when is_list(Config) -> 
  executor(?FORMAT27,?VALUE27a,?OUTPUT27a).

num_parser_test27b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27b(Config) when is_list(Config) -> 
  executor(?FORMAT27,?VALUE27b,?OUTPUT27b).
  
num_parser_test27c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27c(Config) when is_list(Config) -> 
  executor(?FORMAT27,?VALUE27c,?OUTPUT27c).

num_parser_test27d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27d(Config) when is_list(Config) -> 
  executor(?FORMAT27,?VALUE27d,?OUTPUT27d).

num_parser_test27e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test27e(Config) when is_list(Config) -> 
  executor(?FORMAT27,?VALUE27e,?OUTPUT27e).

num_parser_test28a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28a(Config) when is_list(Config) -> 
  executor(?FORMAT28,?VALUE28a,?OUTPUT28a).

num_parser_test28b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28b(Config) when is_list(Config) -> 
  executor(?FORMAT28,?VALUE28b,?OUTPUT28b).
  
num_parser_test28c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28c(Config) when is_list(Config) -> 
  executor(?FORMAT28,?VALUE28c,?OUTPUT28c).

num_parser_test28d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28d(Config) when is_list(Config) -> 
  executor(?FORMAT28,?VALUE28d,?OUTPUT28d).

num_parser_test28e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test28e(Config) when is_list(Config) -> 
  executor(?FORMAT28,?VALUE28e,?OUTPUT28e).

num_parser_test29a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29a(Config) when is_list(Config) -> 
  executor(?FORMAT29,?VALUE29a,?OUTPUT29a).

num_parser_test29b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29b(Config) when is_list(Config) -> 
  executor(?FORMAT29,?VALUE29b,?OUTPUT29b).
  
num_parser_test29c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29c(Config) when is_list(Config) -> 
  executor(?FORMAT29,?VALUE29c,?OUTPUT29c).

num_parser_test29d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29d(Config) when is_list(Config) -> 
  executor(?FORMAT29,?VALUE29d,?OUTPUT29d).

num_parser_test29e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test29e(Config) when is_list(Config) -> 
  executor(?FORMAT29,?VALUE29e,?OUTPUT29e).



%% Skipping
num_parser_test30() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test30(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT30).



num_parser_test31a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31a(Config) when is_list(Config) -> 
  executor(?FORMAT31,?VALUE31a,?OUTPUT31a).

num_parser_test31b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31b(Config) when is_list(Config) -> 
  executor(?FORMAT31,?VALUE31b,?OUTPUT31b).
  
num_parser_test31c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31c(Config) when is_list(Config) -> 
  executor(?FORMAT31,?VALUE31c,?OUTPUT31c).

num_parser_test31d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31d(Config) when is_list(Config) -> 
  executor(?FORMAT31,?VALUE31d,?OUTPUT31d).

num_parser_test31e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test31e(Config) when is_list(Config) -> 
  executor(?FORMAT31,?VALUE31e,?OUTPUT31e).





num_parser_test32() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT32).

num_parser_test33() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT33).

num_parser_test34() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT34).

num_parser_test35() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT35).

num_parser_test36() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT36).

num_parser_test37() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT37).

num_parser_test38() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT38).

num_parser_test39() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src(?FORMAT39).

num_parser_test40(Config) when is_list(Config) -> 
    {erlang,Output}=format:get_src
    (?FORMAT40).

num_parser_test41a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41a(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41a,?OUTPUT41a).

num_parser_test41b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41b(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41b,?OUTPUT41b).

num_parser_test41c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41c(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41c,?OUTPUT41c).

num_parser_test41d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test41d(Config) when is_list(Config) -> 
  executor(?FORMAT41,?VALUE41d,?OUTPUT41d).

num_parser_test42a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test42a(Config) when is_list(Config) -> 
  executor(?FORMAT42,?VALUE42a,?OUTPUT42a).

num_parser_test42b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test42b(Config) when is_list(Config) -> 
  executor(?FORMAT42,?VALUE42b,?OUTPUT42b).

num_parser_test42c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test42c(Config) when is_list(Config) -> 
  executor(?FORMAT42,?VALUE42c,?OUTPUT42c).

num_parser_test42d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test42d(Config) when is_list(Config) -> 
  executor(?FORMAT42,?VALUE42d,?OUTPUT42d).

num_parser_test43a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test43a(Config) when is_list(Config) -> 
  executor(?FORMAT43,?VALUE43a,?OUTPUT43a).

num_parser_test43b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test43b(Config) when is_list(Config) -> 
  executor(?FORMAT43,?VALUE43b,?OUTPUT43b).

num_parser_test43c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test43c(Config) when is_list(Config) -> 
  executor(?FORMAT43,?VALUE43c,?OUTPUT43c).

num_parser_test43d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test43d(Config) when is_list(Config) -> 
  executor(?FORMAT43,?VALUE43d,?OUTPUT43d).

num_parser_test44a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test44a(Config) when is_list(Config) -> 
  executor(?FORMAT44,?VALUE44a,?OUTPUT44a).

num_parser_test44b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test44b(Config) when is_list(Config) -> 
  executor(?FORMAT44,?VALUE44b,?OUTPUT44b).

num_parser_test44c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test44c(Config) when is_list(Config) -> 
  executor(?FORMAT44,?VALUE44c,?OUTPUT44c).

num_parser_test44d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test44d(Config) when is_list(Config) -> 
  executor(?FORMAT44,?VALUE44d,?OUTPUT44d).

num_parser_test45a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test45a(Config) when is_list(Config) -> 
  executor(?FORMAT45,?VALUE45a,?OUTPUT45a).

num_parser_test45b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test45b(Config) when is_list(Config) -> 
  executor(?FORMAT45,?VALUE45b,?OUTPUT45b).

num_parser_test45c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test45c(Config) when is_list(Config) -> 
  executor(?FORMAT45,?VALUE45c,?OUTPUT45c).

num_parser_test45d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test45d(Config) when is_list(Config) -> 
  executor(?FORMAT45,?VALUE45d,?OUTPUT45d).

num_parser_test46a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test46a(Config) when is_list(Config) -> 
  executor(?FORMAT46,?VALUE46a,?OUTPUT46a).

num_parser_test46b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test46b(Config) when is_list(Config) -> 
  executor(?FORMAT46,?VALUE46b,?OUTPUT46b).

num_parser_test46c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test46c(Config) when is_list(Config) -> 
  executor(?FORMAT46,?VALUE46c,?OUTPUT46c).

num_parser_test46d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test46d(Config) when is_list(Config) -> 
  executor(?FORMAT46,?VALUE46d,?OUTPUT46d).

num_parser_test47a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test47a(Config) when is_list(Config) -> 
  executor(?FORMAT47,?VALUE47a,?OUTPUT47a).

num_parser_test47b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test47b(Config) when is_list(Config) -> 
  executor(?FORMAT47,?VALUE47b,?OUTPUT47b).

num_parser_test47c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test47c(Config) when is_list(Config) -> 
  executor(?FORMAT47,?VALUE47c,?OUTPUT47c).

num_parser_test47d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test47d(Config) when is_list(Config) -> 
  executor(?FORMAT47,?VALUE47d,?OUTPUT47d).

num_parser_test48a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test48a(Config) when is_list(Config) -> 
  executor(?FORMAT48,?VALUE48a,?OUTPUT48a).

num_parser_test48b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test48b(Config) when is_list(Config) -> 
  executor(?FORMAT48,?VALUE48b,?OUTPUT48b).

num_parser_test48c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test48c(Config) when is_list(Config) -> 
  executor(?FORMAT48,?VALUE48c,?OUTPUT48c).

num_parser_test48d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test48d(Config) when is_list(Config) -> 
  executor(?FORMAT48,?VALUE48d,?OUTPUT48d).

num_parser_test49a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test49a(Config) when is_list(Config) -> 
  executor(?FORMAT49,?VALUE49a,?OUTPUT49a).

num_parser_test49b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test49b(Config) when is_list(Config) -> 
  executor(?FORMAT49,?VALUE49b,?OUTPUT49b).

num_parser_test49c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test49c(Config) when is_list(Config) -> 
  executor(?FORMAT49,?VALUE49c,?OUTPUT49c).

num_parser_test49d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test49d(Config) when is_list(Config) -> 
  executor(?FORMAT49,?VALUE49d,?OUTPUT49d).

num_parser_test50a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test50a(Config) when is_list(Config) -> 
  executor(?FORMAT50,?VALUE50a,?OUTPUT50a).

num_parser_test50b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test50b(Config) when is_list(Config) -> 
  executor(?FORMAT50,?VALUE50b,?OUTPUT50b).

num_parser_test50c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test50c(Config) when is_list(Config) -> 
  executor(?FORMAT50,?VALUE50c,?OUTPUT50c).

num_parser_test50d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test50d(Config) when is_list(Config) -> 
  executor(?FORMAT50,?VALUE50d,?OUTPUT50d).

num_parser_test51a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test51a(Config) when is_list(Config) -> 
  executor(?FORMAT51,?VALUE51a,?OUTPUT51a).

num_parser_test51b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test51b(Config) when is_list(Config) -> 
  executor(?FORMAT51,?VALUE51b,?OUTPUT51b).

num_parser_test51c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test51c(Config) when is_list(Config) -> 
  executor(?FORMAT51,?VALUE51c,?OUTPUT51c).

num_parser_test51d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test51d(Config) when is_list(Config) -> 
  executor(?FORMAT51,?VALUE51d,?OUTPUT51d).

num_parser_test52a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test52a(Config) when is_list(Config) -> 
  executor(?FORMAT52,?VALUE52a,?OUTPUT52a).

num_parser_test52b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test52b(Config) when is_list(Config) -> 
  executor(?FORMAT52,?VALUE52b,?OUTPUT52b).

num_parser_test52c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test52c(Config) when is_list(Config) -> 
  executor(?FORMAT52,?VALUE52c,?OUTPUT52c).

num_parser_test52d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test52d(Config) when is_list(Config) -> 
  executor(?FORMAT52,?VALUE52d,?OUTPUT52d).

num_parser_test53a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test53a(Config) when is_list(Config) -> 
  executor(?FORMAT53,?VALUE53a,?OUTPUT53a).

num_parser_test53b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test53b(Config) when is_list(Config) -> 
  executor(?FORMAT53,?VALUE53b,?OUTPUT53b).

num_parser_test53c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test53c(Config) when is_list(Config) -> 
  executor(?FORMAT53,?VALUE53c,?OUTPUT53c).

num_parser_test53d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test53d(Config) when is_list(Config) -> 
  executor(?FORMAT53,?VALUE53d,?OUTPUT53d).

num_parser_test54a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test54a(Config) when is_list(Config) -> 
  executor(?FORMAT54,?VALUE54a,?OUTPUT54a).

num_parser_test54b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test54b(Config) when is_list(Config) -> 
  executor(?FORMAT54,?VALUE54b,?OUTPUT54b).

num_parser_test54c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test54c(Config) when is_list(Config) -> 
  executor(?FORMAT54,?VALUE54c,?OUTPUT54c).

num_parser_test54d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test54d(Config) when is_list(Config) -> 
  executor(?FORMAT54,?VALUE54d,?OUTPUT54d).

num_parser_test55a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55a(Config) when is_list(Config) -> 
  executor(?FORMAT55,?VALUE55a,?OUTPUT55a).

num_parser_test55b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55b(Config) when is_list(Config) -> 
  executor(?FORMAT55,?VALUE55b,?OUTPUT55b).

num_parser_test55c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55c(Config) when is_list(Config) -> 
  executor(?FORMAT55,?VALUE55c,?OUTPUT55c).

num_parser_test55d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55d(Config) when is_list(Config) -> 
  executor(?FORMAT55,?VALUE55d,?OUTPUT55d).





num_parser_test56a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test56a(Config) when is_list(Config) -> 
  executor(?FORMAT56,?VALUE56a,?OUTPUT56a).

num_parser_test56b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test56b(Config) when is_list(Config) -> 
  executor(?FORMAT56,?VALUE56b,?OUTPUT56b).

num_parser_test56c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test56c(Config) when is_list(Config) -> 
  executor(?FORMAT56,?VALUE56c,?OUTPUT56c).

num_parser_test56d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test56d(Config) when is_list(Config) -> 
  executor(?FORMAT56,?VALUE56d,?OUTPUT56d).

num_parser_test56e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test56e(Config) when is_list(Config) -> 
  executor(?FORMAT56,?VALUE56e,?OUTPUT56e).

num_parser_test57a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test57a(Config) when is_list(Config) -> 
  executor(?FORMAT57,?VALUE57a,?OUTPUT57a).

num_parser_test57b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test57b(Config) when is_list(Config) -> 
  executor(?FORMAT57,?VALUE57b,?OUTPUT57b).

num_parser_test57c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test57c(Config) when is_list(Config) -> 
  executor(?FORMAT57,?VALUE57c,?OUTPUT57c).

num_parser_test57d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test57d(Config) when is_list(Config) -> 
  executor(?FORMAT57,?VALUE57d,?OUTPUT57d).

num_parser_test57e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test57e(Config) when is_list(Config) -> 
  executor(?FORMAT57,?VALUE57e,?OUTPUT57e).

num_parser_test58a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test58a(Config) when is_list(Config) -> 
  executor(?FORMAT58,?VALUE58a,?OUTPUT58a).

num_parser_test58b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test58b(Config) when is_list(Config) -> 
  executor(?FORMAT58,?VALUE58b,?OUTPUT58b).

num_parser_test58c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test58c(Config) when is_list(Config) -> 
  executor(?FORMAT58,?VALUE58c,?OUTPUT58c).

num_parser_test58d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test58d(Config) when is_list(Config) -> 
  executor(?FORMAT58,?VALUE58d,?OUTPUT58d).

num_parser_test58e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test58e(Config) when is_list(Config) -> 
  executor(?FORMAT58,?VALUE58e,?OUTPUT58e).

%% Failing tests
num_parser_fail1a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_fail1a(Config) when is_list(Config) -> 
  failer(?FORMAT_F1A).

num_parser_fail2a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_fail2a(Config) when is_list(Config) -> 
  failer(?FORMAT_F2A).
