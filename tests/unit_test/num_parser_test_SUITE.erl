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
-define(OUTPUT1a,{number,{black,"33"}}).

-define(VALUE1b,-33).
-define(OUTPUT1b,{number,{black,"-33"}}).

-define(VALUE1c,33.333).
-define(OUTPUT1c,{number,{black,"33"}}).

-define(VALUE1d,-33.33).
-define(OUTPUT1d,{number,{black,"-33"}}).

-define(VALUE1e,0).
-define(OUTPUT1e,{number,{black,"0"}}).

-define(VALUE1f,0.0).
-define(OUTPUT1f,{number,{black,"0"}}).

-define(VALUE1g,"bob").
-define(OUTPUT1g,{number,{black,"bob"}}).

-define(VALUE1h,0.0003333).
-define(OUTPUT1h,{number,{black,"0"}}).

-define(VALUE1i,-0.000033).
-define(OUTPUT1i,{number,{black,"0"}}).

-define(VALUE1j,0.000000000000000000000000000003333).
-define(OUTPUT1j,{number,{black,"0"}}).

-define(VALUE1k,-0.00000000000000000000000000000033).
-define(OUTPUT1k,{number,{black,"0"}}).

-define(VALUE1l,33330000000000000000000000000000000000000000000).
-define(OUTPUT1l,{number,{black,"33330000000000000000000000000000000000000000000"}}).

-define(VALUE1m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT1m,{number,{black,"-33330000000000000000000000000000000000000000000"}}).

-define(VALUE1n,2.999).
-define(OUTPUT1n,{number,{black,"3"}}).

%% Test 1X

-define(FORMAT1X,"00.00").
-define(VALUE1X,2).
-define(OUTPUT1X,{number,{black,"02.00"}}).

%% Test 2
-define(FORMAT2,"0.00").
-define(VALUE2a,33).
-define(OUTPUT2a,{number,{black,"33.00"}}).

-define(VALUE2b,-33).
-define(OUTPUT2b,{number,{black,"-33.00"}}).

-define(VALUE2c,33.333).
-define(OUTPUT2c,{number,{black,"33.33"}}).

-define(VALUE2d,-33.33).
-define(OUTPUT2d,{number,{black,"-33.33"}}).

-define(VALUE2e,0).
-define(OUTPUT2e,{number,{black,"0.00"}}).

-define(VALUE2f,0.0).
-define(OUTPUT2f,{number,{black,"0.00"}}).

-define(VALUE2g,"bob").
-define(OUTPUT2g,{number,{black,"bob"}}).

-define(VALUE2h,0.0003333).
-define(OUTPUT2h,{number,{black,"0.00"}}).

-define(VALUE2i,-0.000033).
-define(OUTPUT2i,{number,{black,"0.00"}}).

-define(VALUE2j,0.000000000000000000000000000003333).
-define(OUTPUT2j,{number,{black,"0.00"}}).

-define(VALUE2k,-0.00000000000000000000000000000033).
-define(OUTPUT2k,{number,{black,"0.00"}}).

-define(VALUE2l,33330000000000000000000000000000000000000000000).
-define(OUTPUT2l,{number,{black,"33330000000000000000000000000000000000000000000.00"}}).

-define(VALUE2m,-33330000000000000000000000000000000000000000000).
-define(OUTPUT2m,{number,{black,"-33330000000000000000000000000000000000000000000.00"}}).

%% Test 3
-define(FORMAT3,"#,##0").

-define(VALUE3a,3333).
-define(OUTPUT3a,{number,{black,"3,333"}}).

-define(VALUE3b,-3333).
-define(OUTPUT3b,{number,{black,"-3,333"}}).

-define(VALUE3c,3333.333).
-define(OUTPUT3c,{number,{black,"3,333"}}).

-define(VALUE3d,-3333.33).
-define(OUTPUT3d,{number,{black,"-3,333"}}).

%% Test 3X
-define(FORMAT3X,"?0.0?").

-define(VALUE3Xa,333.333).
-define(OUTPUT3Xa,{number,{black,"333.33"}}).

-define(VALUE3Xb,-333.333).
-define(OUTPUT3Xb,{number,{black,"-333.33"}}).

-define(VALUE3Xc,3.33).
-define(OUTPUT3Xc,{number,{black," 3.33"}}).

-define(VALUE3Xd,-3.33).
-define(OUTPUT3Xd,{number,{black,"- 3.33"}}).

-define(VALUE3Xe,33.3).
-define(OUTPUT3Xe,{number,{black,"33.3 "}}).

-define(VALUE3Xf,-33.3).
-define(OUTPUT3Xf,{number,{black,"-33.3 "}}).

%% Test 3Y
-define(FORMAT3Y,"#0.0#").

-define(VALUE3Ya,333.333).
-define(OUTPUT3Ya,{number,{black,"333.33"}}).

-define(VALUE3Yb,-333.333).
-define(OUTPUT3Yb,{number,{black,"-333.33"}}).

-define(VALUE3Yc,3.33).
-define(OUTPUT3Yc,{number,{black,"3.33"}}).

-define(VALUE3Yd,-3.33).
-define(OUTPUT3Yd,{number,{black,"-3.33"}}).

-define(VALUE3Ye,33.3).
-define(OUTPUT3Ye,{number,{black,"33.3"}}).

-define(VALUE3Yf,-33.3).
-define(OUTPUT3Yf,{number,{black,"-33.3"}}).

%% Test 4
-define(FORMAT4,"#,##0.00").

-define(VALUE4a,3333).
-define(OUTPUT4a,{number,{black,"3,333.00"}}).

-define(VALUE4b,-3333).
-define(OUTPUT4b,{number,{black,"-3,333.00"}}).

-define(VALUE4c,3333.33).
-define(OUTPUT4c,{number,{black,"3,333.33"}}).

-define(VALUE4d,-3333.33).
-define(OUTPUT4d,{number,{black,"-3,333.33"}}).

%% Test 5
-define(FORMAT5,"#,##0;-#,##0").

-define(VALUE5a,3333).
-define(OUTPUT5a,{number,{black,"3,333"}}).

-define(VALUE5b,-3333).
-define(OUTPUT5b,{number,{black,"-3,333"}}).

-define(VALUE5c,3333.333).
-define(OUTPUT5c,{number,{black,"3,333"}}).

-define(VALUE5d,-3333.33).
-define(OUTPUT5d,{number,{black,"-3,333"}}).

%% Test 6
-define(FORMAT6,"#,##0;[Red]-#,##0").

-define(VALUE6a,3333).
-define(OUTPUT6a,{number,{black,"3,333"}}).

-define(VALUE6b,-3333).
-define(OUTPUT6b,{number,{red,"-3,333"}}).

-define(VALUE6c,3333.333).
-define(OUTPUT6c,{number,{black,"3,333"}}).

-define(VALUE6d,-3333.33).
-define(OUTPUT6d,{number,{red,"-3,333"}}).

%% Test 7
-define(FORMAT7,"#,##0.00;-#,##0.00").

-define(VALUE7a,3333).
-define(OUTPUT7a,{number,{black,"3,333.00"}}).

-define(VALUE7b,-3333).
-define(OUTPUT7b,{number,{black,"-3,333.00"}}).

-define(VALUE7c,3333.333333).
-define(OUTPUT7c,{number,{black,"3,333.33"}}).

-define(VALUE7d,-3333.333333).
-define(OUTPUT7d,{number,{black,"-3,333.33"}}).

%% Test 8
-define(FORMAT8,"#,##0.00;[ReD]-#,##0.00").

-define(VALUE8a,3333).
-define(OUTPUT8a,{number,{black,"3,333.00"}}).

-define(VALUE8b,-3333).
-define(OUTPUT8b,{number,{red,"-3,333.00"}}).

-define(VALUE8c,3333.333333).
-define(OUTPUT8c,{number,{black,"3,333.33"}}).

-define(VALUE8d,-3333.333333).
-define(OUTPUT8d,{number,{red,"-3,333.33"}}).

%% Test 9
-define(FORMAT9,"£#,##0;-£#,##0").

-define(VALUE9a,3333).
-define(OUTPUT9a,{number,{black,"£3,333"}}).

-define(VALUE9b,-3333).
-define(OUTPUT9b,{number,{black,"-£3,333"}}).

-define(VALUE9c,3333.333333).
-define(OUTPUT9c,{number,{black,"£3,333"}}).

-define(VALUE9d,-3333.333333).
-define(OUTPUT9d,{number,{black,"-£3,333"}}).

%% Test 10
-define(FORMAT10,"£#,##0;[Red]-£#,##0").

-define(VALUE10a,3333).
-define(OUTPUT10a,{number,{black,"£3,333"}}).

-define(VALUE10b,-3333).
-define(OUTPUT10b,{number,{red,"-£3,333"}}).

-define(VALUE10c,3333.333333).
-define(OUTPUT10c,{number,{black,"£3,333"}}).

-define(VALUE10d,-3333.333333).
-define(OUTPUT10d,{number,{red,"-£3,333"}}).

%% Test 11
-define(FORMAT11,"£#,##0.00;-£#,##0.00").

-define(VALUE11a,3333).
-define(OUTPUT11a,{number,{black,"£3,333.00"}}).

-define(VALUE11b,-3333).
-define(OUTPUT11b,{number,{black,"-£3,333.00"}}).

-define(VALUE11c,3333.333333).
-define(OUTPUT11c,{number,{black,"£3,333.33"}}).

-define(VALUE11d,-3333.333333).
-define(OUTPUT11d,{number,{black,"-£3,333.33"}}).

%% Test 12
-define(FORMAT12,"£#,##0.00;[Red]-£#,##0.00").

-define(VALUE12a,3333).
-define(OUTPUT12a,{number,{black,"£3,333.00"}}).

-define(VALUE12b,-3333).
-define(OUTPUT12b,{number,{red,"-£3,333.00"}}).

-define(VALUE12c,3333.333333).
-define(OUTPUT12c,{number,{black,"£3,333.33"}}).

-define(VALUE12d,-3333.333333).
-define(OUTPUT12d,{number,{red,"-£3,333.33"}}).

%% Test 13
-define(FORMAT13,"0%").

-define(VALUE13a,3333).
-define(OUTPUT13a,{number,{black,"333300%"}}).

-define(VALUE13b,-3333).
-define(OUTPUT13b,{number,{black,"-333300%"}}).

-define(VALUE13c,3333.333333).
-define(OUTPUT13c,{number,{black,"333333%"}}).

-define(VALUE13d,-3333.333333).
-define(OUTPUT13d,{number,{black,"-333333%"}}).

%% Test 14
-define(FORMAT14,"0.00%").

-define(VALUE14a,3333).
-define(OUTPUT14a,{number,{black,"333300.00%"}}).

-define(VALUE14b,-3333).
-define(OUTPUT14b,{number,{black,"-333300.00%"}}).

-define(VALUE14c,3333.333333).
-define(OUTPUT14c,{number,{black,"333333.33%"}}).

-define(VALUE14d,-3333.333333).
-define(OUTPUT14d,{number,{black,"-333333.33%"}}).

%% Test 15
-define(FORMAT15,"0.00E+0").

-define(VALUE15a,3333).
-define(OUTPUT15a,{number,{black,"3.33e+3"}}).

-define(VALUE15b,-3333).
-define(OUTPUT15b,{number,{black,"-3.33e+3"}}).

-define(VALUE15c,3333.333333).
-define(OUTPUT15c,{number,{black,"3.33e+3"}}).

-define(VALUE15d,-3333.333333).
-define(OUTPUT15d,{number,{black,"-3.33e+3"}}).

%% Test 16
-define(FORMAT16,"##0.0E+00").

-define(VALUE16a,3333).
-define(OUTPUT16a,{number,{black,"3.3e+3"}}).

-define(VALUE16b,-3333).
-define(OUTPUT16b,{number,{black,"-3.3e+3"}}).

-define(VALUE16c,3333.333333).
-define(OUTPUT16c,{number,{black,"3.3e+3"}}).

-define(VALUE16d,-3333.333333).
-define(OUTPUT16d,{number,{black,"-3.3e+3"}}).

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
-define(OUTPUT19a,{date,{black,"18/10/1933"}}).

%% Test 20
-define(FORMAT20,"dd-mmm-yy").

-define(VALUE20a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT20a,{date,{black,"18-Oct-33"}}).

%% Test 21
-define(FORMAT21,"dd-mmm").

-define(VALUE21a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT21a,{date,{black,"18-Oct"}}).

%% Test 22
-define(FORMAT22,"mmm-yy").

-define(VALUE22a,(12345-1+693960)*86400). % corrected to a seconds-based time where 0 is 1st Jan 1900
-define(OUTPUT22a,{date,{black,"Oct-33"}}).

%% Test 23
-define(FORMAT23,"h:mm AM/PM").

-define(VALUE23a,round(0.5*86400)).
-define(OUTPUT23a,{date,{black,"12:00 AM"}}).

-define(VALUE23b,round(0.25*86400)).
-define(OUTPUT23b,{date,{black,"6:00 AM"}}).

-define(VALUE23c,round(0.75*86400)).
-define(OUTPUT23c,{date,{black,"6:00 PM"}}).

-define(VALUE23d,0).
-define(OUTPUT23d,{date,{black,"0:00 AM"}}).

-define(VALUE23e,86400).
-define(OUTPUT23e,{date,{black,"0:00 AM"}}).

%% Test 24
-define(FORMAT24,"h:mm:ss AM/PM").

-define(VALUE24a,round(0.5*86400)).
-define(OUTPUT24a,{date,{black,"12:00:00 AM"}}).

-define(VALUE24b,round(0.25*86400)).
-define(OUTPUT24b,{date,{black,"6:00:00 AM"}}).

-define(VALUE24c,round(0.75*86400)).
-define(OUTPUT24c,{date,{black,"6:00:00 PM"}}).

-define(VALUE24d,0).
-define(OUTPUT24d,{date,{black,"0:00:00 AM"}}).

-define(VALUE24e,86400).
-define(OUTPUT24e,{date,{black,"0:00:00 AM"}}).

%% Test 25
-define(FORMAT25,"hh:mm").

-define(VALUE25a,round(0.5*86400)).
-define(OUTPUT25a,{date,{black,"12:00"}}).

-define(VALUE25b,round(0.25*86400)).
-define(OUTPUT25b,{date,{black,"06:00"}}).

-define(VALUE25c,round(0.75*86400)).
-define(OUTPUT25c,{date,{black,"18:00"}}).

-define(VALUE25d,0).
-define(OUTPUT25d,{date,{black,"00:00"}}).

-define(VALUE25e,86400).
-define(OUTPUT25e,{date,{black,"00:00"}}).

%% Test 26
-define(FORMAT26,"hh:mm:ss").

-define(VALUE26a,round(0.5*86400)).
-define(OUTPUT26a,{date,{black,"12:00:00"}}).

-define(VALUE26b,round(0.25*86400)).
-define(OUTPUT26b,{date,{black,"06:00:00"}}).

-define(VALUE26c,round(0.75*86400)).
-define(OUTPUT26c,{date,{black,"18:00:00"}}).

-define(VALUE26d,0).
-define(OUTPUT26d,{date,{black,"00:00:00"}}).

-define(VALUE26e,86400).
-define(OUTPUT26e,{date,{black,"00:00:00"}}).

%% Test 27
-define(FORMAT27,"dd/mm/yyyy hh:mm").

-define(VALUE27a,round(0.5*86400)).
-define(OUTPUT27a,{date,{black,"01/01/0000 12:00"}}).

-define(VALUE27b,round(0.25*86400)).
-define(OUTPUT27b,{date,{black,"01/01/0000 06:00"}}).

-define(VALUE27c,round(0.75*86400)).
-define(OUTPUT27c,{date,{black,"01/01/0000 18:00"}}).

-define(VALUE27d,0).
-define(OUTPUT27d,{date,{black,"01/01/0000 00:00"}}).

-define(VALUE27e,86400).
-define(OUTPUT27e,{date,{black,"02/01/0000 00:00"}}).

%% Test 28
-define(FORMAT28,"mm:ss").

-define(VALUE28a,round(0.5*86400)).
-define(OUTPUT28a,{date,{black,"00:00"}}).

-define(VALUE28b,round(0.25*86400)).
-define(OUTPUT28b,{date,{black,"00:00"}}).

-define(VALUE28c,round(0.75*86400)).
-define(OUTPUT28c,{date,{black,"00:00"}}).

-define(VALUE28d,0).
-define(OUTPUT28d,{date,{black,"00:00"}}).

-define(VALUE28e,86400).
-define(OUTPUT28e,{date,{black,"00:00"}}).

%% Test 29
-define(FORMAT29,"mm:ss.0").

-define(VALUE29a,round(0.5*86400)).
-define(OUTPUT29a,{date,{black,"00:00.0"}}).

-define(VALUE29b,round(0.25*86400)).
-define(OUTPUT29b,{date,{black,"00:00.0"}}).

-define(VALUE29c,round(0.75*86400)).
-define(OUTPUT29c,{date,{black,"00:00.0"}}).

-define(VALUE29d,0).
-define(OUTPUT29d,{date,{black,"00:00.0"}}).

-define(VALUE29e,86400).
-define(OUTPUT29e,{date,{black,"00:00.0"}}).

%% Test 30
-define(FORMAT30,"@").

%% Test 31
-define(FORMAT31,"[h]:mm:ss").

-define(VALUE31a,round(0.5*86400)).
-define(OUTPUT31a,{date,{black,"12:00:00"}}).

-define(VALUE31b,round(0.25*86400)).
-define(OUTPUT31b,{date,{black,"6:00:00"}}).

-define(VALUE31c,round(0.75*86400)).
-define(OUTPUT31c,{date,{black,"18:00:00"}}).

-define(VALUE31d,0).
-define(OUTPUT31d,{date,{black,"0:00:00"}}).

-define(VALUE31e,86400).
-define(OUTPUT31e,{date,{black,"24:00:00"}}).

%% Test 32
-define(FORMAT32,"_-£* #,##0_-;-£* #,##0_-;_-£* \"-\"_-;_-@_-").

-define(VALUE32a,1111.111).
-define(OUTPUT32a,{number,{black," £ 1,111 "}}).

-define(VALUE32b,-1111.111).
-define(OUTPUT32b,{number,{black,"-£ 1,111 "}}).

-define(VALUE32c,0).
-define(OUTPUT32c,{number,{black," £ - "}}).

-define(VALUE32d,"bob").
-define(OUTPUT32d,{number,{black," bob "}}).

%% Test 33
-define(FORMAT33,"_-* #,##0_-;-* #,##0_-;_-* \"-\"_-;_-@_-").

-define(VALUE33a,1111.111).
-define(OUTPUT33a,{number,{black,"  1,111 "}}).

-define(VALUE33b,-1111.111).
-define(OUTPUT33b,{number,{black,"- 1,111 "}}).

-define(VALUE33c,0).
-define(OUTPUT33c,{number,{black,"  - "}}).

-define(VALUE33d,"bob").
-define(OUTPUT33d,{number,{black," bob "}}).

%% Test 34
-define(FORMAT34,"_-£* #,##0.00_-;-£* #,##0.00_-;_-£* \"-\"??_-;_-@_-").

-define(VALUE34a,1111.111).
-define(OUTPUT34a,{number,{black," £ 1,111.11 "}}).

-define(VALUE34b,-1111.111).
-define(OUTPUT34b,{number,{black,"-£ 1,111.11 "}}).

-define(VALUE34c,0).
-define(OUTPUT34c,{number,{black," £ -   "}}).

-define(VALUE34d,"bob").
-define(OUTPUT34d,{number,{black," bob "}}).

%% Test 35
-define(FORMAT35,"_-* #,##0.00_-;-* #,##0.00_-;_-* \"-\"??_-;_-@_-").

-define(VALUE35a,1111.111).
-define(OUTPUT35a,{number,{black,"  1,111.11 "}}).

-define(VALUE35b,-1111.111).
-define(OUTPUT35b,{number,{black,"- 1,111.11 "}}).

-define(VALUE35c,0).
-define(OUTPUT35c,{number,{black,"  -   "}}).

-define(VALUE35d,"bob").
-define(OUTPUT35d,{number,{black," bob "}}).

%% Test 36
-define(FORMAT36,"[Blue][=<2000]0.00;[Red][>2000]0.00;[Green]??_-;[Yellow]_-@_-").

-define(VALUE36a,3000).
-define(OUTPUT36a,{number,{red,"3000.00"}}).

-define(VALUE36b,2000).
-define(OUTPUT36b,{number,{blue,"2000.00"}}).

-define(VALUE36c,0).
-define(OUTPUT36c,{number,{green,"   "}}).

-define(VALUE36d,"bob").
-define(OUTPUT36d,{number,{yellow," bob "}}).

%% Test 37
-define(FORMAT37,"[=<100]_-* #,##0_-;[>=100]-* #,##0_-;_-* \"-\"_-;_-@_-").

-define(VALUE37a,1234).
-define(OUTPUT37a,{number,{black,"- 1,234 "}}).

-define(VALUE37b,12.34).
-define(OUTPUT37b,{number,{black,"  12 "}}).

-define(VALUE37c,0).
-define(OUTPUT37c,{number,{black,"  - "}}).

-define(VALUE37d,"bob").
-define(OUTPUT37d,{number,{black," bob "}}).

%% Test 38
-define(FORMAT38,"[ReD][>100.0]_-£* #,##0_-;[BLue][<=100.0]-£* #,##0_-;[GreEn]_-£* \"-\"_-;_-@_-").

-define(VALUE38a,1234).
-define(OUTPUT38a,{number,{red," £ 1,234 "}}).

-define(VALUE38b,12.34).
-define(OUTPUT38b,{number,{blue,"-£ 12 "}}).

-define(VALUE38c,0).
-define(OUTPUT38c,{number,{green," £ - "}}).

-define(VALUE38d,"bob").
-define(OUTPUT38d,{number,{black," bob "}}).

-define(FORMAT39,"[<=100.0]_-* #,##0_-;[>=100.0]-* #,##0_-;_-* \"-\"_-;_-@_-").

-define(VALUE39a,1234).
-define(OUTPUT39a,{number,{black,"- 1,234 "}}).

-define(VALUE39b,12.34).
-define(OUTPUT39b,{number,{black,"  12 "}}).

-define(VALUE39c,0).
-define(OUTPUT39c,{number,{black,"  - "}}).

-define(VALUE39d,"bob").
-define(OUTPUT39d,{number,{black," bob "}}).

%% Test 40
-define(FORMAT40,"0;;").

-define(VALUE40a,1234).
-define(OUTPUT40a,{number,{black,"1234"}}).

-define(VALUE40b,-1234.56).
-define(OUTPUT40b,{number,{black,""}}).

-define(VALUE40c,0).
-define(OUTPUT40c,{number,{black,""}}).

-define(VALUE40d,"bob").
-define(OUTPUT40d,{number,{black,"bob"}}).

%% Test 40X
-define(FORMAT40X,"General").

-define(VALUE40Xa,1234).
-define(OUTPUT40Xa,{number,{black,"1234.00"}}).

-define(VALUE40Xb,-1234.56).
-define(OUTPUT40Xb,{number,{black,"-1234.56"}}).

-define(VALUE40Xc,0).
-define(OUTPUT40Xc,{number,{black,"0.00"}}).

-define(VALUE40Xd,"bob").
-define(OUTPUT40Xd,{number,{black,"bob"}}).

%% Now some more tests
-define(FORMAT41,"0000").

-define(VALUE41a,33).
-define(OUTPUT41a,{number,{black,"0033"}}).

-define(VALUE41b,33.3).
-define(OUTPUT41b,{number,{black,"0033"}}).

-define(VALUE41c,-33).
-define(OUTPUT41c,{number,{black,"-0033"}}).

-define(VALUE41d,-33.3).
-define(OUTPUT41d,{number,{black,"-0033"}}).


-define(FORMAT42,"??00").

-define(VALUE42a,33).
-define(OUTPUT42a,{number,{black,"  33"}}).

-define(VALUE42b,33.3).
-define(OUTPUT42b,{number,{black,"  33"}}).

-define(VALUE42c,-33).
-define(OUTPUT42c,{number,{black,"-  33"}}).

-define(VALUE42d,-33.3).
-define(OUTPUT42d,{number,{black,"-  33"}}).

-define(FORMAT43,"##00").

-define(VALUE43a,33).
-define(OUTPUT43a,{number,{black,"33"}}).

-define(VALUE43b,33.3).
-define(OUTPUT43b,{number,{black,"33"}}).

-define(VALUE43c,-33).
-define(OUTPUT43c,{number,{black,"-33"}}).

-define(VALUE43d,-33.3).
-define(OUTPUT43d,{number,{black,"-33"}}).

-define(FORMAT44,"0#?0").

-define(VALUE44a,33).
-define(OUTPUT44a,{number,{black,"033"}}).

-define(VALUE44b,33.3).
-define(OUTPUT44b,{number,{black,"033"}}).

-define(VALUE44c,-33).
-define(OUTPUT44c,{number,{black,"-033"}}).

-define(VALUE44d,-33.3).
-define(OUTPUT44d,{number,{black,"-033"}}).

-define(FORMAT45,"0?#0").

-define(VALUE45a,33).
-define(OUTPUT45a,{number,{black,"0 33"}}).

-define(VALUE45b,33.3).
-define(OUTPUT45b,{number,{black,"0 33"}}).

-define(VALUE45c,-33).
-define(OUTPUT45c,{number,{black,"-0 33"}}).

-define(VALUE45d,-33.3).
-define(OUTPUT45d,{number,{black,"-0 33"}}).

-define(FORMAT46,"0,000").

-define(VALUE46a,3333).
-define(OUTPUT46a,{number,{black,"3,333"}}).

-define(VALUE46b,3333.33).
-define(OUTPUT46b,{number,{black,"3,333"}}).

-define(VALUE46c,-3333).
-define(OUTPUT46c,{number,{black,"-3,333"}}).

-define(VALUE46d,-3333.33).
-define(OUTPUT46d,{number,{black,"-3,333"}}).

-define(FORMAT47,"00,00").

-define(VALUE47a,3333).
-define(OUTPUT47a,{number,{black,"3,333"}}).

-define(VALUE47b,3333.33).
-define(OUTPUT47b,{number,{black,"3,333"}}).

-define(VALUE47c,-3333).
-define(OUTPUT47c,{number,{black,"-3,333"}}).

-define(VALUE47d,-3333.33).
-define(OUTPUT47d,{number,{black,"-3,333"}}).

-define(FORMAT48,"00\"tt\"00").

-define(VALUE48a,3333).
-define(OUTPUT48a,{number,{black,"33tt33"}}).

-define(VALUE48b,3333.33).
-define(OUTPUT48b,{number,{black,"33tt33"}}).

-define(VALUE48c,-3333).
-define(OUTPUT48c,{number,{black,"-33tt33"}}).

-define(VALUE48d,-3333.33).
-define(OUTPUT48d,{number,{black,"-33tt33"}}).

-define(FORMAT49,"0,0\"s\"0\"t\"0\"u\"0\"v\"0\"w\"0\"x\"0\"y\"0").

-define(VALUE49a,12345678).
-define(OUTPUT49a,{number,{black,"01s2,t3u4v5,w6x7y8"}}).

-define(VALUE49b,12345678.98).
-define(OUTPUT49b,{number,{black,"01s2,t3u4v5,w6x7y9"}}).

-define(VALUE49c,-12345678).
-define(OUTPUT49c,{number,{black,"-01s2,t3u4v5,w6x7y8"}}).

-define(VALUE49d,-12345678.98).
-define(OUTPUT49d,{number,{black,"-01s2,t3u4v5,w6x7y9"}}).

-define(FORMAT50,"00\"a\"0.0\"b\"0").

-define(VALUE50a,123456).
-define(OUTPUT50a,{number,{black,"12345a6.0b0"}}).

-define(VALUE50b,123456.78).
-define(OUTPUT50b,{number,{black,"12345a6.7b8"}}).

-define(VALUE50c,-123456).
-define(OUTPUT50c,{number,{black,"-12345a6.0b0"}}).

-define(VALUE50d,-123456.78).
-define(OUTPUT50d,{number,{black,"-12345a6.7b8"}}).

-define(FORMAT51,"00\"a\"0.\"b\"00").

-define(VALUE51a,123456).
-define(OUTPUT51a,{number,{black,"12345a6.b00"}}).

-define(VALUE51b,123456.78).
-define(OUTPUT51b,{number,{black,"12345a6.b78"}}).

-define(VALUE51c,-123456).
-define(OUTPUT51c,{number,{black,"-12345a6.b00"}}).

-define(VALUE51d,-123456.78).
-define(OUTPUT51d,{number,{black,"-12345a6.b78"}}).

-define(FORMAT52,"000\"a\".\"b\"00").

-define(VALUE52a,123456).
-define(OUTPUT52a,{number,{black,"123456a.b00"}}).

-define(VALUE52b,123456.78).
-define(OUTPUT52b,{number,{black,"123456a.b78"}}).

-define(VALUE52c,-123456).
-define(OUTPUT52c,{number,{black,"-123456a.b00"}}).

-define(VALUE52d,-123456.78).
-define(OUTPUT52d,{number,{black,"-123456a.b78"}}).

-define(FORMAT53,"00\"x\"0,00\"y\"0,000\"a\".\"b\"00").

-define(VALUE53a,123456).
-define(OUTPUT53a,{number,{black,"00x0,12y3,456a.b00"}}).

-define(VALUE53b,123456.78).
-define(OUTPUT53b,{number,{black,"00x0,12y3,456a.b78"}}).

-define(VALUE53c,-123456).
-define(OUTPUT53c,{number,{black,"-00x0,12y3,456a.b00"}}).

-define(VALUE53d,-123456.78).
-define(OUTPUT53d,{number,{black,"-00x0,12y3,456a.b78"}}).

-define(FORMAT54,"0,0\"a\"0.0\"b\"0e+2").

-define(VALUE54a,123456).
-define(OUTPUT54a,{number,{black,"1,23a4.5b6e+2"}}).

-define(VALUE54b,123456.78).
-define(OUTPUT54b,{number,{black,"1,23a4.5b7e+2"}}).

-define(VALUE54c,-123456).
-define(OUTPUT54c,{number,{black,"-1,23a4.5b6e+2"}}).

-define(VALUE54d,-123456.78).
-define(OUTPUT54d,{number,{black,"-1,23a4.5b7e+2"}}).

-define(FORMAT55,"0,0\"a\"0.0\"b\"0e-02").

-define(VALUE55a,123456).
-define(OUTPUT55a,{number,{black,"12,345,6a00.b00e-2"}}).

-define(VALUE55b,123456.78).
-define(OUTPUT55b,{number,{black,"12,345,6a78.b00e-2"}}).

-define(VALUE55c,-123456).
-define(OUTPUT55c,{number,{black,"-12,345,6a00.b00e-2"}}).

-define(VALUE55d,-123456.78).
-define(OUTPUT55d,{number,{black,"-12,345,6a78.b00e-2"}}).

-define(FORMAT55X,"0,0\"a\"0.0\"b\"0%").

-define(VALUE55Xa,123456).
-define(OUTPUT55Xa,{number,{black,"12,345,60a0.0b0%"}}).

-define(VALUE55Xb,123456.789).
-define(OUTPUT55Xb,{number,{black,"12,345,67a8.9b0%"}}).

-define(VALUE55Xc,-123456).
-define(OUTPUT55Xc,{number,{black,"-12,345,60a0.0b0%"}}).

-define(VALUE55Xd,-123456.789).
-define(OUTPUT55Xd,{number,{black,"-12,345,67a8.9b0%"}}).

%% Test 56
-define(FORMAT56,"h:mm am/pm").

-define(VALUE56a,round(0.5*86400)).
-define(OUTPUT56a,{date,{black,"12:00 am"}}).

-define(VALUE56b,round(0.25*86400)).
-define(OUTPUT56b,{date,{black,"6:00 am"}}).

-define(VALUE56c,round(0.75*86400)).
-define(OUTPUT56c,{date,{black,"6:00 pm"}}).

-define(VALUE56d,0).
-define(OUTPUT56d,{date,{black,"0:00 am"}}).

-define(VALUE56e,86400).
-define(OUTPUT56e,{date,{black,"0:00 am"}}).

%% Test 57
-define(FORMAT57,"h:mm A/P").

-define(VALUE57a,round(0.5*86400)).
-define(OUTPUT57a,{date,{black,"12:00 A"}}).

-define(VALUE57b,round(0.25*86400)).
-define(OUTPUT57b,{date,{black,"6:00 A"}}).

-define(VALUE57c,round(0.75*86400)).
-define(OUTPUT57c,{date,{black,"6:00 P"}}).

-define(VALUE57d,0).
-define(OUTPUT57d,{date,{black,"0:00 A"}}).

-define(VALUE57e,86400).
-define(OUTPUT57e,{date,{black,"0:00 A"}}).

%% Test 58
-define(FORMAT58,"h:mm a/p").

-define(VALUE58a,round(0.5*86400)).
-define(OUTPUT58a,{date,{black,"12:00 a"}}).

-define(VALUE58b,round(0.25*86400)).
-define(OUTPUT58b,{date,{black,"6:00 a"}}).

-define(VALUE58c,round(0.75*86400)).
-define(OUTPUT58c,{date,{black,"6:00 p"}}).

-define(VALUE58d,0).
-define(OUTPUT58d,{date,{black,"0:00 a"}}).

-define(VALUE58e,86400).
-define(OUTPUT58e,{date,{black,"0:00 a"}}).

%% Test 59
-define(FORMAT59,"\"general\"0.0").

-define(VALUE59,4).
-define(OUTPUT59,{number,{black,"general4.0"}}).

%% Test 60
-define(FORMAT60,"0.0\"general\"").

-define(VALUE60,4).
-define(OUTPUT60,{number,{black,"4.0general"}}).

%% Test 61
-define(FORMAT61,"[m]").

-define(VALUE61a,round(0.5*86400)).
-define(OUTPUT61a,{date,{black,"720"}}).

-define(VALUE61b,round(0.25*86400)).
-define(OUTPUT61b,{date,{black,"360"}}).

-define(VALUE61c,round(0.75*86400)).
-define(OUTPUT61c,{date,{black,"1080"}}).

-define(VALUE61d,0).
-define(OUTPUT61d,{date,{black,"0"}}).

-define(VALUE61e,86400).
-define(OUTPUT61e,{date,{black,"1440"}}).

%% Test 62
-define(FORMAT62,"[s]").

-define(VALUE62a,round(0.5*86400)).
-define(OUTPUT62a,{date,{black,"43200"}}).

-define(VALUE62b,round(0.25*86400)).
-define(OUTPUT62b,{date,{black,"21600"}}).

-define(VALUE62c,round(0.75*86400)).
-define(OUTPUT62c,{date,{black,"64800"}}).

-define(VALUE62d,0).
-define(OUTPUT62d,{date,{black,"0"}}).

-define(VALUE62e,86400).
-define(OUTPUT62e,{date,{black,"86400"}}).

%% These tests were as the result of bugs found in implementing formats 
%% in the test suites

%% Test 501
-define(FORMAT501,"M/D/YY").

-define(VALUE501,((12345-1+693960)*86400)).
-define(OUTPUT501,{date,{black,"10/18/33"}}).

%% Test 502
-define(FORMAT502,"_(\\\"$\\\"*#,##0_);_(\\\"$\\\"*(#,##0);_($\\\"*\\\"-\\\"_);_(@_)").

-define(VALUE502a,1234.56).
-define(OUTPUT502a,{number,{black," \"$\"1,235 "}}).

-define(VALUE502b,-1234.56).
-define(OUTPUT502b,{number,{black," \"$\"(1,235)"}}).

-define(VALUE502c,0).
-define(OUTPUT502c,{number,{black," $\"\"-\" "}}).

-define(VALUE502d,"bob").
-define(OUTPUT502d,{number,{black," bob "}}).

%% Test 503
-define(FORMAT503,"\"$\"#,##0_);[Red]\\(\"$\"#,##0\\)").

-define(VALUE503a,1234.56).
-define(OUTPUT503a,{number,{black,"$1,235 "}}). 

-define(VALUE503b,-1234.56).
-define(OUTPUT503b,{number,{red,"\($1,235\)"}}).

-define(VALUE503c,0).
-define(OUTPUT503c,{number,{black,"$0 "}}).

-define(VALUE503d,"bob").
-define(OUTPUT503d,{number,{black,"bob"}}).

%% Test 504
-define(FORMAT504,"\\_0.00").

-define(VALUE504,1234.5).
-define(OUTPUT504,{number,{black,"_1234.50"}}). 

%% Test 505
-define(FORMAT505,"\\\\_0.00").

-define(VALUE505,1234.5).
-define(OUTPUT505,{number,{black,"_1234.50"}}). 

%% Test 506
-define(FORMAT506,"[Red](\"$\"#,##0\\)").

-define(VALUE506a,1234.5).
-define(OUTPUT506a,{number,{red,"($1,235)"}}). 

-define(VALUE506b,-1234.5).
-define(OUTPUT506b,{number,{red,"($-1,235)"}}). 

-define(VALUE506c,0).
-define(OUTPUT506c,{number,{red,"($0)"}}). 

-define(VALUE506d,"bob").
-define(OUTPUT506d,{number,{black,"bob"}}). 

%% Test 507
-define(FORMAT507,"m").

-define(VALUE507,round(0.5*86400)).
-define(OUTPUT507,{date,{black,"1"}}). 

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
     num_parser_test1X,
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
     num_parser_test3Xa,
     num_parser_test3Xb,
     num_parser_test3Xc,
     num_parser_test3Xd,
     num_parser_test3Xe,
     num_parser_test3Xf,
     num_parser_test3Ya,
     num_parser_test3Yb,
     num_parser_test3Yc,
     num_parser_test3Yd,
     num_parser_test3Ye,
     num_parser_test3Yf,
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
     num_parser_test30, % skip
     num_parser_test31a,
     num_parser_test31b,
     num_parser_test31c,
     num_parser_test31d,
     num_parser_test31e,
     num_parser_test32a,
     num_parser_test32b,
     num_parser_test32c,
     num_parser_test32d,
     num_parser_test33a,
     num_parser_test33b,
     num_parser_test33c,
     num_parser_test33d,
     num_parser_test34a,
     num_parser_test34b,
     num_parser_test34c,
     num_parser_test34d,
     num_parser_test35a,
     num_parser_test35b,
     num_parser_test35c,
     num_parser_test35d,
     num_parser_test36a,
     num_parser_test36b,
     num_parser_test36c,
     num_parser_test36d,
     num_parser_test37a,
     num_parser_test37b,
     num_parser_test37c,
     num_parser_test37d,
     num_parser_test38a,
     num_parser_test38b,
     num_parser_test38c,
     num_parser_test38d,
     num_parser_test39a,
     num_parser_test39b,
     num_parser_test39c,
     num_parser_test39d,
     num_parser_test40a,
     num_parser_test40b,
     num_parser_test40c,
     num_parser_test40d,
     num_parser_test40Xa,
     num_parser_test40Xb,
     num_parser_test40Xc,
     num_parser_test40Xd,
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
     num_parser_test55Xa,
     num_parser_test55Xb,
     num_parser_test55Xc,
     num_parser_test55Xd,
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
     num_parser_test59,
     num_parser_test60,
     num_parser_test61a,
     num_parser_test61b,
     num_parser_test61a,
     num_parser_test61c,
     num_parser_test61d,
     num_parser_test61e,
     num_parser_test62a,
     num_parser_test62b,
     num_parser_test62a,
     num_parser_test62c,
     num_parser_test62d,
     num_parser_test62e,
     num_parser_test501,
     num_parser_test502a,
     num_parser_test502b,
     num_parser_test502c,
     num_parser_test502d,
     num_parser_test503a,
     num_parser_test503b,
     num_parser_test503c,
     num_parser_test503d,
     num_parser_test504,
     num_parser_test505,
     num_parser_test506a,
     num_parser_test506b,
     num_parser_test506c,
     num_parser_test506d,
     num_parser_test507,
     num_parser_fail1a,
     num_parser_fail2a
    ].

%% Case executor
executor(Format,Value,Expected)->
    io:format("in num_parser_test:executor Format is ~p Value is ~p~n",
	      [Format,Value]),
    {erlang,Output}=format:get_src(Format),
    io:format("in num_parser_test:executor got to 2 Output is ~p~n",[Output]),
    {Type,Output2}=Output,
    {ok,Got}=format:run_format(Value,Output2),
    io:format("in num_parser_test:executor Got is ~p~n",[Got]),
    test_util:expected3(Expected,{Type,Got}).

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

num_parser_test1X() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test1X(Config) when is_list(Config) -> 
    executor(?FORMAT1X,?VALUE1X,?OUTPUT1X).

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

num_parser_test3Xa() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xa(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xa,?OUTPUT3Xa).

num_parser_test3Xb() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xb(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xb,?OUTPUT3Xb).

num_parser_test3Xc() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xc(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xc,?OUTPUT3Xc).

num_parser_test3Xd() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xd(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xd,?OUTPUT3Xd).

num_parser_test3Xe() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xe(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xe,?OUTPUT3Xe).

num_parser_test3Xf() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Xf(Config) when is_list(Config) -> 
    executor(?FORMAT3X,?VALUE3Xf,?OUTPUT3Xf).

num_parser_test3Ya() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Ya(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Ya,?OUTPUT3Ya).

num_parser_test3Yb() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Yb(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Yb,?OUTPUT3Yb).

num_parser_test3Yc() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Yc(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Yc,?OUTPUT3Yc).

num_parser_test3Yd() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Yd(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Yd,?OUTPUT3Yd).

num_parser_test3Ye() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Ye(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Ye,?OUTPUT3Ye).

num_parser_test3Yf() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test3Yf(Config) when is_list(Config) -> 
    executor(?FORMAT3Y,?VALUE3Yf,?OUTPUT3Yf).

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
    {erlang,{Type,Output}}=format:get_src(?FORMAT17).

num_parser_test18() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test18(Config) when is_list(Config) -> 
    {erlang,{Type,Output}}=format:get_src(?FORMAT18).





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
    {erlang,{Type,Output}}=format:get_src(?FORMAT30).



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

num_parser_test32a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32a(Config) when is_list(Config) -> 
    executor(?FORMAT32,?VALUE32a,?OUTPUT32a).

num_parser_test32b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32b(Config) when is_list(Config) -> 
    executor(?FORMAT32,?VALUE32b,?OUTPUT32b).

num_parser_test32c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32c(Config) when is_list(Config) -> 
    executor(?FORMAT32,?VALUE32c,?OUTPUT32c).

num_parser_test32d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test32d(Config) when is_list(Config) -> 
    executor(?FORMAT32,?VALUE32d,?OUTPUT32d).

num_parser_test33a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33a(Config) when is_list(Config) -> 
    executor(?FORMAT33,?VALUE33a,?OUTPUT33a).

num_parser_test33b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33b(Config) when is_list(Config) -> 
    executor(?FORMAT33,?VALUE33b,?OUTPUT33b).

num_parser_test33c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33c(Config) when is_list(Config) -> 
    executor(?FORMAT33,?VALUE33c,?OUTPUT33c).

num_parser_test33d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test33d(Config) when is_list(Config) -> 
    executor(?FORMAT33,?VALUE33d,?OUTPUT33d).

num_parser_test34a(Config) when is_list(Config) -> 
    executor(?FORMAT34,?VALUE34a,?OUTPUT34a).

num_parser_test34b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34b(Config) when is_list(Config) -> 
    executor(?FORMAT34,?VALUE34b,?OUTPUT34b).

num_parser_test34c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34c(Config) when is_list(Config) -> 
    executor(?FORMAT34,?VALUE34c,?OUTPUT34c).

num_parser_test34d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test34d(Config) when is_list(Config) -> 
    executor(?FORMAT34,?VALUE34d,?OUTPUT34d).

num_parser_test35a(Config) when is_list(Config) -> 
    executor(?FORMAT35,?VALUE35a,?OUTPUT35a).

num_parser_test35b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35b(Config) when is_list(Config) -> 
    executor(?FORMAT35,?VALUE35b,?OUTPUT35b).

num_parser_test35c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35c(Config) when is_list(Config) -> 
    executor(?FORMAT35,?VALUE35c,?OUTPUT35c).

num_parser_test35d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test35d(Config) when is_list(Config) -> 
    executor(?FORMAT35,?VALUE35d,?OUTPUT35d).

num_parser_test36a(Config) when is_list(Config) -> 
    executor(?FORMAT36,?VALUE36a,?OUTPUT36a).

num_parser_test36b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36b(Config) when is_list(Config) -> 
    executor(?FORMAT36,?VALUE36b,?OUTPUT36b).

num_parser_test36c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36c(Config) when is_list(Config) -> 
    executor(?FORMAT36,?VALUE36c,?OUTPUT36c).

num_parser_test36d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test36d(Config) when is_list(Config) -> 
    executor(?FORMAT36,?VALUE36d,?OUTPUT36d).

num_parser_test37a(Config) when is_list(Config) -> 
    executor(?FORMAT37,?VALUE37a,?OUTPUT37a).

num_parser_test37b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37b(Config) when is_list(Config) -> 
    executor(?FORMAT37,?VALUE37b,?OUTPUT37b).

num_parser_test37c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37c(Config) when is_list(Config) -> 
    executor(?FORMAT37,?VALUE37c,?OUTPUT37c).

num_parser_test37d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test37d(Config) when is_list(Config) -> 
    executor(?FORMAT37,?VALUE37d,?OUTPUT37d).

num_parser_test38a(Config) when is_list(Config) -> 
    executor(?FORMAT38,?VALUE38a,?OUTPUT38a).

num_parser_test38b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38b(Config) when is_list(Config) -> 
    executor(?FORMAT38,?VALUE38b,?OUTPUT38b).

num_parser_test38c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38c(Config) when is_list(Config) -> 
    executor(?FORMAT38,?VALUE38c,?OUTPUT38c).

num_parser_test38d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test38d(Config) when is_list(Config) -> 
    executor(?FORMAT38,?VALUE38d,?OUTPUT38d).

num_parser_test39a(Config) when is_list(Config) -> 
    executor(?FORMAT39,?VALUE39a,?OUTPUT39a).

num_parser_test39b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39b(Config) when is_list(Config) -> 
    executor(?FORMAT39,?VALUE39b,?OUTPUT39b).

num_parser_test39c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39c(Config) when is_list(Config) -> 
    executor(?FORMAT39,?VALUE39c,?OUTPUT39c).

num_parser_test39d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test39d(Config) when is_list(Config) -> 
    executor(?FORMAT39,?VALUE39d,?OUTPUT39d).

num_parser_test40a(Config) when is_list(Config) -> 
    executor(?FORMAT40,?VALUE40a,?OUTPUT40a).

num_parser_test40b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40b(Config) when is_list(Config) -> 
    executor(?FORMAT40,?VALUE40b,?OUTPUT40b).

num_parser_test40c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40c(Config) when is_list(Config) -> 
    executor(?FORMAT40,?VALUE40c,?OUTPUT40c).

num_parser_test40d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40d(Config) when is_list(Config) -> 
    executor(?FORMAT40,?VALUE40d,?OUTPUT40d).

num_parser_test40Xa(Config) when is_list(Config) -> 
    executor(?FORMAT40X,?VALUE40Xa,?OUTPUT40Xa).

num_parser_test40Xb() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40Xb(Config) when is_list(Config) -> 
    executor(?FORMAT40X,?VALUE40Xb,?OUTPUT40Xb).

num_parser_test40Xc() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40Xc(Config) when is_list(Config) -> 
    executor(?FORMAT40X,?VALUE40Xc,?OUTPUT40Xc).

num_parser_test40Xd() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test40Xd(Config) when is_list(Config) -> 
    executor(?FORMAT40X,?VALUE40Xd,?OUTPUT40Xd).

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

num_parser_test55Xa() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55Xa(Config) when is_list(Config) -> 
    executor(?FORMAT55X,?VALUE55Xa,?OUTPUT55Xa).

num_parser_test55Xb() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55Xb(Config) when is_list(Config) -> 
    executor(?FORMAT55X,?VALUE55Xb,?OUTPUT55Xb).

num_parser_test55Xc() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55Xc(Config) when is_list(Config) -> 
    executor(?FORMAT55X,?VALUE55Xc,?OUTPUT55Xc).

num_parser_test55Xd() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test55Xd(Config) when is_list(Config) -> 
    executor(?FORMAT55X,?VALUE55Xd,?OUTPUT55Xd).



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

num_parser_test59() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test59(Config) when is_list(Config) -> 
    executor(?FORMAT59,?VALUE59,?OUTPUT59).

num_parser_test60() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test60(Config) when is_list(Config) -> 
    executor(?FORMAT60,?VALUE60,?OUTPUT60).

num_parser_test61a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test61a(Config) when is_list(Config) -> 
    executor(?FORMAT61,?VALUE61a,?OUTPUT61a).

num_parser_test61b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test61b(Config) when is_list(Config) -> 
    executor(?FORMAT61,?VALUE61b,?OUTPUT61b).

num_parser_test61c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test61c(Config) when is_list(Config) -> 
    executor(?FORMAT61,?VALUE61c,?OUTPUT61c).

num_parser_test61d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test61d(Config) when is_list(Config) -> 
    executor(?FORMAT61,?VALUE61d,?OUTPUT61d).

num_parser_test61e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test61e(Config) when is_list(Config) -> 
    executor(?FORMAT61,?VALUE61e,?OUTPUT61e).

num_parser_test62a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test62a(Config) when is_list(Config) -> 
    executor(?FORMAT62,?VALUE62a,?OUTPUT62a).

num_parser_test62b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test62b(Config) when is_list(Config) -> 
    executor(?FORMAT62,?VALUE62b,?OUTPUT62b).

num_parser_test62c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test62c(Config) when is_list(Config) -> 
    executor(?FORMAT62,?VALUE62c,?OUTPUT62c).

num_parser_test62d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test62d(Config) when is_list(Config) -> 
    executor(?FORMAT62,?VALUE62d,?OUTPUT62d).

num_parser_test62e() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test62e(Config) when is_list(Config) -> 
    executor(?FORMAT62,?VALUE62e,?OUTPUT62e).

%% Bug fixes
num_parser_test501() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test501(Config) when is_list(Config) -> 
    executor(?FORMAT501,?VALUE501,?OUTPUT501).

num_parser_test502a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test502a(Config) when is_list(Config) -> 
    executor(?FORMAT502,?VALUE502a,?OUTPUT502a).

num_parser_test502b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test502b(Config) when is_list(Config) -> 
    executor(?FORMAT502,?VALUE502b,?OUTPUT502b).

num_parser_test502c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test502c(Config) when is_list(Config) -> 
    executor(?FORMAT502,?VALUE502c,?OUTPUT502c).

num_parser_test502d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test502d(Config) when is_list(Config) -> 
    executor(?FORMAT502,?VALUE502d,?OUTPUT502d).

num_parser_test503a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test503a(Config) when is_list(Config) -> 
    executor(?FORMAT503,?VALUE503a,?OUTPUT503a).

num_parser_test503b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test503b(Config) when is_list(Config) -> 
    executor(?FORMAT503,?VALUE503b,?OUTPUT503b).

num_parser_test503c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test503c(Config) when is_list(Config) -> 
    executor(?FORMAT503,?VALUE503c,?OUTPUT503c).

num_parser_test503d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test503d(Config) when is_list(Config) -> 
    executor(?FORMAT503,?VALUE503d,?OUTPUT503d).

num_parser_test504() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test504(Config) when is_list(Config) -> 
    executor(?FORMAT504,?VALUE504,?OUTPUT504).

num_parser_test505() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test505(Config) when is_list(Config) -> 
    executor(?FORMAT505,?VALUE505,?OUTPUT505).

num_parser_test506a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test506a(Config) when is_list(Config) -> 
    executor(?FORMAT506,?VALUE506a,?OUTPUT506a).

num_parser_test506b() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test506b(Config) when is_list(Config) -> 
    executor(?FORMAT506,?VALUE506b,?OUTPUT506b).

num_parser_test506c() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test506c(Config) when is_list(Config) -> 
    executor(?FORMAT506,?VALUE506c,?OUTPUT506c).

num_parser_test506d() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test506d(Config) when is_list(Config) -> 
    executor(?FORMAT506,?VALUE506d,?OUTPUT506d).

num_parser_test507() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_test507(Config) when is_list(Config) -> 
    executor(?FORMAT507,?VALUE507,?OUTPUT507).


%% Failing tests
num_parser_fail1a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_fail1a(Config) when is_list(Config) -> 
    failer(?FORMAT_F1A).

num_parser_fail2a() -> 
    [{userdata,[{doc,"Describe the main purpose of test case"}]}].

num_parser_fail2a(Config) when is_list(Config) -> 
    failer(?FORMAT_F2A).
