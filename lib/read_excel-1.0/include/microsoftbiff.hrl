%% This defines a series of constants taken from the Open Office
%% specification of the Microsoft Biff8 and Biff8X file formats

%% It can be found at:
%% http://sc.openoffice.org/excelfileformat.pdf

%% This is based on version 1.39 of that documemnt - 1.40 is now out!

%% Each constant is commented by the binary signature that it ought
%% to have when used in bit matching...

-define(EXCELRECORDHEADERSIZE,4). % 2 time :16/little-signed-integer

%% Defined in Section 6.1 All of these at :16/little-signed-integer
-define(DIMENSIONS,0).            % 00 00
-define(BLANK,1).                 % 00 01
-define(INTEGER,2).               % 00 02
-define(NUMBER,3).                % 00 03
-define(LABEL,4).                 % 00 04
-define(BOOLERR,5).               % 00 05
-define(FORMULA,6).               % 00 06
-define(STRING,7).                % 00 07
-define(ROW,8).                   % 00 08
-define(BOF,9).                   % 00 09
-define(EOF,10).                  % 00 0A
-define(INDEX,11).                % 00 0B
-define(CALCOUNT,12).             % 00 0C
-define(CALCMODE,13).             % 00 0D
-define(PRECISION,14).            % 00 0E
-define(REFMODE,15).              % OO OF
-define(DELTA,16).                % 00 10
-define(ITERATION,17).            % 00 11
-define(PROTECT,18).              % 00 12
-define(PASSWORD,19).             % 00 13
-define(HEADER,20).               % 00 14
-define(FOOTER,21).               % 00 15
-define(EXTERNCOUNT,22).          % 00 16
-define(EXTERNSHEET,23).          % 00 17
-define(NAME,24).                 % 00 18
-define(WINDOWPROTECT,25).        % 00 19
-define(VERTICALPAGEBREAKS,26).   % 00 1A
-define(HORIZONTALPAGEBREAKS,27). % 00 1B
-define(NOTE,28).                 % 00 1C
-define(SELECTION,29).            % 00 1D
-define(FORMAT,30).               % 00 1E
-define(BUILTINFMCOUNT,31).       % 00 1F
-define(COLUMNDEFAULT,32).        % 00 20
-define(ARRAY,33).                % 00 21
-define(DATEMODE,34).             % 00 22
-define(EXTERNNAME2,35).          % 00 23 (Duplicate)
-define(COLWIDTH,36).             % 00 24
-define(DEFAULTROWHEIGHT,37).     % 00 25
-define(LEFTMARGIN,38).           % 00 26
-define(RIGHTMARGIN,39).          % 00 27
-define(TOPMARGIN,40).            % 00 28
-define(BOTTOMMARGIN,41).         % 00 29
-define(PRINTHEADERS,42).         % 00 2A
-define(PRINTGRIDLINES,43).       % 00 2B
%% No record type 44 -                 2C
%% No record type 45 -                 2D
%% No record type 46 -                 2E
-define(FILEPASS,47).             % 00 2F
%% No record type 48 -                 30
-define(FONT,49).                 % 00 31
%% No record type 50 -                 32
%% No record type 51 -                 33
%% No record type 52 -                 34
%% No record type 53 -                 35
-define(TABLEOP,54).              % 00 36 (Beware TABLEOP_2 is the duplicate of TABLEOP!)
-define(TABLEOP2,55).             % 00 37 (Beware TABLEOP_2 is the duplicate of TABLEOP!)
%% No record type 56 -                 38
%% No record type 57 -                 39
%% No record type 58 -                 3A
%% No record type 59 -                 3B
-define(CONTINUE,60).             % 00 3C
-define(WINDOW1,61).              % 00 3D (Beware doesn't have a duplicate!)
-define(WINDOW2,62).              % 00 3E (Beware WINDOW2_2 is the duplicate of WINDOW2!)
%% No record type 53 -                 3F
-define(BACKUP,64).               % 00 40
-define(PANE,65).                 % 00 41
-define(CODEPAGE,66).             % 00 42
-define(XF,67).                   % 00 43
-define(IXFE,68).                 % 00 44
-define(EFONT,69).                % 00 45
%% No record type 70 -                 46
%% No record type 71 -                 47
%% No record type 72 -                 48
%% No record type 73 -                 49
%% No record type 74 -                 4A
%% No record type 75 -                 4B
%% No record type 76 -                 4C
%% No record type 77 -                 4D
%% No record type 78 -                 4E
%% No record type 79 -                 4F
%% No record type 80 -                 50
-define(DCONREF,81).              % 00 51
%% No record type 82 -                 52
%% No record type 83 -                 53
%% No record type 84 -                 54
-define(DEFCOLWIDTH,85).          % 00 55
-define(BUILTINFMTCOUNT2,86).     % 00 56 (Duplicate)
%% No record type 87 -                 57
%% No record type 88 -                 58
-define(XCT,89).                  % 00 59
-define(CRN,90).                  % 00 5A
-define(FILESHARING,91).          % 00 5B
-define(WRITEACCESS,92).          % 00 5C
%% No record type 93 -                 5D
-define(UNCALCED,94).             % 00 5E
-define(SAVERECALC,95).           % 00 5F
%% No record type 96 -                 60
%% No record type 97 -                 61
%% No record type 98 -                 62
-define(OBJECTPROTECT,99).        % 00 63
%% No record type 99 to 126            64 to 7E
-define(COLINFO,127).             % 00 7F
-define(GUTS,128).                % 00 80
-define(WSBOOL,129).              % 00 81
-define(GRIDSET,130).             % 00 82
-define(HCENTRE,131).             % 00 83
-define(VCENTRE,132).             % 00 84
-define(BOUNDSHEET,133).          % 00 85
-define(WRITEPROT,134).           % 00 86
%% No record type 135 to 139           87 to 8B
-define(COUNTRY,140).             % 00 8C
-define(HIDEOBJ,141).             % 00 8D
-define(SHEETSOFFSET,142).        % 00 8E
-define(SHEETHDR,143).            % 0O 8F
-define(SORT,144).                % 00 90
%% No record type 145                  91
-define(PALETTE,146).             % 00 92
%% No record type 147 to 152           93 to 98
-define(STANDARDWIDTH,153).       % 00 99
%% No record type 154 to 159           9A to 9F
-define(SCL,160).                 % 00 A0
-define(SETUP,161).               % 00 A1
%% No record type 162 to 170           A2 to AA
-define(GCW,171).                 % 00 AB
%% No record type 172 to 188           AC to BC
-define(MULRK,189).               % 00 BD
-define(MULBLANK,190).            % 00 BE
%% No record type 191 to 213           BF to D5
-define(RSTRING,214).             % 00 D6
-define(DBCELL,215).              % 00 D7
%% No record type 216                  D8
%% No record type 217                  D9
-define(BOOKBOOL,218).            % 00 DA
%% No record type 219                  DB
%% No record type 220                  DC
-define(SCENPROTECT,221).         % 00 DD
%% No record type 222                  DE
%% No record type 223                  DF
-define(XF2,224).                 % 00 E0 (Duplicate)
%% No record type 225 to 228           E1 to E4
-define(MERGEDCELLS,229).         % 00 E5
%% No record type 229 to 232           E6 to E8
-define(BITMAP,233).              % 00 E9
%% No record type 234 to 238           EA TO EE
-define(PHONETIC,239).            % 00 EF
%% No record type 240 to 251           F0 TO FB
-define(SST,252).                 % 00 FC
-define(LABELSST,253).            % 00 FD
%% No record type 254                  FE
-define(EXTSST,255).              % 00 FF
%% No record type 256 to 350        01 00 to 01 5E
-define(LABELRANGES,351).         % 01 5F
-define(USESELFS,352).            % 01 60
-define(DSF,353).                 % 01 61
%% No record type 354 to 429        01 62 to 01 AD
-define(SUPBOOK,430).             % 01 AE
%% No record type 431               01 AF
-define(CONDFMT,432).             % 01 B0
%% No record type 433               01 B1
-define(DVAL,434).                % 01 B2
%% No record type 435 to 439        01 B3 to 01 B7
-define(HLINK,440).               % 01 B8
%% No record type 441 to 445        01 B9 to 01 BD
-define(DV,441).                  % 01 BE
%% No record type 442 to 511        01 BF to 01 FF
-define(DIMENSIONS2,512).         % 02 00 (Duplicate)
-define(BLANK2,513).              % 02 01 (Duplicate)
%% No record type 514               02 02
-define(NUMBER2,515).             % 02 03 (Duplicate)
-define(LABEL2,516).              % 02 04 (Duplicate)
-define(BOOLERR2,517).            % 02 05 (Duplicate)
-define(FORMULA2,518).            % 02 06 (Duplicate)
-define(STRING2,519).             % 02 07 (Duplicate)
-define(ROW2,520).                % 02 08 (Duplicate)
-define(BOF2,521).                % 02 09 (Duplicate)
%% No record type 522               02 0A
-define(INDEX2,523).              % 02 0B (Duplicate)
%% No record type 524 to 535        02 0C to 02 17
-define(NAME2,536).               % 02 18 (Duplicate)
%% No record type 537 to 544        02 19 to 02 20
-define(ARRAY2,545).              % 02 21 (Duplicate)
%% No record type 546               02 22
-define(EXTERNNAME,547).          % 02 23
%% No record type 548               02 24
-define(DEFAULTROWHEIGHT2,549).   % 02 25 (Duplicate)
%% No record type 550 to 562        02 26 to 02 30
-define(FONT2,561).               % 02 31 (Duplicate)
%% No record type 562 to 565        02 32 to 02 35
-define(TABLEOP_2,566).           % 02 36 (Duplicate but see TABLEOP2!)
%% No record type 567 to 573        02 37 to 02 3D
-define(WINDOW2_2,574).           % 02 3E (Duplicate)
%% No record type 575 to 578        02 3F to 02 42
-define(XF3,579).                 % 02 43 (Triplicate!)
%% No record type 580 to 637        02 44 to 02 7D
-define(RK,638).                  % 02 7E
%% No record type 639 to 658        02 7F to 02 92
-define(STYLE,659).               % 02 93
%% No record type 660 to 1029       02 94 to 04 05
-define(FORMULA3,1030).           % 04 06 (Triplicate!)
%% No record type 1031 to 1032      04 07 to 04 08
-define(BOF3,1033).               % 04 09 (Triplicate!)
%% No record type 1034 to 1053      04 10 to 04 1D
-define(FORMAT2,1054).            % 04 1E (Duplicate)
%% No record type 1055 to 1090      04 1F to 04 42
-define(XF4,1091).                % 04 43 (Quadruplicate!)
%% No record type 1092 to 1211      04 44 to 04 BB
-define(SHRFMLA,1222).            % 04 BC
%% No record type 1092 to 2047      04 BD to 07 FF
-define(QUICKTIP,2048).           % 08 00
%% No record type 2049 to 2056      08 01 to 08 08
-define(BOF4,2057).               % 08 09 (Quadruplicate!)
%% No record type 2058 to 2145      08 0A to 08 61
-define(SHEETLAYOUT,2146).        % 08 62
%% No record type 2147 to 2150      08 63 to 08 66
-define(SHEETPROTECTION,2151).    % 08 67
-define(RANGEPROTECTION,2152).    % 08 68
