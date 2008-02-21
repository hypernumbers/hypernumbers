%% This defines a series of constants taken from the Open Office
%% specification of the Microsoft Biff8 and Biff8X file formats

%% It can be found at:
%% http://sc.openoffice.org/excelfileformat.pdf

%% This is based on version 1.39 of that documement - 1.41 is now out!

%% Each constant is commented by the binary signature that it ought
%% to have when used in bit matching...

-define(EXCELRECORDHEADERSIZE,4). % 2 time :16/little-signed-integer

%% Defined in Section 6.1 All of these at :16/little-signed-integer
%% Only records in Biff8 are listed here
-define(FORMULA,6).               % 00 06
-define(EOF,10).                  % 00 0A
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
-define(EXTERNSHEET,23).          % 00 17
-define(NAME,24).                 % 00 18
-define(WINDOWPROTECT,25).        % 00 19
-define(VERTICALPAGEBREAKS,26).   % 00 1A
-define(HORIZONTALPAGEBREAKS,27). % 00 1B
-define(NOTE,28).                 % 00 1C
-define(SELECTION,29).            % 00 1D
-define(DATEMODE,34).             % 00 22
-define(EXTERNNAME2,35).          % 00 23 (Duplicate)
-define(LEFTMARGIN,38).           % 00 26
-define(RIGHTMARGIN,39).          % 00 27
-define(TOPMARGIN,40).            % 00 28
-define(BOTTOMMARGIN,41).         % 00 29
-define(PRINTHEADERS,42).         % 00 2A
-define(PRINTGRIDLINES,43).       % 00 2B
-define(FILEPASS,47).             % 00 2F
-define(FONT,49).                 % 00 31
-define(CONTINUE,60).             % 00 3C
-define(WINDOW1,61).              % 00 3D
-define(BACKUP,64).               % 00 40
-define(PANE,65).                 % 00 41
-define(CODEPAGE,66).             % 00 42
-define(DCONREF,81).              % 00 51
-define(DEFCOLWIDTH,85).          % 00 55
-define(XCT,89).                  % 00 59
-define(CRN,90).                  % 00 5A
-define(FILESHARING,91).          % 00 5B
-define(WRITEACCESS,92).          % 00 5C
-define(UNCALCED,94).             % 00 5E
-define(SAVERECALC,95).           % 00 5F
-define(OBJECTPROTECT,99).        % 00 63
-define(COLINFO,127).             % 00 7F
-define(GUTS,128).                % 00 80
-define(WSBOOL,129).              % 00 81
-define(GRIDSET,130).             % 00 82
-define(HCENTRE,131).             % 00 83
-define(VCENTRE,132).             % 00 84
-define(BOUNDSHEET,133).          % 00 85
-define(WRITEPROT,134).           % 00 86
-define(COUNTRY,140).             % 00 8C
-define(HIDEOBJ,141).             % 00 8D
-define(SORT,144).                % 00 90
-define(PALETTE,146).             % 00 92
-define(STANDARDWIDTH,153).       % 00 99
-define(SCL,160).                 % 00 A0
-define(SETUP,161).               % 00 A1
-define(MULRK,189).               % 00 BD
-define(MULBLANK,190).            % 00 BE
-define(RSTRING,214).             % 00 D6
-define(DBCELL,215).              % 00 D7
-define(BOOKBOOL,218).            % 00 DA
-define(SCENPROTECT,221).         % 00 DD
-define(XF2,224).                 % 00 E0 (Duplicate)
-define(MERGEDCELLS,229).         % 00 E5
-define(BITMAP,233).              % 00 E9
-define(PHONETIC,239).            % 00 EF
-define(SST,252).                 % 00 FC
-define(LABELSST,253).            % 00 FD
-define(EXTSST,255).              % 00 FF
-define(LABELRANGES,351).         % 01 5F
-define(USESELFS,352).            % 01 60
-define(DSF,353).                 % 01 61
-define(SUPBOOK,430).             % 01 AE Called EXTERNALBOOK in V1.41!
-define(CONDFMT,432).             % 01 B0
-define(DVAL,434).                % 01 B2
-define(HLINK,440).               % 01 B8
-define(DV,441).                  % 01 BE
-define(DIMENSIONS2,512).         % 02 00 (Duplicate)
-define(BLANK2,513).              % 02 01 (Duplicate)
-define(NUMBER2,515).             % 02 03 (Duplicate)
-define(LABEL2,516).              % 02 04 (Duplicate)
-define(BOOLERR2,517).            % 02 05 (Duplicate)
-define(FORMULA2,518).            % 02 06 (Duplicate)
-define(STRING2,519).             % 02 07 (Duplicate)
-define(ROW2,520).                % 02 08 (Duplicate)
-define(INDEX2,523).              % 02 0B (Duplicate)
-define(ARRAY2,545).              % 02 21 (Duplicate)
-define(DEFAULTROWHEIGHT2,549).   % 02 25 (Duplicate)
-define(TABLEOP_2,566).           % 02 36 (Duplicate but see TABLEOP2!)
-define(WINDOW2_2,574).           % 02 3E (Duplicate)
-define(RK,638).                  % 02 7E
-define(STYLE,659).               % 02 93
-define(FORMAT2,1054).            % 04 1E (Duplicate)
-define(SHRFMLA,1212).            % 04 BC
-define(QUICKTIP,2048).           % 08 00
-define(BOF4,2057).               % 08 09 (Quadruplicate!)
-define(SHEETLAYOUT,2146).        % 08 62
-define(SHEETPROTECTION,2151).    % 08 67
-define(RANGEPROTECTION,2152).    % 08 68
