%%%-------------------------------------------------------------------
%%% File        : excel_records.erl
%%% Author      : Gordon Guthrie <gordonguthrie@gg-laptop>
%%% Description : parses the Excel Records
%%%
%%% Created     : 11 January 2008
%%%-------------------------------------------------------------------
-module(excel_records).

%%% Include files with macros encoding Microsoft File Format constants
-include("microsoftbiff.hrl").
-include("excel_records.hrl").
-include("excel_errors.hrl").
-include("excel_supbook.hrl").

-export([parse_rec/5]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function processes the main record types that are used in the       %%%
%%% BIFF8/BIFF8X formats as described in Section 5.1 of the                  %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_rec(?FORMULA,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[FORMULA *DONE*]"),
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     Result:64/little-unsigned-integer,
     CalcFlag:16/little-unsigned-integer,
     __NotUsed:32/little-unsigned-integer,
     Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
				  "ColIndex is ~p~n"++
				  "XFIndex is ~p~n"++
				  "Result is ~p~n"++
				  "CalcFlag is ~p",
				  [RowIndex,
				   ColIndex,
				   XFIndex,
				   Result,
				   CalcFlag])),
    Formula=parse_FRM_Results(formula,Rest,Tables,FileOut),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{formula,Formula}]),
    excel_util:put_log(FileOut,"[/FORMULA]"),
    {ok,ok};
parse_rec(?EOF,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[EOF]"),
    excel_util:put_log(FileOut,"[/EOF]"),
    {ok,ok};
parse_rec(?CALCOUNT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CALCOUNT]"),
    excel_util:put_log(FileOut,"CALCOUNT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CALCOUNT]"),
    {ok,ok};
parse_rec(?CALCMODE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CALCMODE]"),
    excel_util:put_log(FileOut,"CALCMODE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CALCMODE]"),
    {ok,ok};
parse_rec(?PRECISION,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PRECISION]"),
    excel_util:put_log(FileOut,"PRECISION is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PRECISION]"),
    {ok,ok};
parse_rec(?REFMODE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[REFMODE]"),
    excel_util:put_log(FileOut,"REFMODE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/REFMODE]"),
    {ok,ok};
parse_rec(?DELTA,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DELTA]"),
    excel_util:put_log(FileOut,"DELTA is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DELTA]"),
    {ok,ok};
parse_rec(?ITERATION,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[ITERATION]"),
    excel_util:put_log(FileOut,"ITERATION is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/ITERATION]"),
    {ok,ok};
parse_rec(?PROTECT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PROTECT]"),
    excel_util:put_log(FileOut,"PROTECT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PROTECT]"),
    {ok,ok};
parse_rec(?PASSWORD,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PASSWORD]"),
    excel_util:put_log(FileOut,"PASSWORD is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PASSWORD]"),
    {ok,ok};
parse_rec(?HEADER,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[HEADER]"),
    excel_util:put_log(FileOut,"HEADER is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/HEADER]"),
    {ok,ok};
parse_rec(?FOOTER,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[]"),
    excel_util:put_log(FileOut,"FOOTER is not being processed at the moment"),
    excel_util:put_log(FileOut,"[]"),
    {ok,ok};
parse_rec(?EXTERNSHEET,Bin,_Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[EXTERNSHEET *Done*]"),
    <<NumRefs:16/little-unsigned-integer,
      R2/binary>>=Bin,
      %% io:format("in excel_records:parse_rec for Externsheet NumRefs is ~p~n",
      %%  [NumRefs]),
        {ok,ok}=parse_externsheet(R2,0,Tables,FileOut),
    excel_util:put_log(FileOut,"[/EXTERNSHEET]"),
    {ok,ok};
parse_rec(?NAME,Bin,_Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[NAME *DONE*]"),
    <<OptionFlag:2/binary,
     KybdShortCut:8/little-unsigned-integer,
     NameLength:8/little-unsigned-integer,
     Size:16/little-unsigned-integer,
     _NotUsed:16/little-unsigned-integer,
     SheetIndex:16/little-unsigned-integer,
     MenuTxtLen:8/little-unsigned-integer,
     DescTxtLen:8/little-unsigned-integer,
     HelpTxtLen:8/little-unsigned-integer,
     StatusTxtLen:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("OptionFlag is ~p~n"++
     "KybdShortCut is ~p~n"++
     "NameLength is ~p~n"++
     "Size is ~p~n"++
     "SheetIndex is ~p~n"++
     "MenuTxtLen is ~p~n"++
     "DescTxtLen is ~p~n"++
     "HelpTxtLen is ~p~n"++
     "StatusTxtLen is ~p~n",
    [OptionFlag,
     KybdShortCut,
     NameLength,
     Size,
     SheetIndex,
     MenuTxtLen,
     DescTxtLen,
     HelpTxtLen,
     StatusTxtLen])),
     parse_Name(OptionFlag,KybdShortCut,NameLength,Size,SheetIndex,
     MenuTxtLen,DescTxtLen,HelpTxtLen,StatusTxtLen,Rest,Tables,FileOut),
    excel_util:put_log(FileOut,"[/NAME]"),
    {ok,ok};
parse_rec(?WINDOWPROTECT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WINDOWPROTECT]"),
    excel_util:put_log(FileOut,"WINDOWPROTECT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WINDOWPROTECT]"),
    {ok,ok};
parse_rec(?VERTICALPAGEBREAKS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[VERTICALPAGEBREAKS]"),
    excel_util:put_log(FileOut,"VERTICALPAGEBREAKS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/VERTICALPAGEBREAKS]"),
    {ok,ok};
parse_rec(?HORIZONTALPAGEBREAKS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[HORIZONTALPAGEBREAKS]"),
    excel_util:put_log(FileOut,"HORIZONTALPAGEBREAKS is not being processed "++
	    "at the moment"),
    excel_util:put_log(FileOut,"[/HORIZONTALPAGEBREAKS]"),
    {ok,ok};
parse_rec(?NOTE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[NOTE]"),
    excel_util:put_log(FileOut,"NOTE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/NOTE]"),
    {ok,ok};
parse_rec(?SELECTION,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SELECTION]"),
    excel_util:put_log(FileOut,"SELECTION is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SELECTION]"),
    {ok,ok};
parse_rec(?DATEMODE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DATEMODE]"),
    excel_util:put_log(FileOut,"DATEMODE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DATEMODE]"),
    {ok,ok};
parse_rec(?EXTERNNAME2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[EXTERNNAME2]"),
    excel_util:put_log(FileOut,"EXTERNNAME2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/EXTERNNAME2]"),
    {ok,ok};
parse_rec(?LEFTMARGIN,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[LEFTMARGIN]"),
    excel_util:put_log(FileOut,"LEFTMARGIN is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/LEFTMARGIN]"),
    {ok,ok};
parse_rec(?RIGHTMARGIN,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[RIGHTMARGIN]"),
    excel_util:put_log(FileOut,"RIGHTMARGIN is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/RIGHTMARGIN]"),
    {ok,ok};
parse_rec(?TOPMARGIN,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[TOPMARGIN]"),
    excel_util:put_log(FileOut,"TOPMARGIN is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/TOPMARGIN]"),
    {ok,ok};
parse_rec(?BOTTOMMARGIN,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[BOTTOMMARGIN]"),
    excel_util:put_log(FileOut,"BOTTOMMARGIN is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/BOTTOMMARGIN]"),
    {ok,ok};
parse_rec(?PRINTHEADERS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PRINTHEADERS]"),
    excel_util:put_log(FileOut,"PRINTHEADERS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PRINTHEADERS]"),
    {ok,ok};
parse_rec(?PRINTGRIDLINES,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PRINTGRIDLINES]"),
    excel_util:put_log(FileOut,"PRINTGRIDLINES is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PRINTGRIDLINES]"),
    {ok,ok};
parse_rec(?FILEPASS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[FILEPASS]"),
    excel_util:put_log(FileOut,"FILEPASS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/FILEPASS]"),
    {ok,ok};
parse_rec(?FONT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[FONT]"),
    excel_util:put_log(FileOut,"FONT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/FONT]"),
    {ok,ok};
parse_rec(?CONTINUE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CONTINUE]"),
    excel_util:put_log(FileOut,"CONTINUE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CONTINUE]"),
    {ok,ok};
parse_rec(?WINDOW1,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WINDOW1]"),
    excel_util:put_log(FileOut,"WINDOW1 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WINDOW1]"),
    {ok,ok};
parse_rec(?BACKUP,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[BACKUP]"),
    excel_util:put_log(FileOut,"BACKUP is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/BACKUP]"),
    {ok,ok};
parse_rec(?PANE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PANE]"),
    excel_util:put_log(FileOut,"PANE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PANE]"),
    {ok,ok};
parse_rec(?CODEPAGE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CODEPAGE]"),
    excel_util:put_log(FileOut,"CODEPAGE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CODEPAGE]"),
    {ok,ok};
parse_rec(?DCONREF,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DCONREF]"),
    excel_util:put_log(FileOut,"DCONREF is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DCONREF]"),
    {ok,ok};
parse_rec(?DEFCOLWIDTH,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DEFCOLWIDTH]"),
    excel_util:put_log(FileOut,"DEFCOLWIDTH is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DEFCOLWIDTH]"),
    {ok,ok};
parse_rec(?XCT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[XCT]"),
    excel_util:put_log(FileOut,"XCT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/XCT]"),
    {ok,ok};
parse_rec(?CRN,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CRN]"),
    excel_util:put_log(FileOut,"CRN is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CRN]"),
    {ok,ok};
parse_rec(?FILESHARING,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[FILESHARING]"),
    excel_util:put_log(FileOut,"FILESHARING is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/FILESHARING]"),
    {ok,ok};
parse_rec(?WRITEACCESS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WRITEACCESS]"),
    excel_util:put_log(FileOut,"WRITEACCESS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WRITEACCESS]"),
    {ok,ok};
parse_rec(?UNCALCED,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[UNCALCED]"),
    excel_util:put_log(FileOut,"UNCALCED is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/UNCALCED]"),
    {ok,ok};
parse_rec(?SAVERECALC,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SAVERECALC]"),
    excel_util:put_log(FileOut,"SAVERECALC is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SAVERECALC]"),
    {ok,ok};
parse_rec(?OBJECTPROTECT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[OBJECTPROTECT]"),
    excel_util:put_log(FileOut,"OBJECTPROTECT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/OBJECTPROTECT]"),
    {ok,ok};
parse_rec(?COLINFO,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[COLINFO]"),
    excel_util:put_log(FileOut,"COLINFO is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/COLINFO]"),
    {ok,ok};
parse_rec(?GUTS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[GUTS]"),
    excel_util:put_log(FileOut,"GUTS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/GUTS]"),
    {ok,ok};
parse_rec(?WSBOOL,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WSBOOL]"),
    excel_util:put_log(FileOut,"WSBOOL is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WSBOOL]"),
    {ok,ok};
parse_rec(?GRIDSET,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[GRIDSET]"),
    excel_util:put_log(FileOut,"GRIDSET is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/GRIDSET]"),
    {ok,ok};
parse_rec(?HCENTRE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[HCENTRE]"),
    excel_util:put_log(FileOut,"HCENTRE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/HCENTRE]"),
    {ok,ok};
parse_rec(?VCENTRE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[VCENTRE]"),
    excel_util:put_log(FileOut,"VCENTRE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/VCENTRE]"),
    {ok,ok};
parse_rec(?BOUNDSHEET,Bin,_Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[BOUNDSHEET *DONE*]"),
    {SheetBOF,Visibility,SheetType,Name,SheetName}=excel_util:get_bound_sheet(Bin,Tables,FileOut),
    [{Type,NameBin}]=SheetName,
    excel_util:append_sheet_name(Tables,binary_to_list(NameBin)),
    excel_util:put_log(FileOut,io_lib:fwrite("SheetBOF is ~p~n"++
				  "Visibility is ~p~n"++
				  "SheetType is ~p~n"++
				  "Name is ~p",
				  [SheetBOF,
				   Visibility,
				   SheetType,
				   Name])),
    excel_util:put_log(FileOut,io_lib:fwrite("Visibility can be one of the following:~n"++
				  "* Visible       0~n"++
				  "* Hidden        1~n"++
				  "* Strong Hidden 2~n"++
				  "Sheet Type can be one of the following:~n"++
				  "* Worksheet            0~n"++
				  "* Chart                2~n"++
				  "* Visible Basic Module 6",[])),
    excel_util:put_log(FileOut,io_lib:fwrite("SheetName is ~p",
				  [SheetName])),
    excel_util:put_log(FileOut,"[/BOUNDSHEET]"),
    {ok,ok};
parse_rec(?WRITEPROT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WRITEPROT]"),
    excel_util:put_log(FileOut,"WRITEPROT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WRITEPROT]"),
    {ok,ok};
parse_rec(?COUNTRY,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[COUNTRY]"),
    excel_util:put_log(FileOut,"COUNTRY is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/COUNTRY]"),
    {ok,ok};
parse_rec(?HIDEOBJ,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[HIDEOBJ]"),
    excel_util:put_log(FileOut,"HIDEOBJ is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/HIDEOBJ]"),
    {ok,ok};
parse_rec(?SORT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SORT]"),
    excel_util:put_log(FileOut,"SORT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SORT]"),
    {ok,ok};
parse_rec(?PALETTE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PALETTE]"),
    excel_util:put_log(FileOut,"PALETTE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PALETTE]"),
    {ok,ok};
parse_rec(?STANDARDWIDTH,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[STANDARDWIDTH]"),
    excel_util:put_log(FileOut,"STANDARDWIDTH is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/STANDARDWIDTH]"),
    {ok,ok};
parse_rec(?SCL,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SCL]"),
    excel_util:put_log(FileOut,"SCL is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SCL]"),
    {ok,ok};
parse_rec(?SETUP,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SETUP]"),
    excel_util:put_log(FileOut,"SETUP is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SETUP]"),
    {ok,ok};
parse_rec(?MULRK,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[MULRK *DONE*]"),
    <<RowIndex:16/little-unsigned-integer,
     FirstColIndex:16/little-unsigned-integer,
     Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
				  "FirstColIndex is ~p",
				  [RowIndex,
				   FirstColIndex])),
    %5io:format("in excel_records:parse_rec for MULRK RowIndex is ~p and FirstColIndex is ~p~n",
    %%    [RowIndex,FirstColIndex]),
    Tokens=parse_XF_RK(Rest,Tables,FileOut),
    write_row(Tokens,RowIndex,FirstColIndex,Name,Tables),
    %%io:format("in excel_records:parse_rec for MULRK Tokens are:~n ~p~n",[Tokens]),
    excel_util:put_log(FileOut,"[/MULRK]"),
    {ok,ok};
parse_rec(?MULBLANK,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[MULBLANK]"),
    excel_util:put_log(FileOut,"MULBLANK is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/MULBLANK]"),
    {ok,ok};
parse_rec(?RSTRING,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[RSTRING]"),
    excel_util:put_log(FileOut,"RSTRING is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/RSTRING]"),
    {ok,ok};
parse_rec(?DBCELL,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DBCELL]"),
    excel_util:put_log(FileOut,"DBCELL is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DBCELL]"),
    {ok,ok};
parse_rec(?BOOKBOOL,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[BOOKBOOL]"),
    excel_util:put_log(FileOut,"BOOKBOOL is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/BOOKBOOL]"),
    {ok,ok};
parse_rec(?SCENPROTECT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SCENPROTECT]"),
    excel_util:put_log(FileOut,"SCENPROTECT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SCENPROTECT]"),
    {ok,ok};
parse_rec(?XF2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[XF2]"),
    excel_util:put_log(FileOut,"XF2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/XF2]"),
    {ok,ok};
parse_rec(?MERGEDCELLS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[MERGEDCELLS]"),
    excel_util:put_log(FileOut,"MERGEDCELLS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/MERGEDCELLS]"),
    {ok,ok};
parse_rec(?BITMAP,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[BITMAP]"),
    excel_util:put_log(FileOut,"BITMAP is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/BITMAP]"),
    {ok,ok};
parse_rec(?PHONETIC,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[PHONETIC]"),
    excel_util:put_log(FileOut,"PHONETIC is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/PHONETIC]"),
    {ok,ok};
parse_rec(?SST,[H|T],Name,Tables,FileOut)->
    %%io:format("in excel_records:parse_rec for SST~n"),
    excel_util:put_log(FileOut,"[SST *DONE*]"),
    <<NoStringsUsed:32/little-unsigned-integer,
     NoActualStrings:32/little-unsigned-integer,
     Rest/binary>>=H,
    excel_util:put_log(FileOut,io_lib:fwrite("NoStringsUsed is ~p~n "++
				  "and NoActualString is ~p~n",
				  [NoStringsUsed,NoActualStrings])),
    parse_SST(0,NoActualStrings,Tables,[Rest|T],FileOut),
    excel_util:put_log(FileOut,"[/SST]"),
    {ok,ok};
parse_rec(?LABELSST,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[LABELSST *Done*]"),
       <<RowIndex:16/little-unsigned-integer,
        ColIndex:16/little-unsigned-integer,
        XFIndex:16/little-unsigned-integer,
        SSTIndex:32/little-unsigned-integer,
        Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
				  "ColIndex is ~p~n"++
				  "XFIndex is ~p~n"++
				  "SSTIndex is ~p",
				  [RowIndex,
				   ColIndex,
				   XFIndex,
				   SSTIndex])),
    %% Now look up the string in the string table
    String=excel_util:lookup_string(Tables,SSTIndex),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{string,String}]),
    excel_util:put_log(FileOut,"[/LABELSST]"),    
    {ok,ok};
parse_rec(?EXTSST,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[EXTSST]"),
    excel_util:put_log(FileOut,"EXTSST is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/EXTSST]"),
    {ok,ok};
parse_rec(?LABELRANGES,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[LABELRANGES]"),
    excel_util:put_log(FileOut,"LABELRANGES is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/LABELRANGES]"),
    {ok,ok};
parse_rec(?USESELFS,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[USESELFS]"),
    excel_util:put_log(FileOut,"USESELFS is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/USESELFS]"),
    {ok,ok};
parse_rec(?DSF,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DSF]"),
    excel_util:put_log(FileOut,"DSF is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DSF]"),
    {ok,ok};
parse_rec(?SUPBOOK,Bin,_Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[SUPBOOK *Done]"),
    %% io:format("in excel_records:parse_rec for SUPBOOK Bin is ~p~n",[Bin]),
    case Bin of
        <<NoSheets:16/little-unsigned-integer,
        ?SheetsInBook:16/little-unsigned-integer>> ->
          excel_util:put_log(FileOut,
            io_lib:fwrite("SupBook No of sheets in workbook is ~p~n",[NoSheets])),
          excel_util:write(Tables,misc,[{noofsheets,NoSheets}]);
        _ ->
          excel_util:put_log(FileOut,
            io_lib:fwrite("This type of SUPBOOK not being processed!~n",[]))
    end,
    excel_util:put_log(FileOut,"[/SUPBOOK]"),
    {ok,ok};
parse_rec(?CONDFMT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[CONDFMT]"),
    excel_util:put_log(FileOut,"CONDFMT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/CONDFMT]"),
    {ok,ok};
parse_rec(?DVAL,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DVAL]"),
    excel_util:put_log(FileOut,"DVAL is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DVAL]"),
    {ok,ok};
parse_rec(?HLINK,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[HLINK]"),
    excel_util:put_log(FileOut,"HLINK is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/HLINK]"),
    {ok,ok};
parse_rec(?DV,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DV]"),
    excel_util:put_log(FileOut,"DV is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DV]"),
    {ok,ok};
parse_rec(?DIMENSIONS2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DIMENSIONS2]"),
    excel_util:put_log(FileOut,"DIMENSIONS2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DIMENSIONS2]"),
    {ok,ok};
parse_rec(?BLANK2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[BLANK2]"),
    excel_util:put_log(FileOut,"BLANK2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/BLANK2]"),
    {ok,ok};
parse_rec(?NUMBER2,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[NUMBER2 *done*]"),
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     Float:64/little-unsigned-float>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("(Unparsed) Bin is ~p",[Bin])),
    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
				  "ColIndex is ~p~n"++
				  "XFIndex is ~p~n"++
				  "Float (unparsed Binary) is ~p",
				  [RowIndex,
				   ColIndex,
				   XFIndex,
				   Float])),
    %%RKValue2=parse_CRS_RK(<<RKValue:64/little-unsigned-integer>>,FileOut),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{value,number,Float}]),
    excel_util:put_log(FileOut,io_lib:fwrite("(Parsed) RKValue is ~p",[Float])),
    excel_util:put_log(FileOut,"[/NUMBER2]"),
    {ok,ok};
parse_rec(?LABEL2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[LABEL2]"),
    excel_util:put_log(FileOut,"LABEL2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/LABEL2]"),
    {ok,ok};
parse_rec(?BOOLERR2,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[BOOLERR2 *DONE*]"),
    %% One might think that a record called BoolErr would contain a boolean error
    %% and error pertaining or obtaining in some straightforwar dway to Booleans
    %% wouldn't one? But on no - it contains a Boolean *OR* an Error
    %% How mad the fuck is that?
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     BoolErr:8/little-unsigned-integer,
     Type:8/little-unsigned-integer>>=Bin,
    {ValType,Value} = case Type of
        0 -> 
            excel_util:put_log(FileOut,"It's a Boolean!"),
            case BoolErr of
              0 -> {boolean,false};
              1 -> {boolean,true}
           end;
        1 ->
            excel_util:put_log(FileOut,"It's an Error!"),
            case BoolErr of
              ?NullError    -> {error,"#NULL!"};
              ?DivZeroError -> {error,"#DIV/0!"};
              ?ValueError   -> {error,"#VALUE!"};
              ?RefError     -> {error,"#REF!"};
              ?NameError    -> {error,"#NAME?"};
              ?NumError     -> {error,"#NUM!"};
              ?NAError      -> {error,"#N/A"}
          end
    end,
    io:format("in excel_records:parse_rec ValType is ~p and Value is ~p~n",
        [ValType,Value]),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{value,ValType,Value}]),
    excel_util:put_log(FileOut,"[/BOOLERR2]"),
    {ok,ok};
parse_rec(?STRING2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[STRING2]"),
    excel_util:put_log(FileOut,"STRING2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/STRING2]"),
    {ok,ok};
parse_rec(?ROW2,Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[ROW2 *DONE*]"),
    case Bin of
	<<RowIndex:16/little-unsigned-integer,
	 FirstColIndex:16/little-unsigned-integer,
	 LastColIndex:16/little-unsigned-integer,
	 Height:16/little-unsigned-integer,
	 __NotUsed:16/little-unsigned-integer,
	 DefaultsFlag:8/little-unsigned-integer,
	 RelativeOffset:16/little-unsigned-integer>> ->
	    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
					  "FirstColIndex is ~p~n"++
					  "LastColIndex is ~p~n"++
					  "Height is ~p~n"++
					  "__NotUsed is ~p~n"++
					  "DefaultsFlag is ~p~n"++
					  "RelativeOffset is ~p",
					  [RowIndex,
					   FirstColIndex,
					   LastColIndex,
					   Height,
					   __NotUsed,
					   DefaultsFlag,
					   RelativeOffset]));
	<<RowIndex:16/little-unsigned-integer,
	 FirstColIndex:16/little-unsigned-integer,
	 LastColIndex:16/little-unsigned-integer,
	 Height:16/little-unsigned-integer,
	 __NotUsed:16/little-unsigned-integer,
	 DefaultsFlag:8/little-unsigned-integer,
	 RelativeOffset:16/little-unsigned-integer,
	 DefaultRowAttributes:24/little-unsigned-integer>> ->
	    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
					  "FirstColIndex is ~p~n"++
					  "LastColIndex is ~p~n"++
					  "Height is ~p~n"++
					  "__NotUsed is ~p~n"++
					  "DefaultsFlag is ~p~n"++
					  "RelativeOffset is ~p~n"++
					  "DefaultRowAttributes is ~p",
					  [RowIndex,
					   FirstColIndex,
					   LastColIndex,
					   Height,
					   __NotUsed,
					   DefaultsFlag,
					   RelativeOffset,
					   DefaultRowAttributes])),
	    excel_util:put_log(FileOut,"DefaultRowAttributes not being processed yet");
	<<RowIndex:16/little-unsigned-integer,
	 FirstColIndex:16/little-unsigned-integer,
	 LastColIndex:16/little-unsigned-integer,
	 Height:16/little-unsigned-integer,
	 __NotUsed:16/little-unsigned-integer,
	 DefaultsFlag:8/little-unsigned-integer,
	 RelativeOffset:16/little-unsigned-integer,
	 XFIndex:16/little-unsigned-integer>> ->
	    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
					  "FirstColIndex is ~p~n"++
					  "LastColIndex is ~p~n"++
					  "Height is ~p~n"++
					  "__NotUsed is ~p~n"++
					  "DefaultsFlag is ~p~n"++
					  "RelativeOffset is ~p~n"++
					  "XFIndex is ~p",
					  [RowIndex,
					   FirstColIndex,
					   LastColIndex,
					   Height,
					   __NotUsed,
					   DefaultsFlag,
					   RelativeOffset,
					   XFIndex])),
	    excel_util:put_log(FileOut,"XFIndex not being processed yet")
    end,
    excel_util:put_log(FileOut,"[/ROW2]"),
    {ok,ok};
parse_rec(?INDEX2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[INDEX2]"),
    excel_util:put_log(FileOut,"INDEX2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/INDEX2]"),
    {ok,ok};
parse_rec(?ARRAY2,Bin,_Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[ARRAY2 *DONE*]"),
    <<Range:8/binary, % 6 + 2 bits
      _NotUsed:4/binary,
      Tokens/binary>>=Bin,
      io:format("in excel_records:parse_rec for ARRAY~n-Range is ~p~n",[Range]),
      {[{Row,Col,_,_}],_Bin}=excel_util:read_cell_range_addies(1,Range,FileOut),
      io:format("in excel_records:parse_rec for ARRAY~n-Row is ~p~n-Col is ~p~n"++
          "-TokenArray is ~p~n",[Row,Col,Tokens]),
      Formula="{"++parse_FRM_Results(formula,Tokens,Tables,FileOut)++"}",
      io:format("in excel_records:parse_rec for ARRAY~n-Formula are ~p~n",[Formula]),
      excel_util:write(Tables,arrayformula,[{{row_index,Row},{col_index,Col}},{formula,Formula}]),
      %% filefilters:dump(Tables),
      %% exit("goodbye from within excel_records - fix arrays"),
    excel_util:put_log(FileOut,"[/ARRAY2]"),
    {ok,ok};
parse_rec(?DEFAULTROWHEIGHT2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[DEFAULTROWHEIGHT2]"),
    excel_util:put_log(FileOut,"DEFAULTROWHEIGHT2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/DEFAULTROWHEIGHT2]"),
    {ok,ok};
parse_rec(?TABLEOP_2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[TABLEOP_2]"),
    excel_util:put_log(FileOut,"TABLEOP_2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/TABLEOP_2]"),
    {ok,ok};
parse_rec(?WINDOW2_2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[WINDOW2_2]"),
    excel_util:put_log(FileOut,"WINDOW2_2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/WINDOW2_2]"),
    {ok,ok};
parse_rec(?RK,Bin,Name,Tables,FileOut)->
    excel_util:put_log(FileOut,"[RK *DONE*]"),
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     RKValue:32/little-unsigned-integer>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("(Unparsed) Bin is ~p",[Bin])),
    excel_util:put_log(FileOut,io_lib:fwrite("RowIndex is ~p~n"++
				  "ColIndex is ~p~n"++
				  "XFIndex is ~p~n"++
				  "RKValue (unparsed Binary) is ~p",
				  [RowIndex,
				   ColIndex,
				   XFIndex,
				   RKValue])),
    RKValue2=excel_util:parse_CRS_RK(<<RKValue:32/little-unsigned-integer>>,FileOut),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{value,number,RKValue2}]),
    excel_util:put_log(FileOut,io_lib:fwrite("(Parsed) RKValue is ~p",[RKValue2])),
    excel_util:put_log(FileOut,"[/RK]"),
    {ok,ok};
parse_rec(?STYLE,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[STYLE]"),
    excel_util:put_log(FileOut,"STYLE is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/STYLE]"),
    {ok,ok};
parse_rec(?FORMAT2,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[FORMAT2]"),
    excel_util:put_log(FileOut,"FORMAT2 is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/FORMAT2]"),
    {ok,ok};
parse_rec(?SHRFMLA,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SHRFMLA]"),
    excel_util:put_log(FileOut,"SHRFMLA is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SHRFMLA]"),
    {ok,ok};
parse_rec(?QUICKTIP,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[QUICKTIP]"),
    excel_util:put_log(FileOut,"QUICKTIP is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/QUICKTIP]"),
    {ok,ok};
parse_rec(?BOF4,Bin,_Name,_Tables,FileOut)->
    %% BOF BIFF8 Section 5.8.1 excelfileformat.pdf V1.40
    excel_util:put_log(FileOut,"[BOF4 RECORD *DONE*]"),
    <<BiffVsn:16/little-unsigned-integer,
     Type:16/little-unsigned-integer,
     BuildID:16/little-unsigned-integer,
     BuildYr:16/little-unsigned-integer,
     FileHist:32/little-unsigned-integer,
     LowestVsn:32/little-unsigned-integer>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("BiffVsn is ~p~n"++
				  "(BOF4) Type is ~p~n"++
				  "BuildID is ~p~n"++
				  "BuildYr is ~p~n"++
				  "FileHist is ~p~n"++
				  "LowestVsn is ~p",
				  [BiffVsn,
				   Type,
				   BuildID,
				   BuildYr,
				   FileHist,
				   LowestVsn])),
    excel_util:put_log(FileOut,io_lib:fwrite("Type can have the following values:~n"++
				  "Workbook Globals ~p~n"++
				  "VB Modules       ~p~n"++
				  "Sheet/Dialog     ~p~n"++
				  "Chart            ~p~n"++
				  "Macro            ~p~n"++
				  "Workspace        ~p~n",
				  [?rc_BOF_WorkbookGlobals,
				   ?rc_BOF_VBModule,
				   ?rc_BOF_Worksheet,
				   ?rc_BOF_Chart,
				   ?rc_BOF_MacroSheet,
				   ?rc_BOF_Workspace])),
    excel_util:put_log(FileOut,"[/BOF4 RECORD]"),
    {ok,ok};
parse_rec(?SHEETLAYOUT,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SHEETLAYOUT]"),
    excel_util:put_log(FileOut,"SHEETLAYOUT is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SHEETLAYOUT]"),
    {ok,ok};
parse_rec(?SHEETPROTECTION,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[SHEETPROTECTION]"),
    excel_util:put_log(FileOut,"SHEETPROTECTION is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/SHEETPROTECTION]"),
    {ok,ok};
parse_rec(?RANGEPROTECTION,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[RANGEPROTECTION]"),
    excel_util:put_log(FileOut,"RANGEPROTECTION is not being processed at the moment"),
    excel_util:put_log(FileOut,"[/RANGEPROTECTION]"),
    {ok,ok};
parse_rec(Other,_Bin,_Name,_Tables,FileOut)->
    excel_util:put_log(FileOut,"[****ERROR*****]"),
    excel_util:put_log(FileOut,io_lib:fwrite("Non-existant type 0x~.16b",[Other])),
    excel_util:put_log(FileOut,"[\****ERROR*****]"),
    FileOut2=FileOut++".error",
    excel_util:put_log(FileOut2,io_lib:fwrite("Non-existant type 0x~.16b",[Other])),
    {ok,ok}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These helper functions help process the formulas in the BIFF8/BIFF8X     %%%
%%% formats as described in Section 3 of the excelfileformat.pdf (V1.40)     %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function entry is described in Section 5.47 of
%% excelfileformat.pdf (V1.40)
%%
%% Don't think this entry point is used anywhere...
%%
%% COMMENTED OUT 2008_01_13
%%
%% If still commented out after a good time then just delete it...
%%
%%parse_FRM_Results(type,Bin,Tables,_FileOut)->
%%    <<Type:8/integer,_Rest:56/integer>>=Bin,
%%    io:format("in excel_records:parse_FRM_Results Type is ~p~n",[Type]),
%%    _Type2=case Type of
%%	      ?rc_FORMULA_STRING     -> string;
%%	      ?rc_FORMULA_BOOLEAN    -> boolean;
%%	      ?rc_FORMULA_ERROR      -> error;
%%	      ?rc_FORMULA_EMPTY_CELL -> empty_cell;
%%	      _Other                 -> number
%%	  end;

%% This function entry is described in Section 3.1 of
%% excelfileformat.pdf (V1.40)
parse_FRM_Results(formula,<<>>,Tables,FileOut)->
    excel_util:put_log(FileOut,io_lib:fwrite("in parse_FRM_Results - "++
				  "finished parsing results",[])),
    ok;
parse_FRM_Results(formula,Bin,Tables,FileOut) ->
    <<Size:16/little-unsigned-integer,Rest/binary>>=Bin,
    excel_util:put_log(FileOut,io_lib:fwrite("in parse_FRM_Results the "++
				  "record size is ~p~nbinary is ~p",
				  [Size,Bin])),
    Tokens=case Size of
	       0     -> 
            io:format("in excel_records:parse_FRM_Results - zero length formula!"),
			      [];
	       RPN_Size -> 
            <<RPN:RPN_Size/binary,TokenArray/binary>>=Rest,
            io:format("in excel_records:parse_FRM_Results RPN is ~p~n-RPN_Size is ~p~n"++
                            "-TokenArray is ~p~n",[RPN,RPN_Size,TokenArray]),
            excel_tokens:parse_tokens(RPN,TokenArray,[],Tables,FileOut)
    end,
    excel_util:put_log(FileOut,io_lib:fwrite("in parse_FRM_Results the "++
				  "tokens are ~p",[Tokens])),
    Tokens.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the name records defined in Section 5.67 of          %%%
%%% excelfileformat.pdf (V1.40) and extracts the name table                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_Name(OptionFlag,KybdShortCut,NameLength,Size,SheetIndex,
  MenuTxtLen,DescTxtLen,HelpTxtLen,StatusTxtLen,Bin,Tables,FileOut)->
  <<Hidden:1/integer,Func:1/integer,VBasic:1/integer,Macro:1/integer,
    Complex:1/integer,BuiltIn:1/integer,FuncGroup1:1/integer,
    FuncGroup2:1/integer,FuncGroup3:1/integer,FuncGroup4:1/integer,
    FuncGroup5:1/integer,FuncGroup6:1/integer,
    Binary:1/integer,_A:1/integer,_B:1/integer,_C:1/integer>>=OptionFlag,
  excel_util:put_log(FileOut,io_lib:fwrite(" OptionsFlags are:~n-Hidden is ~p~n"++
    "-Func is ~p~n"++
    "-VBasic is ~p~n"++
    "-Macro is ~p~n"++
    "-Complex is ~p~n"++
    "-BuiltIn is ~p~n"++
    "-FuncGroups are ~p ~p ~p ~p ~p ~p~n"++
    "-Binary is ~p~n",
    [Hidden,
     Func,
     VBasic,
     Macro,
     Complex,
     BuiltIn,
     FuncGroup1,
     FuncGroup2,
     FuncGroup3,
     FuncGroup4,
     FuncGroup5,
     FuncGroup6,
     Binary])),
  <<Options:1/binary,Name:NameLength/binary,Rest/binary>>=Bin,
  excel_util:put_log(FileOut,io_lib:fwrite("Just ignoring the compression options for "++
    "Unicode names at the moment - will wig when not using Latin-1~n",[])),
  excel_util:put_log(FileOut,io_lib:fwrite("Name is ~p~n",[Name])),
  Scope = case SheetIndex of
            0 -> global;
            _ -> local
        end,
  excel_util:write(Tables,names,[{index,SheetIndex},{type,Scope},{name,Name}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the Multiple RK records defined in Section 5.66 of   %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_XF_RK(Bin,Tables,FileOut)->
  parse_XF_RK(Bin,[],Tables,FileOut).

%%parse_XF_RK(<<>>,Residuum,Tables,FileOut)->
%%    excel_util:put_log(FileOut,"**ERROR** There ought to be a LastColIndex fragment"),
%%    excel_util:put_log(FileOut++".error","ERROR There should be a LastColIndex fragment"),
parse_XF_RK(<<LastColIndex:16/little-unsigned-integer>>,Residuum,Tables,FileOut)->
    excel_util:put_log(FileOut,io_lib:fwrite("LastColIndex is ~p",[LastColIndex])),
    lists:reverse(Residuum);
parse_XF_RK(<<XFIndex:16/little-unsigned-integer,
	     RKValue:32/little-unsigned-integer,
	     Rest/binary>>,Residuum,Tables,FileOut)->
    excel_util:put_log(FileOut,io_lib:fwrite("Rest is ~p",[Rest])),
    excel_util:put_log(FileOut,io_lib:fwrite("XFIndex is ~p~n"++
				  "RKValue is ~p",
				  [XFIndex,
				   RKValue])),
    Num=excel_util:parse_CRS_RK(<<RKValue:32/little-unsigned-integer>>,FileOut),
    excel_util:put_log(FileOut,io_lib:fwrite("Num is ~p",[Num])),
    NewResiduum=[{{xf_index,XFIndex},{value,number,Num}}|Residuum],
    parse_XF_RK(Rest,NewResiduum,Tables,FileOut).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process composites of the main record types of           %%%
%%% Section 5.1 of excelfileformat.pdf (V1.40) and the common record         %%%
%%% substructures of Section 2.5                                             %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_SST(NoOfStrings,NoOfStrings,_Tables,_,FileOut)->
    excel_util:put_log(FileOut,"String table parsed");
parse_SST(StringNo,NoOfStrings,Tables,[BinHead|BinTail],FileOut)->
    BinLen1=length(binary_to_list(BinHead)),
    <<BinLen2:16/little-unsigned-integer,_Rest/binary>>=BinHead,
    %% This clause handles the case where a record falls over a contination
    %% if it does it rejigs parse_SST to move down the Binary List
    %% io:format("in excel_records:parse_SST BinLen1 is ~p and BinLen2 is ~p~n",
    %%	      [BinLen1,BinLen2]),
    if
	(BinLen1 > BinLen2+3) ->NewBinHead = BinHead,
                                NewBinTail = BinTail,
                                ParseBin=BinHead;
	(BinLen1 == BinLen2+3)->case BinTail of
                                    []    -> H1 = [],
                                             T1 = [];
                                    Other -> [H1|T1]=BinTail
                                end,
                                NewBinTail=T1,
                                ParseBin=BinHead,
                                NewBinHead=list_to_binary([ParseBin,H1]);
	true                  ->ExtLen=BinLen2+3-BinLen1,
                                [H2|T2] = BinTail,
                                %% remember to discard the 8 byte unicode flag
                                <<Bits:1/binary,Ext:ExtLen/binary,
                                 NewBinHeadPart/binary>>=H2,
                                NewBinTail=T2,
                                ParseBin=list_to_binary([BinHead,Ext]),
                                NewBinHead=list_to_binary([ParseBin,
                                                           NewBinHeadPart])
    end,
    {[{_Type,String}],_,_}=excel_util:parse_CRS_Uni16(ParseBin,2,FileOut),
    Len=string:len(binary_to_list(String)),
    BinLen=8*(Len+3), % add an offset for the 2 byte index and 1 byte flags
    <<String2:BinLen/little-unsigned-integer,Rest/binary>>=NewBinHead,
    excel_util:write(Tables,strings,[{index,StringNo},{string,binary_to_list(String)}]),
    parse_SST(StringNo+1,NoOfStrings,Tables,[Rest|NewBinTail],FileOut).

write_row([],RowIndex,FirstColIndex,Name,TablSUPBOOKes)->
  {ok,ok};
write_row([{{xf_index,XFIndex},{value,number,Number}}|T],RowIndex,FirstColIndex,Name,Tables)->
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,FirstColIndex}},
		       {xf_index,XFIndex},{value,number,Number}]),
    write_row(T,RowIndex,FirstColIndex+1,Name,Tables).

 
parse_externsheet(<<>>,N,Tables,FileOut)->
  {ok,ok};
parse_externsheet(Bin,N,Tables,FileOut)->
  <<SubRec:16/little-unsigned-integer,
    FirstSheet:16/little-unsigned-integer,
    LastSheet:16/little-unsigned-integer,
    Rest/binary>>=Bin,
    Record=[{sheet_no,N},{subrec,SubRec},{firstsheet,FirstSheet},
                 {lastsheet,LastSheet}],
    excel_util:write(Tables,externsheets,Record),
    excel_util:put_log(FileOut,
        io_lib:fwrite("Externsheet record is ~p~n",[Record])),
    parse_externsheet(Rest,N+1,Tables,FileOut).
    