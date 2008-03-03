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
%%-include("excel_externname.hrl").

-export([parse_rec/5]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function processes the main record types that are used in the       %%%
%%% BIFF8/BIFF8X formats as described in Section 5.1 of the                  %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_rec(?FORMULA,Bin,Name,_CurrentFormula,Tables)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     _Result:64/little-unsigned-integer,
     _CalcFlag:16/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     Rest/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for FORMULA~n"),
    {Tokens,TokenArrays}=parse_FRM_Results(Rest,Name),
    excel_util:write(Tables,cell_tokens,[{{sheet,Name},{row_index,RowIndex},
                    {col_index,ColIndex}},{xf_index,XFIndex},
                    {tokens,Tokens},{tokenarrays,TokenArrays}]),
  NewCurrentFormula={{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
  {ok,NewCurrentFormula};
parse_rec(?EOF,_Bin,_Name,CurrentFormula,_Tables)->
    {ok,CurrentFormula};
parse_rec(?CALCOUNT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CALCOUNT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CALCMODE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CALCMODE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRECISION,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PRECISION"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?REFMODE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"REFMODE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DELTA,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DELTA"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ITERATION,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"ITERATION"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PROTECT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PROTECT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PASSWORD,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PASSWORD"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HEADER,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"HEADER"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FOOTER,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"FOOTER"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?EXTERNSHEET,Bin,_Name,CurrentFormula,Tables)->
    <<_NumRefs:16/little-unsigned-integer,
      R2/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for EXTERNSHEET~n"),
    parse_externsheet(R2,0,Tables),
    {ok,CurrentFormula};
parse_rec(?NAME,Bin,_Name,CurrentFormula,Tables)->
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
     %% io:format("in excel_records:parse_rec for NAME~n"),
     parse_Name(OptionFlag,KybdShortCut,NameLength,Size,SheetIndex,
     MenuTxtLen,DescTxtLen,HelpTxtLen,StatusTxtLen,Rest,Tables),
    {ok,CurrentFormula};
parse_rec(?WINDOWPROTECT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WINDOWPROTECT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?VERTICALPAGEBREAKS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"VERTICALPAGEBREAKS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HORIZONTALPAGEBREAKS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"HORIZONTALPAGEBREAKS"},
    {source,excel_records.erl},{msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?NOTE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"NOTE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SELECTION,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SELECTION"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DATEMODE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DATEMODE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?EXTERNNAME2,_Bin,_Name,CurrentFormula,Tables)->
    %% io:format("in excel_records EXTERNNAME~n"),
    %% parse_externname(Bin,Tables),
  excel_util:write(Tables,lacunae,[{identifier,"EXTERNNAME2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?LEFTMARGIN,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"LEFTMARGIN"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RIGHTMARGIN,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"RIGHTMARGIN"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?TOPMARGIN,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"TOPMARGIN"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOTTOMMARGIN,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"BOTTOMMARGIN"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRINTHEADERS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PRINTHEADERS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRINTGRIDLINES,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PRINTGRIDLINES"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FILEPASS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"FILEPASS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FONT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"FONT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CONTINUE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CONTINUE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WINDOW1,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WINDOW1"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BACKUP,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"BACKUP"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PANE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PANE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CODEPAGE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CODEPAGE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DCONREF,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DCONREF"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DEFCOLWIDTH,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DEFCOLWIDTH"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?XCT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"XCT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CRN,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CRN"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FILESHARING,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"FILESHARING"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WRITEACCESS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WRITEACCESS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?UNCALCED,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"UNCALCED"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SAVERECALC,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SAVERECALC"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?OBJECTPROTECT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"OBJECTPROTECT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?COLINFO,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"COLINFO"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?GUTS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"GUTS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WSBOOL,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WSBOOL"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?GRIDSET,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"GRIDSET"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HCENTRE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"HCENTRE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?VCENTRE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"VCENTRE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOUNDSHEET,Bin,_Name,CurrentFormula,Tables)->
    {_SheetBOF,_Visibility,_SheetType,_Name2,SheetName}=excel_util:get_bound_sheet(Bin,Tables),
    excel_util:append_sheetname(Tables,excel_util:get_utf8(SheetName)),
    {ok,CurrentFormula};
parse_rec(?WRITEPROT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WRITEPROT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?COUNTRY,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"COUNTRY"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HIDEOBJ,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"HIDEOBJ"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SORT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SORT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PALETTE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PALETTE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?STANDARDWIDTH,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"STANDARDWIDTH"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SCL,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SCL"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SETUP,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SETUP"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?MULRK,Bin,Name,CurrentFormula,Tables)->
    <<RowIndex:16/little-unsigned-integer,
     FirstColIndex:16/little-unsigned-integer,
     Rest/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for MULRK~n"),
    Tokens=parse_XF_RK(Rest),
    write_row(Tokens,RowIndex,FirstColIndex,Name,Tables),
    {ok,CurrentFormula};
parse_rec(?MULBLANK,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"MULBLANK"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RSTRING,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"RSTRING"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DBCELL,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DBCELL"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOOKBOOL,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"BOOKBOOL"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SCENPROTECT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SCENPROTECT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?XF2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"XF2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?MERGEDCELLS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"MERGEDCELLS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BITMAP,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"BITMAP"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PHONETIC,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"PHONETIC"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SST,[H|T],_Name,CurrentFormula,Tables)->
    <<_NoStringsUsed:32/little-unsigned-integer,
     NoActualStrings:32/little-unsigned-integer,
     Rest/binary>>=H,
     %% io:format("in excel_records:parse_rec for SST~n"),
    parse_SST(0,NoActualStrings,Tables,[Rest|T]),
    {ok,CurrentFormula};
parse_rec(?LABELSST,Bin,Name,CurrentFormula,Tables)->
       <<RowIndex:16/little-unsigned-integer,
        ColIndex:16/little-unsigned-integer,
        XFIndex:16/little-unsigned-integer,
        SSTIndex:32/little-unsigned-integer,
        _Rest/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for LABELSST~n"),
    %% Now look up the string in the string table
    String=excel_util:lookup_string(Tables,SSTIndex),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{string,String}]),
    {ok,CurrentFormula};
parse_rec(?EXTSST,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"EXTSST"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?LABELRANGES,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"LABELRANGES"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?USESELFS,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"USESELFS"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DSF,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DSF"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SUPBOOK,Bin,_Name,CurrentFormula,Tables)->
     %% io:format("in excel_records:parse_rec for SUPBOOK~n"),
    case Bin of
        <<NoSheets:16/little-unsigned-integer,
            ?InternalReferences:16/little-unsigned-integer>> ->
                  excel_util:write(Tables,misc,[{noofsheets,NoSheets}]);
        <<?Add_In_Fns1:16/little-unsigned-integer,
            ?Add_In_Fns2:16/little-unsigned-integer>> ->
                  excel_util:write(Tables,lacunae,[{identifier,"Add_In_Fns"},
                        {source,excel_records.erl}]);
          <<?DDE_OLE:16/little-unsigned-integer,_Rest/binary>> ->
                  excel_util:write(Tables,lacunae,
                        [{identifier,"DDE and OLE links"},
                        {source,excel_records.erl}]);
        _ -> 
                  parse_external_refs(Bin,Tables)
    end,
    {ok,CurrentFormula};
parse_rec(?CONDFMT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"CONDFMT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DVAL,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DVAL"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HLINK,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"HLINK"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DV,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DV"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DIMENSIONS2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DIMENSIONS2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BLANK2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"BLANK2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?NUMBER2,Bin,Name,CurrentFormula,Tables)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     Float:64/little-unsigned-float>>=Bin,
     %% io:format("in excel_records:parse_rec for NUMBER~n"),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
		       {xf_index,XFIndex},{value,number,Float}]),
    {ok,CurrentFormula};
parse_rec(?LABEL2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"LABEL2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOOLERR2,Bin,Name,CurrentFormula,Tables)->
    %% One might think that a record called BoolErr would contain a boolean error
    %% and error pertaining or obtaining in some straightforwar dway to Booleans
    %% wouldn't one? But on no - it contains a Boolean *OR* an Error
    %% How mad the fuck is that?
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     BoolErr:8/little-unsigned-integer,
     Type:8/little-unsigned-integer>>=Bin,
     %% io:format("in excel_records:parse_rec for BOOLERR~n"),
    {ValType,Value} = case Type of
        0 -> 
            case BoolErr of
              0 -> {boolean,false};
              1 -> {boolean,true}
           end;
        1 ->
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
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},
    {col_index,ColIndex}},{xf_index,XFIndex},{value,ValType,Value}]),
    {ok,CurrentFormula};
parse_rec(?STRING2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"STRING2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ROW2,_Bin,_Name,CurrentFormula,Tables)->
%%    case Bin of
%%	<<RowIndex:16/little-unsigned-integer,
%%	 FirstColIndex:16/little-unsigned-integer,
%%	 LastColIndex:16/little-unsigned-integer,
%%	 Height:16/little-unsigned-integer,
%%	 __NotUsed:16/little-unsigned-integer,
%%	 DefaultsFlag:8/little-unsigned-integer,
%%	 RelativeOffset:16/little-unsigned-integer>> ->
%%	<<RowIndex:16/little-unsigned-integer,
%%	 FirstColIndex:16/little-unsigned-integer,
%%	 LastColIndex:16/little-unsigned-integer,
%%	 Height:16/little-unsigned-integer,
%%	 __NotUsed:16/little-unsigned-integer,
%%	 DefaultsFlag:8/little-unsigned-integer,
%%	 RelativeOffset:16/little-unsigned-integer,
%%	 DefaultRowAttributes:24/little-unsigned-integer>> ->
%%	<<RowIndex:16/little-unsigned-integer,
%%	 FirstColIndex:16/little-unsigned-integer,
%%	 LastColIndex:16/little-unsigned-integer,
%%	 Height:16/little-unsigned-integer,
%%	 __NotUsed:16/little-unsigned-integer,
%%	 DefaultsFlag:8/little-unsigned-integer,
%%	 RelativeOffset:16/little-unsigned-integer,
%%	 XFIndex:16/little-unsigned-integer>> ->
%%    end,
     %% io:format("in excel_records:parse_rec for ROW2~n"),
  excel_util:write(Tables,lacunae,[{identifier,"ROW2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?INDEX2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"INDEX2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ARRAY2,Bin,Name,CurrentFormula,Tables)->
    <<Range:6/binary, % 6
      _Options:2/binary,
      _NotUsed:4/binary,
      RawTokens/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for ARRAY2~n"),
    Return1=excel_util:read_cell_range_addies(1,'8bit',Range),
    {[{FirstRow,LastRow,FirstCol,LastCol}],_}=Return1,
    Return2=parse_FRM_Results(RawTokens,Name),
    {Tokens,TokenArrays}=Return2,
    excel_util:write(Tables,sh_arr_formula,[{{sheet,Name},{firstrow,FirstRow},
      {firstcol,FirstCol},{lastrow,LastRow},{lastcol,LastCol}},{type,array},
      {tokens,Tokens},{tokenarrays,TokenArrays}]),
    {ok,CurrentFormula};
parse_rec(?DEFAULTROWHEIGHT2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"DEFAULTROWHEIGHT2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?TABLEOP_2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"TABLEOP_2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WINDOW2_2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"WINDOW2_2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RK,Bin,Name,CurrentFormula,Tables)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     RKValue:32/little-unsigned-integer>>=Bin,
     %% io:format("in excel_records:parse_rec for RK~n"),
    RKValue2=excel_util:parse_CRS_RK(<<RKValue:32/little-unsigned-integer>>),
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},
    {col_index,ColIndex}},{xf_index,XFIndex},{value,number,RKValue2}]),
    {ok,CurrentFormula};
parse_rec(?STYLE,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"STYLE"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FORMAT2,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"FORMAT2"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SHRFMLA,Bin,Name,CurrentFormula,Tables)->
  <<Range:6/binary,
   _NotUsed:8/little-unsigned-integer,
   _NoRecords:8/little-unsigned-integer,
   Rest/binary>>=Bin,
     %% io:format("in excel_records:parse_rec for SHRFMLA~n"),
  {[{FirstRow,LastRow,FirstCol,LastCol}],_}=excel_util:read_cell_range_addies(1,'8bit',Range),
  {Tokens,TokenArrays}=parse_FRM_Results(Rest,Name),
  %% io:format("in excel_records:parse_rec for SHRFMLA Tokens are ~p~n",[Tokens]),
  excel_util:write(Tables,sh_arr_formula,[{{sheet,Name},{firstrow,FirstRow},
    {firstcol,FirstCol},{lastrow,LastRow},{lastcol,LastCol}},{type,shared},{tokens,Tokens},{tokenarrays,TokenArrays}]),
    {ok,CurrentFormula};
parse_rec(?QUICKTIP,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"QUICKTIP"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOF4,Bin,_Name,CurrentFormula,_Tables)->
    %% BOF BIFF8 Section 5.8.1 excelfileformat.pdf V1.40
    <<_BiffVsn:16/little-unsigned-integer,
     _Type:16/little-unsigned-integer,
     _BuildID:16/little-unsigned-integer,
     _BuildYr:16/little-unsigned-integer,
     _FileHist:32/little-unsigned-integer,
     _LowestVsn:32/little-unsigned-integer>>=Bin,
     %% io:format("in excel_records:parse_rec for BOF~n"),
    {ok,CurrentFormula};
parse_rec(?SHEETLAYOUT,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SHEETLAYOUT"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SHEETPROTECTION,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"SHEETPROTECTION"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RANGEPROTECTION,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,"RANGEPROTECTION"},{source,excel_records.erl},
    {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(Other,_Bin,_Name,CurrentFormula,Tables)->
  excel_util:write(Tables,lacunae,[{identifier,Other},{source,excel_records.erl},
    {msg,"not being checked for and processed"}]),
    {ok,CurrentFormula}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These helper functions help process the formulas in the BIFF8/BIFF8X     %%%
%%% formats as described in Section 3 of the excelfileformat.pdf (V1.40)     %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function entry is described in Section 5.47 of
%% excelfileformat.pdf (V1.40)

%% This function entry is described in Section 3.1 of
%% excelfileformat.pdf (V1.40)

%% this parses the formulae tokens but doens't reverse compile them
%% that happens later. When we are reading a formula right now it will
%% make reference to things like names and arrayformulae that we haven't
%% read yet
parse_FRM_Results(<<>>,_Name)->
    ok;
parse_FRM_Results(Bin,Name) ->
    <<Size:16/little-unsigned-integer,Rest/binary>>=Bin,
    {Tokens,TokenArray2}=case Size of
	       0        -> io:format("in excel_records:parse_FRM_Results "++
                               "- zero length formula!"),
			      {[],[]};
	       RPN_Size -> <<RPN:RPN_Size/binary,TokenArray/binary>>=Rest,
                           excel_tokens:parse_tokens(RPN,Name,TokenArray,[])
    end,
    {Tokens,TokenArray2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the name records defined in Section 5.67 of          %%%
%%% excelfileformat.pdf (V1.40) and extracts the name table                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_Name(OptionFlag,_KybdShortCut,NameLength,_Size,SheetIndex,
  _MenuTxtLen,_DescTxtLen,_HelpTxtLen,_StatusTxtLen,Bin,Tables)->
  <<_Hidden:1/integer,_Func:1/integer,_VBasic:1/integer,_Macro:1/integer,
    _Complex:1/integer,_BuiltIn:1/integer,_FuncGroup1:1/integer,
    _FuncGroup2:1/integer,_FuncGroup3:1/integer,_FuncGroup4:1/integer,
    _FuncGroup5:1/integer,_FuncGroup6:1/integer,
    _Binary:1/integer,_A:1/integer,_B:1/integer,_C:1/integer>>=OptionFlag,
  <<_Options:1/binary,Name:NameLength/binary,_Rest/binary>>=Bin,
  io:format("Just ignoring the compression options for "++
    "Unicode names at the moment - will wig when not using Latin-1~n"),
  Scope = case SheetIndex of
            0 -> global;
            _ -> local
        end,
  Index=excel_util:get_length(Tables,names),
  excel_util:write(Tables,names,[{index,Index},{sheetindex,SheetIndex},
      {type,Scope},{name,binary_to_list(Name)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the Multiple RK records defined in Section 5.66 of   %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_XF_RK(Bin)->
  parse_XF_RK(Bin,[]).

parse_XF_RK(<<_LastColIndex:16/little-unsigned-integer>>,Residuum)->
    lists:reverse(Residuum);
parse_XF_RK(<<XFIndex:16/little-unsigned-integer,
	     RKValue:32/little-unsigned-integer,
	     Rest/binary>>,Residuum)->
    Num=excel_util:parse_CRS_RK(<<RKValue:32/little-unsigned-integer>>),
    NewResiduum=[{{xf_index,XFIndex},{value,number,Num}}|Residuum],
    parse_XF_RK(Rest,NewResiduum).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions process composites of the main record types of           %%%
%%% Section 5.1 of excelfileformat.pdf (V1.40) and the common record         %%%
%%% substructures of Section 2.5                                             %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_SST(NoOfStrings,NoOfStrings,_Tables,_)->
    io:format("String table parsed~n");
parse_SST(StringNo,NoOfStrings,Tables,[BinHead|BinTail])->
    BinLen1=length(binary_to_list(BinHead)),
    <<BinLen2:16/little-unsigned-integer,_Rest/binary>>=BinHead,
    %% This clause handles the case where a record falls over a contination
    %% if it does it rejigs parse_SST to move down the Binary List
    if
	(BinLen1 > BinLen2+3) ->NewBinHead = BinHead,
                                 NewBinTail = BinTail,
                                 ParseBin=BinHead;
	(BinLen1 == BinLen2+3)->case BinTail of
                                    []     -> H1 = [],
                                              T1 = [];
                                    _Other -> [H1|T1]=BinTail
                                end,
                                NewBinTail=T1,
                                ParseBin=BinHead,
                                NewBinHead=list_to_binary([ParseBin,H1]);
	true                  ->ExtLen=BinLen2+3-BinLen1,
                                [H2|T2] = BinTail,
                                %% remember to discard the 8 byte unicode flag
                                <<_Bits:1/binary,Ext:ExtLen/binary,
                                 NewBinHeadPart/binary>>=H2,
                                NewBinTail=T2,
                                ParseBin=list_to_binary([BinHead,Ext]),
                                NewBinHead=list_to_binary([ParseBin,
                                                           NewBinHeadPart])
    end,
    Return=excel_util:parse_CRS_Uni16(ParseBin,2),
    String=excel_util:get_utf8(Return),
    {[{_Type,BinString}],_StringLen,_RestLen}=Return,    
    Len=string:len(binary_to_list(BinString)),
    BinLen=8*(Len+3), % add an offset for the 2 byte index and 1 byte flags
    <<_String2:BinLen/little-unsigned-integer,Rest/binary>>=NewBinHead,
    excel_util:write(Tables,strings,[{index,StringNo},{string,String}]),
    parse_SST(StringNo+1,NoOfStrings,Tables,[Rest|NewBinTail]).

write_row([],_RowIndex,_FirstColIndex,_Name,_Tables)->
  {ok,ok};
write_row([{{xf_index,XFIndex},{value,number,Number}}|T],RowIndex,FirstColIndex,Name,Tables)->
    excel_util:write(Tables,cell,[{{sheet,Name},{row_index,RowIndex},
        {col_index,FirstColIndex}},{xf_index,XFIndex},{value,number,Number}]),
    write_row(T,RowIndex,FirstColIndex+1,Name,Tables).

parse_externsheet(<<>>,_N,_Tables)->
  {ok,ok};
parse_externsheet(Bin,N,Tables)->
  <<SubRec:16/little-unsigned-integer,
    FirstSheet:16/little-unsigned-integer,
    LastSheet:16/little-unsigned-integer,
    Rest/binary>>=Bin,
    Record=[{index,N},{subrec,SubRec},{firstsheet,FirstSheet},
                 {lastsheet,LastSheet}],
    excel_util:write(Tables,externsheets,Record),
    parse_externsheet(Rest,N+1,Tables).

%% parses external references as defined in Section 5.99.1 of excelvileformatV1.40.pdf
%% that definition is buggy as this record doesn't contain any details of the external file
%% name - don't know from whence that is got at the mo...
parse_external_refs(<<_NoOfSheets:16/little-unsigned-integer,Rest/binary>>,Tables)->
    [RawFilename|BinaryList]=get_ext_ref_names(Rest,[]),
    io:format("in excel_records:parse_external_refs BinaryList is ~p~n",[BinaryList]),
    Names=[binary_to_list(X) || X <- BinaryList],
    ParsedFileName=parse_filename(RawFilename),
    Index=excel_util:get_length(Tables,externalrefs),
    excel_util:write(Tables,externalrefs,[{index,Index},{file,ParsedFileName},Names]).
    
get_ext_ref_names(<<>>,Residuum)->
  lists:reverse(Residuum);
get_ext_ref_names(Bin,Residuum)->
    Return=excel_util:parse_CRS_Uni16(Bin,2),
    {[{_Type,String}],StringLen,_RestLen}=Return,
    Utf8String=excel_util:get_utf8(Return),
    StringLen2=StringLen*8,
    <<_String2:StringLen2/little-unsigned-integer,Rest/binary>>=Bin,
    get_ext_ref_names(Rest,[list_to_binary(Utf8String)|Residuum]).

parse_filename(<<?chEncode:8/little-unsigned-integer,
               chVolume:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chSameVolume:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chDownDir:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chUpDir:8/little-unsigned-integer,
               Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chStartUpDir:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chAltStartUpDir:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               chLibDir:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
               Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(Bin) -> "./"++binary_to_list(Bin)++"/".

snip_xls(Bin)->
  FileName=binary_to_list(Bin),
  RegExp=".xls$",
  case regexp:gsub(FileName,RegExp,"") of
      {ok, NewString,_} -> NewString;
      {error,_}         -> FileName
  end.

%% parse_externname(Bin,Tables)->
  %% we dont care mostly but for the first 4 bits (the minioptions) and we chuck the remaining 12 bits
  %% away (the discard) mostly - except when we don't :(
%%  <<MiniOptions:4/little-unsigned-integer,Discard:12/little-unsigned-integer,Rest/binary>>=Bin,
%%  case MiniOptions of
%%    ?STANDARD   -> case Discard of
%%                        0 -> excel_util:write(Tables,lacunae,[{identifier,"Built-in functions"},
%%                                {source,excel_records.erl},{msg,"not being processed"}]);
%%                        Other -> write_externname(Rest,Tables)
%%                    end;
%%    ?BUILT_IN   -> write_externname(Rest,Tables);
%%    ?MANUAL_DDE -> excel_util:write(Tables,lacunae,[{identifier,"MANUAL_DDE"},
%%                        {source,excel_records.erl},{msg,"not being processed"}]);
%%    ?AUTO_DDE   -> excel_util:write(Tables,lacunae,[{identifier,"AUTO_DDE"},
%%                        {source,excel_records.erl},{msg,"not being processed"}]);
%%    ?MANUAL_OLE -> excel_util:write(Tables,lacunae,[{identifier,"MANUAL_OLE"},
%%                        {source,excel_records.erl},{msg,"not being processed"}]);
%%    ?AUTO_OLE   -> excel_util:write(Tables,lacunae,[{identifier,"AUTO_OLE"},
%%                        {source,excel_records.erl},{msg,"not being processed"}])
%%    end.

%% write_externname(<<_NotUsed:16/little-unsigned-integer,Rest/binary>>,Tables)->
%%  ok.
