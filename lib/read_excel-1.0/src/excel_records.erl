%%%-------------------------------------------------------------------
%%% File    excel_records.erl
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc     This module parses the Excel records.
%%% 
%%%          It calls excel_tokens when it gets a formula record
%%%
%%% @end
%%% Created     : 11 January 2008
%%%-------------------------------------------------------------------
-module(excel_records).

%%% Include files with macros encoding Microsoft File Format constants
-include("microsoftbiff.hrl").
-include("excel_records.hrl").
-include("excel_errors.hrl").
-include("excel_supbook.hrl").
-include("excel_externname.hrl").

-export([parse_rec/5, prepend_quote/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function processes the main record types that are used in the       %%%
%%% BIFF8/BIFF8X formats as described in Section 5.1 of the                  %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_rec(?FORMULA,Bin,Name,_CurrentFormula,Tbl)->
    % io:format("in parse_rec for FORMULA~n"),
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     _Result:64/little-unsigned-integer,
     _CalcFlag:16/little-unsigned-integer,
     _NotUsed:32/little-unsigned-integer,
     Rest/binary>>=Bin,
    {Tokens,TokenArrays}=parse_FRM_Results(Rest,Name),
    excel_util:write(Tbl,tmp_cell,[{{sheet,Name},{row_index,RowIndex},
                                    {col_index,ColIndex}},{xf_index,XFIndex},
                                   {tokens,Tokens},{tokenarrays,TokenArrays}]),
    NewCurrentFormula={{sheet,Name},{row_index,RowIndex},{col_index,ColIndex}},
    {ok,NewCurrentFormula};
parse_rec(?EOF,_Bin,_Name,CurrentFormula,_Tbl)->
    % io:format("in parse_rec for EOF~n"),
    {ok,CurrentFormula};
parse_rec(?CALCOUNT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CALCOUNT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"CALCOUNT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CALCMODE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CALCMODE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"CALCMODE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRECISION,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PRECISION~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PRECISION"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?REFMODE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for REFMODE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"REFMODE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DELTA,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DELTA~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DELTA"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ITERATION,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for ITERATION~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"ITERATION"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PROTECT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PROTECT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PROTECT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PASSWORD,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PASSWORD~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PASSWORD"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HEADER,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for HEADER~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"HEADER"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FOOTER,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for FOOTER~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"FOOTER"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?EXTERNSHEET,Bin,_Name,CurrentFormula,Tbl)->
    <<_NumRefs:16/little-unsigned-integer,
     R2/binary>>=Bin,
    % io:format("in parse_rec for EXERNSHEET~n"),
    parse_externsheet(R2,0,Tbl),
    {ok,CurrentFormula};
parse_rec(?NAME,Bin,Name,CurrentFormula,Tbl)-> % renamed DEFINEDNAME in v1.42
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
    % io:format("in parse_rec for NAME~n"),
    parse_name(OptionFlag,KybdShortCut,NameLength,Size,SheetIndex,
               MenuTxtLen,DescTxtLen,HelpTxtLen,StatusTxtLen,Name,Rest,Tbl),
    {ok,CurrentFormula};
parse_rec(?WINDOWPROTECT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WINDOWPROTECT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WINDOWPROTECT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?VERTICALPAGEBREAKS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for VERTICALPAGEBREAKS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"VERTICALPAGEBREAKS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HORIZONTALPAGEBREAKS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for HORIZONTALPAGEBREAKS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"HORIZONTALPAGEBREAKS"},
                                  {source,excel_records.erl},{msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?NOTE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for NOTE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"NOTE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SELECTION,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SELECTION~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SELECTION"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DATEMODE,Bin,_Name,CurrentFormula,Tbl)->
    <<DateMode:16/little-unsigned-integer>>=Bin,
    % io:format("in parse_rec for DATEMODE~n"),
    DateMode2=case DateMode of
                  ?rc_DATEMODE_WINDOWS   -> "Windows";
                  ?rc_DATEMODE_MACINTOSH -> "Macintosh"
              end,
    excel_util:write(Tbl,misc,[{index,datemode},{value,DateMode2}]),
    {ok,CurrentFormula};
parse_rec(?EXTERNNAME2,Bin,_Name,CurrentFormula,Tbl)->
    % Best described by Section 5.39 of excelfileformatV1-41.pdf
    % io:format("in parse_rec for EXTERNNAME2~n"),
    parse_externname(Bin,Tbl),
    {ok,CurrentFormula};
parse_rec(?LEFTMARGIN,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for LEFTMARGIN~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"LEFTMARGIN"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RIGHTMARGIN,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for RIGHTMARGIN~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"RIGHTMARGIN"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?TOPMARGIN,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for TOPMARGIN~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"TOPMARGIN"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOTTOMMARGIN,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for BOTTOMMARGIN~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"BOTTOMMARGIN"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRINTHEADERS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PRINTHEADERS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PRINTHEADERS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PRINTGRIDLINES,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PRINTGRIDLINES~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PRINTGRIDLINES"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FILEPASS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for FILEPASS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"FILEPASS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FONT,Bin,_Name,CurrentFormula,Tbl)->
    <<Height:16/little-unsigned-integer,
     Options:2/binary,
     ColourIdx:16/little-unsigned-integer,
     FontWeight:16/little-unsigned-integer,
     Escapement:16/little-unsigned-integer,
     UnderlineType:8/little-unsigned-integer,
     FontFamily:8/little-unsigned-integer,
     _CharacterSet:8/little-unsigned-integer,
     _NotUsed:8/little-unsigned-integer,
     FontName/binary>>=Bin,
    % io:format("in parse_rec for FONT~n"),
    % First up parse the options
    OptionsCSS=parse_font_options(Options),
    % Now turn all this lot into CSS formats
    % FONT-WEIGHT
    % 
    % THIS WILL WIG OUT IF EXCEL SETS WEIGHTS OTHER THAN BOLD AND NORMAL
    % BUT HEY!
    % 
    FontWeightCSS = case FontWeight of
                        400 -> []; % normal
                        700 -> [{'font-weight',["bold"]}]
                    end,
    % FONT-SIZE CSS
    FontSizeCSS=[{'font-size',[integer_to_list(round(Height/20))++"px"]}],
    % VERTICAL-ALIGN CSS
    VAlignCSS = case Escapement of
                    ?rc_FONT_ESCAPEMENT_NONE        ->
                        [];
                    ?rc_FONT_ESCAPEMENT_SUPERSCRIPT ->
                        [{'vertical-align',["super"]}];
                    ?rc_FONT_ESCAPEMENT_SUBSCRIPT   ->
                        [{'vertical-align',["sub"]}]
                end,

    % Accounting underlines are slightly different to normal ones
    % Normal underline underlines all text - accounting ones just
    % go from the currency symbol to the end of the number
    %
    % Hell mend thae accounting johnnies!
    % 
    % BORDER-BOTTOM-STYLE CSS
    BorderBotStyleCSS = case UnderlineType of
                            ?rc_FONT_UNDERLINE_NONE       ->
                                [];
                            ?rc_FONT_UNDERLINE_SINGLE     ->
                                [{'border-bottom-style',["single"]}];
                            ?rc_FONT_UNDERLINE_DOUBLE     ->
                                [{'border-bottom-style',["double"]}];
                            ?rc_FONT_UNDERLINE_SINGLE_ACC ->
                                [{'border-bottom-style',["single"]}];
                            ?rc_FONT_UNDERLINE_DOUBLE_ACC ->
                                [{'border-bottom-style',["double"]}]
                        end,
    % FONT-FAMILY CSS
    FontFamCSSBits = case FontFamily of
                         ?rc_FONT_FAMILY_STANDARD   -> [];
                         ?rc_FONT_FAMILY_ROMAN      -> ["serif"];
                         ?rc_FONT_FAMILY_SWISS      -> ["sans-serif"];
                         ?rc_FONT_FAMILY_MODERN     -> ["monospace"];
                         ?rc_FONT_FAMILY_SCRIPT     -> ["cursive"];
                         ?rc_FONT_FAMILY_DECORATIVE -> []
                     end,
    FontNameCSSBits = excel_util:get_utf8(excel_util:parse_CRS_Uni16(FontName)),
    FontFamilyCSS=[{'font-family',[FontNameCSSBits|FontFamCSSBits]}],
    CSS = lists:merge([OptionsCSS, FontWeightCSS, FontSizeCSS, VAlignCSS,
                       BorderBotStyleCSS, FontFamilyCSS]),
    excel_util:append(Tbl,tmp_fonts,[{colour_index,ColourIdx},{css,CSS}]),
    {ok,CurrentFormula};
parse_rec(?CONTINUE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CONTINUE~n"),
    exit("in excel_records:parse_rec for ?CONTINUE this oughta be a fatal..."),
    excel_util:write(Tbl,lacunae,[{identifier,"CONTINUE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WINDOW1,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WINDOW1~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WINDOW1"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BACKUP,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for BACKUP~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"BACKUP"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PANE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PANE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PANE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CODEPAGE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CODEPAGE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"CODEPAGE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DCONREF,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DCONREF~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DCONREF"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DEFCOLWIDTH,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DEFCOLWIDTH~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DEFCOLWIDTH"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?XCT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for XCT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"XCT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?CRN,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CRN~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"CRN"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FILESHARING,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for FILESHARING~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"FILESHARING"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WRITEACCESS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WRITEACCESS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WRITEACCESS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?UNCALCED,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for UNCALCED~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"UNCALCED"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SAVERECALC,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SAVERECALC~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SAVERECALC"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?OBJECTPROTECT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for OBJECTPROTECT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"OBJECTPROTECT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?COLINFO,Bin,_Name,CurrentFormula,Tbl)->
    <<_FirstColIndex:16/little-unsigned-integer,
     _LastColIndex:16/little-unsigned-integer,
     _Width:16/little-unsigned-integer,
     _XFRef:12/little-unsigned-integer,
     _Discard:4/little-unsigned-integer,
     _Options:16/little-unsigned-integer,
     _NotUsed:16/little-unsigned-integer>> = Bin,
    % io:format("in parse_rec for COLINFO~n"),
    % Can't seem to get a COLINFO record somehow...
    % rc_COLUMN_HIDDEN
    % rc_COLUMN_COLLAPSED
    excel_util:write(Tbl,lacunae,[{identifier,"COLINFO"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?GUTS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for GUTS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"GUTS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WSBOOL,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WSBOOL~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WSBOOL"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?GRIDSET,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for GRIDSET~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"GRIDSET"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HCENTRE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for HCENTRE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"HCENTRE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?VCENTRE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for VCENTRE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"VCENTRE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOUNDSHEET,Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for BOUNDSHEET~n"),
    {_ShBOF,_Vis,_ShType,_Name2,ShName}=excel_util:get_bound_sheet(Bin,Tbl),
    excel_util:append_sheetname(Tbl,excel_util:get_utf8(ShName)),
    {ok,CurrentFormula};
parse_rec(?WRITEPROT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WRITEPROT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WRITEPROT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?COUNTRY,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for COUNTRY~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"COUNTRY"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HIDEOBJ,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for HIDEOBJ~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"HIDEOBJ"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SORT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SORT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SORT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PALETTE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PALETTE~n"),
    excel_util:write(Tbl,warnings,["Custom Palettes not being imported - "++
                                   "the standard palette will be used!"]),
    {ok,CurrentFormula};
parse_rec(?STANDARDWIDTH,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for STANDARDWIDTH~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"STANDARDWIDTH"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SCL,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SCL~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SCL"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SETUP,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SETUP~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SETUP"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?MULRK,Bin,Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     FirstColIndex:16/little-unsigned-integer,
     Rest/binary>>=Bin,
    % io:format("in parse_rec for MULRK~n"),
    Tokens=parse_XF_RK(Rest),
    write_row(Tokens,RowIndex,FirstColIndex,Name,Tbl),
    {ok,CurrentFormula};
parse_rec(?MULBLANK,Bin,Name,CurrentFormula,Tbl)->
    Size = size(Bin),
    XFSize = Size-6,
    <<RowIndex:16/little-unsigned-integer,
     FirstColIndex:16/little-unsigned-integer,
     XFRecords:XFSize/binary,
     LastColIndex:16/little-unsigned-integer>> = Bin,
    % calculate the number of XF records
    % io:format("in parse_rec for MULBLANK~n"),
    write_blanks(Name,RowIndex,FirstColIndex,LastColIndex,XFRecords,Tbl),
    {ok,CurrentFormula};
parse_rec(?RSTRING,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for RSTRING~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"RSTRING"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DBCELL,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DBCELL~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DBCELL"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOOKBOOL,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for BOOKBOL~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"BOOKBOOL"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SCENPROTECT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SCENPROTECT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SCENPROTECT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?XF2,Bin,_Name,CurrentFormula,Tbl)->
    <<FontIndex:16/little-unsigned-integer,
     FormatIndex:16/little-unsigned-integer,
     XFTypeAndParent:16/little-unsigned-integer,
     XFAlignment:8/little-unsigned-integer,
     _XFRotation:8/little-unsigned-integer,
     _XFIndentation:8/little-unsigned-integer,
     XFFlags:8/little-unsigned-integer,
     XFCellBorders1:32/little-unsigned-integer,
     XFCellBorders2:32/little-unsigned-integer,
     XFCellBorders3:16/little-unsigned-integer>> = Bin,
    
    XFType =  case (XFTypeAndParent band ?rc_XF_XF_TYPE_MASK) of
                  4 -> style; % yup, its a mask match so it is a power of 2
                  0 -> cell
              end,

    XFParentIndex = trunc((XFTypeAndParent band ?rc_XF_PARENT_INDEX_MASK)/16),

    % now check that all the style XF records have a parent index of FF
    case {XFType, XFParentIndex} of
        {style, 16#FFF} ->
            ok;
        {style, _S}      ->
            exit("an XF style record must have a parent index of 16#FFF");
        {cell, 16#FFF}  ->
            exit("an XF cell record cannot have a parent index of 16#FFF");
        {cell, _Other}  ->
            ok
    end,

    TextAlignCSSbits1 =
        case (XFAlignment band ?rc_XF_H_ALIGN_MASK) of
            ?rc_XF_H_ALIGN_GENERAL        -> [];
            ?rc_XF_H_ALIGN_LEFT           -> ["left"];
            ?rc_XF_H_ALIGN_CENTERED       -> ["center"];
            ?rc_XF_H_ALIGN_RIGHT          -> ["right"];
            ?rc_XF_H_ALIGN_FILLED         -> [];          % cant do fill with numbers
            ?rc_XF_H_ALIGN_JUSTIFIED      -> ["justify"];
            ?rc_XF_H_ALIGN_CEN_ACROSS_SEL -> ["center"];  % not the same as Excel!
            ?rc_XF_H_ALIGN_DISTRIBUTED    -> ["justify"]
        end,

    % Ignore wrapping as set by the flag rc_XF_TEXT_WRAPPED

    % Mask off the Vertical Alignment and then divide it by 8
    % to lop of the 4 bits of binary down the line

    VAlignCSSbits =
        case trunc((XFAlignment band ?rc_XF_V_ALIGN_MASK)/8) of
            ?rc_XF_V_ALIGN_TOP         -> ["text-top"];
            ?rc_XF_V_ALIGN_CENTRED     -> ["middle"];
            ?rc_XF_V_ALIGN_BOTTOM      -> ["text-bottom"];
            ?rc_XF_V_ALIGN_JUSTIFIED   -> ["middle"]; % cant be done
            ?rc_XF_V_ALIGN_DISTRIBUTED -> ["middle"]  % cant be done
        end,

    VAlignCSS=[{'vertical-align',VAlignCSSbits}],

    TextAlignCSSbits2 =
        case (XFAlignment band ?rc_XF_LAST_CHAR_JUSTIFY_MASK) of
            ?rc_XF_LAST_CHAR_JUSTIFY -> ["justify"];
            _                        -> []
        end,

    TextAlignCSSbitsmerged = lists:merge([TextAlignCSSbits1, TextAlignCSSbits2]),
    TextAlignCSS = case TextAlignCSSbitsmerged of
                       [] -> [];
                       Other -> [{'text-align',Other}]
                   end,

    % Cant do text rotation

    % the attributes depend on whether or not this is a cell or a style record
    {Set, Unset} = case XFType of
                       style -> {ignore, valid};
                       cell  -> {use_this, use_parent}
                   end,

    % Ignore identation

    % bit shift XFlags 2 to the right
    XFFlags2 = trunc(XFFlags/4),

    % now do all the attributes
    NumberAttribute     = case (XFFlags2 band ?rc_XF_FLAG_NUMBERFORMAT) of
                              ?rc_XF_FLAG_NUMBERFORMAT -> {number, Set};
                              _                        -> {number, Unset}
                          end,
    FontAttribute       = case (XFFlags2 band ?rc_XF_FLAG_FONT) of
                              ?rc_XF_FLAG_FONT         -> {font, Set};
                              _                        -> {font, Unset}
                          end,
    TextAttribute       = case (XFFlags2 band ?rc_XF_FLAG_TEXT) of
                              ?rc_XF_FLAG_TEXT         -> {text, Set};
                              _                        -> {text, Unset}
                          end,
    BorderAttribute     = case (XFFlags2 band ?rc_XF_FLAG_BORDER) of
                              ?rc_XF_FLAG_BORDER       -> {border, Set};
                              _                        -> {border, Unset}
                          end,
    BackgroundAttribute = case (XFFlags2 band ?rc_XF_FLAG_BACKGROUND) of
                              ?rc_XF_FLAG_BACKGROUND   -> {background, Set};
                              _                        -> {background, Unset}
                          end,
    CellProtAttribute   = case (XFFlags2 band ?rc_XF_FLAG_CELL_PROT) of
                              ?rc_XF_FLAG_CELL_PROT    -> {protection, Set};
                              _                        -> {protection, Unset}
                          end,

    % now get the border stuff
    LeftLineStyleBits   = (XFCellBorders1 band ?rc_XF_LEFT_LINE_MASK),
    RightLineStyleBits  = (XFCellBorders1 band ?rc_XF_RIGHT_LINE_MASK)/?rc_XF_LEFT_LINE_MASK,
    TopLineStyleBits    = (XFCellBorders1 band ?rc_XF_TOP_LINE_MASK)/?rc_XF_RIGHT_LINE_MASK,
    BottomLineStyleBits = (XFCellBorders1 band ?rc_XF_BOTTOM_LINE_MASK)/?rc_XF_TOP_LINE_MASK,

    LeftLineStyleCSS   = get_style("left",   LeftLineStyleBits),
    RightLineStyleCSS  = get_style("right",  trunc(RightLineStyleBits)),
    TopLineStyleCSS    = get_style("top",    trunc(TopLineStyleBits)),
    BottomLineStyleCSS = get_style("bottom", trunc(BottomLineStyleBits)),

    % now punch out the colour indices
    <<_Diagonal:2,
     RightColourIndex:7/little-unsigned-integer,
     LeftColourIndex:7/little-unsigned-integer,
     _Bits:16>> = <<XFCellBorders1:32>>,

    % now get more indices
    <<_FillPattern:6/little-unsigned-integer,
     _Skip:1,
     _DiagonalLineStyle:4,
     _DiagonalColourIndex:7/little-unsigned-integer,
     BottomColourIndex:7/little-unsigned-integer,
     TopColourIndex:7/little-unsigned-integer>> = <<XFCellBorders2:32>>,

    % now get the last of the indices
    <<_Skip2:2,
     _PatternBackgroundColourIndex:7/little-unsigned-integer,
     PatternColourIndex:7/little-unsigned-integer>> = <<XFCellBorders3:16>>,

    CSS = lists:merge([VAlignCSS, TextAlignCSS, LeftLineStyleCSS,
                       RightLineStyleCSS, TopLineStyleCSS, BottomLineStyleCSS]),

    Attributes = [NumberAttribute, FontAttribute, TextAttribute,
                  BorderAttribute, BackgroundAttribute, CellProtAttribute],

    Colours1 = [{left,LeftColourIndex},{right,RightColourIndex},
                {top,TopColourIndex},{bottom,BottomColourIndex}],

    Colours2 = [{background,PatternColourIndex}],

    excel_util:append(Tbl,tmp_xf,[{format_index,FormatIndex},
                                  {type,XFType},
                                  {parent_index,XFParentIndex},
                                  {font_index,FontIndex},
                                  {css,CSS},
                                  {attributes,Attributes},
                                  {border_colour,Colours1},
                                  {bg_colour,Colours2}]),
    {ok,CurrentFormula};
parse_rec(?MERGEDCELLS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for MERGEDCELLS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"MERGEDCELLS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BITMAP,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for BITMAP~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"BITMAP"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?PHONETIC,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for PHONETIC~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"PHONETIC"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SST,[H|T],_Name,CurrentFormula,Tbl)->
    <<_NoStringsUsed:32/little-unsigned-integer,
     NoActualStrings:32/little-unsigned-integer,
     Rest/binary>>=H,
    % io:format("in parse_rec for SST~n"),
    parse_SST(0,NoActualStrings,Tbl,[Rest|T]),
    {ok,CurrentFormula};
parse_rec(?LABELSST,Bin,Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     SSTIndex:32/little-unsigned-integer,
     _Rest/binary>>=Bin,
    % io:format("in parse_rec for LABELSST~n"),
    % Now look up the string in the string table
    String=excel_util:lookup_string(Tbl,SSTIndex),
    excel_util:write(Tbl,cell,[{{sheet,Name},{row_index,RowIndex},
                                {col_index,ColIndex}},
                               {xf_index,XFIndex},{string,String}]),
    {ok,CurrentFormula};
parse_rec(?EXTSST,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for EXTSST~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"EXTSST"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?LABELRANGES,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for LABELRANGES~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"LABELRANGES"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?USESELFS,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for USESELFS~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"USESELFS"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DSF,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DSF~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DSF"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
% SUPBOOK is called EXTERNALBOOK in excelfileformatv1-42.pdf
% 
% the workbook structure will be built up fror the appropriate records
% as described in Section 4.10.3 of the excelfileformatsV1-42.pdf
% 
% The records involved are:
% * SUPBOOK/EXTERNALBOOK
% * EXERNSHEET
% * EXTERNNAME
% * NAME/DEFINEDNAME
% 
% The strategy is to take advantage of the sequential nature of 
% the Excel file format. As an EXTERNALBOOK record is read it
%  will be appended to the table:
% * tmp_workbook
%
% an EXTERNSHEET record will be given an index to the last SUPBOOK/EXTERNALBOOK
% record
% 
% In addition entries into the table 
parse_rec(?SUPBOOK,Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SUPBOOK~n"),
    case Bin of
        <<NoSheets:16/little-unsigned-integer,
         ?InternalReferences:16/little-unsigned-integer>> ->
            write_externalref({this_file,placeholder},[],Tbl),
            excel_util:write(Tbl,misc,[{index,noofsheets},{value,NoSheets}]);
        <<?Add_In_Fns1:16/little-unsigned-integer,
         ?Add_In_Fns2:16/little-unsigned-integer>> ->
            write_externalref({skipped,add_ins},[],Tbl),
            excel_util:write(Tbl,lacunae,[{identifier,"Add_In_Fns"},
                                          {source,excel_records.erl},
                                          {msg,"not being processed"}]);
        <<?DDE_OLE:16/little-unsigned-integer,_Rest/binary>> ->
            write_externalref({skipped,dde_ole},[],Tbl),
            excel_util:write(Tbl,lacunae,
                             [{identifier,"DDE and OLE links",Tbl},
                              {source,excel_records.erl}]);
        _ -> 
            parse_externalrefs(Bin,Tbl)
    end,
    {ok,CurrentFormula};
parse_rec(?CONDFMT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for CONDFMT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"CONDFMT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DVAL,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DVAL~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DVAL"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?HLINK,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for HLINK~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"HLINK"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DV,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DV~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DV"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?DIMENSIONS2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DIMENSIONS2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DIMENSIONS2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BLANK2,Bin,Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer>> = Bin,
    % io:format("in parse_rec for BLANK~n"),
    excel_util:write(Tbl,tmp_blanks,[{{sheet,Name},{row_index,RowIndex},
                                      {col_index,ColIndex}},{xf_index,XFIndex}]),
    {ok,CurrentFormula};
parse_rec(?NUMBER2,Bin,Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     Float:64/little-unsigned-float>>=Bin,
    % io:format("in parse_rec for NUMBERS2~n"),
    excel_util:write(Tbl,cell,[{{sheet,Name},{row_index,RowIndex},
                                {col_index,ColIndex}},
                               {xf_index,XFIndex},{value,number,Float}]),
    {ok,CurrentFormula};
parse_rec(?LABEL2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for LABEL2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"LABEL2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOOLERR2,Bin,Name,CurrentFormula,Tbl)->
    % One might think that a record called BoolErr would contain a boolean error
    % and error pertaining or obtaining in some straightforward way to Booleans
    % wouldn't one? But on no - it contains a Boolean *OR* an Error
    % How mad the fuck is that?
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     BoolErr:8/little-unsigned-integer,
     Type:8/little-unsigned-integer>>=Bin,

    % io:format("in parse_rec for BOOLERR2~n"),
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
    excel_util:write(Tbl,cell,[{{sheet,Name},{row_index,RowIndex},
                                {col_index,ColIndex}},{xf_index,XFIndex},
                               {value,ValType,Value}]),
    {ok,CurrentFormula};
parse_rec(?STRING2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for STRING2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"STRING2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ROW2,Bin,_Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     FirstColIndex:16/little-unsigned-integer,
     LastColIndex:16/little-unsigned-integer,
     _Height:16/little-unsigned-integer,
     _NotUsed:16/little-unsigned-integer,
     _NotUsed2:16/little-unsigned-integer,
     _Options:16/little-unsigned-integer,
     XFRef:12/little-unsigned-integer,
     _Discard1:4/little-unsigned-integer>>=Bin,
    % io:format("in parse_rec for ROW2~n"),
    excel_util:write(Tbl,tmp_rows,[{row_index,RowIndex},
                                   {first_col,FirstColIndex},
                                   {last_col,LastColIndex},
                                   {format_index,XFRef}]),
    {ok,CurrentFormula};
parse_rec(?INDEX2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for INDEX2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"INDEX2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?ARRAY2,Bin,Name,CurrentFormula,Tbl)->
    <<Range:6/binary, % 6
     _Options:2/binary,
     _NotUsed:4/binary,
     RawTokens/binary>>=Bin,
    % io:format("in parse_rec for ARRAY2~n"),
    Return1=excel_util:read_cell_range_addies(1,'8bit',Range),
    {[{FirstRow,LastRow,FirstCol,LastCol}],_}=Return1,
    {Tokens,TokenArrays}=parse_FRM_Results(RawTokens,Name),
    excel_util:write(Tbl,tmp_sh_arr_fml,[{{sheet,Name},{firstrow,FirstRow},
                                          {firstcol,FirstCol},
                                          {lastrow,LastRow},{lastcol,LastCol}},
                                         {type,array},
                                         {tokens,Tokens},
                                         {tokenarrays,TokenArrays}]),
    {ok,CurrentFormula};
parse_rec(?DEFAULTROWHEIGHT2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for DEFAULTROWHEIGHT2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"DEFAULTROWHEIGHT2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?TABLEOP_2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for TABLEOP-2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"TABLEOP_2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?WINDOW2_2,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for WINDOW2_2~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"WINDOW2_2"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RK,Bin,Name,CurrentFormula,Tbl)->
    <<RowIndex:16/little-unsigned-integer,
     ColIndex:16/little-unsigned-integer,
     XFIndex:16/little-unsigned-integer,
     RKValue:32/little-unsigned-integer>>=Bin,
    RKValue2=excel_util:parse_CRS_RK(<<RKValue:32/little-unsigned-integer>>),
    % io:format("in parse_rec for RK~n"),
    excel_util:write(Tbl,cell,[{{sheet,Name},{row_index,RowIndex},
                                {col_index,ColIndex}},
                               {xf_index,XFIndex},{value,number,RKValue2}]),
    {ok,CurrentFormula};
parse_rec(?STYLE,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for STYLE~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"STYLE"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?FORMAT2,Bin,_Name,CurrentFormula,Tbl)->
    <<FormatIndex:16/little-unsigned-integer,
     FormatBin/binary>>=Bin,
    Return=excel_util:parse_CRS_Uni16(FormatBin),
    FormatString=excel_util:get_utf8(Return),
    % io:format("in parse_rec for FORMAT2~n"),
    excel_util:write(Tbl,tmp_formats,[{format_index,FormatIndex},
                                      {type,unknown_as_yet},
                                      {category,userdefined},
                                      {format,FormatString}]),
    {ok,CurrentFormula};
parse_rec(?SHRFMLA,Bin,Name,CurrentFormula,Tbl)->
    <<Range:6/binary,
     _NotUsed:8/little-unsigned-integer,
     _NoRecords:8/little-unsigned-integer,
     Rest/binary>>=Bin,
    % io:format("in parse_rec for SHRFLMA~n"),
    {[{FR,LR,FC,LC}],_}=excel_util:read_cell_range_addies(1,'8bit',Range),
    {Tokens,TokenArrays}=parse_FRM_Results(Rest,Name),
    excel_util:write(Tbl,tmp_sh_arr_fml,[{{sheet,Name},{firstrow,FR},
                                          {firstcol,FC},{lastrow,LR},
                                          {lastcol,LC}},{type,shared},
                                         {tokens,Tokens},{tokenarrays,TokenArrays}]),
    {ok,CurrentFormula};
parse_rec(?QUICKTIP,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for ~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"QUICKTIP"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?BOF4,Bin,_Name,CurrentFormula,_Tbl)->
    % BOF BIFF8 Section 5.8.1 excelfileformat.pdf V1.40
    <<_BiffVsn:16/little-unsigned-integer,
     _Type:16/little-unsigned-integer,
     _BuildID:16/little-unsigned-integer,
     _BuildYr:16/little-unsigned-integer,
     _FileHist:32/little-unsigned-integer,
     _LowestVsn:32/little-unsigned-integer>>=Bin,
    % io:format("in parse_rec for BOF4~n"),
    {ok,CurrentFormula};
parse_rec(?SHEETLAYOUT,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SHEETLAYOUT~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SHEETLAYOUT"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?SHEETPROTECTION,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for SHEETPROTECTION~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"SHEETPROTECTION"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(?RANGEPROTECTION,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for RANGEPROTECTION~n"),
    excel_util:write(Tbl,lacunae,[{identifier,"RANGEPROTECTION"},
                                  {source,excel_records.erl},
                                  {msg,"not being processed"}]),
    {ok,CurrentFormula};
parse_rec(Other,_Bin,_Name,CurrentFormula,Tbl)->
    % io:format("in parse_rec for OTHER - value of ~p~n", [Other]),
    excel_util:write(Tbl,lacunae,[{identifier,{"undocumented record type",
                                               Other}},{source,
                                                        excel_records.erl},
                                  {msg,"not being checked and processed"}]),
    {ok,CurrentFormula}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This helper functions parses the border options describe in              %%%
%%% Section 2.5.11 of the excelfileformatV1.42.pdf                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_style(Loc, LineStyle) ->
    Border = list_to_atom("border-"++Loc),
    Style = list_to_atom("border-"++Loc++"-style"),
    case LineStyle of
        ?rc_XF_BORDER_NO_LINE              -> [];
        ?rc_XF_BORDER_THIN                 -> [{Border, ["solid" ]},
                                               {Style,  ["thin"  ]}];
        ?rc_XF_BORDER_MEDIUM               -> [{Border, ["solid" ]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_DASHED               -> [{Border, ["dashed"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_DOTTED               -> [{Border, ["dotted"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_THICK                -> [{Border, ["solid" ]},
                                               {Style,  ["thick" ]}];
        ?rc_XF_BORDER_DOUBLE               -> [{Border, ["double"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_HAIR                 -> [{Border, ["solid" ]},
                                               {Style,  ["thin"  ]}];
        ?rc_XF_BORDER_MED_DASHED           -> [{Border, ["dashed"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_THIN_DASH_DOT        -> [{Border, ["dotted"]},
                                               {Style,  ["thin"  ]}];
        ?rc_XF_BORDER_MED_DASH_DOT         -> [{Border, ["dotted"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_THIN_DASH_DOT_DOT    -> [{Border, ["dotted"]},
                                               {Style,  ["thin"  ]}];
        ?rc_XF_BORDER_MED_DASH_DOT_DOT     -> [{Border, ["dotted"]},
                                               {Style,  ["medium"]}];
        ?rc_XF_BORDER_SLANTED_MED_DASH_DOT -> [{Border, ["dotted"]},
                                               {Style,  ["medium"]}]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This helper functions parses the font options describe in Section 5.45   %%%
%%% of the excelfileformatV1.42.pdf                                          %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_font_options(Bin) ->
    <<Ex:1, Cond:1, Shadow:1, _Outline:1, StruckOut:1, _Und:1,
     Italic:1, _Bold:1, 0:8>> = Bin,
    FontStyleCSS = case Italic of
                       1 -> [{'font-style',["italic"]}];
                       0 -> []
                   end,
    TextDecorationCSS = case StruckOut of
                            1 -> [{'text-decoration',["line-through"]}];
                            0 -> []
                        end,
    TextShadowCSS = case Shadow of
                        1 -> [{'text-shadow',["2px","2px","2px"]}];
                        0 -> []
                    end,
    FSCSSbits1 = case Cond of
                     1 -> ["condensed"];
                     0 -> []
                 end,
    FSCSSbits2 = case Ex of
                     1 -> ["expanded"];
                     0 -> []
                 end,
    FontStretchCSS = case {FSCSSbits1,FSCSSbits2} of
                         {[], []} -> [];
                         {[], A}  -> [{'font-stretch',A}];
                         {A, []}  -> [{'font-stretch',A}];
                         {_A, _B} -> exit("invalid record of type FONT")
                     end,
    lists:merge([FontStyleCSS, TextDecorationCSS, TextShadowCSS, FontStretchCSS]).

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
parse_FRM_Results(<<>>, _Name) ->
    ok;
parse_FRM_Results(Bin, Name) ->
    <<Size:16/little-unsigned-integer,Rest/binary>>=Bin,
    {Tks,TkArray2}=case Size of
                       0        -> {[],[]};
                       RPN_Size -> <<RPN:RPN_Size/binary,TkArray/binary>>=Rest,
                                   excel_tokens:parse_tokens(RPN,Name,TkArray,[])
                   end,
    {Tks,TkArray2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the name records defined in Section 5.67 of          %%%
%%% excelfileformat.pdf (V1.40) and extracts the name table                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_name(_OptionFlag,_KybdShortCut,NameLength,_Size,SheetIndex,
           _MenuTxtLen,_DescTxtLen,_HelpTxtLen,_StatusTxtLen,Name,Bin,Tbl)->
    %    <<Hidden:1/integer,Func:1/integer,VBasic:1/integer,Macro:1/integer,
    %     Complex:1/integer,BuiltIn:1/integer,FuncGroup:6/integer,
    %     Binary:1/integer,_A:1/integer,_B:1/integer,_C:1/integer>>=OptionFlag,
    <<_Options:1/binary,NameName:NameLength/binary,Rest/binary>>=Bin,    
    Tokens=binary_to_list(Rest),
    Len=length(Tokens),
    % in FORMULA the RPN tokens may have an additional token stream appended
    % to them that the formula uses (see 3.1.1 of excelfileformatV1-42)
    % as a result they start with a len record that describes how long the
    % token string is - for a NAME that length is just the length of
    % the RPN Token stream so add it
    RPN2= <<Len:16/little,Rest/binary>>,
    {RPNTokens, _TokenArray}=parse_FRM_Results(RPN2,Name),
    Scope = case SheetIndex of
                0 -> global;
                _ -> local
            end,
    Index=excel_util:get_length(Tbl,tmp_names),
    ExtBookIndex = excel_util:get_length(Tbl, tmp_externalbook) -1,
    excel_util:write(Tbl,tmp_names,[{index,Index},{extbook,ExtBookIndex},
                                    {sheetindex,SheetIndex},{type,Scope},
                                    {name,binary_to_list(NameName)},
                                    {rpn,RPNTokens}]).

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
parse_SST(NoOfStrings,NoOfStrings,_Tbl,_)->
    ok;

parse_SST(StringNo,NoOfStrings,Tbl,[BinHead|BinTail])->
    % io:format("In parse_SST StringNo is ~p NoOfStrings is ~p~n",
    %          [StringNo, NoOfStrings]),
    BinLen1=length(binary_to_list(BinHead)),
    <<BinLen2:16/little-unsigned-integer,_Rest/binary>>=BinHead,
    % This clause handles the case where a record falls over a continuation
    % if it does it rejigs parse_SST to move down the Binary List
    % 
    % This is described in Section 5.21 of excelfileformatV1-42.pdf
    if
        (BinLen1 > BinLen2+3) ->
            NewBinHead = BinHead,
            NewBinTail = BinTail,
            ParseBin=BinHead;
        (BinLen1 == BinLen2+3)->
            %io:format("In if (2)~n"),
            case BinTail of
                [] -> 
                    %io:format("In if (2a)~n"),
                    H1 = [],
                    T1 = [];
                _Other -> 
                    %io:format("In if (2b)~n"),
                    [H1|T1] = BinTail
            end,
            NewBinTail=T1,
            ParseBin=BinHead,
            NewBinHead=list_to_binary([ParseBin,H1]);
        (BinLen1 < BinLen2 + 3) ->
            %io:format("BinLen1 is ~p~nBinLen2 is ~p~n", [BinLen1, BinLen2]),
            ExtLen=BinLen2+3-BinLen1,
            %io:format("ExtLen is ~p~n", [ExtLen]),
            [H2|T2] = BinTail,
            % remember to discard the 8 byte unicode flag
            % This is a BIG PROBLEM see Section 5.21 of excelfileformatV1-43.pdf
            % Don't know how to trigger it thought...
            <<_Bits:1/binary,Ext:ExtLen/binary,NewBinHeadPart/binary>>=H2,
            %io:format("Bits is ~p~n", [Bits]),
            %io:format("ExtLen is ~p and NewBinHeadPart is ~p long~n",
            %          [ExtLen, erlang:size(NewBinHeadPart)]),
            %io:format("BinHead is ~w~nExt is ~w~n", [BinHead, Ext]),
            NewBinTail=T2,
            % kept over from the old world...
            ParseBin=list_to_binary([BinHead,Ext]),
            NewBinHead=list_to_binary([ParseBin,
                                       NewBinHeadPart])
    end,
    Return=excel_util:parse_CRS_Uni16(ParseBin,2),
    String=excel_util:get_utf8(Return),
    String2=prepend_quote(String),
    % io:format("in parse_SST String is ~p~n", [String]),
    {_,StringLen,_RestLen}=Return, 
    BinLen=8*StringLen,
    <<_String2:BinLen/little-unsigned-integer,Rest/binary>>=NewBinHead,
    excel_util:write(Tbl,tmp_strings,[{index,StringNo},{string,String2}]),
    parse_SST(StringNo+1,NoOfStrings,Tbl,[Rest|NewBinTail]).

%% This is nasty, sure excel stores this data somewhere?
prepend_quote(String) ->
    try _ = list_to_integer(String),
        "'"++String
    catch _:_ ->
            try _ = list_to_float(String),
                "'"++String
            catch _:_ -> String end
    end.

write_row([],_RowIndex,_FirstColIndex,_Name,_Tbl)->
    {ok,ok};
write_row([{{xf_index,XFIndex},{value,number,Number}}|T],RowIndex,
          FirstColIndex,Name,Tbl)->
    excel_util:write(Tbl,cell,[{{sheet,Name},{row_index,RowIndex},
                                {col_index,FirstColIndex}},{xf_index,XFIndex},
                               {value,number,Number}]),
    write_row(T,RowIndex,FirstColIndex+1,Name,Tbl).

parse_externsheet(<<>>,_N,_Tbl)->
    {ok,ok};
parse_externsheet(Bin,N,Tbl)->
    <<SubRec:16/little-unsigned-integer,
     FirstSheet:16/little-signed-integer,
     LastSheet:16/little-signed-integer,
     Rest/binary>>=Bin,
    % ExtBookIndex = excel_util:get_length(Tbl, tmp_externalbook) - 1,
    Record=[{index,N},{extbook_index,SubRec},
            {firstsheet,FirstSheet},{lastsheet,LastSheet}],
    excel_util:write(Tbl,tmp_extsheets,Record),
    parse_externsheet(Rest,N+1,Tbl).

%% parses external references as defined in Section 5.99.1 of 
%% excelvileformatV1.40.pdf only covers the sheet reference used and
%% not the URL or file part of it
parse_externalrefs(<<_NoOfSh:16/little-unsigned-integer,Rest/binary>>,Tbl)->
    [RawFileName|BinaryList]=get_ext_ref_names(Rest,[]),
    Names=[binary_to_list(X) || X <- BinaryList],
    ParsedFileName=parse_filename(RawFileName),
    write_externalref({name,ParsedFileName},Names,Tbl).

write_externalref(Entry,List,Tbl)->
    Index=excel_util:get_length(Tbl,tmp_externalbook),
    excel_util:write(Tbl,tmp_externalbook,[{index,Index},Entry,List]).

get_ext_ref_names(<<>>,Residuum)->
    lists:reverse(Residuum);
get_ext_ref_names(Bin,Residuum)->
    Return=excel_util:parse_CRS_Uni16(Bin,2),
    {[{_Type,_String}],StringLen,_RestLen}=Return,
    Utf8String=excel_util:get_utf8(Return),
    StringLen2=StringLen*8,
    <<_String2:StringLen2/little-unsigned-integer,Rest/binary>>=Bin,
    get_ext_ref_names(Rest,[list_to_binary(Utf8String)|Residuum]).

parse_filename(<<?chEncode:8/little-unsigned-integer,
                chVolume:8/little-unsigned-integer,
                Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chSameVolume:8/little-unsigned-integer,
                Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chDownDir:8/little-unsigned-integer,
                Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chUpDir:8/little-unsigned-integer,
                Rest/binary>>) -> "../../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chStartUpDir:8/little-unsigned-integer,
                Rest/binary>>) -> "./"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chAltStartUpDir:8/little-unsigned-integer,
                Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                chLibDir:8/little-unsigned-integer,
                Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(<<?chEncode:8/little-unsigned-integer,
                Rest/binary>>) -> "../"++snip_xls(Rest)++"/";
parse_filename(Bin) -> "../"++binary_to_list(Bin)++"/".

snip_xls(Bin)->
    FileName=binary_to_list(Bin),
    RegExp=".xls$", %" comment to fix syntax highlighting
        case regexp:gsub(FileName,RegExp,"") of 
            {ok, NewString,_} -> NewString;
            {error,_}         -> FileName
        end.

parse_externname(Bin,Tbl)->
    % we dont care mostly but for the first 4 bits (the minioptions) and
    % we chuck the remaining 12 bits away (the discard) mostly - except
    % when we don't :(
    <<MiniOptions:4/little-unsigned-integer,
     Discard:12/little-unsigned-integer,
     Rest/binary>>=Bin,
    case MiniOptions of
        ?STANDARD   -> <<_NameIndex:16/little-unsigned-integer,
                        _NotUsed:16/little-unsigned-integer,
                        Name/binary>>=Rest,
                       {[{_,Name2}],Len1,_Len2}=excel_util:parse_CRS_Uni16(Name,1),
                       NameLen=8*Len1,
                       <<_Name:NameLen/little-unsigned-integer,_Rest2/binary>>=Name,
                       Name3=binary_to_list(Name2),
                       case Discard of
                           0 ->
                               % first get the index of the index of the current
                               % SUPBOOK/EXTERNAME
                               EXBIdx = excel_util:get_length(Tbl,tmp_externalbook) - 1,
                               % now get all externames that match that SBidx
                               {value, {_TableName, Tid}} = lists:keysearch(tmp_externnames,1,Tbl),
                               ExtNameList = ets:lookup(Tid, {extbook_index, EXBIdx}),
                               Len = length(ExtNameList),
                               excel_util:write(Tbl,tmp_externnames,[{extbook_index, EXBIdx},
                                                                     {ext_index, Len},
                                                                     {name, Name3}]);
                           _ ->
                               write_externname(Name3,Tbl)
                       end;
        ?BUILT_IN   -> write_externname(Rest,Tbl);
        ?MANUAL_DDE -> excel_util:write(Tbl,lacunae,
                                        [{identifier,"MANUAL_DDE"},
                                         {source,excel_records.erl},
                                         {msg,"not being processed"}]);
        ?AUTO_DDE   -> excel_util:write(Tbl,lacunae,
                                        [{identifier,"AUTO_DDE"},
                                         {source,excel_records.erl},
                                         {msg,"not being processed"}]);
        ?MANUAL_OLE -> excel_util:write(Tbl,lacunae,
                                        [{identifier,"MANUAL_OLE"},
                                         {source,excel_records.erl},
                                         {msg,"not being processed"}]);
        ?AUTO_OLE   -> excel_util:write(Tbl,lacunae,
                                        [{identifier,"AUTO_OLE"},
                                         {source,excel_records.erl},
                                         {msg,"not being processed"}])
    end.

write_externname(_Name,_Tbl)->
    % ExtBookIndex = excel_util:get_length(Tbl, tmp_externalbook),
    % io:format("ExtBookIndex is ~p~n",[ExtBookIndex]),
    %io:format("in excel_records Name is ~p~n",[Name]),
    %io:format("in excel_records JUST KINDA WIGGIN OUT...~n"),
    ok.

write_blanks(_Name,_RowIndex,_FirstColIndex,_LastColIndex,<<>>,_Tbl) -> {ok, ok};
write_blanks(Name,RowIndex,FirstColIndex,LastColIndex,XFRecords,Tbl) ->
    <<XFIndex:16/little-unsigned-integer,Rest/binary>> = XFRecords,
    excel_util:write(Tbl,tmp_blanks,[{{sheet,Name},{row_index,RowIndex},
                                      {col_index,FirstColIndex}},{xf_index,XFIndex}]),
    write_blanks(Name,RowIndex,FirstColIndex+1,LastColIndex,Rest,Tbl).


