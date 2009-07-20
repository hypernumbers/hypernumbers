%%%-------------------------------------------------------------------
%%% File    excel_records.erl
%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc     This module parses the Excel records.
%%%-------------------------------------------------------------------
-module(excel_records).

%%% Include files with macros encoding Microsoft File Format constants
-include("microsoftbiff.hrl").
-include("excel_records.hrl").
-include("excel_errors.hrl").
-include("excel_supbook.hrl").
-include("excel_externname.hrl").

-export([ parse_rec/4 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function processes the main record types that are used in the       %%%
%%% BIFF8/BIFF8X formats as described in Section 5.1 of the                  %%%
%%% excelfileformat.pdf (V1.40)                                              %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
parse_rec(?FORMULA, Bin, Name, _Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer,   _Result:64, _CalcFlag:16, _NotUsed:32,
     Rest/binary>> = Bin,
    
    {Tok, TokArr} = parse_FRM_Results(Rest, Name),
    Data = [mref(Name, Row, Col), {xf_index,XF}, {tokens, Tok}, {tokenarrays, TokArr}],
    {write, tmp_cell, Data, mref(Name, Row, Col)};

parse_rec(?EOF, _Bin, _Name, _Tbl) ->
    ok;

parse_rec(?EXTERNSHEET, <<_NumRefs:16, R2/binary>>, _Name, Tbl) ->
    parse_externsheet(R2, 0, Tbl);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% This function takes the name records defined in Section 5.67 of          %%%
%%% excelfileformat.pdf (V1.40) and extracts the name table                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in FORMULA the RPN tokens may have an additional token stream appended
% to them that the formula uses (see 3.1.1 of excelfileformatV1-42)
% as a result they start with a len record that describes how long the
% token string is - for a NAME that length is just the length of
% the RPN Token stream so add it
% renamed DEFINEDNAME in v1.42
parse_rec(?NAME, Bin, Name, Tbl) ->

    <<_OptionFlag:2/binary, _KybdShortCut:8, Len:8/little-unsigned-integer,
     _Size:16, _NotUsed:16, SheetId:16/little-unsigned-integer,
     _MenuTxt:8, _DescTxt:8, _HelpTxt:8, _StatusTxt:8, Rest/binary>> = Bin,

    <<_Options:1/binary, NameName:Len/binary, Rest2/binary>>=Rest,
    Tokens = binary_to_list(Rest2),
    NLen = length(Tokens),
    {RPNTokens, _Arr} = parse_FRM_Results(<<NLen:16/little,Rest2/binary>>, Name),
    Scope = case SheetId of 0 -> global; _ -> local end,
    Id = excel_util:get_length(Tbl,tmp_names),
    ExtBookId = excel_util:get_length(Tbl, tmp_externalbook) - 1,
    Data = [{index,Id}, {extbook,ExtBookId}, {sheetindex,SheetId},
            {type,Scope}, {name,binary_to_list(NameName)}, {rpn,RPNTokens}],
    
    {write, tmp_names, Data};

parse_rec(?DATEMODE, <<?rc_DATEMODE_WINDOWS:16/little-unsigned-integer>>, _, _) ->
    {write, misc, [{index, datemode}, {value, "Windows"}]};
parse_rec(?DATEMODE, <<?rc_DATEMODE_MACINTOSH:16/little-unsigned-integer>>, _, _) ->
    {write, misc, [{index, datemode}, {value, "Macintosh"}]};

parse_rec(?EXTERNNAME2, Bin, _Name, Tbl) ->
    % Best described by Section 5.39 of excelfileformatV1-41.pdf
    parse_externname(Bin, Tbl),
    ok;

parse_rec(?FONT, Bin, _Name, _Tbl) ->
    <<Height:16/little-unsigned-integer, Options:2/binary,
     ColourIdx:16/little-unsigned-integer, FontWeight:16/little-unsigned-integer,
     Escapement:16/little-unsigned-integer, UnderlineType:8/little-unsigned-integer,
     FontFamily:8/little-unsigned-integer, _CharSet:8, _:8, FontName/binary>> = Bin,
    % First up parse the options
    OptionsCSS = parse_font_options(Options),
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
    {append, tmp_fonts, [{colour_index, ColourIdx}, {css, CSS}]};

parse_rec(?BOUNDSHEET, Bin, _Name, Tbl) ->
    {_ShBOF, _Vis, _ShType, _Name2, ShName} = excel_util:get_bound_sheet(Bin, Tbl),
    excel_util:append_sheetname(Tbl, excel_util:get_utf8(ShName)),
    ok;

parse_rec(?PALETTE, _Bin, _Name, _Tbl)->
    {write, warnings, ["Custom Palettes not being imported - "++
                       "the standard palette will be used!"]};

parse_rec(?MULRK, Bin, Name, Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     Rest/binary>> = Bin,
    write_row(parse_XF_RK(Rest), Row, Col, Name, Tbl);

parse_rec(?MULBLANK, Bin, Name, Tbl) ->
    Size = (size(Bin)-6),
    <<Row:16/little-unsigned-integer, FCol:16/little-unsigned-integer,
     XF:Size/binary, LCol:16/little-unsigned-integer>> = Bin,
    write_blanks(Name, Row, FCol, LCol, XF, Tbl);

parse_rec(?XF2, Bin, _Name, _Tbl) ->
    <<FontIndex:16/little-unsigned-integer, FormatIndex:16/little-unsigned-integer,
     XFTypeAndParent:16/little-unsigned-integer, XFAlignment:8/little-unsigned-integer,
     _XFRotation:8, _XFIndentation:8, XFFlags:8/little-unsigned-integer,
     XFCellBorders1:32/little-unsigned-integer, XFCellBorders2:32/little-unsigned-integer,
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
    LeftLineStyleBits   =
        (XFCellBorders1 band ?rc_XF_LEFT_LINE_MASK),
    RightLineStyleBits  =
        (XFCellBorders1 band ?rc_XF_RIGHT_LINE_MASK)/?rc_XF_LEFT_LINE_MASK,
    TopLineStyleBits    =
        (XFCellBorders1 band ?rc_XF_TOP_LINE_MASK)/?rc_XF_RIGHT_LINE_MASK,
    BottomLineStyleBits =
        (XFCellBorders1 band ?rc_XF_BOTTOM_LINE_MASK)/?rc_XF_TOP_LINE_MASK,

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

    Data = [{format_index,FormatIndex},
            {type,XFType},
            {parent_index,XFParentIndex},
            {font_index,FontIndex},
            {css,CSS},
            {attributes,Attributes},
            {border_colour,Colours1},
            {bg_colour,Colours2}],
    
    {append, tmp_xf, Data};

parse_rec(?SST, [H | T], _Name, Tbl) ->
    <<_Used:32, StringCount:32/little-unsigned-integer, Rest/binary>> = H,
    parse_SST(0, StringCount, Tbl, [Rest | T]);

parse_rec(?LABELSST, Bin, Name, Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer, SST:32/little-unsigned-integer,
     _Rest/binary>> = Bin,
    % Now look up the string in the string table
    String = excel_util:lookup_string(Tbl, SST),
    {write, cell, [mref(Name, Row, Col), {xf_index, XF}, {string, String}]};

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
parse_rec(?SUPBOOK, Bin, _Name, Tbl) ->
    case Bin of
        <<NoSheets:16/little-unsigned-integer,
         ?InternalReferences:16/little-unsigned-integer>> ->
            write_externalref({this_file,placeholder},[],Tbl),
            {write, misc, [{index,noofsheets},{value,NoSheets}]};
        <<?Add_In_Fns1:16/little-unsigned-integer,
         ?Add_In_Fns2:16/little-unsigned-integer>> ->
            write_externalref({skipped,add_ins},[],Tbl),
            {write, lacunae,[{identifier,"Add_In_Fns"},
                             {source,excel_records.erl},
                             {msg,"not being processed"}]};
        <<?DDE_OLE:16/little-unsigned-integer,_Rest/binary>> ->
            write_externalref({skipped,dde_ole},[],Tbl),
            {write, lacunae, [{identifier,"DDE and OLE links",Tbl},
                              {source,excel_records.erl}]};
        _ -> 
            parse_externalrefs(Bin,Tbl),
            ok
    end;

parse_rec(?BLANK2, Bin, Name, _Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer>> = Bin,
    {write, tmp_blanks, [mref(Name, Row, Col), {xf_index, XF}]};

parse_rec(?NUMBER2, Bin, Name, _Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer, Val:64/little-unsigned-float>> = Bin,
    {write, cell, [mref(Name, Row, Col), {xf_index, XF}, {value, number, Val}]};

parse_rec(?BOOLERR2, Bin, Name, _Tbl) ->
    % One might think that a record called BoolErr would contain a boolean error
    % and error pertaining or obtaining in some straightforward way to Booleans
    % wouldn't one? But on no - it contains a Boolean *OR* an Error
    % How mad the fuck is that?
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer,   BoolErr:8/little-unsigned-integer,
     Type:8/little-unsigned-integer>>=Bin,

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
    {write, cell, [mref(Name, Row, Col), {xf_index, XF}, {value,ValType,Value}]};

parse_rec(?ROW2, Bin, _Name, _Tbl) ->
    <<Row:16/little-unsigned-integer, FCol:16/little-unsigned-integer,
     LCol:16/little-unsigned-integer, _Height:16, _NotUsed:16,
     _NotUsed2:16, _Options:16, XFRef:12/little-unsigned-integer,
     _Discard1:4/little-unsigned-integer>> = Bin,
    Data = [{row_index, Row}, {first_col, FCol},
            {last_col, LCol}, {format_index, XFRef}],
    {write, tmp_rows, Data};

parse_rec(?ARRAY2, Bin, Name, _Tbl) ->
    <<Range:6/binary, _Options:2/binary, _NotUsed:4/binary,
     RawTokens/binary>> = Bin,
    {[{FR, LR, FC, LC}],_} = excel_util:read_cell_range_addies(1, '8bit', Range),
    {Tokens, Arr} = parse_FRM_Results(RawTokens, Name),
    Data = [ mref(Name, FR, FC, LR, LC), {type, array},
             {tokens,Tokens}, {tokenarrays, Arr}],
    {write, tmp_sh_arr_fml, Data};

parse_rec(?RK, Bin, Name, _Tbl) ->
    <<Row:16/little-unsigned-integer, Col:16/little-unsigned-integer,
     XF:16/little-unsigned-integer, RK:32/little-unsigned-integer>>=Bin,
    RK2 = excel_util:parse_CRS_RK(<<RK:32/little-unsigned-integer>>),
    {write, cell, [mref(Name, Row, Col), {xf_index,XF}, {value, number, RK2}]};

parse_rec(?FORMAT2, Bin, _Name, _Tbl) ->
    <<FormatId:16/little-unsigned-integer, FormatBin/binary>> = Bin,
    FormatStr = excel_util:get_utf8(excel_util:parse_CRS_Uni16(FormatBin)),
    Data = [{format_index, FormatId}, {type, unknown_as_yet},
            {category, userdefined},  {format, FormatStr}],
    {write, tmp_formats, Data};

parse_rec(?SHRFMLA, Bin, Name, _Tbl) ->
    <<Range:6/binary, _NotUsed:8, _NoRecords:8, Rest/binary>>=Bin,
    {[{FR,LR,FC,LC}],_}=excel_util:read_cell_range_addies(1, '8bit', Range),
    {Tok, TokArr} = parse_FRM_Results(Rest,Name),
    Data = [mref(Name, FR, FC, LR, LC), {type, shared},
            {tokens, Tok}, {tokenarrays, TokArr}],
    {write, tmp_sh_arr_fml, Data};

parse_rec(Other, _Bin, _Name, _Tbl) ->
    
    {Id, Msg} = case lists:member(Other, not_processed()) of
                    true  -> {"Hmm", "not being processed"};
                    false -> {{"undocumented record type",Other},
                              "not being processed"}
                end,
    {write, lacunae, [{identifier, Id}, {source, excel_records.erl}, {msg, Msg}]}.

not_processed() ->
    [?CALCOUNT, ?PRECISION, ?REFMODE, ?DELTA, ?ITERATION, ?PROTECT, ?PASSWORD,
     ?HEADER, ?FOOTER, ?WINDOWPROTECT, ?VERTICALPAGEBREAKS, ?HORIZONTALPAGEBREAKS,
     ?NOTE, ?SELECTION, ?LEFTMARGIN, ?RIGHTMARGIN, ?TOPMARGIN, ?BOTTOMMARGIN,
     ?PRINTHEADERS, ?PRINTGRIDLINES, ?FILEPASS, ?CONTINUE, ?WINDOW1, ?BACKUP,
     ?PANE, ?CODEPAGE, ?DCONREF, ?DEFCOLWIDTH, ?XCT, ?CRN, ?FILESHARING, ?WRITEACCESS,
     ?UNCALCED, ?SAVERECALC, ?OBJECTPROTECT, ?COLINFO, ?GUTS, ?WSBOOL, ?GRIDSET,
     ?HCENTRE, ?VCENTRE, ?WRITEPROT, ?COUNTRY, ?HIDEOBJ, ?SORT, ?STANDARDWIDTH,
     ?SCL, ?SETUP, ?RSTRING, ?DBCELL, ?BOOKBOOL, ?SCENPROTECT, ?MERGEDCELLS,
     ?BITMAP, ?PHONETIC, ?EXTSST, ?LABELRANGES, ?USESELFS, ?DSF, ?CONDFMT, ?DVAL,
     ?HLINK, ?DV, ?DIMENSIONS2, ?LABEL2, ?STRING2, ?INDEX2, ?DEFAULTROWHEIGHT2,
     ?TABLEOP_2, ?WINDOW2_2, ?STYLE, ?QUICKTIP, ?SHEETLAYOUT, ?SHEETPROTECTION,
     ?RANGEPROTECTION].

mref(Sheet, Row, Col) ->
    {{sheet, Sheet}, {row_index, Row}, {col_index, Col}}.
mref(Sheet, FirstRow, FirstCol, LastRow, LastCol) ->
    {{sheet, Sheet}, {firstrow, FirstRow}, {firstcol, FirstCol},
     {lastrow, LastRow}, {lastcol, LastCol}}.

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
parse_SST(NoOfStrings, NoOfStrings, _Tbl, _)->
    ok;

parse_SST(StringNo, NoOfStrings, Tbl, [BinHead | BinTail])->
    
    BinLen1 = length(binary_to_list(BinHead)),
    <<BinLen2:16/little-unsigned-integer, _Rest/binary>> = BinHead,
    % This clause handles the case where a record falls over a continuation
    % if it does it rejigs parse_SST to move down the Binary List
    % 
    % This is described in Section 5.21 of excelfileformatV1-42.pdf
    if
        ( BinLen1 > BinLen2 ) ->
            NewBinHead = BinHead,
            NewBinTail = BinTail,
            ParseBin = BinHead;
        
        ( BinLen1 == BinLen2 ) ->
            case BinTail of
                [] -> 
                    H1 = [],
                    T1 = [];
                _Other -> 
                    [H1|T1] = BinTail
            end,
            NewBinTail = T1,
            ParseBin = BinHead,
            NewBinHead = list_to_binary([ParseBin, H1]);
        (BinLen1 < BinLen2) ->
            ExtLen = (BinLen2 - BinLen1),
            [H2 | T2] = BinTail,
            % remember to discard the 8 byte unicode flag
            % This is a BIG PROBLEM see Section 5.21 of excelfileformatV1-43.pdf
            % Don't know how to trigger it thought...
            <<_Bits:1/binary, Ext:ExtLen/binary, NewBinHeadPart/binary>> = H2,
            NewBinTail = T2,
            ParseBin   = list_to_binary([BinHead, Ext]),            
            NewBinHead = list_to_binary([ParseBin, NewBinHeadPart])
    end,
    
    {_, StringLen, _RestLen} = Return = excel_util:parse_CRS_Uni16(ParseBin, 2),
    String = excel_util:get_utf8(Return),
    BinLen = 8 * StringLen,
    <<_String2:BinLen/little-unsigned-integer, Rest/binary>> = NewBinHead,
    excel_util:write(Tbl, tmp_strings, [{index,  StringNo},
                                        {string, prepend_quote(String)}]),
    parse_SST(StringNo+1, NoOfStrings, Tbl, [ Rest | NewBinTail]).

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
    ok;
write_row([{{xf_index, XFIndex}, {value,number,Number}}|T], Row, FirstCol, Name, Tbl) ->
    excel_util:write(Tbl,cell,[mref(Name, Row, FirstCol), 
                               {xf_index,XFIndex},
                               {value,number,Number}]),
    write_row(T, Row, FirstCol+1, Name, Tbl).

parse_externsheet(<<>>,_N,_Tbl)->
    ok;
parse_externsheet(Bin,N,Tbl)->
    <<SubRec:16/little-unsigned-integer,
     FirstSheet:16/little-signed-integer,
     LastSheet:16/little-signed-integer,
     Rest/binary>> = Bin,
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
    re:replace(FileName, ".xls$", "", [{return, list}, global]). %"

parse_externname(Bin,Tbl)->
    % we dont care mostly but for the first 4 bits (the minioptions) and
    % we chuck the remaining 12 bits away (the discard) mostly - except
    % when we don't :(
    <<MiniOptions:4/little-unsigned-integer, Discard:12/little-unsigned-integer,
     Rest/binary>>=Bin,
    
    case MiniOptions of
        ?STANDARD   ->
            <<_NameIndex:16, _NotUsed:16, Name/binary>> = Rest,
            {[{_, Name2}], Len1, _Len2} = excel_util:parse_CRS_Uni16(Name, 1),
            NameLen= 8 * Len1,
            <<_Name:NameLen/little-unsigned-integer,_Rest2/binary>>=Name,
            Name3 = binary_to_list(Name2),
            case Discard of
                0 ->
                    % first get the index of the index of the current
                    % SUPBOOK/EXTERNAME
                    EXBIdx = excel_util:get_length(Tbl, tmp_externalbook) - 1,
                    % now get all externames that match that SBidx
                    {value, {_TableName, Tid}} = lists:keysearch(tmp_externnames, 1, Tbl),
                    ExtNameList = ets:lookup(Tid, {extbook_index, EXBIdx}),
                    {write, tmp_externnames,[{extbook_index, EXBIdx},
                                             {ext_index, length(ExtNameList)},
                                             {name, Name3}]};
                _ ->
                    write_externname(Name3, Tbl)
            end;
        ?BUILT_IN   ->
            write_externname(Rest,Tbl);
        ?MANUAL_DDE ->
            {write, lacunae, [{identifier,"MANUAL_DDE"},
                              {source,excel_records.erl},
                              {msg,"not being processed"}]};
        ?AUTO_DDE   ->
            {write, lacunae, [{identifier,"AUTO_DDE"},
                              {source,excel_records.erl},
                              {msg,"not being processed"}]};
        ?MANUAL_OLE ->
            {write, lacunae, [{identifier,"MANUAL_OLE"},
                              {source,excel_records.erl},
                              {msg,"not being processed"}]};
        ?AUTO_OLE   ->
            {write, lacunae, [{identifier,"AUTO_OLE"},
                              {source,excel_records.erl},
                              {msg,"not being processed"}]}
    end.

write_externname(_Name,_Tbl)->
    % ExtBookIndex = excel_util:get_length(Tbl, tmp_externalbook),
    % io:format("ExtBookIndex is ~p~n",[ExtBookIndex]),
    %io:format("in excel_records Name is ~p~n",[Name]),
    %io:format("in excel_records JUST KINDA WIGGIN OUT...~n"),
    ok.

write_blanks(_Name, _Row,_FirstCol, _LastCol, <<>>, _Tbl) ->
    ok;
write_blanks(Name, Row, FirstCol, LastCol, XF, Tbl) ->
    <<XFId:16/little-unsigned-integer, Rest/binary>> = XF,
    excel_util:write(Tbl,tmp_blanks,[mref(Name, Row, FirstCol), {xf_index, XFId}]),
    write_blanks(Name, Row, FirstCol+1, LastCol, Rest, Tbl).
