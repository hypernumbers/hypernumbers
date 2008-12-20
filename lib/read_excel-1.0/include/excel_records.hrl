%%% This file contains macro definitions for various record constants
%%% in Microsoft Excel BIFF8 formats

%%% The comments that describe the format are taken from the document:
%%% http://sc.openoffice.org/excelfileformat.pdf

%%% This file is based on Version 1.40 of that document

%%% They are descibed in Section 6.

%%% The constants are listed here alphabetically by RECORD NAME
%%% that is to say for every record in microsoftbiff.hrl there
%%% may or may not be associated record costants - but if there
%%% are they will be grouped here by the microsoftbiff.hrl
%%% RECORD NAME

%% DATEMODE BIFF8 Section 5.28 of excelfileformatV1-41.pdf
-define(rc_DATEMODE_WINDOWS,0).
-define(rc_DATEMODE_MACINTOSH,1).

-define(rc_CELL_XF,0).
-define(rc_STYLE_XF,1).

%% BOF BIFF8 Section 5.8.1 excelfileformat.pdf V1.40
-define(rc_BOF_WorkbookGlobals,5).
-define(rc_BOF_VBModule,6).
-define(rc_BOF_Worksheet,16).
-define(rc_BOF_Chart,32).
-define(rc_BOF_MacroSheet,64).
-define(rc_BOF_Workspace,256).

%% BOUNDSHEET BIFF8 B Section 5.12 excelfileformat.pdf V1.40
-define(rc_BOUNDSHEET_Visible,0).
-define(rc_BOUNDSHEET_Hidden,1).
-define(rc_BOUNDSHEET_StrongHidden,2).
-define(rc_BOUNDSHEET_Worksheet,0).
-define(rc_BOUNDSHEET_Chart,2).
-define(rc_BOUNDSHEET_VBModule,6).

%% FORMULA BIFF8 Section 5.47 excelfileformat.pdf V1.40
-define(rc_FORMULA_ALWAYS_RECALC_BITFLAG,1).
-define(rc_FORMULA_CALC_ON_OPEN_BITFLAG,2).
-define(rc_FORMULA_PART_SHARED_BITFLAG,8).
-define(rc_FORMULA_STRING,0).
-define(rc_FORMULA_BOOLEAN,1).
-define(rc_FORMULA_ERROR,2).
-define(rc_FORMULA_EMPTY_CELL,3).

%% FONTS BIFF8 Section 5.45 of excelfileformat.pdf V1.42
-define(rc_FONT_OPTIONS_BOLD,16#0001).
-define(rc_FONT_OPTIONS_ITALIC,16#0002).
-define(rc_FONT_OPTIONS_UNDERLINED,16#0004).
-define(rc_FONT_OPTIONS_STRUCKOUT,16#0008).
-define(rc_FONT_OPTIONS_OUTLINED,16#0010).
-define(rc_FONT_OPTIONS_SHADOWED,16#0020).
-define(rc_FONT_OPTIONS_CONDENSED,16#0040).
-define(rc_FONT_OPTIONS_EXTENDED,16#0080).

-define(rc_FONT_ESCAPEMENT_NONE,0).
-define(rc_FONT_ESCAPEMENT_SUPERSCRIPT,1).
-define(rc_FONT_ESCAPEMENT_SUBSCRIPT,2).

-define(rc_FONT_UNDERLINE_NONE,0).
-define(rc_FONT_UNDERLINE_SINGLE,1).
-define(rc_FONT_UNDERLINE_DOUBLE,2).
-define(rc_FONT_UNDERLINE_SINGLE_ACC,16#21).
-define(rc_FONT_UNDERLINE_DOUBLE_ACC,16#22).

-define(rc_FONT_FAMILY_STANDARD,0).
-define(rc_FONT_FAMILY_ROMAN,1).
-define(rc_FONT_FAMILY_SWISS,2).           
-define(rc_FONT_FAMILY_MODERN,3).
-define(rc_FONT_FAMILY_SCRIPT,4).
-define(rc_FONT_FAMILY_DECORATIVE,5).

-define(rc_FONT_ANSI_Latin,0).
-define(rc_FONT_System_default,1).
-define(rc_FONT_Symbol,2).
-define(rc_FONT_Apple_Roman,77).
-define(rc_FONT_ANSI_Japanese_Shift_JIS,128).
-define(rc_FONT_ANSI_Korean_Hangul,129).
-define(rc_FONT_ANSI_Korean_Johab,130).
-define(rc_FONT_ANSI_Chinese_Simplified_GBK,134).
-define(rc_FONT_ANSI_Chinese_Traditional_BIG5,136).
-define(rc_FONT_ANSI_Greek,161).
-define(rc_FONT_ANSI_Turkish,162).
-define(rc_FONT_ANSI_Vietnamese,163).
-define(rc_FONT_ANSI_Hebrew,177).
-define(rc_FONT_ANSI_Arabic,178).
-define(rc_FONT_ANSI_Baltic,186).
-define(rc_FONT_ANSI_Cyrillic,204).
-define(rc_FONT_ANSI_Thai,222).
-define(rc_FONT_ANSI_Latin_II_Central_European,238).
-define(rc_FONT_OEM_Latin_I,255).

-define(rc_COLUMN_HIDDEN,16#0001).
-define(rc_COLUMN_COLLAPSED,16#1000).

%% XF BIFF8 Section 5.115 of excelfileformatv1-42.pdf
-define(rc_XF_CELL_LOCKED_MASK,16#01).
-define(rc_XF_HIDDEN_FORMULA_MASK,16#02).
-define(rc_XF_XF_TYPE_MASK,16#04).
-define(rc_XF_CELL,16#00).
-define(rc_XF_STYLE,16#01).
-define(rc_XF_PARENT_INDEX_MASK,16#FFF0).

-define(rc_XF_H_ALIGN_MASK,2#111). % use to band out the horizontal alignment 

-define(rc_XF_H_ALIGN_GENERAL,0).
-define(rc_XF_H_ALIGN_LEFT,1).
-define(rc_XF_H_ALIGN_CENTERED,2).
-define(rc_XF_H_ALIGN_RIGHT,3).
-define(rc_XF_H_ALIGN_FILLED,4).
-define(rc_XF_H_ALIGN_JUSTIFIED,5).
-define(rc_XF_H_ALIGN_CEN_ACROSS_SEL,6).
-define(rc_XF_H_ALIGN_DISTRIBUTED,7).

% -define(rc_XF_TEXT_WRAPPED,8). % not implemented

-define(rc_XF_V_ALIGN_MASK,2#1110000). % use to band out the vertical alignment 

-define(rc_XF_V_ALIGN_TOP,0).
-define(rc_XF_V_ALIGN_CENTRED,1).
-define(rc_XF_V_ALIGN_BOTTOM,2).
-define(rc_XF_V_ALIGN_JUSTIFIED,3).
-define(rc_XF_V_ALIGN_DISTRIBUTED,4).

-define(rc_XF_LAST_CHAR_JUSTIFY_MASK,2#10000000).
-define(rc_XF_LAST_CHAR_JUSTIFY,2#10000000).

-define(rc_XF_FLAG_NUMBERFORMAT,16#1).
-define(rc_XF_FLAG_FONT,16#2).
-define(rc_XF_FLAG_TEXT,16#4).
-define(rc_XF_FLAG_BORDER,16#8).
-define(rc_XF_FLAG_BACKGROUND,16#10).
-define(rc_XF_FLAG_CELL_PROT,16#20).

-define(rc_XF_LEFT_LINE_MASK,16#F).
-define(rc_XF_RIGHT_LINE_MASK,16#F0).
-define(rc_XF_TOP_LINE_MASK,16#F00).
-define(rc_XF_BOTTOM_LINE_MASK,16#F000).
-define(rc_XF_COLOUR_INDEX_LEFT,16#7F000).
-define(rc_XF_COLOUR_INDEX_RIGHT,16#38F00000).
-define(rc_XF_COLOUR_TOPLEFT_BOTTOMRIGHT,16#40000000).
-define(rc_XF_COLOUR_TOPRIGHT_BOTTOMLEFT,16#80000000).

-define(rc_XF_BORDER_NO_LINE,16#00).
-define(rc_XF_BORDER_THIN,16#01).
-define(rc_XF_BORDER_MEDIUM,16#02).
-define(rc_XF_BORDER_DASHED,16#03).
-define(rc_XF_BORDER_DOTTED,16#04).
-define(rc_XF_BORDER_THICK,16#05).
-define(rc_XF_BORDER_DOUBLE,16#06).
-define(rc_XF_BORDER_HAIR,16#07).
-define(rc_XF_BORDER_MED_DASHED,16#08).
-define(rc_XF_BORDER_THIN_DASH_DOT,16#09).
-define(rc_XF_BORDER_MED_DASH_DOT,16#0A).
-define(rc_XF_BORDER_THIN_DASH_DOT_DOT,16#0B).
-define(rc_XF_BORDER_MED_DASH_DOT_DOT,16#0C).
-define(rc_XF_BORDER_SLANTED_MED_DASH_DOT,16#0D).

%% these define the Flags for the NAME record as described
%% in Section 5.33 of excelfileformatsV1-42.pdf
%% 
%% NOT USED AT THE MO...
%% 
%-define(rc_NAME_OPT_HIDDEN_MASK,16#1).
%-define(rc_NAME_OPT_FUNC_MASK,16#2).
%-define(rc_NAME_OPT_VBASIC_MASK,16#4).
%-define(rc_NAME_OPT_MACRO_MASK,16#8).
%-define(rc_NAME_OPT_COMPLEX_MASK,16#10).
%-define(rc_NAME_OPT_BUILTIN_MASK,16#20).
%-define(rc_NAME_OPT_FUNCGROUP_MASK,16#0FC0).
%-define(rc_NAME_OPT_BINARY_MASK,16#1000).
