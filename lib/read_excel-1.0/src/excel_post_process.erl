%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2008, hypernumbers.com
%%% @doc       This module post-processes tables read from Excel 
%%%            files. Excel functions are stored in a Reverse-Polish
%%%            Notation (RPN) token stream and the primary task of
%%%            this module is to revese compile that  which it does 
%%%            by calling {@link excel_rev_comp}.
%%%
%%% @end
%%% Created :  20 Nov 2008 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(excel_post_process).

-export([post_process_tables/1]).

-include("spriki.hrl").

-define(read,excel_util:read).
-define(write,excel_util:write).
-define(k,lists:keysearch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% The excel file is processed on record at a time into a set of ets   %%%
%%% tables. Some of the entries in these records cannot be properly     %%%
%%% handled at read time because they depend on as yet unread records.  %%%
%%% These tables are then post-processed by these functions             %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
post_process_tables(Tables) ->
    % Excel has a number of built in formats
    {ok,ok}=add_built_in_formats(Tables),
    % excel_util:dump(Tables),
    type_formats(Tables),
    fix_up_externalrefs(Tables),
    fix_up_names(Tables),
    fix_up_cells(Tables),
    convert_dates(Tables),
    fix_up_formats(Tables),
    create_array_formulae(Tables),
    fix_up_sheetnames(Tables),
    % excel_util:dump(Tables),
    ok.

%% This function takes the raw format data and turns into the apppriate format
%% * resolves which format record pertains to each cell
%% * resolves what CSS styles apply to which cell
%% 
%% It does this by:
%% * first resolving the XF record and its parent style
%% * then resolving the row formats and applying them to unformatted cells
%% * then resolving the column formats and applying them to unformatted cells
%% 
%% See Section 4.6.2 of excelfileformatV1-42.pdf
fix_up_formats(Tables) ->
    % this function applies all the format information for each cell
    % both those with formulae and the tmp_blanks
    {value,{cell,       CellId}    }   = ?k(cell    ,   1, Tables),
    {value,{tmp_blanks, Tmp_BlanksId}} = ?k(tmp_blanks, 1, Tables),
    % now we step through the formats and later we will use hn_main:set_attribute 
    % to set them
    Fun1 = fun(X, _Acc)->
                   {CellRef, [{xf_index, XFIndex}, _]} = X,
                   {ok, ok} = set_formats(CellRef, XFIndex, Tables)
           end,
    ets:foldl(Fun1, [], CellId),
    Fun2 = fun(X, _Acc)->
                   {CellRef, [{xf_index, XFIndex}]} = X,
                   {ok, ok} = set_formats(CellRef, XFIndex, Tables)
           end,
    ets:foldl(Fun2, [], Tmp_BlanksId).

%% this function deals with all the names defined in the names table
%% these names are used in the reverse compile of the actual cell records
fix_up_names(Tables) ->
    % Excel names have two different scopes
    % * global <-- pertains to a workbook
    % * local  <-- pertains to a worksheet
    % 
    % There are some interesting glitches
    % * the same name can be on different worksheets
    % * the same name can be a global name and one or more local names
    % * names are *NOT* case sensitive
    % * names can be called from external workbooks
    % * a name on a page can point to a cell on a different page
    % 
    % There are some subtleties that will be hard to fix up...
    % 
    % ****Quote From Excel Help ********
    % You can even define the same name, GrossProfit, for the global
    % workbook level, but again the scope is unique. In this case, 
    % however, there can be a name conflict. To resolve this 
    % conflict, by default Excel uses the name that is defined 
    % for the worksheet because the local worksheet level takes 
    % precedence over the global workbook level. If you want 
    % to override the precedence and you want to use the workbook 
    % name, you can disambiguate the name by prefixing the workbook 
    % name as the following example shows:
    % 
    % WorkbookFile!GrossProfit
    % 
    % You can override the local worksheet level for all worksheets
    % in the workbook, with the exception of the first worksheet, 
    % which always uses the local name if there is a name conflict
    % and cannot be overridden.
    % 
    % *********End Quote************
    {value,{tmp_names, Tid1}} = ?k(tmp_names, 1, Tables),
    {value,{names,     Tid2}} = ?k(names,     1, Tables),
    Fun=fun(X, _Acc)->
                {Index,[{extbook, _EXBIdx}, {sheetindex, SheetIndex}, {type, Scope},
                        {name, Name}, {rpn, RPNTokens}]} = X,
                Page = case Scope of
                           local  -> [{_I, [_T, List]}]=?read(Tables, tmp_externalbook, 0),
                                     RawName = lists:nth(SheetIndex, List),
                                     excel_util:esc_tab_name(RawName);
                           global -> ""
                       end,
                case excel_rev_comp:reverse_compile(Index, RPNTokens, [],
                                                    Tables) of
                    {ok,dont_process} -> exit("I dont understand!");
                    NameVal           -> ets:insert(Tid2, [{Index,[{scope, Scope},
                                                                   {page, Page},
                                                                   {name, "@"++Name},
                                                                   {value, NameVal}]}])
                end
        end,
    ets:foldl(Fun, [], Tid1).

%% this function adds the data type to format information
%% eg is this format for text, numbers or dates or general
%% the application of a particular format to a particular cell happens latter
type_formats(Tables) ->
    {value,{tmp_formats, Tid}}=?k(tmp_formats, 1, Tables),
    Fun = fun(X, _Residuum) ->
                  {Idx, [{type, _Type1}, Category, {format, Fmt}]} = X,
                  Return=format:get_src(Fmt),
                  {Type2, NewFmt}
                      = case Return of
                            {erlang, {Type3, _Output}} -> {Type3, Fmt};
                            {error,error_in_format} ->
                                Warn = "Warning: The Excel Built-In format "++ Fmt ++
                                    " doesn't compile",
                                excel_util:append(Tables, warnings, Warn),
                                {text, "general"}
                        end,
                  ets:insert(Tid, [{Idx, [{type, Type2}, Category, NewFmt]}])
          end,
    ets:foldl(Fun, [], Tid).

%% reverse compile the basic cells
%% this fun reverse compiles all the tokens in the table 'tmp_cell' and
%% then injects them into the table 'cells' which is prepopulated with
%% all the non-formulae cell values
fix_up_cells(Tables)->
    {value, {tmp_cell, Tmp_CellId}} = ?k(tmp_cell, 1, Tables),
    {value, {cell,     CellId}}     = ?k(cell,     1, Tables),
    Fun=fun(X, _Acc)->
                {Index, [XF, {tokens, Tokens}, {tokenarrays, TokenArray}]} = X,
                case excel_rev_comp:reverse_compile(Index, Tokens, TokenArray,
                                                    Tables) of
                    {ok,dont_process} ->
                        {ok, ok};
                    Formula           ->
                        ets:insert(CellId, [{Index, [XF, {formula, Formula}]}])
                end
        end,
    ets:foldl(Fun, [], Tmp_CellId).

%% This function merges the contents of the ets table 'sheetnames' into
%% 'tmp_externalbook'
fix_up_externalrefs(Tables)->
    {value, {tmp_externalbook, ExternalRefs}}=?k(tmp_externalbook, 1, Tables),
    Fun=fun(X, Y) ->
                case X of
                    {Index, [{this_file, placeholder},[]]} -> [Index | Y];
                    _                                     -> Y
                end
        end,
    case ets:info(ExternalRefs, size) of
        0 -> ok;
        _ -> [Index] = ets:foldl(Fun, [], ExternalRefs),
             SheetNames = lists:reverse(get_sheetnames(Tables)),
             ?write(Tables, tmp_externalbook, [Index, {this_file,expanded},
                                          SheetNames])
    end.

%% This function merges the escaped contents of the ets table 'tmp_sheetnames' into
%% 'sheetnames'
fix_up_sheetnames(Tables) ->
    Sheetnames = lists:reverse(get_sheetnames(Tables)),
    Fun = fun(X, Idx) ->
                  ?write(Tables, sheetnames, [Idx, excel_util:esc_tab_name(X)]),
                  Idx + 1
          end,
    lists:foldl(Fun, 1, Sheetnames).

convert_dates(Tables)->
    {value,{cell, Tid1}}       = ?k(cell,        1, Tables),
    {value,{tmp_xf, Tid2}}     = ?k(tmp_xf,      1, Tables),
    {value,{tmp_formats,Tid3}} = ?k(tmp_formats, 1, Tables),
    % Fun looks up the values from the cells table and the format table
    % (via XF) if the format is of type 'date' it switches the type
    % of the number from 'number' to 'date'
    Fun=fun(X, _Acc) ->
                {Index, [{xf_index, XF}, Body]} = X,
                XFVal = ets:lookup(Tid2,{index ,XF}),
                [{_, [{format_index, Idx}, _, _, _, _, _, _, _]}] = XFVal,
                Format = ets:lookup(Tid3, {format_index, Idx}),
                [{_, [{type, Type}, _, _]}] = Format,
                Body2 = case Type of
                            date   -> convert_dates2(Body, Tables);
                            _      -> Body
                        end,
                ets:insert(Tid1, [{Index, [{xf_index, XF}, Body2]}])
        end,
    ets:foldl(Fun, [], Tid1).

convert_dates2({value, number, Number}, Tables) ->
    [{_, [{value, DateMode}]}] = excel_util:read(Tables, misc, datemode),
    Date = case DateMode of
               "Windows"   -> muin_date:excel_win_to_gregorian(Number);
               "Macintosh" -> muin_date:excel_mac_to_gregorian(Number)
           end,
    {value, date, Date};
convert_dates2(Body, _Tables) -> Body.

create_array_formulae(Tables)->
    {value, {tmp_sh_arr_fml, ShArrId} }  = ?k(tmp_sh_arr_fml, 1, Tables),
    {value, {array_formulae, ArrFormId}} = ?k(array_formulae, 1, Tables),
    Fun = fun(X, _Acc) ->
                  {Id2, [{type, Type}, {tokens, Tokens}, {tokenarrays, TkArr}]} = X,
                  case Type of
                      shared -> {ok, ok};
                      array  ->
                          Formula=excel_rev_comp:reverse_compile(Id2, Tokens,
                                                                 TkArr, Tables),
                          ets:insert(ArrFormId, [{Id2, {formula, Formula}}])
                  end
          end,
    ets:foldl(Fun, [], ShArrId).

get_sheetnames(Tables)->
    Fun = fun({_Index, [{name, SheetName}]}, Y) ->
                  [SheetName | Y]
          end,
    {value, {tmp_sheetnames, SheetNames}} = ?k(tmp_sheetnames, 1, Tables),
    ets:foldl(Fun, [], SheetNames).

get_fonts(FontIdx, Tables) ->
    % As Section 5.45 of the excelfileformatV1-42.pdf points out there is no
    % font 4, so if FontIdx > 4 decrement it by one
    FontIdx2 = if
                   FontIdx > 4 -> FontIdx -1;
                   true        -> FontIdx
               end,
    {value,{tmp_fonts, FontsId}}     = ?k(tmp_fonts,   1, Tables),
    {value,{tmp_colours, ColoursId}} = ?k(tmp_colours, 1, Tables),
    [{_Index, [ColourIdx, {css, CSS}]}]  = ets:lookup(FontsId, {index, FontIdx2}),
    % lookup the colour
    [{_, [{colour,Col}]}] = ets:lookup(ColoursId, ColourIdx),
    lists:merge([[{'color', [Col]}],CSS]).

get_colours(ColourList, Tables) -> 
    get_colours1(ColourList, Tables, []).

get_colours1([], _Tables, Acc) -> Acc;
get_colours1([{Where, ColourIndex} | T], Tables, Acc) ->
    {value,{tmp_colours, ColoursId}} = ?k(tmp_colours, 1, Tables),
    [{_, [{colour, Col}]}] = ets:lookup(ColoursId, {colour_index, ColourIndex}),
    CSS = "border-" ++ atom_to_list(Where) ++ "-color",
    CSS2 = list_to_atom(CSS),
    get_colours1(T, Tables, [{CSS2, Col} | Acc]).

get_css(CSSList, TypesList) -> get_css(CSSList, TypesList, []).

get_css(_CSSList, [], Acc)    -> Acc;
get_css(CSSList, [H|T], Acc) ->
    Return = ?k(H, 1, CSSList),
    case Return of
        false         -> get_css(CSSList, T, Acc);
        {value, Attr} -> get_css(CSSList, T, [Attr | Acc])
    end.

set_formats(CellRef, XFIndex, Tables) ->

    {value, {tmp_xf, XFId}}           = ?k(tmp_xf,      1, Tables),
    {value, {tmp_formats, FormatsId}} = ?k(tmp_formats, 1, Tables),
    {value, {tmp_colours, ColoursId}} = ?k(tmp_colours, 1, Tables),
    % filefilters:dump(Tables),
    [{_XF, XFList}] = ets:lookup(XFId, {index, XFIndex}),

    % Formats in Excel are stored in a heirarchical arrangement
    % We need to know the parent (or style) record for this record)

    {value, {type, Type}} = ?k(type, 1, XFList),
    % It appears that sometimes a cell format can reference a style record directly!
    % By definition in this case the cell format has no parent

    PXFList = case Type of
                  cell  ->
                      % Look up the parent XF record
                      {value, {parent_index, XFParentIndex}} =
                          ?k(parent_index, 1, XFList),
                      [{_PXF, PXFList2}] = ets:lookup(XFId, {index, XFParentIndex}),
                      % the parent should be a style - check that it is so
                      {value, {type, PType}} = ?k(type, 1, PXFList2),
                      case PType of
                          cell  -> exit("I don't think this should ever read a "++
                                        "cell XF record");
                          style -> ok
                      end,
                      PXFList2;
                  style -> io:format("I don't think this should ever read "++
                                     "a style XF record~n"),
                           % Just return the XFList
                           % ie kid on that the format is its own parent
                           XFList
              end,

    % 
    % What attributes apply are determined by the XF attributes so get them first
    % 
    {value, {attributes, AttrList}}  = ?k(attributes, 1, XFList),
    {value, {attributes, PAttrList}} = ?k(attributes, 1, PXFList),

    % get the state of play in CSS
    {value, {css, CSSList}}  = ?k(css, 1, XFList),
    {value, {css, PCSSList}} = ?k(css, 1, PXFList),

    % Attribute 1 NUMBER
    % get the appropriate FORMAT_INDEX
    {value, {number, NumAttr}}  = ?k(number, 1, AttrList), 
    {value, {number, PNumAttr}} = ?k(number, 1, PAttrList),
    
    {value, {format_index, FormatIdx}} =
        case {NumAttr, PNumAttr} of
            {use_this, _}        -> ?k(format_index, 1, XFList);
            {use_parent, valid}  -> ?k(format_index, 1, PXFList);
            {valid, valid}       -> ?k(format_index, 1, PXFList); % parent is child
            {use_parent, ignore} -> ?k(format_index, 1, XFList)
        end,
    Return = ets:lookup(FormatsId, {format_index, FormatIdx}),
    [{_Idx, [_Type, _Category, Format]}] = Return,
    
    % Attribute 2 FONT
    % get the appropriate FONT index
    {value, {font, FontAttr}}  = ?k(font, 1, AttrList), 
    {value, {font, PFontAttr}} = ?k(font, 1, PAttrList),

    {value, {font_index, FontIdx}} =
        case {FontAttr, PFontAttr} of
            {use_this, _}        -> ?k(font_index, 1, XFList);
            {use_parent, valid}  -> ?k(font_index, 1, PXFList);
            % parent is child
            {valid, valid}       -> ?k(font_index, 1, PXFList);
            {use_parent, ignore} -> ?k(font_index, 1, XFList);
            {ignore, ignore}     -> ?k(font_index, 1, XFList)
        end,
    % now look up the font
    FontCSS = get_fonts(FontIdx, Tables),
    % Attribute 3 TEXT
    % get the following CSS elements
    % * 'vertical-align'
    % * 'text-align'
    {value, {text, TextAttr}}  = ?k(text, 1, AttrList), 
    {value, {text, PTextAttr}} = ?k(text, 1, PAttrList), 
    TextCSS =
        case {TextAttr, PTextAttr} of
            {use_this, _}        -> get_css(CSSList, ['vertical-align',
                                                      'text-align']);
            {use_parent, valid}  -> get_css(PCSSList,['vertical-align',
                                                      'text-align']);
            {valid, valid}       -> get_css(PCSSList,['vertical-align',
                                                      'text-align']); % parent is child
            {use_parent, ignore} -> get_css(CSSList, ['vertical-align',
                                                      'text-align'])
        end,
    % Attribute 4 BORDER
    % get the following CSS elements
    % * 'border-left      '
    % * 'border-right'
    % * 'border-top'
    % * 'border-bottom'
    % 
    % and then looks up the colour indices to generate the following:
    % * 'border-colour'
    %
    % First get the attributes
    {value, {border, BorderAttr}}  = ?k(border, 1, AttrList), 
    {value, {border, PBorderAttr}} = ?k(border, 1, PAttrList),

    % Now get the colour indices
    {value, {border_colour, ColoursList}}  = ?k(border_colour, 1, XFList),
    {value, {border_colour, PColoursList}} = ?k(border_colour, 1, PXFList),

    BorderCSS =
        case {BorderAttr, PBorderAttr} of
            {use_this, _}        ->
                S = get_css(CSSList, ['border-left', 'border-right',
                                      'border-top',  'border-bottom',
                                      'border-left-style', 'border-right-style',
                                      'border-top-style',  'border-bottom-style']),
                C = get_colours(ColoursList,Tables),
                lists:merge([S, C]);
            {use_parent, valid}  ->
                S = get_css(PCSSList, ['border-left', 'border-right',
                                       'border-top',  'border-bottom',
                                       'border-left-style', 'border-right-style',
                                       'border-top-style',  'border-bottom-style']),
                C = get_colours(PColoursList,Tables),
                lists:merge([S, C]);
            {valid, valid}       -> % parent is child
                S = get_css(PCSSList, ['border-left', 'border-right',
                                       'border-top',  'border-bottom',
                                       'border-left-style', 'border-right-style',
                                       'border-top-style',  'border-bottom-style']),
                C = get_colours(PColoursList,Tables),
                lists:merge([S, C]);
            {use_parent, ignore} ->
                S = get_css(CSSList, ['border-left', 'border-right',
                                      'border-top',  'border-bottom',
                                      'border-left-style', 'border-right-style',
                                      'border-top-style',  'border-bottom-style']),
                C = get_colours(ColoursList,Tables),
                lists:merge([S, C])
        end,

    % Attribute 5 BACKGROUND
    % uses the PatternBackgroundColourIndex to generate the background CSS
    {value, {background, BackgroundAttr}}  = ?k(background, 1, AttrList), 
    {value, {background, PBackgroundAttr}} = ?k(background, 1, PAttrList),

    % Now get the colour indices
    {value, {bg_colour, [{background, BGColour}]}}  = ?k(bg_colour, 1, XFList),
    {value, {bg_colour, [{background, PBGColour}]}} = ?k(bg_colour, 1, PXFList),

    BackgroundCSS =
        case {BackgroundAttr, PBackgroundAttr} of
            {use_this, _}        ->
                C2  = ets:lookup(ColoursId,{colour_index, BGColour}),
                [{_, [{colour,Col}]}] = C2,
                {'background-color', [Col]};
            {use_parent, valid}  ->
                C2 = ets:lookup(ColoursId,{colour_index, PBGColour}),
                [{_, [{colour,Col}]}] = C2,
                {'background-color', [Col]};
            {valid, valid}       -> % parent is child
                C2 = ets:lookup(ColoursId,{colour_index, PBGColour}),
                [{_, [{colour,Col}]}] = C2,
                {'background-color', [Col]};
            {use_parent, ignore} ->
                C2  = ets:lookup(ColoursId,{colour_index, BGColour}),
                [{_, [{colour,Col}]}] = C2,
                {'background-color', [Col]}
        end,

    % Attribute 6 PROTECTION
    % at the moment we don't use protection

    % Write all the formatting info out
    case Format of
        [] -> ok;
        _  -> ?write(Tables, formats, [CellRef, Format])
    end,
    List1 = case BackgroundCSS of
                [] -> ok;
                _  -> [BackgroundCSS]
            end,
    List2 = case FontCSS of
                [] -> List1;
                _  -> lists:append([List1, FontCSS])
    end,
    List3 = case TextCSS of
                [] -> List2;
                _  -> lists:append([List2, TextCSS])
    end,
    List4 = case BorderCSS of
                [] -> ok;
                _  -> lists:append([List3, BorderCSS])
    end,
    StyleRecord = ms_util:make_record(magic_style, List4),
    ?write(Tables, css, [CellRef, StyleRecord]),
    {ok, ok}.

%% Excel has a number of built in number formats
%% These are described in Section 5.49 of excelfileformatV1-41.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% BUG     - this routine just sticks in a Dollar sign - but it should be the %%
%%           appropriate currency symbol of the country that Excel is using!  %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_built_in_formats(Tables) ->
    L = [[{format_index,0}, {type,general}, {category,general},  {format,"General"}],
         [{format_index,1}, {type,number},  {category,decimal},  {format,"0"}],
         [{format_index,2}, {type,number},  {category,decimal},  {format,"0.00"}],
         [{format_index,3}, {type,number},  {category,decimal},  {format,"#,##0"}],
         [{format_index,4}, {type,number},  {category,decimal},  {format,"#,##0.00"}],
         [{format_index,5}, {type,number},  {category,currency},
          {format,"\"$\"##0_);(\"$\"#,##0)"}], 
          [{format_index,6}, {type,number}, {category,currency},
           {format,"\"$\"##0_);[Red](\"$\"#,##0)"}],
           [{format_index,7}, {type,number}, {category,currency},
            {format,"\"$\"##0.00_);(\"$\"#,##0.00)"}],
       [{format_index,8}, {type,number}, {category,currency},
        {format,"\"$\"##0.00_);[Red](\"$\"#,##0.00)"}],
       [{format_index,9},   {type,number}, {category,percent},    {format,"0%"}],
        [{format_index,10}, {type,number}, {category,percent},    {format,"0.00%"}],
        [{format_index,11}, {type,number}, {category,scientific}, {format,"0.00e+00"}],
        [{format_index,12}, {type,number}, {category,fraction},   {format,"#?/?"}],
        [{format_index,13}, {type,number}, {category,fraction},   {format,"#??/??"}],
        [{format_index,14}, {type,date},   {category,date},       {format,"M/D/YY"}],
        [{format_index,15}, {type,date},   {category,date},       {format,"D-MMM-YY"}],
        [{format_index,16}, {type,date},   {category,date},       {format,"D-MMM"}],
        [{format_index,17}, {type,date},   {category,date},       {format,"MMM-YY"}],
        [{format_index,18}, {type,date},   {category,time},       {format,"h:mm AM/PM"}],
        [{format_index,19}, {type,date},   {category,time},       {format,"h:mm:ss AM/PM"}],
        [{format_index,20}, {type,date},   {category,time},       {format,"h:mm"}],
        [{format_index,21}, {type,date},   {category,time},       {format,"h:mm:ss"}],
        [{format_index,22}, {type,date},   {category,datetime},   {format,"M/D/YY h:mm"}],
        [{format_index,37}, {type,number}, {category,account},    {format,"_(#,##0_);(#,###0)"}],
        [{format_index,38}, {type,number}, {category,account},
         {format,"_(#,##0_);[Red](#,###0)"}],
        [{format_index,39}, {type,number}, {category,account},
         {format,"_(#,##0.00_);(#,###0.00)"}],
        [{format_index,40}, {type,number}, {category,account},
         {format,"_(#,##0.00_);[Red](#,###0.00)"}],
% these formats are different in the Microsoft Book
% Microsoft Excel97 Developers Kit p427!
% index 41 and 42 are transposed as are index 43 and 44!
        [{format_index,41}, {type,number}, {category,currency},
         {format,"_(\"$\"*#,##0_);_(\"$\"*(#,##0);_(\$\"*\"-\"_);_(@_)"}],
         [{format_index,42}, {type,number}, {category,currency},
          {format,"_(*#,##0_);_(*(#,##0);_(*\"-\"_);_(@_)"}],
         [{format_index,43}, {type,number}, {category,currency},
          {format,"_(\"$\"*#,##0.00_);_(\"$\"*(#,##0.00);_(\$\"*\"-\"??_);_(@_)"}],
          [{format_index,44}, {type,number}, {category,currency},
           {format,"_(*#,##0.00_);_(*(#,##0.00);_(*\"-\"??_);_(@_)"}],
          [{format_index,45}, {type,date},   {category,time},       {format,"mm:ss"}],
          [{format_index,46}, {type,date},   {category,time},       {format,"[h]:mm:ss"}],
          [{format_index,47}, {type,date},   {category,time},       {format,"mm:ss.0"}],
          [{format_index,48}, {type,number}, {category,scientific}, {format,"##0.0E+0"}],
          [{format_index,49}, {type,string}, {category,text},       {format,"@"}]],
         Fun = fun(Record,[]) -> ?write(Tables,tmp_formats,Record), [] end,
         []=lists:foldl(Fun,[],L),
         io:format("in excel:add_built_in_formats All built in formats added!~n"),
         {ok,ok}.

