%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2008, hypernumbers.com
%%% @doc       post-processes tables read from Excel files
%%%
%%% @end
%%% Created :  20 Nov 2008 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(excel_post_process).

-export([post_process_tables/1]).

-define(read,excel_util:read).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                     %%%
%%% The excel file is processed on record at a time into a set of ets   %%%
%%% tables. Some of the entries in these records cannot be properly     %%%
%%% handled at read time because they depend on as yet unread records.  %%%
%%% These tables are then post-processed by these functions             %%%
%%%                                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
post_process_tables(Tables) ->
%% Excel has a number of built in formats
    {ok,ok}=add_built_in_formats(Tables),
    % filefilters:dump(Tables),
    type_formats(Tables),
    fix_up_externalrefs(Tables),
    fix_up_names(Tables),
    fix_up_cells(Tables),
    convert_dates(Tables),
    create_array_formulae(Tables),
    % filefilters:dump(Tables),
    ok.

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
    {value,{names,Tid1}}=lists:keysearch(names,1,Tables),
    
    {value,{fixedupnames,Tid2}}=lists:keysearch(fixedupnames,1,Tables),
    Fun=fun(X,_Acc)->
                {Index,[{sheetindex,SheetIndex},{type,Scope},
                        {name,Name},{rpn,RPNTokens}]}=X,
                Page=case Scope of
                         local  -> [{_I,[_T,List]}]=?read(Tables,externalrefs,0),
                                   RawName=lists:nth(SheetIndex,List),
                                   excel_util:esc_tab_name(RawName);
                         global -> ""
                     end,
                case excel_rev_comp:reverse_compile(Index,RPNTokens,[],
                                                    Tables) of
                    {ok,dont_process} -> exit("I dont understand!");
                    NameVal           -> io:format("in excel_post_process:fix_up_names "++
                                                   "Scope is ~p Name is ~p "++
                                                   "NameVal is ~p~n",
                                                   [Scope,Name,NameVal]),
                                         ets:insert(Tid2,[{Index,[{scope,Scope},
                                                                  {page,Page},
                                                                  {name,"@"++Name},
                                                                  {value,NameVal}]}])
                end
        end,
    ets:foldl(Fun,[],Tid1).

%% this function adds the data type to format information
%% eg is this format for text, numbers or dates or general
type_formats(Tables) ->
    {value,{formats,Tid}}=lists:keysearch(formats,1,Tables),
    Fun = fun(X,_Residuum) ->
                  {Index,[{type,_Type1},Category,{format,Format}]}=X,
                  Return=format:get_src(Format),
                  case Return of
                      {erlang,{Type2,_Output}} ->
                          ok;
                      {error,error_in_format} ->
                          io:format("in excel:type_formats bug in "++
                                    "format parser for format: ~p~n",
                                    [Format]),
                          Type2='needed to make the case safe',
                          exit("number format parser bug!")
                  end,
                  NewFormat={format,Format},
                  ets:insert(Tid,[{Index,[{type,Type2},Category,{NewFormat}]}])
          end,
    ets:foldl(Fun,[],Tid).

%% reverse compile the basic cells
%% this fun reverse compiles all the tokens in the table 'cell_tokens' and
%% then injects them into the table 'cells' which is prepopulated with
%% all the non-formulae cell values
fix_up_cells(Tables)->
    {value,{cell_tokens,Cell_TokensId}}=lists:keysearch(cell_tokens,1,Tables),
    {value,{cell,CellId}}              =lists:keysearch(cell,1,Tables),
    Fun=fun(X,_Acc)->
                {Index,[XF,{tokens,Tokens},{tokenarrays,TokenArray}]}=X,
                case excel_rev_comp:reverse_compile(Index,Tokens,TokenArray,
                                                    Tables) of
                    {ok,dont_process} -> {ok,ok};
                    Formula           ->
                        io:format("in fix_up_cells Formula is ~p~n",[Formula]),
                        ets:insert(CellId,[{Index,[XF,{formula,Formula}]}])
                end
        end,
    ets:foldl(Fun,[],Cell_TokensId).

%% This function merges the contents of the ets table 'sheetnames' into
%% 'externalrefs'
fix_up_externalrefs(Tables)->
    {value,{externalrefs,ExternalRefs}}=lists:keysearch(externalrefs,1,Tables),
    Fun=fun(X,Y) ->
                case X of
                    {Index,[{this_file,placeholder},[]]} -> [Index|Y];
                    _                                     -> Y
                end
        end,
    case ets:info(ExternalRefs,size) of
        0 -> ok;
        _ -> [Index]=ets:foldl(Fun,[],ExternalRefs),
             SheetNames=lists:reverse(get_sheetnames(Tables)),
             excel_util:write(Tables,externalrefs,[Index,{this_file,expanded},
                                                   SheetNames])
    end.

convert_dates(Tables)->
    {value,{cell,Tid1}}   =lists:keysearch(cell,1,Tables),
    {value,{xf,Tid2}}     =lists:keysearch(xf,1,Tables),
    {value,{formats,Tid3}}=lists:keysearch(formats,1,Tables),
%%Fun looks up the values from the cells table and the format table
%% (via XF) if the format is of type 'date' it switches the type
%% of the number from 'number' to 'date'
    Fun=fun(X,_Acc) ->
                {Index,[{xf_index,XF},Body]}=X,
                XFVal=ets:lookup(Tid2,{index,XF}),
                [{_,[{format_index,Idx},_,_]}]=XFVal,
                Format=ets:lookup(Tid3,{format_index,Idx}),
                [{_,[{type,Type},_,_]}]=Format,
                Body2=case Type of
                          date   -> convert_dates2(Body,Tables);
                          _      -> Body
                      end,
                %{Index,Body2},
                ets:insert(Tid1,[{Index,[{xf_format,XF},Body2]}])
        end,
    ets:foldl(Fun,[],Tid1).

convert_dates2({value,number,Number},Tables) ->
    [{_,[{value,DateMode}]}]=excel_util:read(Tables,misc,datemode),
    Date=case DateMode of
             "Windows"   -> muin_date:excel_win_to_gregorian(Number);
             "Macintosh" -> muin_date:excel_mac_to_gregorian(Number)
         end,
    {value,date,Date};
convert_dates2(Body,_Tables) -> Body.

create_array_formulae(Tables)->
    {value,{sh_arr_formula,ShArrId} }  = lists:keysearch(sh_arr_formula,1,Tables),
    {value,{array_formulae,ArrFormId}} = lists:keysearch(array_formulae,1,Tables),
    Fun = fun(X,_Acc) ->
                  {Id2,[{type,Type},{tokens,Tokens},{tokenarrays,TkArr}]} = X,
                  case Type of
                      shared -> {ok, ok};
                      array  ->
                          Formula=excel_rev_comp:reverse_compile(Id2,Tokens,
                                                                 TkArr,Tables),
                          ets:insert(ArrFormId,[{Id2,{formula,Formula}}])
                  end
          end,
    ets:foldl(Fun,[],ShArrId).

get_sheetnames(Tables)->
    Fun = fun({_Index,[{name,SheetName}]},Y) ->
                  [SheetName|Y]
          end,
    {value,{sheetnames,SheetNames}}=lists:keysearch(sheetnames,1,Tables),
    ets:foldl(Fun,[],SheetNames).


%% Excel has a number of built in number formats
%% These are described in Section 5.49 of excelfileformatV1-41.pdf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% BUG     - this routine just sticks in a Dollar sign - but it should be the %%
%%           appropriate currency symbol of the country that Excel is using!  %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_built_in_formats(Tables) ->
    L=[[{format_index,0},{type,general},{category,general},
        {format,"General"}],
       [{format_index,1},{type,number},{category,decimal},
        {format,"0"}],
       [{format_index,2},{type,number},{category,decimal},
        {format,"0.00"}],
       [{format_index,3},{type,number},{category,decimal},
        {format,"#,##0"}],
       [{format_index,4},{type,number},{category,decimal},
        {format,"#,##0.00"}],
       [{format_index,5},{type,number},{category,currency},
        {format,"\"$\"##0_);"++
     "(\"$\"#,##0)"}],
       [{format_index,6},{type,number},{category,currency},
        {format,"\"$\"##0_);"++
     "[Red](\"$\"#,##0)"}],
       [{format_index,7},{type,number},{category,currency},
        {format,"\"$\"##0.00_);"++
     "(\"$\"#,##0.00)"}],
       [{format_index,8},{type,number},{category,currency},
        {format,"\"$\"##0.00_);"++
     "[Red](\"$\"#,##0.00)"}],
       [{format_index,9},{type,number},{category,percent},
        {format,"0%"}],
       [{format_index,10},{type,number},{category,percent},
        {format,"0.00%"}],
       [{format_index,11},{type,number},{category,scientific},
        {format,"0.00e+00"}],
       [{format_index,12},{type,number},{category,fraction},
        {format,"#?/?"}],
       [{format_index,13},{type,number},{category,fraction},
        {format,"#??/??"}],
       [{format_index,14},{type,date},{category,date},
        {format,"M/D/YY"}],
       [{format_index,15},{type,date},{category,date},
        {format,"D-MMM-YY"}],
       [{format_index,16},{type,date},{category,date},
        {format,"D-MMM"}],
       [{format_index,17},{type,date},{category,date},
        {format,"MMM-YY"}],
       [{format_index,18},{type,date},{category,time},
        {format,"h:mm AM/PM"}],
       [{format_index,19},{type,date},{category,time},
        {format,"h:mm:ss AM/PM"}],
       [{format_index,20},{type,date},{category,time},
        {format,"h:mm"}],
       [{format_index,21},{type,date},{category,time},
        {format,"h:mm:ss"}],
       [{format_index,22},{type,date},{category,datetime},
        {format,"M/D/YY h:mm"}],
       [{format_index,37},{type,number},{category,account},
        {format,"_(#,##0_);(#,###0)"}],
       [{format_index,38},{type,number},{category,account},
        {format,"_(#,##0_);"++
         "[Red](#,###0)"}],
       [{format_index,39},{type,number},{category,account},
        {format,"_(#,##0.00_);"++
         "(#,###0.00)"}],
       [{format_index,40},{type,number},{category,account},
        {format,"_(#,##0.00_);"++
         "[Red](#,###0.00)"}],
%% these formats are different in the Microsoft Book
%% Microsoft Excel97 Developers Kit p427!
%% index 41 and 42 are transposed as are index 43 and 44!
       [{format_index,41},{type,number},{category,currency},
        {format,"_(\"$\"*#,##0_);"++
     "_(\"$\"*(#,##0);"++
     "_(\$\"*\"-\"_);"++
        "_(@_)"}],
    [{format_index,42},{type,number},{category,currency},
     {format,"_(*#,##0_);"++
      "_(*(#,##0);"++
      "_(*\"-\"_);"++
      "_(@_)"}],
    [{format_index,43},{type,number},{category,currency},
     {format,"_(\"$\"*#,##0.00_);"++
     "_(\"$\"*(#,##0.00);"++
     "_(\$\"*\"-\"??_);"++
     "_(@_)"}],
[{format_index,44},{type,number},{category,currency},
 {format,"_(*#,##0.00_);"++
  "_(*(#,##0.00);"++
  "_(*\"-\"??_);"++
  "_(@_)"}],
[{format_index,45},{type,date},{category,time},
 {format,"mm:ss"}],
[{format_index,46},{type,date},{category,time},
 {format,"[h]:mm:ss"}],
[{format_index,47},{type,date},{category,time},
 {format,"mm:ss.0"}],
[{format_index,48},{type,number},{category,scientific},
 {format,"##0.0E+0"}],
[{format_index,49},{type,string},{category,text},
 {format,"@"}]],
Fun = fun(Record,[]) -> excel_util:write(Tables,formats,Record), [] end,
[]=lists:foldl(Fun,[],L),
io:format("in excel:add_built_in_formats All built in formats added!~n"),
{ok,ok}.
