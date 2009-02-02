%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module provides the data access api.
%%%            Each function in this should call functions from
%%%            hn_db_wu.erl which provides work units and it
%%%            should wrap them in an mnesia transaction.
%%%            
%%%            mnesia MUST NOT be called from any function in
%%%            this module.
%%%
%%% @end
%%% Created : 24 Jan 2009 by gordon@hypernumbers.com
%%%-------------------------------------------------------------------
-module(hn_db_api).

-include("spriki.hrl").

-define(mn_ac,mnesia:activity).
-define(mn_sl,mnesia:select).
%-define(mn_dl,mnesia:delete).
%-define(mn_dlo,mnesia:delete_object).

%% record_info isnt available at runtime
-define(create(Name,Type,Storage),
        fun() ->
                Attr = [{attributes, record_info(fields, Name)},
                        {type,Type},{Storage, [node()]}],
                {atomic,ok} = mnesia:create_table(Name, Attr)
        end()).

-export([
         write_attributes/2,
         write_last/1,
         % write_permission/2,
         % write_style/2,
         % read_attribute/1,
         % read_rawvalue/1,
         % read_value/1,
         read/1,
         % read_page/1,
         % read_style/1,
         % read_permissions/1,
         % updates
         % update_style/2,
         % recalculate_cell/1,
         % recalculate_range/1,
         % reformat_cell/1,
         drag_n_drop/2,
         copy_n_paste/2,
         cut_n_paste/2,
         % copy_page/2,
         insert/1,
         insert/2,
         delete/1,
         delete/2
         % delete_permission/1,
         % delete_style/1
        ]).

%%% Debugging interface
-export([copy_DEBUG/0,
         delete_cell_contents_DEBUG/1,
         clear_cells_DEBUG/1,
         insert_delete_DEBUG/0,
         delete_DEBUG/2,
         insert_DEBUG/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% API Interfaces                                                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec write_attributes(RefX :: #refX{}, List) -> {ok, ok} 
%% List = [{Key, Value}]
%% Key = atom()
%% Value = term()
%% @doc writes out all the attributes in the list to the RefX which can be
%% one of:
%% <ul>
%% <li>a cell</li>
%% <li>a range</li>
%% </ul>
write_attributes(RefX, List) when is_record(RefX, refX), is_list(List) ->
    Fun = fun() ->
                  [hn_db_wu:write_attr(RefX, X) || X <- List]
          end,
    mnesia:activity(Fun).

%% @spec write_last(List) -> {ok, ok}
%% List = [{#refX{}, Val}]
%% Val = [list() | float() | integer()]
%% @doc takes a list of #refX{}'s all of which must be either a:
%% <ul>
%% <li>column</li>
%% <li>row</li>
%% </ul>
%% They must also be on the same page
%% It will write the values to the last row/column as if they were
%% 'formula' attributes
write_last(List) when is_list(List) ->
    io:format("in hn_db_api:write_last List is ~p~n", [List]),
    [{#refX{site = S, path = P, obj = O} = RefX, _} | T] = List,
    Fun =
        fun() ->
                io:format("in Fun~n"),
                {LastCol, LastRow} = hn_db_wu:get_last(RefX),
                % Add 1 to because we are adding data 'as the last row'
                % (or column) ie one more than the current last row/column
                {Type, Pos} = case O of
                                  {row, _}    -> {row,    LastRow + 1};
                                  {column, _} -> {column, LastCol + 1}
                              end,
                % now convert the column or row references to cell references
                
                % The matches in the Fun ensure that all the cells are the same
                % page and are row or column references as appropriate 
                Fun1 =
                    fun({#refX{site = S, path = P, obj = {Type, Idx}}, Val})  ->
                            io:format("in Fun1~n"),
                               Obj = case Type of
                                         row    -> {cell, {Pos, Idx}};
                                         column -> {cell, {Idx, Pos}}
                                     end,
                            RefX2 = #refX{site = S, path = P, obj = Obj},
                            io:format("in hn_db_api:write_last RefX2 is ~p~n",
                                      [RefX2]),
                            hn_db_wu:write_attr(RefX2, {formula, Val})
                    end,
                [Fun1(X) || X <- List]
        end,
    Return = mnesia:activity(transaction, Fun),
    io:format("mnesia returned with ~p~n", [Return]),
    {ok, ok}.

%% @spec read(#refX{}) -> [{#refX{}, {Key, Value}}]
%% Key = atom()
%% Value = term()
%% @doc read takes a refererence which can be one of a:
%% <ul>
%% <li>cell</li>
%% <li>range</li>
%% <li>column</li>
%% <li>row</li>
%% <li>page</li>
%% </ul>
read(RefX) when is_record(RefX, refX) ->
    io:format("in hn_db_api:read RefX is ~p~n", [RefX]),
    Fun = fun() ->
                  hn_db_wu:read_attrs(RefX)
          end,
    mnesia:activity(transaction, Fun).

%% @spec insert(RefX :: #refX{}) -> ok
%% @doc inserts a column,a row or page
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul><li>row</li>
%% <li>colum</li></ul>
%% @todo insert page
insert(#refX{obj = {R, _}} = RefX) when R == row orelse R == column  ->
    Disp = case R of
               row -> vertical;
               column -> horizontal
           end,
    Fun =
        fun() ->
                hn_db_wu:shift(RefX, Disp, insert)
        end,
    mnesia:activity(transaction, Fun).

%% @spec insert(RefX :: #refX{}, Type) -> ok 
%% Type = [horizontal | vertical]
%% @doc inserts a cell or range
%% 
%% The <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>range</li></ul>
insert(#refX{obj = {R, _}} = RefX, Disp) when R == cell orelse R == range ->
    Fun =
        fun() ->
                hn_db_wu:shift(RefX, Disp, insert)
        end,
    mnesia:activity(transaction, Fun).

%% @spec delete(Ref :: #refX{}) -> ok
%% @doc deletes a column or a row
delete(#refX{obj = {R, _}} = RefX) when R == column orelse R == row ->
    Disp = case R of
               row    -> vertical;
               column -> horizontal
           end,
    Fun =
        fun() ->
                hn_db_wu:shift(RefX, Disp, delete)
        end,
    mnesia:activity(transaction, Fun).

%% @spec delete(RefX :: #refX{}, Type) -> ok
%% Type = [contents | all | horizontal | vertical]
%% @doc deletes the value (but not any formatting information) of
%% <ul>
%% <li>a cell</li>
%% <li>a row</li>
%% <li>a column</li>
%% <li>a range</li>
%% </ul>
%% 
%% This clause deletes a cell or range and closes up the rest of them
%% if Disp is Horizontal it moves cells right-to-left to close the gap
%% if Disp is vertical is moves cells bottom-to-top to close the gap
%% 
%% @todo
%% <ul>
%% <li>keep the cell formulae/values but delete all the formatting</li>
%% <li>delete the attachment of a style to a cell/range</li>
%% <li>delete an individual attribute of a cell/range</li>
%% <li>delete a page/a page with sub-pages</li>
%% </ul>
delete(#refX{obj = {R, _}} = RefX, horizontal) when R == cell orelse R == range ->
    Fun =
        fun() ->
                hn_db_wu:shift(RefX, horizontal, delete)
        end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {R, _}} = RefX, vertical) when R == cell orelse R == range ->
    Fun =
        fun() ->
                hn_db_wu:shift(RefX, vertical, delete)
        end,
    mnesia:activity(transaction, Fun);
%%
%% These clauses delete the contents of the cell or range
%% but don't delete the cell or range itself, ie
%% * formula
%% * values
%% but not the other attributes of the cell or range
%% * format
%% * styles
%% * user-defined attributes
%% * etc, etc
delete(#refX{obj = {range, _} = RefX}, contents) ->
    List = hn_util:range_to_list(RefX),
    Fun =
        fun() ->
                hn_db_wu:delete_list(List, contents)
        end,
    mnesia:activity(transaction, Fun);
delete(#refX{obj = {cell, _}} = RefX, contents) ->
    Fun =
        fun() ->
                hn_db_wu:delete_cell(RefX, contents)
        end,
    mnesia:activity(transaction, Fun);
%%
%% these clauses delete everything from a cell or range but
%% don't delete the cell or range itself
delete(#refX{obj = {range, _}} = RefX, all) ->
    List = hn_util:range_to_list(RefX),
    Fun =
        fun() ->
                hn_db_wu:delete_list(List, all)
        end,
    mnesia:activity(transaction, Fun);
delete(RefX = #refX{obj = {cell, _}} = RefX, all) ->
    Fun =
        fun() ->
                hn_db_wu:delete_cell(RefX, all)
        end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% server side drag'n'drop                                                    %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec cut_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination then deletes the original
%% (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%% 
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul>
%% <li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li>
%% </ul>
%% 
%% If a range is to be cut'n'pasted to a range then one of the following criteria MUST
%% true:
%% <ul>
%% <li>the <b>from</b> range must be the same dimensions as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be one cell high and the same width as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be one cell wide and the same height as the
%% <b>to</b> range</li>
%% </ul> 
%% 
%% @todo cut'n'paste a page
cut_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    Fun = fun() ->
                  CellList = case is_valid_c_n_p(From, To) of
                                 {ok, single_cell}    -> ok;
                                 {ok, 'onto self'}    -> [];
                                 {ok, cell_to_range}  -> copy_cut_drag_n_paste_drop2(From, To, false);
                                 {ok, range_to_range} -> exit("erk!")
                             end,
                  % TODO should write CellList out n'est pas
                  delete(From, contents)
          end,
    mnesia:activity(transaction, Fun).
%    % _ supresses a warning
%    _ = case is_valid_c_n_p(From, To) of
%            {ok, single_cell}    -> copy_cut_drag_n_paste_drop(From, To, false);
%            {ok, 'onto self'}    -> {ok, ok};
%            {ok, cell_to_range}  -> copy_cut_drag_n_paste_drop2(From, To, false);
%            {ok, range_to_range} -> exit("erk!")
%        end,
%    delete(From, contents).

%% @spec copy_n_paste(From :: #refX{}, To :: #refX{}) -> ok
%% @doc copies the formula and formats from a cell or range and 
%% pastes them to the destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%% 
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%% @todo copy'n'paste a page
copy_n_paste(From, To) when is_record(From, refX), is_record(To, refX) ->
    case is_valid_c_n_p(From, To) of
        {ok, single_cell}    -> copy_cut_drag_n_paste_drop(From, To, false);
        {ok, 'onto self'}    -> {ok, ok};
        {ok, cell_to_range}  -> copy_cut_drag_n_paste_drop2(From, To, false);
        {ok, range_to_range} -> exit("erk!")
    end.

%% @spec drag_n_drop(From :: #refX{}, To :: #refX{}) -> ok
%% @doc takes the formula and formats from a cell and drag_n_drops 
%% them over a destination (the difference between drag'n'drop
%% and copy/cut'n'paste is that drag'n'drop increments)
%% 
%% drag'n'drop has an interesting specification
%% (taken from Excel 2007 help)
%% currently excludes customer autofil
%% 
%% <code>Initial selection       Extended series</code>
%% 
%% <code>-----------------       ---------------</code>
%% 
%% <code>1, 2, 3                 4, 5, 6,... </code>
%% 
%% <code>9:00 10:00,             11:00, 12:00,... </code>
%% 
%% <code>Mon Tue,                Wed, Thu,... </code>
%% 
%% <code>Monday Tuesday,         Wednesday, Thursday,... </code>
%% 
%% <code>Jan Feb,                Mar, Apr,... </code>
%% 
%% <code>Jan, Apr                Jul, Oct, Jan,... </code>
%% 
%% <code>Jan-07, Apr-07          Jul-07, Oct-07, Jan-08,... </code>
%% 
%% <code>15-Jan, 15-Apr          15-Jul, 15-Oct,... </code>
%% 
%% <code>2007, 2008              2009, 2010, 2011,... </code>
%% 
%% <code>1-Jan, 1-Mar            1-May, 1-Jul, 1-Sep,... </code>
%% 
%% <code>Qtr3                    Qtr4, Qtr1, Qtr2,... </code>
%% 
%% <code>Q3                      Q4, Q1, Q2,... </code>
%% 
%% <code>Quarter3                Quarter4, Quarter1, Quarter2,... </code>
%% 
%% <code>text1, textA text2,     textA, text3, textA,... </code>
%% 
%% <code>1st Period              2nd Period, 3rd Period,... </code>
%% 
%% <code>Product 1               Product 2, Product 3,... </code>
%% 
%% <code>1 Product               2 Product, 3 Product</code>
%% 
%% Either <code>#refX{}</code> can be one of the following types:
%% <ul><li>cell</li>
%% <li>row</li>
%% <li>colum</li>
%% <li>range</li></ul>
%% 
%% If a range is to be drag'n'dropped to a range then
%% one of the following criteria MUST be true:
%% <ul><li>the <b>from</b> range must be the same dimensions as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> range must be the same width as the 
%% <b>to</b> range</li>
%% <li>the <b>from</b> must be the same height as the
%% <b>to</b> range</li></ul> 
drag_n_drop(From, To) when is_record(From, refX), is_record(To, refX) ->
    case is_valid_d_n_d(From, To) of
        {ok, single_cell, Incr}   -> copy_cut_drag_n_paste_drop(From, To, Incr);
        {ok, 'onto self', _Incr}  -> {ok, ok};
        {ok, cell_to_range, Incr} -> copy_cut_drag_n_paste_drop2(From, To, Incr)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Internal Functions                                                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the last parameter returned is whether dates and integers should be 
%% incremented this can only be true for a vertical or horizontal drag
%% (returning 'y' and 'x') or is otherwise false
%% cell to cell drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, A}}, #refX{obj = {cell, A}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {cell, {X, _Y1}}}, #refX{obj = {cell, {X, _Y2}}}) ->
    {ok, single_cell, y};
is_valid_d_n_d(#refX{obj = {cell, {_X1, Y}}}, #refX{obj = {cell, {_X2, Y}}}) ->
    {ok, single_cell, x};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {cell, _}}) ->
    {ok, single_cell, false};
%% cell to range drag'n'drop
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, {TX, _TY1, TX, _TY2}}}) ->
    {ok, cell_to_range, y};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, {_TX1, TY, _TX2, TY}}}) ->
    {ok, cell_to_range, x};
is_valid_d_n_d(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range, false};
%% range to range drag'n'drop
is_valid_d_n_d(#refX{obj = {range, Range}}, #refX{obj = {range, Range}}) ->
    {ok, 'onto self', false};
is_valid_d_n_d(#refX{obj = {range, {FX, FY1, FX, FY2}}}, #refX{obj = {range, TRange}}) ->
    {_TX1, TY1, _TX2, TY2} = TRange,
    case ((TY2 - TY1) - (FY2 - FY1)) of
        0    -> {ok, col_range_to_range};
        true -> {error, "target range is not the same height as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, {FX1, FY, FX2, FY}}}, #refX{obj = {range, TRange}}) ->
    {TX1, _TY1, TX2, _TY2} = TRange,
    case ((TX2 - TX1) - (FX2 - FX1)) of
        0    -> {ok, row_range_to_range};
        true -> {error, "target range is not the same width as the source range"}
    end;
is_valid_d_n_d(#refX{obj = {range, _}}, #refX{obj = {range, _}}) ->
    {error, "from range is invalid"};
is_valid_d_n_d(_, _) -> {error, "not valid either"}.

filter_for_drag_n_drop(List) -> fl(List, [], []).

fl([], A, B)                                                 -> {A, B};
fl([{_, {ref, _, _, _, value, _}, _}| T], A, B)              -> fl(T, A, B);
fl([{_, {ref, _, _, _, rawvalue, _}, _}| T], A, B)           -> fl(T, A, B);
fl([{_, {ref, _, _, _, parents, _}, _}| T], A, B)            -> fl(T, A, B);
fl([{_, {ref, _, _, _, 'dependancy-tree', _}, _}| T], A, B)  -> fl(T, A, B);
fl([{_, {ref, _, _, _, '__ast', _}, _}| T], A, B)            -> fl(T, A, B);
fl([{_, {ref, _, _, _, formula, _}, F}| T], A, B)            -> fl(T, [F | A], B);
fl([H | T], A, B)                                            -> fl(T, A, [H | B]).

copy_cut_drag_n_paste_drop(From, To, Incr) when is_record(From, refX),
                                                is_record(To, refX) ->
    FromList = hn_db:get_item(From),
    {Contents, FilteredList} = filter_for_drag_n_drop(FromList),
    Output = case Contents of
                 [Contents2] -> superparser:process(Contents2);
                 []          -> ""
             end,
    #refX{obj = {cell, {FX, FY}}} = From,
    #refX{obj = {cell, {TX, TY}}} = To,
    case Output of
        {formula, Formula} ->
            {ok, Toks} = xfl_lexer:lex(super_util:upcase(Formula), {FX, FY}),
            NewToks = offset(Toks, (TX - FX), (TY - FY)),
            NewFormula = make_formula(NewToks),
            % hn_main:set_cell(To#refX{name=formula}, NewFormula); % Fix Me!
            exit("Fix me!");
        [{Type, V},  _A, _F] ->
            V2 = case Incr of
                     false ->
                         tconv:to_s(V);
                     _     -> 
                         case Type of
                             int      -> NewV = V + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s(NewV);
                             datetime -> {datetime, {Y, M, D}, T} = V,
                                         D2 = D + diff(FX, FY, TX, TY, Incr),
                                         tconv:to_s({datetime, {Y, M, D2}, T}); 
                             _        -> tconv:to_s(V)
                         end
                 end,
            exit("Fix me 2!");
            % hn_main:set_cell(To#refX{name=formula}, V2); % Fix me!
        []  -> delete(To, all)
    end,
    % You want to copy the attributes AFTER setting the value
    % because setting a value sets the default alignment and format
    % and if the source cell has been reformatted after the data was entered
    % you want to carry that forward.
    {ok, ok} = hn_db_wu:copy_attributes(FilteredList, To),
    ok.

copy_cut_drag_n_paste_drop2(From, To, Incr) when is_record(From, refX),
                                                 is_record(To, refX) ->
    List = hn_util:range_to_list(To),
    lists:map(fun(X) -> copy_cut_drag_n_paste_drop(From, X, Incr) end, List).

is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {cell, _}})  ->
    {ok, single_cell};
is_valid_c_n_p(#refX{obj = {cell, _}}, #refX{obj = {range, _}}) ->
    {ok, cell_to_range};
is_valid_c_n_p(#refX{obj = {range, {FX1, FY1, FX2, FY2}}},
               #refX{obj = {range, {TX1, TY1, TX2, TY2}}}) ->
    % two ranges for copy'n'paste (and cut'n'paste) must be the same shape
    X = TX2 - TX1,
    Y = TY2 - TY1,
    case {FX2 - FX1, FY2 - FY1} of
        {X, Y} -> {ok, range_to_range};
        _Other -> exit(invalid_range)
    end.

shift(RefX, Disp, _Type) when is_record(RefX, refX) ->
    % single mnesia transaction
    % 
    % The process is:
    % * get all the cells which will be displaced
    %  * for each cell (in a transaction)
    %    * delete
    %    * rewrite it
    %    * save it
    % Need to do funny stuff with remote hypernumbers...

    % io:format("in shift Ref is ~p Disp is ~p Type is ~p~n", [Ref, Disp, Type]),
    #refX{site = Site, path = Path, obj = {Range, R}} = RefX,

    % get the 'ShiftedCells'
    RefX2 = RefX#refX{obj = {cell, {'$1', '$2'}},  auth  = '_'},
    Head = ms_util:make_ms(hn_item, [{addr, RefX2}]),
    % Cond selects the cells to be 'adjusted' and Offset 
    % determines how they are to be 'adjusted'
    {Cond, Offset}
        = case Range of
              row   ->
                  A = [{'>', '$1', R}],
                  B = {0, -1},
                  {A, B};
              col   ->
                  A = [{'>', '$2', R}],
                  B = {-1, 0},
                  {A, B};
              cell  ->
                  {X, Y} = R,
                  case Disp of
                      horizontal ->
                          A = [{'and', {'>',  '$1', Y}, {'==', '$2', X}}],
                          B = {-1, 0},
                          {A, B};
                      vertical   ->
                          A = [{'and', {'==', '$1', X}, {'>',  '$2', Y}}],
                          B = {0, -1},
                          {A, B}
                  end;
              range ->
                  {X1, Y1, X2, Y2} = R,
                  case Disp of
                      horizontal ->
                          A = [{'and', {'>=', '$1', Y1}, {'==', '$2', X1}}],
                          B = [{'and', {'=<', '$1', Y2}, {'==', '$2', X2}}],
                          C = {0, Y1 - Y2}, % should be negative!
                          {[{'and', A, B}], C};
                      vertical   ->
                          A = [{'and', {'==', '$1', X1}, {'>=', '$2', Y1}}],
                          B = [{'and', {'==', '$1', X2}, {'=<', '$2', Y2}}],
                          C = {0, X1 - X2}, % should be negative!
                          {[{'and', A, B}], C}                         
                  end
          end,
    Body = ['$_'],
    Fun1 = fun() -> ?mn_sl(hn_item, [{Head, Cond, Body}]) end,
    ShiftedCells = ?mn_ac(transaction, Fun1),
    % now shift the cells
    Fun2 = fun() -> hn_db_wu:shift(ShiftedCells, Offset) end,
    {ok, ok} = ?mn_ac(transaction, Fun2),
    {ok, ok}.

offset(Toks, XOffset, YOffset) -> offset(Toks, XOffset, YOffset, []).

offset([], _XOffset, _YOffset, Acc) -> lists:reverse(Acc);
offset([{ref, Col, Row, Path, Cell} | T], XOffset, YOffset, Acc) ->
    {XDollar, X, YDollar, Y} = parse_cell(Cell),
    NewCell = make_cell(XDollar, X, XOffset, YDollar, Y, YOffset),
    NewRef = {ref, Col, Row, Path, NewCell},
    offset(T, XOffset, YOffset, [NewRef | Acc]);
offset([H | T], XOffset, YOffset, Acc) ->
    offset(T, XOffset, YOffset, [H | Acc]).

parse_cell(Ref) ->
    {XDollar, Rest} = case Ref of
                          [$$ | T1] -> {true, T1};
                          _         -> {false, Ref}
                      end,
    Fun = fun(XX) ->
                  if XX < 97  -> false;
                     XX > 122 -> false;
                     true     -> true
                  end
          end,
    {XBits, YBits} = lists:partition(Fun,string:to_lower(Rest)),
    {YDollar, Y} = case YBits of
                       [$$ | T2] -> {true, T2};
                       _         -> {false, YBits}
                   end,
    {XDollar, tconv:to_i(XBits), YDollar, list_to_integer(Y)}.

make_cell(false, X, XOffset, false, Y, YOffset) ->
    tconv:to_b26(X + XOffset)++tconv:to_s(Y + YOffset);
make_cell(true, X, _XOffset, false, Y, YOffset) ->
    [$$]++tconv:to_b26(X)++tconv:to_s(Y + YOffset);
make_cell(false, X, XOffset, true, Y, _YOffset) ->
    tconv:to_b26(X + XOffset)++[$$]++tconv:to_s(Y);
make_cell(true, X, _XOffset, true, Y, _YOffset)  -> 
    [$$]++tconv:to_b26(X)++[$$]++tconv:to_s(Y).

make_formula(Toks) -> mk_f(Toks, []).

mk_f([], Acc)                        -> "="++lists:flatten(lists:reverse(Acc));
mk_f([{ref, _, _, _, Ref} | T], Acc) -> mk_f(T, [Ref | Acc]);
mk_f([{atom, H} | T], Acc)           -> mk_f(T, [H | Acc]);
mk_f([{H} | T], Acc)                 -> mk_f(T, [atom_to_list(H) | Acc]).

diff( FX, _FY,  TX, _TY, x) -> TX - FX;
diff(_FX,  FY, _TX,  TY, y) -> TY - FY.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Debugging interfaces                                                       %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
insert_delete_DEBUG() ->
    insert_delete_DEBUG2("insert").
% insert_delete_DEBUG2("delete").

%% @hidden
delete_DEBUG(Ref, Type) -> delete(Ref, Type).

%% @hidden
insert_DEBUG(Ref, Type) -> insert(Ref, Type).

%% @hidden
copy_DEBUG() ->
    copy_DEBUG2("drag_n_drop"),
    copy_DEBUG2("copy_n_paste"),
    copy_DEBUG2("cut_n_paste").

%% @hidden
delete_cell_contents_DEBUG(Site, Path) ->
    Target = #ref{site = Site, path = Path, ref = {range, {1, 1, 30, 30}}},
    delete(Target, contents).

%% @hidden
delete_cell_contents_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    delete_cell_contents_DEBUG(Site, Path).

%% @hidden
clear_cells_DEBUG(Site, Path) ->
    Target = #refX{site = Site, path = Path, obj = {range, {1, 1, 30, 30}}},
    delete(Target, all).

%% @hidden
clear_cells_DEBUG(Path) ->
    Site = "http://127.0.0.1:9000",
    clear_cells_DEBUG(Site, Path).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%% Used In Debugging interfaces                                               %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_delete_DEBUG2(FunName) ->

    FunName2 = [FunName],

    clear_cells_DEBUG(FunName),

    write_value(FunName2, FunName, {1, 1}, [bold, underline, center]),

    write_value(FunName2, "before: "++FunName, {3, 1}, [bold, underline, center]),
    colour(FunName2, {3, 1}, "grey"),

    % cell stuff first
    write_value(FunName2, "Cell "++FunName++" (down)", {1, 3}, [bold, underline, center]),
    write_value(FunName2, "=./data/A1", {1, 4}, []),
    write_value(FunName2, FunName++" a cell here", {1, 5}, []),
    write_value(FunName2, "=./data/B1", {1, 6}, []),
    write_value(FunName2, "=hn(\"http://il_ballo.dev/data/C1?hypernumbers\")", {1, 7}, []),
    colour(FunName2, {1, 4}, "orange"),
    colour(FunName2, {1, 5}, "orange"),
    colour(FunName2, {1, 6}, "orange"),
    colour(FunName2, {1, 7}, "orange"),

    %    write_value(FunName2, "Cell "++FunName++" (right)", {1, 9}, [bold, underline, center]),
    %    write_value(FunName2, FunName++" a cell here", {1, 10}, []),
    %    write_value(FunName2, "=./data/A1", {2, 10}, []),
    %    colour(FunName2, {1, 10}, "orange"),
    %    colour(FunName2, {2, 10}, "orange"),

    make_thick(FunName2, 1),

    %    write_value(FunName2, "Row "++FunName++" (right)", {1, 12}, [bold, underline, center]),
    %    write_value(FunName2, FunName++" a row here", {1, 13}, []),
    %    write_value(FunName2, "=./data/A1", {1, 14}, []),
    %    colour(FunName2, {1, 13}, "orange"),
    %    colour(FunName2, {1, 14}, "orange"),

    make_thin(FunName2, 2),

    %    write_value(FunName2, FunName++" Column Here", {3, 3}, [bold, underline, center]),

    %    make_thick(FunName2, 3),

    %    make_thin(FunName2, 4),

    %    make_thick(FunName2, 5),

    %    write_value(FunName2, "Range "++FunName++" (down)", {5, 14}, [bold, underline, center]),
    %    write_value(FunName2, "=./data/A1", {5, 15}, []),
    %    write_value(FunName2, "=./data/B1", {5, 16}, []),
    %    write_value(FunName2, "=./data/C1", {5, 17}, []),
    %    write_value(FunName2, "=./data/D1", {5, 18}, []),
    %    colour(FunName2, {5, 15}, "orange"),
    %    colour(FunName2, {5, 16}, "orange"),
    %    colour(FunName2, {5, 17}, "orange"),
    %    colour(FunName2, {5, 18}, "orange"),

    %    write_value(FunName2, "=./data/E1", {6, 15}, []),
    %    write_value(FunName2, "=./data/F1", {6, 16}, []),
    %    write_value(FunName2, "=./data/G1", {6, 17}, []),
    %    write_value(FunName2, "=./data/H1", {6, 18}, []),
    %    colour(FunName2, {6, 15}, "orange"),
    %    colour(FunName2, {6, 16}, "yellow"),
    %    colour(FunName2, {6, 17}, "yellow"),
    %    colour(FunName2, {6, 18}, "orange"),

    %    write_value(FunName2, "=./data/I1", {7, 15}, []),
    %    write_value(FunName2, "=./data/J1", {7, 16}, []),
    %    write_value(FunName2, "=./data/K1", {7, 17}, []),
    %    write_value(FunName2, "=./data/L1", {7, 18}, []),
    %    colour(FunName2, {7, 15}, "orange"),
    %    colour(FunName2, {7, 16}, "yellow"),
    %    colour(FunName2, {7, 17}, "yellow"),
    %    colour(FunName2, {7, 18}, "orange"),

    %    write_value(FunName2, "=./data/M1", {8, 15}, []),
    %    write_value(FunName2, "=./data/N1", {8, 16}, []),
    %    write_value(FunName2, "=./data/O1", {8, 17}, []),
    %    write_value(FunName2, "=./data/P1", {8, 18}, []),
    %    colour(FunName2, {8, 15}, "orange"),
    %    colour(FunName2, {8, 16}, "orange"),
    %    colour(FunName2, {8, 17}, "orange"),
    %    colour(FunName2, {8, 18}, "orange"),

    %    make_thin(FunName2, 9),

    %    write_value(FunName2, "Range "++FunName++" (right)", {10, 14}, [bold, underline, center]),
    %    write_value(FunName2, "A", {10, 15}, []),
    %    write_value(FunName2, "B", {10, 16}, []),
    %    write_value(FunName2, "C", {10, 17}, []),
    %    write_value(FunName2, "D", {10, 18}, []),
    %    colour(FunName2, {10, 15}, "orange"),
    %    colour(FunName2, {10, 16}, "orange"),
    %    colour(FunName2, {10, 17}, "orange"),
    %    colour(FunName2, {10, 18}, "orange"),

    %    write_value(FunName2, "E", {11, 15}, []),
    %    write_value(FunName2, "F", {11, 16}, []),
    %    write_value(FunName2, "G", {11, 17}, []),
    %    write_value(FunName2, "H", {11, 18}, []),
    %    colour(FunName2, {11, 15}, "orange"),
    %    colour(FunName2, {11, 16}, "yellow"),
    %    colour(FunName2, {11, 17}, "yellow"),
    %    colour(FunName2, {11, 18}, "orange"),

    %    write_value(FunName2, "I", {12, 15}, []),
    %    write_value(FunName2, "J", {12, 16}, []),
    %    write_value(FunName2, "K", {12, 17}, []),
    %    write_value(FunName2, "L", {12, 18}, []),
    %    colour(FunName2, {12, 15}, "orange"),
    %    colour(FunName2, {12, 16}, "yellow"),
    %    colour(FunName2, {12, 17}, "yellow"),
    %    colour(FunName2, {12, 18}, "orange"),

    %    write_value(FunName2, "M", {13, 15}, []),
    %    write_value(FunName2, "N", {13, 16}, []),
    %    write_value(FunName2, "O", {13, 17}, []),
    %    write_value(FunName2, "P", {13, 18}, []),
    %    colour(FunName2, {13, 15}, "orange"),
    %    colour(FunName2, {13, 16}, "orange"),
    %    colour(FunName2, {13, 17}, "orange"),
    %    colour(FunName2, {13, 18}, "orange"),

    %    make_thick(FunName2, 10),

    %
    % Now set up the various bits of data
    % 
    write_data([FunName,"data"]),
    write_data("http://il_ballo.dev:9000",[FunName,"data"]),

    %
    % Now do the inserts and deletes
    % 
    io:format("'bout to wait...~n"),
    test_util:wait(100),
    io:format("'done waitin...~n"),
    write_value(FunName2, "after: "++FunName, {3, 1}, [bold, underline, center]),
    colour(FunName2, {3, 1}, "red"),

    insert_delete(FunName, FunName2, {cell, {1, 5}}, vertical),
    %    % insert_delete(FunName, FunName2, {cell, {1, 5}}, horizontal),
    %    % insert_delete(FunName, FunName2, {row, 12}),
    %    % insert_delete(FunName, FunName2, {column, 3}),
    %    % insert_delete(FunName, FunName2, {range, {6, 16, 7, 18}}, vertical),
    %    % insert_delete(FunName, FunName2, {range, {11, 16, 13, 18}}, vertical),

    ok.

copy_DEBUG2(FunName) ->

    FunName2 = [FunName],

    clear_cells_DEBUG(FunName),
    write_value(FunName2, FunName++" - cell to cell", {1, 1}, [bold, underline]),

    % cell to cell drop down
    write_value(FunName2, "integer below", {1, 2}, [bold]),
    write_value(FunName2, "1", {1, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 3}}, {cell, {1, 4}}),
    colour(FunName2, {1, 3}, "cyan"),

    write_value(FunName2, "float below", {1, 5}, [bold]),
    write_value(FunName2, "1.1", {1, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 6}}, {cell, {1, 7}}),
    colour(FunName2, {1, 6}, "cyan"),

    write_value(FunName2, "string below", {1, 8}, [bold]),
    write_value(FunName2, "hey!", {1, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 9}}, {cell, {1, 10}}),
    colour(FunName2, {1, 9}, "cyan"),

    write_value(FunName2, "date below", {1, 11}, [bold]),
    write_value(FunName2, "1/2/3 4:5:6", {1, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 12}}, {cell, {1, 13}}),
    colour(FunName2, {1, 12}, "cyan"),

    write_value(FunName2, "boolean below", {1, 14}, [bold]),
    write_value(FunName2, "true", {1, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 15}}, {cell, {1, 16}}),
    colour(FunName2, {1, 15}, "cyan"),

    % cell to cell across
    write_value(FunName2, "integer beside", {2, 2}, [bold]),
    write_value(FunName2, "1", {2, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 3}}, {cell, {3, 3}}),
    colour(FunName2, {2, 3}, "cyan"),

    write_value(FunName2, "float beside", {2, 5}, [bold]),
    write_value(FunName2, "1.1", {2, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 6}}, {cell, {3, 6}}),
    colour(FunName2, {2, 6}, "cyan"),

    write_value(FunName2, "string beside", {2, 8}, [bold]),
    write_value(FunName2, "hey!", {2, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 9}}, {cell, {3, 9}}),
    colour(FunName2, {2, 9}, "cyan"),

    write_value(FunName2, "date beside", {2, 11}, [bold]),
    write_value(FunName2, "1/2/3 4:5:6", {2, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 12}}, {cell, {3, 12}}),
    colour(FunName2, {2, 12}, "cyan"),

    write_value(FunName2, "boolean beside", {2, 14}, [bold]),
    write_value(FunName2, "true", {2, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 15}}, {cell, {3, 15}}),
    colour(FunName2, {2, 15}, "cyan"),

    make_thin(FunName2, 4),

    write_value(FunName2, "Drag'n'Drop - cell to down 'thin' range", {5, 1}, [bold, underline]),
    % cell to range down
    write_value(FunName2, "integer below", {5, 2}, [bold]),
    write_value(FunName2, "1", {5, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 3}}, {range, {5, 4, 5, 5}}),
    colour(FunName2, {5, 3}, "cyan"),

    write_value(FunName2, "float below", {6, 5}, [bold]),
    write_value(FunName2, "1.1", {6, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {6, 6}}, {range, {6, 7, 6, 8}}),
    colour(FunName2, {6, 6}, "cyan"),

    write_value(FunName2, "string below", {5, 8}, [bold]),
    write_value(FunName2, "hey!", {5, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 9}}, {range, {5, 10, 5, 11}}),
    colour(FunName2, {5, 9}, "cyan"),

    write_value(FunName2, "date below", {6, 11}, [bold]),
    write_value(FunName2, "1/2/3 4:5:6", {6, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {6, 12}}, {range, {6, 13, 6, 14}}),
    colour(FunName2, {6, 12}, "cyan"),

    write_value(FunName2, "boolean below", {5, 14}, [bold]),
    write_value(FunName2, "true", {5, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 15}}, {range, {5, 16, 5, 17}}),
    colour(FunName2, {5, 15}, "cyan"),

    write_value(FunName2, "Drag'n'Drop - cell to across 'thin' range", {7, 1}, [bold, underline]),
    % cell to range down
    write_value(FunName2, "integer beside", {7, 2}, [bold]),
    write_value(FunName2, "1", {7, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 3}}, {range, {7, 4, 8, 4}}),
    colour(FunName2, {7, 3}, "cyan"),

    write_value(FunName2, "float beside", {7, 5}, [bold]),
    write_value(FunName2, "1.1", {7, 6}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 6}}, {range, {7, 7, 8, 7}}),
    colour(FunName2, {7, 6}, "cyan"),

    write_value(FunName2, "string beside", {7, 8}, [bold]),
    write_value(FunName2, "hey!", {7, 9}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 9}}, {range, {7, 10, 8, 10}}),
    colour(FunName2, {7, 9}, "cyan"),

    write_value(FunName2, "date beside", {7, 11}, [bold]),
    write_value(FunName2, "1/2/3 4:5:6", {7, 12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 12}}, {range, {7, 13, 8, 13}}),
    colour(FunName2, {7, 12}, "cyan"),

    write_value(FunName2, "boolean beside", {7, 14}, [bold]),
    write_value(FunName2, "true", {7, 15}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {7, 15}}, {range, {7, 16,  8, 16}}),
    colour(FunName2, {7, 15}, "cyan"),

    make_thin(FunName2, 9),

    % cell to 'thick' ranges don't increment even if they are drag'n'drop
    write_value(FunName2, "Drag'n'Drop - cell to 'thick' range", {10,1}, [bold, underline]),

    write_value(FunName2, "integer", {10,2}, [bold]),
    write_value(FunName2, "1", {10,3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {10,3}}, {range, {10,4, 11, 10}}),
    colour(FunName2, {10,3}, "cyan"),

    % same as above but arsey backwards range
    write_value(FunName2, "testing inverted range", {10,11}, [bold]),
    write_value(FunName2, "1", {10,12}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {10,12}}, {range, {10, 20, 11, 13}}),
    colour(FunName2, {10,3}, "cyan"),

    make_thin(FunName2, 12),

    % set up formula data
    write_value(FunName2, "data for formula", {13,2}, [bold]),
    colour(FunName2, {13, 2}, "yellow"),
    write_value(FunName2, "1", {13, 3}, []),
    write_value(FunName2, "22", {13, 4}, []),
    write_value(FunName2, "333", {13, 5}, []),
    write_value(FunName2, "4444", {13, 6}, []),
    write_value(FunName2, "5555", {13, 7}, []),
    write_value(FunName2, "11111", {14, 3}, []),
    write_value(FunName2, "222222", {14, 4}, []),
    write_value(FunName2, "333333", {14, 5}, []),
    write_value(FunName2, "4444444", {14, 6}, []),
    write_value(FunName2, "55555555", {14, 7}, []),
    colour(FunName2, {13, 3}, "orange"),
    colour(FunName2, {13, 4}, "orange"),
    colour(FunName2, {13, 5}, "orange"),
    colour(FunName2, {13, 6}, "orange"),
    colour(FunName2, {13, 7}, "orange"),
    colour(FunName2, {14, 3}, "orange"),
    colour(FunName2, {14, 4}, "orange"),
    colour(FunName2, {14, 5}, "orange"),
    colour(FunName2, {14, 6}, "orange"),
    colour(FunName2, {14, 7}, "orange"),

    make_thin(FunName2, 15),

    % some formula stuff
    write_value(FunName2, "formula below", {16, 2}, [bold]),
    write_value(FunName2, "=m3+n3", {16, 3}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 3}}, {range, {16, 4, 17, 6}}),
    colour(FunName2, {16, 3}, "cyan"),

    write_value(FunName2, "fix col formula below", {16, 7}, [bold]),
    write_value(FunName2, "=$m3+n3", {16, 8}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 8}}, {range, {16, 9, 17, 11}}),
    colour(FunName2, {16, 8}, "cyan"),

    write_value(FunName2, "fix row formula below", {16, 12}, [bold]),
    write_value(FunName2, "=m$3+n3", {16, 13}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 13}}, {range, {16, 14, 17, 16}}),
    colour(FunName2, {16, 13}, "cyan"),

    write_value(FunName2, "fix row and col formula below", {16,17}, [bold]),
    write_value(FunName2, "=$m$3+n3", {16, 18}, [{colour, "yellow"}]),
    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {16, 18}}, {range, {16, 19, 17, 21}}),
    colour(FunName2, {16, 18}, "cyan").

%    io:format("in drag_n_drop should go through (4)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 8}}, {cell, {2, 8}}),
%    io:format("in drag_n_drop should go through (5)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {1, 9}}, {cell, {2, 10}}),    
%    io:format("in drag_n_drop should go through (6)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {2, 1}}, {range, {2, 2, 3, 6}}).
%    io:format("in drag_n_drop should go through (7)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {4, 1}}, {range, {4, 2, 4, 6}}),
%    io:format("in drag_n_drop should go through (8)~n"),
%    cut_n_drag_n_copy_n_drop_n_paste(FunName, {cell, {5, 1}}, {range, {6, 1, 8, 1}}).

insert_delete(Fun, Path, Target) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj = Target},
    erlang:apply(?MODULE, list_to_atom(Fun), [Ref]).

insert_delete(Fun, Path, Target, Type) ->
    Site = "http://127.0.0.1:9000",
    Ref = #refX{site = Site, path = Path, obj= Target},
    erlang:apply(?MODULE, list_to_atom(Fun), [Ref, Type]).

cut_n_drag_n_copy_n_drop_n_paste(Path, From, To) ->
    Site = "http://127.0.0.1:9000",
    From1 = #refX{site =  Site, path = Path, obj = From},
    To1   = #refX{site =  Site, path = Path, obj = To},
    erlang:apply(?MODULE, list_to_atom(Path), [From1, To1]).

%% choose the site to write to
write_value(Site, Path, Value, {X, Y}, Attributes) ->
    Cell = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    exit("Fix me 3!"),
    % hn_main:set_cell(Cell#refX{name = formula}, Value), % Fix me!
    write_attributes(Attributes, {X, Y}, Cell).

%% just write to the default
write_value(Path, Value, {X, Y}, Attributes) ->
    Site = "http://127.0.0.1:9000",
    write_value(Site, Path, Value, {X, Y}, Attributes).

write_attributes([], _, _) -> ok;
write_attributes([Attr | T], {X, Y}, Cell) ->
    {Name, V} = case Attr of
                    bold              -> {'font-weight', "bold"};
                    underline         -> {'text-decoration', "underline"};
                    center            -> {'font-align', "center"};
                    {colour, Colour}  -> {'background-color', Colour};
                    thin              -> {width, 30};
                    thick             -> {width, 200}
                end,
    Addr = Cell#ref{name=Name},
    hn_main:set_attribute(Addr, V),
    write_attributes(T, {X, Y}, Cell).

colour(Path, {X, Y}, Colour) ->
    Site = "http://127.0.0.1:9000",
    Cell = #refX{site = Site, path = Path, obj = {cell, {X, Y}}},
    write_attributes([{colour, Colour}], {X, Y}, Cell).

make_thin(Path, X) ->
    Site = "http://127.0.0.1:9000",
    % RefX = #refX{site = Site, path = Path, ob = {column, X}, name = width},
    hn_main:set_attribute("RefX", "20").

make_thick(Path, X) ->
    Site = "http://127.0.0.1:9000",
    % RefX = #refX{site = Site, path = Path, obj = {column, X}, name = width},
    hn_main:set_attribute("RefX", "200").

write_data(Path) ->
    Site = "http://127.0.0.1:9000",
    write_data(Site, Path).

write_data(Site, Path) ->
    io:format("in write_data Site is ~p Path is ~p~n", [Site, Path]),
    clear_cells_DEBUG(Path),
    % writes out sample data for hyperlinks
    write_value(Site, Path, "A", {1, 1}, []),
    write_value(Site, Path, "B", {2, 1}, []),
    write_value(Site, Path, "C", {3, 1}, []),
    write_value(Site, Path, "D", {4, 1}, []),
    write_value(Site, Path, "E", {5, 1}, []),
    write_value(Site, Path, "F", {6, 1}, []),
    write_value(Site, Path, "G", {7, 1}, []),
    write_value(Site, Path, "H", {8, 1}, []),
    write_value(Site, Path, "I", {9, 1}, []),
    write_value(Site, Path, "J", {10, 1}, []),
    write_value(Site, Path, "K", {11, 1}, []),
    write_value(Site, Path, "L", {12, 1}, []),
    write_value(Site, Path, "M", {13, 1}, []),
    write_value(Site, Path, "N", {14, 1}, []),
    write_value(Site, Path, "O", {15, 1}, []),
    write_value(Site, Path, "P", {16, 1}, []),
    write_value(Site, Path, "Q", {17, 1}, []),
    write_value(Site, Path, "R", {18, 1}, []),
    write_value(Site, Path, "S", {19, 1}, []),
    write_value(Site, Path, "T", {20, 1}, []),
    write_value(Site, Path, "U", {21, 1}, []),
    write_value(Site, Path, "V", {22, 1}, []),
    write_value(Site, Path, "W", {23, 1}, []),
    write_value(Site, Path, "X", {24, 1}, []),
    write_value(Site, Path, "Y", {25, 1}, []),
    write_value(Site, Path, "Z", {26, 1}, []).

