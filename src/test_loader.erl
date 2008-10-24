%%%-------------------------------------------------------------------
%%% @author U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%% @copyright (C) 2008, U-psytoo\gordonguthrie
%%% @doc
%%%
%%% @end
%%% Created :  9 Sep 2008 by U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%%-------------------------------------------------------------------
-module(test_loader).

-export([run/0]).

-include_lib("hypernumbers/include/spriki.hrl").
-include_lib("hypernumbers/include/hypernumbers.hrl").

-import(lists, [foldl/3, foreach/2, map/2]).
-import(test_util, [conv_for_post/1, conv_from_get/1, cmp/2, hnpost/3, hnget/2, readxls/1]).

run() ->
    toolbar:start(),
    %% Files=["z_junk_2"],
    %% Files=["d_gnumeric_address"],
    %% Files=["d_gnumeric_date_and_time"],
    %% Files=["d_gnumeric_db"],
    %% Files=["d_gnumeric_engineering"],
    %% Files=["d_gnumeric_financial"],
    %% Files=["d_gnumeric_logical"],
    %% Files=["d_gnumeric_lookup"],
    %% Files=["d_gnumeric_maths"],
    %%	   d_gnumeric_stats.xls,
    %%	   d_gnumeric_text.xls,
    %% Files = ["e_operator"],
    %Files = ["c_basic_functions_a_b"],
    %%	   e_trig.xls,
    %%	   f_12_month_cash_flow_statement.xls,
    %%	   Files = ["f_billing_statement"],
    %%	   Files = ["f_check_register.xls"],
    	   Files = ["f_employee_shift_schedule"],
    %%	   f_employee_timecard.xls,
    %%	   f_loan_amortization_schedule.xls,
    %%	   f_loan_calculator.xls,
    %%	   f_loan_calculator_with_extra_payments.xls,
    %%	   f_mortgage_amortization_schedule.xls,
    %%	   f_payroll_calculator.xls,
    %%	   f_services_invoice_with_tax_calculation.xls,
    %%	   f_timeline.xls,
    %%	   f_weekly_time_sheet_with_breaks.xls],
    Fun = fun(X) -> run_loader(X),
		    ?INFO("File ~p loaded~n~n",[X])
	  end,
    lists:foreach(Fun,Files).

run_loader(File)->
    bits:clear_db(),

    error_logger:info_msg("Starting loading ~p ~n",[File]),
    Celldata = readxls("../tests/excel_files/Win_Excel07_As_97/" ++File++".xls"),
    error_logger:info_msg("Read Spreadsheet ~p ~n",[File]),
    {Lits, Flas} = % split data into literals and formulas
	foldl(fun(X, _Acc = {Ls, Fs}) ->
                  {{{sheet, Sheetn}, {row_index, Row}, {col_index, Col}}, Val} = X,

		      {ok, Sheet, _} = regexp:gsub(Sheetn, "\\s+", "_"), % No whitespace in URLs eh.
		      Postdata = conv_for_post(Val),
		      Path = "/" ++ File ++ "/" ++ Sheet ++ "/",
		      Ref = tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1),
		      Datatpl = {Path, Ref, Postdata},
		      case Postdata of
			  [$= | _ ] -> % formula
			      {Ls, Fs ++ [Datatpl]};
			  _ ->
			      {Ls ++ [Datatpl], Fs}
		      end
	      end,
	      {[], []}, Celldata),

    Dopost = fun({Path, Ref, Postdata}) ->
                     Url = string:to_lower("http://127.0.0.1:9000"++Path++Ref),
                     {ok,NRef} = hn_util:parse_url(Url),
                     try 
                         hn_main:set_attribute(NRef#ref{name=formula},Postdata)
                     catch
                         error:Error ->
                             ?INFO("~p",[Error])
                     end
	     end,

    gen_server:cast(dirty_cell,  {setstate, passive}),
    foreach(Dopost, Lits),
    foreach(Dopost, Flas),
    gen_server:cast(dirty_cell, {setstate, active}),
    gen_server:call(dirty_cell, flush, infinity),
    ok.
