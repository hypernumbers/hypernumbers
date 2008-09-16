%%%-------------------------------------------------------------------
%%% @author U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%% @copyright (C) 2008, U-psytoo\gordonguthrie
%%% @doc
%%%
%%% @end
%%% Created :  9 Sep 2008 by U-psytoo\gordonguthrie <gordonguthrie@psytoo>
%%%-------------------------------------------------------------------
-module(test_loader).

-export([test/0]).

-import(lists, [foldl/3, foreach/2, map/2]).
-import(test_util, [conv_for_post/1, conv_from_get/1, cmp/2, hnpost/3, hnget/2, readxls/1]).

test() ->
    Files=[d_gnumeric_address.xls],
%%	   d_gnumeric_date_and_time.xls,
%%	   d_gnumeric_db.xls,
%%	   d_gnumeric_engineering.xls,
%%	   d_gnumeric_financial.xls,
%%	   d_gnumeric_information.xls,
%%	   d_gnumeric_logical.xls,
%%	   d_gnumeric_lookup.xls,
%%	   d_gnumeric_maths.xls,
%%	   d_gnumeric_stats.xls,
%%	   d_gnumeric_text.xls,
%%	   e_operator.xls,
%%	   e_trig.xls,
%%	   f_12_month_cash_flow_statement.xls,
%%	   f_billing_statement.xls,
%%	   f_check_register.xls,
%%	   f_employee_shift_schedule.xls,
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
		    io:format("File ~p loaded~n~n",[X])
	  end,
    lists:foreach(Fun,Files).

run_loader(File)->
    hn_loaddb:create_db(),
    Celldata = readxls("../tests/excel_files/Win_Excel07_As_97/" ++File),
    {Lits, Flas} = % split data into literals and formulas
	foldl(fun(X, _Acc = {Ls, Fs}) ->
		      {{{sheet, Sheetn}, {row_index, Row}, {col_index, Col}}, Val} = X,
		      io:format("in foldl File is ~p Sheet is ~p Row is ~p Col is ~p~n",[File,Sheetn,Row,Col]),
		      {ok, Sheet, _} = regexp:gsub(Sheetn, "\\s+", "_"), % No whitespace in URLs eh.
		      Postdata = conv_for_post(Val),
		      Path = "/" ++ "d_gnumeric_address" ++ "/" ++ Sheet ++ "/",
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
		     hnpost(Path, Ref, Postdata)
	     end,

    gen_server:cast(dirty_cell,  {setstate, passive}),
    foreach(Dopost, Lits),
    foreach(Dopost, Flas),
    gen_server:cast(dirty_cell, {setstate, active}),
    gen_server:call(dirty_cell, flush, infinity),
    ok.
