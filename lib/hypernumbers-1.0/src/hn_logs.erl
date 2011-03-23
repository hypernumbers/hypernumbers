%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Produces the log view
%%%
%%% @end
%%% Created : 23 Mar 2011 by gordon@hypernumbers.com

-module(hn_logs).

-export([
         get_logs/1
        ]).

-include("spriki.hrl").

-define(DOUBLEQUOTES, 34).

get_logs(RefX) when is_record(RefX, refX)->
    Fun = fun(#logging{timestamp = Tm1}, #logging{timestamp = Tm2}) ->
                  if
                      Tm1 >  Tm2 -> false;
                      Tm1 =< Tm2 -> true
                  end
          end,
    Logs = lists:sort(Fun, hn_db_api:get_logs(RefX)),
    tidy_up(Logs, hn_util:refX_to_url(RefX)).

tidy_up(Logs, Title) ->
    "<html><head><title = '" ++ Title ++ "</title>"
        ++ "<link href='<link href='/hypernumbers/logs.css' rel='stylesheet'>"
        ++ "</head><body>"
        ++ make_html(Logs, [])
        ++ "</body></html>".

make_html([], Acc) -> "<table><thead><td></td><td>User</td><td>At The Time</td>"
                          "<td></td><td></td><td>Change</td></thead>" ++
                          lists:flatten(Acc) ++ "</table>";
make_html([H | T], Acc) ->
    #logging{idx = _I, timestamp = Tm, uid = U, action = A, actiontype = AT,
          type = _Ty, path = _P, obj = O, log = Msg} = H,
    Date = dh_date:format("d/m/y h:m", util2:timestamp_to_date(Tm)),
    Date2 = io_lib:format("~s", [Date]),
    Row = [Date2, U, hn_util:obj_to_ref(O), format_actions_and_type(A),
           format_actions_and_type(AT), format_msg(Msg)],
    make_html(T, [make_row(Row, []) | Acc]).

make_row([], Acc)       -> "<tr>" ++ lists:flatten(lists:reverse(Acc)) ++ "</tr>";
make_row([H | []], Acc) -> make_row([], ["<td>" ++ H ++ "</td>" | Acc]);
make_row([H | T], Acc)  -> make_row(T, ["<td class='hn_grey'>" ++
                                        H ++ "</td>" | Acc]).

format_msg([])                  -> [];
format_msg(X) when is_binary(X) -> binary_to_term(X).

format_actions_and_type([])                -> [];
format_actions_and_type(X) when is_atom(X) -> atom_to_list(X).
