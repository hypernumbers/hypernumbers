%%% @author    Gordon Guthrie <>
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Produces the log view
%%%
%%% @end
%%% Created : 23 Mar 2011 by gordon@hypernumbers.com

-module(hn_logs).

-export([
         get_logs/1,
         get_json_logs/1
        ]).

-include("spriki.hrl").

-define(DOUBLEQUOTES, 34).

get_json_logs(RefX) when is_record(RefX, refX) ->
    Fun = fun(#logging{timestamp = Tm1}, #logging{timestamp = Tm2}) ->
                  if
                      Tm1 >  Tm2 -> false;
                      Tm1 =< Tm2 -> true
                  end
          end,
    Logs = lists:sort(Fun, new_db_api:get_logs(RefX)),
    FilteredLogs = [{Ts, {struct, [{action, Act},
                                   {actiontype, AT},
                                   {ref, hn_util:obj_to_ref(Obj)},
                                   {user, to_email(Uid)},
                                   {oldformula, SubLog#sublog.oldformula}]}}
                    || #logging{timestamp = Ts, uid = Uid, log = SubLog,
                                action = Act, actiontype = AT,
                                obj = Obj} <- Logs],
    {struct, FilteredLogs}.

to_email([])  -> [];
to_email(Uid) -> {ok, Email} = passport:uid_to_email(Uid),
                 Email.

get_logs(RefX) when is_record(RefX, refX) ->
    Fun = fun(#logging{timestamp = Tm1}, #logging{timestamp = Tm2}) ->
                  if
                      Tm1 >  Tm2 -> false;
                      Tm1 =< Tm2 -> true
                  end
          end,
    Logs = lists:sort(Fun, new_db_api:get_logs(RefX)),
    URL = hn_util:refX_to_url(RefX),
    tidy_up(Logs, URL, hn_util:refX_to_url(RefX)).

tidy_up(Logs, URL, Title) ->
    "<html><head><title = '" ++ Title ++ "></title>"
        ++ "<link href='/hypernumbers/hn.logs.css' rel='stylesheet' />"
        ++ "<link rel='stylesheet' href='/hypernumbers/hn.sheet.css' />"
        ++ "</head><body>"
        ++ make_html(Logs, URL, [])
        ++ "</body>"
        ++ "<script src='/hypernumbers/jquery-1.7.1.js'></script>"
        ++ "<script src='/hypernumbers/json2.min.js'></script>"
        ++ "<script src='/hypernumbers/hn.logs.js'></script>"
        ++ "</html>".

make_html([], _URL, Acc) ->
    "<table><thead><td></td><td></td><td>User</td><td>At The Time</td>"
        "<td></td><td></td><td>Change</td></thead>" ++
        lists:flatten(Acc) ++ "</table>";
make_html([H | T], URL, Acc) ->
    make_html(T, URL, [make_row(H) | Acc]).

make_row(#logging{idx = _I, timestamp = Tm, uid = U, action = A,
                  actiontype = AT, type = _Ty, path = _P, obj = O,
                  log = Msg}) ->
    Date = dh_date:format("d/m/y h:m:s", util2:timestamp_to_date(Tm)),
    Date2 = io_lib:format("~s", [Date]),
    {ok, Email} = case U of
                      [] -> {ok, ""};
                      _  -> passport:uid_to_email(U)
                  end,
    Ref = hn_util:obj_to_ref(O),
    A2 = format_actions_and_type(A),
    AT2 = format_actions_and_type(AT),
    Msg2 = format_msg(Msg),
    Rev = integer_to_list(Tm),
    Button = case Msg of
                 #sublog{} ->
                     "<input class='button' type='button' value='Revert'"
                         ++ "data-reversion='" ++ Rev ++ "' />";
                 _ ->
                     ""
             end,
    "<tr>"
        ++ td(Date2,  1)
        ++ td(Button, 1)
        ++ td(Email,  1)
        ++ td(Ref,    1)
        ++ td(A2,     1)
        ++ td(AT2,    1)
        ++ td(Msg2,   0)
        ++ "</tr>".

td(X, 0) -> "<td>" ++ X ++ "</td>";
td(X, 1) -> "<td class='hn_grey'>" ++ X ++ "</td>".

format_msg([])                  -> [];
format_msg(#sublog{msg = Msg})  -> binary_to_term(Msg);
format_msg(X) when is_binary(X) -> binary_to_term(X).

format_actions_and_type([])                -> [];
format_actions_and_type(X) when is_atom(X) -> atom_to_list(X).
