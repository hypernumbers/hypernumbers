%%% @author     Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       Implements a profiling system for mnesia queries
%%%            Will produce a slew of user messages if compiled in
%%% @end
%%% Created :  7 Jun 2011 by gordon@hypernumbers.com

-module(mnesia_mon).

-export([
         report/1,
         get_stamp/1,
         log_act/3
        ]).

get_transaction() -> {mnesia, Transaction, _} = get(mnesia_activity_state),
             Transaction.

get_stamp(Str) ->
    {Str, "stamp" ++ integer_to_list(util2:get_timestamp())}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% These fns are compiled in or out for mnesia monitoring
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Monitoring Off
report(Report) -> ok.

% Monitoring On
%% report(Report) ->
%%     {tid, Tid, Pid} = get_transaction(),
%%     mnesia:report_event({report, Tid, pid_to_list(Pid),
%%                          Report}),
%%     ok.

% Monitoring Off
log_act(Type, Fun, Report) -> mnesia:activity(Type, Fun).

% Monitoring On
%% log_act(Type, Fun, Report) ->
%%     Now = util2:get_timestamp(),
%%     Ret = mnesia:activity(Type, Fun),
%%     End = util2:get_timestamp(),
%%     Time = tconv:to_s((End - Now)/1000000),
%%     Msg = {measurement, Report, Time},
%%     mnesia:report_event(Msg),
%%     Ret.
