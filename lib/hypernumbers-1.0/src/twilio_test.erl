-module(twilio_test).

-export([
         testing1/0,
         testing2/0
        ]).

% normal call
testing1() ->
    Sid = util2:get_timestamp(),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_inbound_call(Sid)),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = hn_twilio_mochi:handle_c2_DEBUG(complete_inbound_call(Sid)),
    io:format("Ret2 is ~p~n", [Ret2]),
    ok.

% normal call with recording
testing2() ->
    Sid = util2:get_timestamp(),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_inbound_call(Sid)),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = hn_twilio_mochi:handle_c2_DEBUG(completed_with_recording(Sid)),
    io:format("Ret2 is ~p~n", [Ret2]),
    Ret3 = hn_twilio_mochi:handle_c2_DEBUG(complete_inbound_call(Sid)),
    io:format("Ret3 is ~p~n", [Ret3]),
    ok.

start_inbound_call(Sid) ->
    {twilio,"AC7a076e30da6d49119b335d3a6de43844",[],"inbound","ringing",
     Sid,"2010-04-01",[],
     {twilio_called,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     {twilio_caller,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_from,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_to,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     null,null,null}.

complete_inbound_call(Sid) ->
    {twilio,"AC7a076e30da6d49119b335d3a6de43844",[],"inbound","completed",
     Sid,"2010-04-01",[],
     {twilio_called,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     {twilio_caller,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_from,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_to,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     {twilio_duration,"1","8"},
     null,null}.

completed_with_recording(Sid) ->
    {twilio,"AC7a076e30da6d49119b335d3a6de43844",[],"inbound","completed",
     Sid,"2010-04-01",[],
     {twilio_called,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     {twilio_caller,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_from,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_to,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     null,
     {twilio_inprogress,"hangup",undefined},
     {twilio_recording,"RE4617c03d3b9d4fdc47b7187ed3077f24","3",
      "http%3A%2F%2Fapi.twilio.com%2F2010-04-01%2FAccounts%2FAC7a076e30da6d49119b335d3a6de43844%2FRecordings%2FRE4617c03d3b9d4fdc47b7187ed3077f24"}}.

