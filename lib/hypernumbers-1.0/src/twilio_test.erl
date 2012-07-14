-module(twilio_test).

-define(SITE, "http://hypernumbers.dev:9000").

-export([
         testing1/0,
         testing2/0,
         testing3/0,
         testing4/0
        ]).

% normal call
testing1() ->
    Sid = integer_to_list(util2:get_timestamp()),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_inbound_call(Sid), ?SITE),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = hn_twilio_mochi:handle_c2_DEBUG(complete_inbound_call(Sid), ?SITE),
    io:format("Ret2 is ~p~n", [Ret2]),
    ok.

% normal call with recording
testing2() ->
    Sid = integer_to_list(util2:get_timestamp()),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_inbound_call(Sid), ?SITE),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = hn_twilio_mochi:handle_c2_DEBUG(completed_with_recording(Sid), ?SITE),
    io:format("Ret2 is ~p~n", [Ret2]),
    Ret3 = hn_twilio_mochi:handle_c2_DEBUG(complete_inbound_call(Sid), ?SITE),
    io:format("Ret3 is ~p~n", [Ret3]),
    ok.

% normal call with in-progress recording
testing3() ->
    Sid = integer_to_list(util2:get_timestamp()),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_inbound_call(Sid), ?SITE),
    io:format("Ret1 is ~p~n", [Ret1]),
    Ret2 = hn_twilio_mochi:handle_c2_DEBUG(inprogress_with_recording(Sid), ?SITE),
    io:format("Ret2 is ~p~n", [Ret2]),
    Ret3 = hn_twilio_mochi:handle_c2_DEBUG(complete_inbound_call(Sid), ?SITE),
    io:format("Ret3 is ~p~n", [Ret3]),
    ok.

% setup an outbound call
testing4() ->
    Sid = integer_to_list(util2:get_timestamp()),
    Ret1 = hn_twilio_mochi:handle_c2_DEBUG(start_outbound_call(Sid), ?SITE),
    io:format("Ret1 is ~p~n", [Ret1]),
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

inprogress_with_recording(Sid) ->
    {twilio,"AC7a076e30da6d49119b335d3a6de43844",[],"inbound","in-progress",
     Sid,"2010-04-01",[],
     {twilio_called,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     {twilio_caller,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_from,"07776251669",[],[],[],"United Kingdom","GB","+44"},
     {twilio_to,"01315101875",[],[],"Edinburgh","United Kingdom","GB",
      "+44"},
     null,
     {twilio_inprogress,"*",undefined},
     {twilio_recording,"REc44d27ac765c0129e9cd819409735543","5",
      "http://api.twilio.com/2010-04-01/Accounts/AC7a076e30da6d49119b335d3a6de43844/Recordings/REc44d27ac765c0129e9cd819409735543"}}.

start_outbound_call(SID) ->
    {twilio,"AC7a076e30da6d49119b335d3a6de43844",
     "AP93d273f3cc624008805842376d561bed","inbound","ringing",
     SID,"2010-04-01",
     [{"hypertag",
       "f8a6711f90a55a09fa54410966863564b97493ad20742b0854601dcf6602d319a441041b334e53428f08c0d97d0f6c9ce568a23ed66b0d6d66e0bbde40bfabc3d770d2043cd7b5fc75f8f9225f6aa9127bdc22c6a87e410f3d2bbd3cc8ea0d03f2ec663145f6958c9358c00e775cdae09b487f43d86e2fcb8683a06abc1fc53d6ae7d7c095cc307cb6be17d623cef9d3"},
         {"site","http://hypernumbers.dev:9000"}],
     null,null,null,null,null,null,null}.

