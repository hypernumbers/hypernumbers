%%%-------------------------------------------------------------------
%%% @author Ryan Huffman <ryanhuffman@gmail.com>
%%% @copyright 2011,
%%% @doc Encodes twilio records into an XML document.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(twiml).

-export([
         encode/1,
         validate/1,
         is_valid/1,
         compile/1
        ]).

-export([
         testing/0
        ]).

-include("twilio.hrl").

%% @doc Encodes a set of twiml records as an XML document.
encode(Elements) ->
    Content = [{'Response', [], [to_xmerl_element(El) || El <- Elements]}],
    xmerl:export_simple(Content, xmerl_xml).

%% @doc Converts a twiml record to an xmerl XML element.
to_xmerl_element(#say{} = Say) ->
    Attrs = [{voice,    Say#say.voice},
             {language, Say#say.language},
             {loop,     Say#say.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Say', CleanAttrs, [Say#say.text]};
to_xmerl_element(#play{} = Play) ->
    Attrs = [{loop, Play#play.loop}],
    CleanAttrs = remove_undefined(Attrs),
    {'Play', CleanAttrs, [Play#play.url]};
to_xmerl_element(#gather{} = Gather) ->
    Attrs = [{action,      Gather#gather.action},
             {method,      Gather#gather.method},
             {timeout,     Gather#gather.timeout},
             {finishOnKey, Gather#gather.finish_on_key},
             {numDigits,   Gather#gather.num_digits}],
    CleanAttrs = remove_undefined(Attrs),
    Body = [to_xmerl_element(Element) || Element <- Gather#gather.body],
    {'Gather', CleanAttrs, Body};
to_xmerl_element(#record{} = Record) ->
    Attrs = [{action,             Record#record.action},
             {method,             Record#record.method},
             {timeout,            Record#record.timeout},
             {finishOnKey,        Record#record.finish_on_key},
             {maxLength,          Record#record.max_length},
             {transcribe,         Record#record.transcribe},
             {transcribeCallback, Record#record.transcribe_callback},
             {playBeep,           Record#record.play_beep}],
    CleanAttrs = remove_undefined(Attrs),
    {'Record', CleanAttrs, []};
to_xmerl_element(#sms{} = Sms) ->
    Attrs = [{to,             Sms#sms.to},
             {from,           Sms#sms.from},
             {action,         Sms#sms.action},
             {method,         Sms#sms.method},
             {statusCallback, Sms#sms.status_callback}],
    CleanAttrs = remove_undefined(Attrs),
    {'Sms', CleanAttrs, [Sms#sms.text]};
to_xmerl_element(#dial{} = Dial) ->
    Attrs = [{action,       Dial#dial.action},
             {method,       Dial#dial.method},
             {timeout,      Dial#dial.timeout},
             {hangupOnStar, Dial#dial.hangup_on_star},
             {timeLimit,    Dial#dial.time_limit},
             {callerId,     Dial#dial.caller_id},
             {record,       Dial#dial.record}],
    CleanAttrs = remove_undefined(Attrs),

    case is_list(Dial#dial.body) of
        true ->
            Body = [Dial#dial.body];
        false ->
            Body = [to_xmerl_element(Dial#dial.body)]
    end,
    {'Dial', CleanAttrs, Body};
to_xmerl_element(#number{} = Number) ->
    Attrs = [{sendDigits, Number#number.send_digits},
             {url, Number#number.url}],
    CleanAttrs = remove_undefined(Attrs),
    {'Number', CleanAttrs, [Number#number.number]};
to_xmerl_element(#redirect{} = Redirect) ->
    Attrs = [{method, Redirect#redirect.method}],
    CleanAttrs = remove_undefined(Attrs),
    {'Redirect', CleanAttrs, [Redirect#redirect.url]};
to_xmerl_element(#pause{} = Pause) ->
    Attrs = [{length, Pause#pause.length}],
    CleanAttrs = remove_undefined(Attrs),
    {'Pause', CleanAttrs, []};
to_xmerl_element(#hangup{}) ->
    {'Hangup', [], []};
to_xmerl_element(#reject{} = Reject) ->
    Attrs = [{reason, Reject#reject.reason}],
    CleanAttrs = remove_undefined(Attrs),
    {'Reject', CleanAttrs, []};
to_xmerl_element(#client{} = C) ->
    {'Client', [], [C#client.client]};
to_xmerl_element(#conference{} = C) ->
        Attrs = [{muted,                  C#conference.muted},
                 {beep,                   C#conference.beep},
                 {startConferenceOnEnter, C#conference.startConferenceOnEnter},
                 {endConferenceOnExit,    C#conference.endConferenceOnExit},
                 {waitUrl,                C#conference.waitUrl},
                 {waitMethod,             C#conference.waitMethod},
                 {maxParticipants,        C#conference.maxParticipants}],
    CleanAttrs = remove_undefined(Attrs),
    {'Conference', CleanAttrs, [C#conference.conference]}.

%% @doc Removes any undefined attributes.
remove_undefined(Attrs) ->
    [Attr || {_, Value} = Attr <- Attrs, Value =/= undefined].

compile(Elements) ->
    {Type, Recs} = comp2(Elements, 'no-state', []),
    {Type, lists:flatten(encode(Recs))}.

comp2([], Type, Acc) ->
    {Type, lists:reverse(Acc)};
comp2([H | T], Type, Acc)  when is_record(H, say)
                                orelse is_record(H, play)
                                orelse is_record(H, record)
                                orelse is_record(H, number)
                                orelse is_record(H, dial)
                                orelse is_record(H, sms)
                                orelse is_record(H, redirect)
                                orelse is_record(H, pause)
                                orelse is_record(H, hangup)
                                orelse is_record(H, reject) ->
    comp2(T, Type, [H | Acc]);
comp2([#gather{} = H | T], _Type, Acc) ->
    {_, NewBody} = comp2(H#gather.body, state, []),
    NewH = H#gather{body = NewBody},
    comp2(T, state, [NewH | Acc]).

check(#say{} = Say, Acc) ->
    NewAcc = is_member("#say{}", [{voice,    Say#say.voice,    ?SAYVoices},
                                  {language, Say#say.language, ?SAYLanguages}],
                       Acc),
    check_int(Say#say.loop, ?SAYLoopMin, "loop", "#say{}", NewAcc);
% should really check the url as well
check(#play{} = Play, Acc) ->
    check_int(Play#play.loop, ?PLAYLoopMin, "loop", "#play{}", Acc);
check(#gather{} = G, Acc) ->
    NAcc = is_member("#gather{}",
                     [{gather, G#gather.method,        ?GATHERMethod},
                      {gather, G#gather.finish_on_key, ?GATHERFOnKey}],
                     Acc),
    NAcc2 = check_int(G#gather.timeout, ?GATHERTimeoutMin, "timeout",
                      "#gather{}", NAcc),
    NAcc3 = check_int(G#gather.num_digits, ?GATHERTimeoutMin, "num_digits",
                      "#gather{}", NAcc2),
    lists:foldl(fun check_gather/2, NAcc3, G#gather.body);
check(#record{} = R, Acc) ->
    NAcc = is_member("#record{}",
                     [{record, R#record.method,        ?RECORDMethod},
                      {record, R#record.finish_on_key, ?RECORDFOnKey}],
                     Acc),
    NAcc2 = check_bool(R#record.transcribe, "transcribe", "#record{}", NAcc),
    NAcc3 = check_bool(R#record.play_beep,  "play_beeb",  "#record{}", NAcc2),
    NAcc4 = check_int(R#record.timeout, ?RECORDTimeoutMin, "timeout",
                      "#record{}", NAcc3),
    check_int(R#record.max_length, ?RECORDMaxLen, "max_length",
              "#record{}", NAcc4);
% we assume that the phone number is properly formatted
% even though they yanks at twilio accept 'Merican formatted
% numbers as well
check(#number{} = N, Acc) ->
    NAcc = check_send_digits(N#number.send_digits, Acc),
    check_phone_no(N#number.number, "number", "#number{}", NAcc);
check(#dial{} = D, Acc) ->
    NAcc = is_member("#dial{}", [{method, D#dial.method, ?DIALMethod}], Acc),
    NAcc2 = check_int(D#dial.timeout, ?DIALTimeoutMin, "timeout",
                      "#dial{}", NAcc),
    NAcc3 = check_int(D#dial.time_limit, ?DIALTimeLimitMin, "time_limit",
                      "#dial{}", NAcc2),
    NAcc4 = check_bool(D#dial.hangup_on_star, "hangup_on_star",
                       "#dial{}", NAcc3),
    NAcc5 = check_bool(D#dial.record, "#record{}",
               "#dial{}", NAcc4),
    lists:foldl(fun check_dial/2, NAcc5, D#dial.body);
% we assume that the phone numbers (to and from) are properly formatted
% even though they yanks at twilio accept 'Merican formatted
% numbers as well
check(#sms{} = S, Acc) ->
    NAcc = is_member("#sms{}", [{method, S#sms.method, ?SMSMethod}], Acc),
    NAcc2 = check_phone_no(S#sms.to, "to", "#sms{}", NAcc),
    check_phone_no(S#sms.from, "from", "#sms{}", NAcc2);
check(#redirect{} = R, Acc) ->
    is_member("#redirect{}", [{method, R#redirect.method, ?REDIRECTMethod}], Acc);
check(#pause{} = P, Acc) ->
    check_int(P#pause.length, ?PAUSELengthMin, "length", "#pause{}", Acc);
check(#hangup{}, Acc) ->
    Acc;
check(#reject{} = R, Acc) ->
    is_member("#reject{}", [{method, R#reject.reason, ?REJECTReason}], Acc);
% check is a noun with no attributes
check(#client{} = C, Acc) ->
    if
        is_list(C#client.client) ->
            Acc;
        true ->
            [io_lib:format("Invalid client in #client{} ~p~n",
                           [C#client.client]) | Acc]
    end;
check(#conference{} = C, Acc) ->
    NAcc = is_member("#conference", [{conference, C#conference.waitMethod,
                                      ?CONFERENCEWaitMethod}],
                     Acc),
    NAcc2 = check_bool(C#conference.muted, "muted", "#conference{}", NAcc),
    NAcc3 = check_bool(C#conference.beep, "beep", "#conference{}", NAcc2),
    NAcc4 = check_bool(C#conference.startConferenceOnEnter,
                       "startConferenceOnEnter", "#conference{}", NAcc3),
    NAcc5 = check_bool(C#conference.endConferenceOnExit,
                       "endConferenceOnExit", "#conference{}", NAcc4),
    MaxP = C#conference.maxParticipants,
    NAcc6 = if MaxP == undefined ->
                    NAcc5;
               MaxP =< ?CONFERENCEMaxParticipants
               andalso MaxP >= ?CONFERENCEMinParticipants ->
            NAcc5;
               true ->
                    [io_lib:format("Invalid maxPartipants in #conference{} ~p~n",
                                   [MaxP]) | Acc]
            end,
    if
        is_list(C#conference.conference) ->
            NAcc6;
        true ->
            [io_lib:format("Invalid conference in #conference{} ~p~n",
                           [C#conference.conference]) | Acc]
    end.

% can gather #say, #play and #pause
check_gather(#say{}   = S, Acc) -> check(S, Acc);
check_gather(#play{}  = P, Acc) -> check(P, Acc);
check_gather(#pause{} = P, Acc) -> check(P, Acc);
% anything else is wrong
check_gather(Rec, Acc) ->
    El = element(1, Rec),
    [io_lib:format("Can't #gather ~p~n", [El]) | Acc].

% can dial #number, #client and #conference
check_dial(#number{}     = S, Acc) -> check(S, Acc);
check_dial(#client{}     = P, Acc) -> check(P, Acc);
check_dial(#conference{} = P, Acc) -> check(P, Acc);
% anything else is wrong
check_dial(Rec, Acc) ->
    El = element(1, Rec),
    [io_lib:format("Can't #dial ~p~n", [El]) | Acc].

check_send_digits(undefined, Acc) ->
    Acc;
check_send_digits(String, Acc) ->
    S2 = string:strip(String, left, $w),
    check_int_as_str(S2, "send_digit", "#number{}", Acc).

check_phone_no("+" ++ Num, Fld, Rec, Acc) ->
    check_int_as_str(Num, Fld, Rec, Acc);
check_phone_no(Val, Fld, Rec, Acc) ->
    [io_lib:format("Invalid ~p in ~p ~p~n", [Fld, Rec, Val]) | Acc].

check_int_as_str(undefined, _, _, Acc) ->
    Acc;
check_int_as_str(Str, Fld, Rec, Acc) ->
    IsValid = try list_to_integer(Str)
              catch
                  error: _ ->
                      false
              end,
    case IsValid of
        false ->
            [io_lib:format("Invalid ~p in ~p ~p~n", [Fld, Rec, Str])
             | Acc];
        _ ->
            Acc
    end.

check_bool(undefined, _Fld, _Rec, Acc) ->
    Acc;
check_bool(Val, Fld, Rec, Acc) ->
    case string:to_lower(Val) of
        "true" ->
            Acc;
        "false" ->
            Acc;
        _Other ->
            [io_lib:format("Invalid ~p in ~p ~p~n", [Fld, Rec, Val]) | Acc]
    end.

check_int(undefined, _Min, _Fld, _Rec, Acc) ->
    Acc;
check_int(Val, Min, Fld, Rec, Acc) ->
    if
        is_integer(Val) andalso Val >= Min ->
            Acc;
        true ->
            [io_lib:format("Invalid ~p in ~p ~p~n", [Fld, Rec, Val]) | Acc]
    end.

is_member(_, [], Acc) ->
    Acc;
is_member(Label, [{_SubLab, undefined, _Vs} | T], Acc) ->
    is_member(Label, T, Acc);
is_member(Label, [{SubLab, K, Vs} | T], Acc) ->
    NewAcc = case lists:member(string:to_lower(K), Vs) of
                 true  -> Acc;
                 false -> [io_lib:format("Invalid ~s in ~s ~p~n",
                                         [SubLab, Label, K]) | Acc]
             end,
    is_member(Label, T, NewAcc).

is_valid(Elements) ->
    case validate(Elements) of
        {ok, ok} -> true;
        _        -> false
    end.

validate(Elements) ->
    case lists:flatten(lists:reverse(lists:foldl(fun check/2, [], Elements))) of
        []  -> {ok, ok};
        Err -> {error, Err}
    end.

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(XML(D), "<?xml version=\"1.0\"?><Response>"D"</Response>").

encode_test_() ->
    [{"say twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Say></Say>"),
                 lists:flatten(encode([#say{}]))),
              ?assertEqual(
                 ?XML("<Say>Hello!</Say>"),
                 lists:flatten(encode([#say{text="Hello!"}]))),
              ?assertEqual(
                 ?XML("<Say language=\"en\" loop=\"2\">Hello!</Say>"),
                 lists:flatten(encode([#say{text="Hello!", loop=2, language="en"}])))
      end},
     {"play twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Play></Play>"),
                 lists:flatten(encode([#play{}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\"></Play>"),
                 lists:flatten(encode([#play{loop=2}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                 lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}]))),
              ?assertEqual(
                 ?XML("<Play loop=\"2\">https://someurlhere.com/blah</Play>"),
                 lists:flatten(encode([#play{url="https://someurlhere.com/blah", loop=2}])))
      end},
     {"gather twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Gather/>"),
                 lists:flatten(encode([#gather{}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\"/>"),
                 lists:flatten(encode([#gather{action="eval_gather"}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\" timeout=\"60\" numDigits=\"10\"/>"),
                 lists:flatten(encode([#gather{action="eval_gather", num_digits=10, timeout=60}]))),
              ?assertEqual(
                 ?XML("<Gather action=\"eval_gather\"><Say>Hello</Say></Gather>"),
                 lists:flatten(encode([#gather{action="eval_gather", body=[#say{text="Hello"}]}])))
      end},
     {"record twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Record/>"),
                 lists:flatten(encode([#record{}]))),
              ?assertEqual(
                 ?XML("<Record action=\"eval_reCOrd\"/>"),
                 lists:flatten(encode([#record{action="eval_reCOrd"}]))),
              ?assertEqual(
                 ?XML("<Record action=\"eval_record\" method=\"GET\" timeout=\"10\"/>"),
                 lists:flatten(encode([#record{action="eval_record", timeout=10, method='GET'}])))
      end},
     {"sms twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Sms></Sms>"),
                 lists:flatten(encode([#sms{}]))),
              ?assertEqual(
                 ?XML("<Sms>Hello!</Sms>"),
                 lists:flatten(encode([#sms{text="Hello!"}]))),
              ?assertEqual(
                 ?XML("<Sms action=\"default/sms\">Hello!</Sms>"),
                 lists:flatten(encode([#sms{text="Hello!", action="default/sms"}])))
      end},
     {"dial twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Dial></Dial>"),
                 lists:flatten(encode([#dial{}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\">1234833</Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff", body="1234833"}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\"><Number>1234833</Number></Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff", body=#number{number="1234833"}}]))),
              ?assertEqual(
                 ?XML("<Dial action=\"/do_stuff\"><Number sendDigits=\"9528\">1234833</Number></Dial>"),
                 lists:flatten(encode([#dial{action="/do_stuff",
                                             body=#number{send_digits="9528", number="1234833"}}])))
      end},
     {"composite twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Say>Hello! This is a voice message</Say><Say>This is another message</Say><Dial>48321523</Dial>"),
                 lists:flatten(encode([#say{text="Hello! This is a voice message"},
                                       #say{text="This is another message"},
                                       #dial{body="48321523"}])))
      end},
     {"redirect twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Redirect></Redirect>"),
                 lists:flatten(encode([#redirect{}]))),
              ?assertEqual(
                 ?XML("<Redirect>/do_stuff</Redirect>"),
                 lists:flatten(encode([#redirect{url="/do_stuff"}])))
      end},
     {"pause twiml",
      fun() ->
              ?assertEqual(
                 ?XML("<Pause/>"),
                 lists:flatten(encode([#pause{}]))),
              ?assertEqual(
                 ?XML("<Pause length=\"10\"/>"),
                 lists:flatten(encode([#pause{length=10}])))
      end}
    ].

compile_test_() ->
    [
     {"simple_say",
      fun() ->
              ?assertEqual(
                 {'no-state', ?XML("<Say></Say>")},
                 compile([#say{}])
                )
      end
     }
    ].

validate_test_() ->
    [
     % SAY passing
     ?_assertEqual(true, is_valid([#say{}])),
     ?_assertEqual(true, is_valid([#say{loop=3}])),
     ?_assertEqual(true, is_valid([#say{language="de"}])),
     ?_assertEqual(true, is_valid([#say{language="En"}])),
     % SAY failing
     ?_assertEqual(false, is_valid([#say{voice="benny"}])),
     ?_assertEqual(false, is_valid([#say{language="klingon"}])),
     ?_assertEqual(false, is_valid([#say{loop="3"}])),
     ?_assertEqual(false, is_valid([#say{loop="dd"}])),
     ?_assertEqual(false, is_valid([#say{loop=1.1}])),
     ?_assertEqual(false, is_valid([#say{loop=-9}])),

     % PLAY passing
     ?_assertEqual(true, is_valid([#play{}])),
     ?_assertEqual(true, is_valid([#play{loop = 7}])),
     % PLAY failing
     ?_assertEqual(false, is_valid([#play{loop = "fsfs"}])),

     % GATHER passing
     ?_assertEqual(true, is_valid([#gather{}])),
     ?_assertEqual(true, is_valid([#gather{method = "pOsT"}])),
     ?_assertEqual(true, is_valid([#gather{timeout = 3}])),
     ?_assertEqual(true, is_valid([#gather{finish_on_key = "*"}])),
     ?_assertEqual(true, is_valid([#gather{num_digits = 333}])),
     ?_assertEqual(true, is_valid([#gather{body = [#play{}, #say{}, #pause{}]}])),
     % GATHER failing
     ?_assertEqual(false, is_valid([#gather{method = "panda"}])),
     ?_assertEqual(false, is_valid([#gather{timeout = -3}])),
     ?_assertEqual(false, is_valid([#gather{finish_on_key = "^"}])),
     ?_assertEqual(false, is_valid([#gather{num_digits = 333.45}])),
     ?_assertEqual(false, is_valid([#gather{body = [#dial{}, #number{}]}])),
     ?_assertEqual(false, is_valid([#gather{body = [#play{loop = "erk"},
                                                    #say{voice = "bandy"}]}])),

     % RECORD passing
     ?_assertEqual(true, is_valid([#record{}])),
     ?_assertEqual(true, is_valid([#record{method = "Post"}])),
     ?_assertEqual(true, is_valid([#record{timeout = 2}])),
     ?_assertEqual(true, is_valid([#record{max_length = 123}])),
     ?_assertEqual(true, is_valid([#record{finish_on_key = "*"}])),
     ?_assertEqual(true, is_valid([#record{transcribe = "tRue"}])),
     ?_assertEqual(true, is_valid([#record{play_beep = "False"}])),
     % RECORD failing
     ?_assertEqual(false, is_valid([#record{method = "Pongo"}])),
     ?_assertEqual(false, is_valid([#record{timeout = 0}])),
     ?_assertEqual(false, is_valid([#record{max_length = 123.44}])),
     ?_assertEqual(false, is_valid([#record{finish_on_key = "&"}])),
     ?_assertEqual(false, is_valid([#record{transcribe = "tRuth"}])),
     ?_assertEqual(false, is_valid([#record{play_beep = "Farts"}])),

     % NUMBER passing
     ?_assertEqual(true, is_valid([#number{send_digits = "ww234",
                                           number="+123"}])),
     ?_assertEqual(true, is_valid([#number{number = "+234"}])),
     % NUMBER failing
     ?_assertEqual(false, is_valid([#number{}])),
     ?_assertEqual(false, is_valid([#number{send_digits = "dww234",
                                            number="+123"}])),
     ?_assertEqual(false, is_valid([#number{number = "234"}])),

     % DIAL passing
     ?_assertEqual(true, is_valid([#dial{}])),
     ?_assertEqual(true, is_valid([#dial{method = "GeT"}])),
     ?_assertEqual(true, is_valid([#dial{timeout = 3}])),
     ?_assertEqual(true, is_valid([#dial{hangup_on_star = "trUE"}])),
     ?_assertEqual(true, is_valid([#dial{time_limit = 4}])),
     ?_assertEqual(true, is_valid([#dial{record = "fAlse"}])),
     % DIAL failing
     ?_assertEqual(false, is_valid([#dial{method = "GeT=rund"}])),
     ?_assertEqual(false, is_valid([#dial{timeout = "33"}])),
     ?_assertEqual(false, is_valid([#dial{hangup_on_star = "banjo"}])),
     ?_assertEqual(false, is_valid([#dial{time_limit = -4}])),
     ?_assertEqual(false, is_valid([#dial{record = "fAlsies"}])),

     % SMS passiing
     ?_assertEqual(true, is_valid([#sms{from = "+123", to = "+345"}])),
     ?_assertEqual(true, is_valid([#sms{method = "GEt",
                                         from = "+123", to = "+345"}])),
     % SMS failing
     ?_assertEqual(false, is_valid([#sms{}])),
     ?_assertEqual(false, is_valid([#sms{method = "GEt",
                                         from = "123", to = "+345"}])),
     ?_assertEqual(false, is_valid([#sms{method = "GEt",
                                         from = "+123", to = "345"}])),
     ?_assertEqual(false, is_valid([#sms{method = "Git",
                                         from = "+123", to = "+345"}])),

     % REDIRECT passing
     ?_assertEqual(true, is_valid([#redirect{method = "GEt"}])),
     % REDIRECT failing
     ?_assertEqual(false, is_valid([#redirect{method = "Git"}])),

     % PAUSE passing
     ?_assertEqual(true, is_valid([#pause{}])),
     ?_assertEqual(true, is_valid([#pause{length = 3}])),
     % PAUSE failing
     ?_assertEqual(false, is_valid([#pause{length = -3}])),

     % HANGUP passing
     ?_assertEqual(true, is_valid([#hangup{}])),

     % REJECT passing
     ?_assertEqual(true, is_valid([#reject{}])),
     ?_assertEqual(true, is_valid([#reject{reason = "BuSy"}])),
     % REJECT failing
     ?_assertEqual(false, is_valid([#reject{reason = "BuSTy"}])),

     % CLIENT passing
     ?_assertEqual(true, is_valid([#client{client = "adb"}])),

     % CLIENT failing
     ?_assertEqual(false, is_valid([#client{}])),
     ?_assertEqual(false, is_valid([#client{client = 33}])),

     % CONFERENCE passing
     ?_assertEqual(true, is_valid([#conference{conference = "a32"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               muted = "tRUe"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               beep = "fALse"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               startConferenceOnEnter = "trUe"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               endConferenceOnExit = "FAlSE"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               waitMethod = "pOst"}])),
     ?_assertEqual(true, is_valid([#conference{conference = "a32",
                                               maxParticipants = 34}])),

     % CONFERENCE failing
     ?_assertEqual(false, is_valid([#conference{}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                muted = "filibuters"}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                beep = "Trudy"}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                startConferenceOnEnter = "f"}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                endConferenceOnExit = "erk"}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                waitMethod = "pOster boy"}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                maxParticipants = 1}])),
     ?_assertEqual(false, is_valid([#conference{conference = "a32",
                                                maxParticipants = 41}]))
    ].

%-endif.

testing() ->
    A = #number{},
    validate([A]).
