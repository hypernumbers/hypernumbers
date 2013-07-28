%%% @author    Gordon Guthrie                         >
%%% @copyright (C) 2013, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2013 by <gordon@vixo.com>

-module(mandrill).

-export([
         ping/0,
         testing/0
        ]).

-export([
         email/5,
         email/6
        ]).

ping() ->
    {ok, Server}   = application:get_env(hypernumbers, mandrillserver),
    {ok, APIKey}   = application:get_env(hypernumbers, mandrillapikey),

    Page = "users/ping.json",

    Json = {struct, [{"key", APIKey}]},

    Accept = [{"Accept", "application/json"}],
    Type = "application/json",

    Json2 = mochijson:encode(Json),

    URL = Server ++ Page,

    io:format("URL is ~p~n", [URL]),

    R = httpc:request(post, {URL, Accept, Type, lists:flatten(Json2)}, [], []),
    io:format("R is ~p~n", [R]),
    ok.


testing() ->

    {ok, Server}   = application:get_env(hypernumbers, mandrillserver),
    {ok, APIKey}   = application:get_env(hypernumbers, mandrillapikey),

    io:format("Server is ~p APIKey is ~p~n", [Server, APIKey]),

    Page = "messages/send-template.json",

    To       = "gordon@vixo.com",
    ToName   = "Gordon Guthrie",
    From     = "robot@vixo.com",
    FromName = "Vixo Robot",
    ReplyTo  = "gordon@vixo.com",

    HighRiseEmail = "dropbox@66774760.vixo1.highrisehq.com",
    _TodayTask     = "task+today@66774760.vixo1.highrisehq.com",
    _TomorrowTask  = "task+tomorrow@66774760.vixo1.highrisehq.com",
    _ThisWeekTask  = "task+thisweek@66774760.vixo1.highrisehq.com",
    _NextWeekTask  = "task+nextweek@66774760.vixo1.highrisehq.com",
    _LaterTask     = "task+later@66774760.vixo1.highrisehq.com",

    Campaign = "site commissioning email",

    SiteCommissioned = "some site",
    TypeCommissioned = "blank",

    UID = "12345",

    GlobalVars = [
                  {struct, [
                            {"LNAME", "Big Boy!"},
                            {"SITENAME", "http://yer.new.site.com"},
                            {"SITETYPE", "blank"},
                            {"URLSIGNIN", "http://example.com"}
                           ]
                  }
                 ],

    MergeVars =  [
                  %% {struct,
                  %%  [
                  %%   {"rcpt", To},
                  %%   {"vars",
                  %%    {array, [
                  %%            ]
                  %%    }
                  %%   }
                  %%  ]
                  %% }
                 ],

    Attachments = {"attachments",
                   {array,
                    [
                     % {struct,
                     %  [
                     %   {"type", "text/plain"},
                     %   {"name", "myfile.txt"},
                     %   {"content", "ZXhhbXBsZSBmaWxl"}
                     %   ]
                     % }
                    ]
                   }
                  },

    Images =  {"images",
               {array,
                [
                 % {struct,
                 %  [
                 %   {"type", "image/png"},
                 %   {"name", "IMAGECID"},
                 %   {"content", "ZXhhbXBsZSBmaWxl"}
                 %  ]
                 % }
                ]
               }
              },

    Json = {struct, [
                     {"key", APIKey},
                     {"template_name", "commission"},
                     {"template_content",
                      {array, [
                              ]
                      }
                     },
                     {"message",
                      {struct, [
                                {"html", "<p>Example HTML content</p>"},
                                {"text", "Example text content"},
                                {"subject", "Commissioned a new site"},
                                {"from_email", From},
                                {"from_name", FromName},
                                {"to",
                                 {array, [
                                          {struct, [
                                                    {"email", To},
                                                    {"name", ToName}
                                                   ]
                                          }
                                         ]
                                 }
                                },
                                {"headers",
                                 {struct,
                                  [
                                   {"Reply-To", ReplyTo}
                                  ]
                                 }
                                },
                                {"important", false},
                                {"track_opens", null},
                                {"track_clicks", null},
                                {"auto_text", null},
                                {"auto_html", null},
                                {"inline_css", null},
                                {"url_strip_qs", null},
                                {"preserve_recipients", null},
                                {"bcc_address", HighRiseEmail},
                                {"tracking_domain", null},
                                {"signing_domain", null},
                                {"merge", true},
                                {"global_merge_vars",
                                 {array,
                                  GlobalVars
                                 }
                                },
                                {"merge_vars",
                                 {array,
                                  MergeVars
                                 }
                                },
                                {"tags",
                                 {array,
                                  [
                                   "commission site"
                                  ]
                                 }
                                },
                                {"google_analytics_domains",
                                 {array, [
                                          "vixo.com"
                                         ]
                                 }
                                },
                                {"google_analytics_campaign", Campaign},
                                {"metadata",
                                 {struct, [
                                           {"site commissioned", SiteCommissioned},
                                           {"type commissioned", TypeCommissioned}
                                          ]
                                 }
                                },
                                {"recipient_metadata",
                                 {array, [
                                          {struct, [
                                                    {"rcpt", To},
                                                    {"values",
                                                     {struct, [
                                                               {"UID", UID}
                                                              ]
                                                     }
                                                    }
                                                   ]
                                          }
                                         ]
                                 }
                                },
                                Attachments,
                                Images
                               ]
                       }
                      },
                       {"async", false}
                      ]
                     },

                     Accept = [{"Accept", "application/json"}],
                     Type = "application/json",

                     Json2 = mochijson:encode(Json),
                     io:format("Json2 is ~p~n", [lists:flatten(Json2)]),

                     URL = Server ++ Page,

                     io:format("URL is ~p~n", [URL]),

                     R = httpc:request(post, {URL, Accept, Type, lists:flatten(Json2)}, [], []),
                     io:format("R is ~p~n", [R]),
                     ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Email
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

email(To, CC, From, Subject, Msg) ->
    {ok, Server}   = application:get_env(hypernumbers, mandrillserver),
    {ok, Password} = application:get_env(hypernumbers, mandrillpassword),
    {ok, User}     = application:get_env(hypernumbers, mandrilluser),
    email([{server, Server}, {user, User}, {password, Password}],
          To, CC, From, Subject, Msg).

email(Details, To, _CC, _From, _Subject, _Msg) ->

    _Tos = string:tokens(To, ";"),

    _Server = proplists:get_value(server, Details),
    _User   = proplists:get_value(user, Details),
    _Pass   = proplists:get_value(password, Details),
    ok.

