%%%-----------------------------------------------------------------------------
%%% File        : http_get_spec_test_suite.erl
%%% Author      : Gordon Guthrie gordon@vixo.com
%%% Description : http GET authorization tests
%%%
%%% Created     : 10th March 2013 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(http_get_spec_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Fn = case Status of
                        loggedinjson          ->
                            get_logged_in_json_TEST;
                        loggedinjsonnotadmin  ->
                            get_logged_in_json_notadmin_TEST;
                        loggedoutjson         ->
                            get_logged_out_json_TEST;
                        loggedinhtml          ->
                            get_logged_in_html_TEST;
                        loggedinhtmlnotadmin  ->
                            get_logged_in_notadmin_html_TEST;
                        loggedouthtml         ->
                            get_logged_out_html_TEST
                    end,
               Got = test:Fn(URL),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

-define(tapi(Name, Path, Expected, Status, Accept),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Got = test:get_api_TEST(URL, Status, Accept),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

%% callbacks
init_per_suite(Config)               -> Config.
end_per_suite(_Config)               -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

read_config(Key) ->
    Root                   = code:lib_dir(hypernumbers)++"/../../",
    {ok, [Config]}         = file:consult([Root, "/var/", "sys.config"]),
    {hypernumbers, HNConf} = lists:keyfind(hypernumbers, 1, Config),
    {Key, Val}             = lists:keyfind(Key, 1, HNConf),
    Val.

all2() ->
    [
    ].

%% tests to run
all() ->
    [
     % special pages
     get_invite2,
     get_invite_root3,
     get_invite3,
     get_mynewsite_root1,
     get_mynewsite1,
     get_mynewsite_root2,
     get_mynewsite2,
     get_mynewsite_root3,
     get_mynewsite3,
     get_validate_root1,
     get_validate1,
     get_validate_root2,
     get_validate2,
     get_validate_root3,
     get_validate3,
     get_authorize_root1,
     get_authorize1,
     get_authorize_root2,
     get_authorize2,
     get_authorize_root3,
     get_authorize3,
     get_logout_root1,
     get_logout1,
     get_logout_root2,
     get_logout2,
     get_logout_root3,
     get_logout3,

     % site structure pages
     get_site_root1,
     get_site_root1a,
     get_site_root2,
     get_site_root2a,
     get_site_root3,
     get_site_root3a,
     get_site_root3b,
     get_site_root3c,
     get_pages_root1,
     get_pages_root2,
     get_pages_root3,
     get_pages_root3a,

     % statistics
     get_statistics_root1,
     get_statistics_root2,
     get_statistics_root3,
     get_statistics_root3a,

%% conventional pages like /_sites, /_replies and /_contacts
%% off the main page for forms and phones and stuff
%% normally all pages beginning with '_' under root are
%% not creatable
     get_underscore_sites1,
     get_underscore_sites2,
     get_underscore_sites3,
     get_underscore_sites3a,

     % audit pages
     get_underscore_audit_root1,
     get_underscore_audit_root1a,
     get_underscore_audit_root2,
     get_underscore_audit_root3,
     get_underscore_audit_root3a,
     get_underscore_audit1,
     get_underscore_audit1a,
     get_underscore_audit2,
     get_underscore_audit3,
     get_underscore_audit3a,

     % now phone stuff
     % need to work this stuff out before enabling them
     get_services_phone1,
     get_services_phone1a,
     get_services_phone2,
     get_services_phone2a,
     get_services_phone3,

     % not testing these
     %% get_services_phoneredirect1,
     %% get_services_phoneredirect1a,
     %% get_services_phoneredirect2,
     %% get_services_phoneredirect2a,
     %% get_services_phoneredirect3,

     % reprovisioning usability site
     get_reprovision1,
     get_reprovision1a,
     get_reprovision2,
     get_reprovision2a,
     get_reprovision3,
     get_reprovision3a,

     % forgotten password
     get_forgotten_pwd_root1,
     get_forgotten_pwd_root1a,
     get_forgotten_pwd_root2,
     get_forgotten_pwd_root2a,
     get_forgotten_pwd_root3,
     get_forgotten_pwd_root3a,

     get_forgotten_pwd1,
     get_forgotten_pwd1a,
     get_forgotten_pwd2,
     get_forgotten_pwd2a,
     get_forgotten_pwd3,

     % finally we get to some real tests :(
     get_update1,
     get_update1a,
     get_update2,
     get_update2a,
     get_update3,
     get_update3a,

     % twilio stuff
     get_twilio_recording1,
     get_twilio_recording1a,
     get_twilio_recording2,
     get_twilio_recording2a,
     get_twilio_recording3,

     % debug stuff
     get_debug1,
     get_debug1x,
     get_debug1a,
     get_debug2,
     get_debug2a,
     get_debug3,

     % recalc stuff
     get_recalc1,
     get_recalc2,
     get_recalc2a,
     get_recalc3,

     % parse expressions
     get_parse_expression1,
     get_parse_expression1a,
     get_parse_expression2,
     get_parse_expression2a,
     get_parse_expression3,
     get_parse_expression3a,

     get_login1,
     get_login1a,
     get_login2,
     get_login2a,
     get_login3,
     get_login3a
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
%% The special pages tests
?test(get_invite_root1, "/_invite/",  401, loggedinhtml).  % can't see that page
?test(get_invite1, "/_invite/bingo/", 500, loggedinhtml). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(get_invite_root2, "/_invite/",  401, loggedouthtml).  % can't see that page
?test(get_invite2, "/_invite/bingo/", 500, loggedouthtml). % no valid hypertag will cause it to crash (but it has been allowed...)

?tapi(get_invite_root3, "/_invite/",  401, api, html).  % can't see that page
?tapi(get_invite3, "/_invite/bingo/", 401, api, html). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(get_mynewsite_root1, "/_mynewsite/",  401, loggedinhtml).  % can't see that page
?test(get_mynewsite1, "/_mynewsite/bingo/", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_mynewsite_root2, "/_mynewsite/",  401, loggedouthtml).  % can't see that page
?test(get_mynewsite2, "/_mynewsite/bingo/", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_mynewsite_root3, "/_mynewsite/",  401, api, html).  % can't see that page
?tapi(get_mynewsite3, "/_mynewsite/bingo/", 401, api, html). % no valid hypertag will cause it to

?test(get_validate_root1, "/_validate/",  401, loggedinhtml).  % can't see that page
?test(get_validate1, "/_validate/bingo/", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_validate_root2, "/_validate/",  401, loggedouthtml).  % can't see that page
?test(get_validate2, "/_validate/bingo/", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_validate_root3, "/_validate/",  401, api, html).  % can't see that page
?tapi(get_validate3, "/_validate/bingo/", 401, api, html). % no valid hypertag will cause it to

?test(get_authorize_root1, "/_authorize/",  401, loggedinhtml).  % can't see that page
?test(get_authorize1, "/_authorize/bingo/", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_authorize_root2, "/_authorize/",  401, loggedouthtml).  % can't see that page
?test(get_authorize2, "/_authorize/bingo/", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_authorize_root3, "/_authorize/",  401, api, html).  % can't see that page
?tapi(get_authorize3, "/_authorize/bingo/", 401, api, html). % no valid hypertag will cause it to

?test(get_logout_root1, "/_logout/?return=" ++ ?SITE, 200, loggedinhtml).  % can see this page
?test(get_logout1, "/_logout/bingo/", 401, loggedinhtml). % no valid subpage

?test(get_logout_root2, "/_logout/?return=" ++ ?SITE, 200, loggedouthtml).  % can see this page
?test(get_logout2, "/_logout/bingo/", 401, loggedouthtml). % no valid subpage

?tapi(get_logout_root3, "/_logout/?return=" ++ ?SITE, 401, api, html).  % can see this page
?tapi(get_logout3, "/_logout/bingo/", 401, api, html). % no valid subpage

%% the generic site and pages stuff
?test(get_site_root1,  "/_site/", 200, loggedinjson).
?test(get_site_root1a, "/_site/", 401, loggedinhtml).
?test(get_site_root2,  "/_site/", 200, loggedoutjson).
?test(get_site_root2a, "/_site/", 401, loggedouthtml).
?tapi(get_site_root3,  "/_site/", 401, api, json).
?tapi(get_site_root3a, "/_site/", 401, api_subdirs, html).
?tapi(get_site_root3b, "/_site/", 401, api, json).
?tapi(get_site_root3c, "/_site/", 200, api_subdirs, json).

% same code as _site so don't bother to write lots of tests
?test(get_pages_root1,  "/_pages/", 200, loggedinjson).
?test(get_pages_root2,  "/_pages/", 200, loggedoutjson).
?tapi(get_pages_root3,  "/_pages/", 401, api, json).
?tapi(get_pages_root3a, "/_pages/", 200, api_subdirs, json).

%% statistics - only logged in admins should be able to see it
%% can't be arsed to write a special I am logged in as admin
%% test - that works, I know...
?test(get_statistics_root1,  "/_statistics/", 200, loggedinhtml).
?test(get_statistics_root2,  "/_statistics/", 401, loggedouthtml).
?tapi(get_statistics_root3,  "/_statistics/", 401, api, json).
?tapi(get_statistics_root3a, "/_statistics/", 401, api_subdirs, json).

% conventional site types under root
?test(get_underscore_sites_root1,  "/_sites/",                   200, loggedinhtml).
?test(get_underscore_sites_root1a, "/_sites/?view=spreadsheet",  200, loggedinhtml).
?test(get_underscore_sites_root1b, "/_sites/?view=webpage",      200, loggedinhtml).
?test(get_underscore_sites_root1c, "/_sites/?view=wikipage",     200, loggedinhtml).
?test(get_underscore_sites_root1d, "/_sites/?view=table",        200, loggedinhtml).
?test(get_underscore_sites_root2,  "/_sites/",                   401, loggedouthtml).
?test(get_underscore_sites_root2a, "/_sites/?view=spreadsheet",  401, loggedouthtml).
?test(get_underscore_sites_root2b, "/_sites/?view=webpage",      401, loggedouthtml).
?test(get_underscore_sites_root2c, "/_sites/?view=wikipage",     401, loggedouthtml).
?test(get_underscore_sites_root2d, "/_sites/?view=table",        401, loggedouthtml).
?tapi(get_underscore_sites_root3,  "/_sites/",                   401, api, json).
?tapi(get_underscore_sites_root3a, "/_sites/",                   200, api_subdirs, json).

?test(get_underscore_sites1,  "/_sites/blen/", 404, loggedinhtml).
?test(get_underscore_sites2,  "/_sites/blon/", 404, loggedouthtml).
?tapi(get_underscore_sites3,  "/_sites/blan/", 404, api, json).
?tapi(get_underscore_sites3a, "/_sites/blun/", 404, api_subdirs, json).

?test(get_underscore_audit_root1,  "/_audit/", 200, loggedinhtml).
?test(get_underscore_audit_root1a, "/_audit/", 200, loggedinjson).
?test(get_underscore_audit_root2,  "/_audit/", 401, loggedouthtml).
?tapi(get_underscore_audit_root3,  "/_audit/", 401, api, json).
?tapi(get_underscore_audit_root3a, "/_audit/", 200, api_subdirs, json).

?test(get_underscore_audit1,  "/_audit/blah/", 200, loggedinhtml).
?test(get_underscore_audit1a, "/_audit/blah/", 200, loggedinjson).
?test(get_underscore_audit2,  "/_audit/bleh/", 401, loggedouthtml).
?tapi(get_underscore_audit3,  "/_audit/bloh/", 401, api, json).
?tapi(get_underscore_audit3a, "/_audit/bluh/", 200, api_subdirs, json).

?test(get_services_phone1,  "/_services/phone/", 200, loggedinhtml).
?test(get_services_phone1a, "/_services/phone/", 401, loggedinjson).
?test(get_services_phone2,  "/_services/phone/", 200, loggedouthtml).
?test(get_services_phone2a, "/_services/phone/", 401, loggedoutjson).
?tapi(get_services_phone3,  "/_services/phone/", 401, api_subdirs, json).

%% ?test(get_services_phoneredirect1,  "/_services/phoneredirect/", 200, loggedinhtml).
%% ?test(get_services_phoneredirect1a, "/_services/phoneredirect/", 404, loggedinjson).
%% ?test(get_services_phoneredirect2,  "/_services/phoneredirect/", 401, loggedouthtml).
%% ?test(get_services_phoneredirect2a, "/_services/phoneredirect/", 401, loggedoutjson).
%% ?tapi(get_services_phoneredirect3,  "/_services/phoneredirect/", 401, api_subdirs, json).

% reprovision only works on usability site
?test(get_reprovision1,  "/_reprovision/", 401, loggedinhtml).
?test(get_reprovision1a, "/_reprovision/", 401, loggedinjson).
?test(get_reprovision2,  "/_reprovision/", 401, loggedouthtml).
?test(get_reprovision2a, "/_reprovision/", 401, loggedoutjson).
?tapi(get_reprovision3,  "/_reprovision/", 401, api, json).
?tapi(get_reprovision3a, "/_reprovision/", 401, api_subdirs, json).

% forgotten passport only works on some sites
?test(get_forgotten_pwd_root1,  "/_forgotten_password/", 200, loggedinhtml).
?test(get_forgotten_pwd_root1a, "/_forgotten_password/", 401, loggedinjson).
?test(get_forgotten_pwd_root2,  "/_forgotten_password/", 200, loggedouthtml).
?test(get_forgotten_pwd_root2a, "/_forgotten_password/", 401, loggedoutjson).
?tapi(get_forgotten_pwd_root3,  "/_forgotten_password/", 401, api, json).
?tapi(get_forgotten_pwd_root3a, "/_forgotten_password/", 401, api_subdirs, json).

?test(get_forgotten_pwd1,  "/_forgotten_password/blah/", 200, loggedinhtml).
?test(get_forgotten_pwd1a, "/_forgotten_password/blah/", 401, loggedinjson).
?test(get_forgotten_pwd2,  "/_forgotten_password/blah/", 200, loggedouthtml).
?test(get_forgotten_pwd2a, "/_forgotten_password/blah/", 401, loggedoutjson).
?tapi(get_forgotten_pwd3,  "/_forgotten_password/blah/", 401, api_subdirs, json).

%% Proper tests - at last - updates
?test(get_update1,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 404, loggedinhtml).
?test(get_update1a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 200, loggedinjson).
?test(get_update2,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 404, loggedouthtml).
?test(get_update2a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 401, loggedoutjson).
?tapi(get_update3,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 401, api, json).
?tapi(get_update3a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", 401, api_subdirs, json).

% twilio recordings
% no valid hypertag will cause the first test to crash - its cool
?test(get_twilio_recording1,  "/some/page/?view=recording", 500, loggedinhtml).
?test(get_twilio_recording1a, "/some/page/?view=recording", 401, loggedinjson).
?test(get_twilio_recording2,  "/some/page/?view=recording", 401, loggedouthtml).
?test(get_twilio_recording2a, "/some/page/?view=recording", 401, loggedoutjson).
?tapi(get_twilio_recording3,  "/some/page/?view=recording", 401, api_subdirs, json).

% debug view
?test(get_debug1,  "/some/page/a1?view=debug", 200, loggedinhtml).
?test(get_debug1x, "/some/page/?view=debug",   404, loggedinhtml).
?test(get_debug1a, "/some/page/a1?view=debug", 401, loggedinjson).
?test(get_debug2,  "/some/page/a1?view=debug", 401, loggedouthtml).
?test(get_debug2a, "/some/page/a1?view=debug", 401, loggedoutjson).
?tapi(get_debug3,  "/some/page/a1?view=debug", 401, api_subdirs, json).

% recalc
% some recalcs can wedget the server in the test suite
% not sure how...
?test(get_recalc1,  "/some/page/a1?view=recalc", 401, loggedinjson).
?test(get_recalc2,  "/some/page/a1?view=recalc", 401, loggedouthtml).
?test(get_recalc2a, "/some/page/a1?view=recalc", 401, loggedoutjson).
?tapi(get_recalc3,  "/some/page/a1?view=recalc", 401, api_subdirs, json).

% parse expresssions
?test(get_parse_expression1,  "/_parse_expression/", 401, loggedinhtml).
?test(get_parse_expression1a, "/_parse_expression/", 401, loggedinjson).
?test(get_parse_expression2,  "/_parse_expression/", 401, loggedouthtml).
?test(get_parse_expression2a, "/_parse_expression/", 401, loggedoutjson).
?tapi(get_parse_expression3,  "/_parse_expression/", 401, api, json).
?tapi(get_parse_expression3a, "/_parse_expression/", 401, api_subdirs, json).

% login
?test(get_login1,  "/_login/", 401, loggedinhtml).
?test(get_login1a, "/_login/", 401, loggedinjson).
?test(get_login2,  "/_login/", 401, loggedouthtml).
?test(get_login2a, "/_login/", 401, loggedoutjson).
?tapi(get_login3,  "/_login/", 401, api, json).
?tapi(get_login3a, "/_login/", 401, api_subdirs, json).

% logs
?test(get_logs1,  "/some/page/?view=logs", 200, loggedinhtml).
?test(get_logs1a, "/some/page/?view=logs", 401, loggedinjson).
?test(get_logs2,  "/some/page/?view=logs", 401, loggedouthtml).
?test(get_logs2a, "/some/page/?view=logs", 401, loggedoutjson).
?tapi(get_logs3,  "/some/page/?view=logs", 401, api, json).
?tapi(get_logs3a, "/some/page/?view=logs", 401, api_subdirs, json).

?test(get_logs4,  "/some/page/a1?view=logs", 200, loggedinhtml).
?test(get_logs4a, "/some/page/a1?view=logs", 200, loggedinjson).
?test(get_logs5,  "/some/page/a1?view=logs", 401, loggedouthtml).
?test(get_logs5a, "/some/page/a1?view=logs", 401, loggedoutjson).
?tapi(get_logs6,  "/some/page/a1?view=logs", 401, api, json).
?tapi(get_logs6a, "/some/page/a1?view=logs", 200, api_subdirs, json).

?test(get_logs7,  "/some/page/a1:b2?view=logs", 401, loggedinhtml).
?test(get_logs7a, "/some/page/a1:b2?view=logs", 401, loggedinjson).
?test(get_logs8,  "/some/page/a1:b2?view=logs", 401, loggedouthtml).
?test(get_logs8a, "/some/page/a1:b2?view=logs", 401, loggedoutjson).
?tapi(get_logs9,  "/some/page/a1:b2?view=logs", 401, api, json).
?tapi(get_logs9a, "/some/page/a1:b2?view=logs", 401, api_subdirs, json).

?test(get_logs10,  "/some/page/b:c?view=logs", 401, loggedinhtml).
?test(get_logs10a, "/some/page/b:c?view=logs", 401, loggedinjson).
?test(get_logs11,  "/some/page/b:c?view=logs", 401, loggedouthtml).
?test(get_logs11a, "/some/page/b:c?view=logs", 401, loggedoutjson).
?tapi(get_logs12,  "/some/page/b:c?view=logs", 401, api, json).
?tapi(get_logs12a, "/some/page/b:c?view=logs", 401, api_subdirs, json).

?test(get_logs13,  "/some/page/10:11?view=logs", 401, loggedinhtml).
?test(get_logs13a, "/some/page/10:11?view=logs", 401, loggedinjson).
?test(get_logs14,  "/some/page/10:11?view=logs", 401, loggedouthtml).
?test(get_logs14a, "/some/page/10:11?view=logs", 401, loggedoutjson).
?tapi(get_logs15,  "/some/page/10:11?view=logs", 401, api, json).
?tapi(get_logs15a, "/some/page/10:11?view=logs", 401, api_subdirs, json).
