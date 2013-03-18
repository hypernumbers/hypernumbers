%%%-----------------------------------------------------------------------------
%%% File        : http_post_spec_test_suite.erl
%%% Author      : Gordon Guthrie <gordonguthrie@backawinner.gg>
%%% Description : tests http POST authorisation
%%%
%%% Created     : 18th March 2013 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(http_post_spec_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").

-define(test(Name, Path, Body, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Fn = case Status of
                        loggedinjson          ->
                            post_logged_in_json_TEST;
                        loggedinjsonnotadmin  ->
                            post_logged_in_json_notadmin_TEST;
                        loggedoutjson         ->
                            post_logged_out_json_TEST;
                        loggedinhtml          ->
                            post_logged_in_html_TEST;
                        loggedinhtmlnotadmin  ->
                            post_logged_in_notadmin_html_TEST;
                        loggedouthtml         ->
                            post_logged_out_html_TEST
                    end,
               Got = test:Fn(URL),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

-define(tapi(Name, Path, Expected, Body, Status, Accept),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Got = test:post_api_TEST(URL, Body, Status, Accept),
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

%% tests to run
all() ->
    [
     % special pages
     get_invite2 %,
     %% %% get_invite_root3,
     %% %% get_invite3,
     %% %% get_mynewsite_root1,
     %% %% get_mynewsite1,
     %% %% get_mynewsite_root2,
     %% %% get_mynewsite2,
     %% %% get_mynewsite_root3,
     %% %% get_mynewsite3,
     %% %% get_validate_root1,
     %% %% get_validate1,
     %% %% get_validate_root2,
     %% %% get_validate2,
     %% %% get_validate_root3,
     %% %% get_validate3,
     %% %% get_authorize_root1,
     %% %% get_authorize1,
     %% %% get_authorize_root2,
     %% %% get_authorize2,
     %% %% get_authorize_root3,
     %% %% get_authorize3,
     %% %% get_logout_root1,
     %% %% get_logout1,
     %% %% get_logout_root2,
     %% %% get_logout2,
     %% %% get_logout_root3,
     %% %% get_logout3,

     % site structure pages
     %% %% get_site_root1,
     %% %% get_site_root1a,
     %% %% get_site_root2,
     %% %% get_site_root2a,
     %% %% get_site_root3,
     %% %% get_site_root3a,
     %% %% get_site_root3b,
     %% %% get_site_root3c,
     %% %% get_pages_root1,
     %% %% get_pages_root2,
     %% %% get_pages_root3,
     %% %% get_pages_root3a,

     % statistics
     %% %% get_statistics_root1,
     %% %% get_statistics_root2,
     %% %% get_statistics_root3,
     %% %% get_statistics_root3a,

%% conventional pages like /_sites, /_replies and /_contacts
%% off the main page for forms and phones and stuff
%% normally all pages beginning with '_' under root are
%% not creatable
     %% %% get_underscore_sites1,
     %% %% get_underscore_sites2,
     %% %% get_underscore_sites3,
     %% %% get_underscore_sites3a,

     % audit pages
     %% %% get_underscore_audit_root1,
     %% %% get_underscore_audit_root1a,
     %% %% get_underscore_audit_root2,
     %% %% get_underscore_audit_root3,
     %% %% get_underscore_audit_root3a,
     %% %% get_underscore_audit1,
     %% %% get_underscore_audit1a,
     %% %% get_underscore_audit2,
     %% %% get_underscore_audit3,
     %% %% get_underscore_audit3a,

     % now phone stuff
     % need to work this stuff out before enabling them
     %% %% get_services_phone1,
     %% %% get_services_phone1a,
     %% %% get_services_phone2,
     %% %% get_services_phone2a,
     %% %% get_services_phone3,

     % not testing these
     %% get_services_phoneredirect1,
     %% get_services_phoneredirect1a,
     %% get_services_phoneredirect2,
     %% get_services_phoneredirect2a,
     %% get_services_phoneredirect3,

     % reprovisioning usability site
     %% %% get_reprovision1,
     %% %% get_reprovision1a,
     %% %% get_reprovision2,
     %% %% get_reprovision2a,
     %% %% get_reprovision3,

     % forgotten password
     %% %% get_forgotten_pwd_root1,
     %% %% get_forgotten_pwd_root1a,
     %% %% get_forgotten_pwd_root2,
     %% %% get_forgotten_pwd_root2a,
     %% %% get_forgotten_pwd_root3,

     %% %% get_forgotten_pwd1,
     %% %% get_forgotten_pwd1a,
     %% %% get_forgotten_pwd2,
     %% %% get_forgotten_pwd2a,
     %% %% get_forgotten_pwd3,

     % finally we get to some real tests :(
     %% %% get_update1,
     %% %% get_update1a,
     %% %% get_update2,
     %% %% get_update2a,
     %% %% get_update3,
     %% %% get_update3a,

     % twilio stuff
     %% %% get_twilio_recording1,
     %% %% get_twilio_recording1a,
     %% %% get_twilio_recording2,
     %% %% get_twilio_recording2a,
     %% %% get_twilio_recording3,

     % debug stuff
     %% %% get_debug1,
     %% %% get_debug1x,
     %% %% get_debug1a,
     %% %% get_debug2,
     %% %% get_debug2a,
     %% %% get_debug3,

     % logs stuff
     %% %% get_logs1,
     %% %% get_logs1x,
     %% %% get_logs1a,
     %% %% get_logs2,
     %% %% get_logs2a,
     %% %% get_logs3,

     % recalc stuff
     %% %% get_recalc1a,
     %% %% get_recalc2,
     %% %% get_recalc2a,
     %% %% get_recalc3

    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
%% The special pages tests
?test(get_invite_root1, "/_invite/",  "erk", 404, loggedinhtml).  % can't see that page
?test(get_invite1, "/_invite/bingo/", "erk", 500, loggedinhtml). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(get_invite_root2, "/_invite/",  "erk", 404, loggedouthtml).  % can't see that page
?test(get_invite2, "/_invite/bingo/", "erk", 500, loggedouthtml). % no valid hypertag will cause it to crash (but it has been allowed...)

?tapi(get_invite_root3, "/_invite/",  "erk", 404, api, html).  % can't see that page
?tapi(get_invite3, "/_invite/bingo/", "erk", 401, api, html). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(get_mynewsite_root1, "/_mynewsite/",  "erk", 404, loggedinhtml).  % can't see that page
?test(get_mynewsite1, "/_mynewsite/bingo/", "erk", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_mynewsite_root2, "/_mynewsite/",  "erk", 404, loggedouthtml).  % can't see that page
?test(get_mynewsite2, "/_mynewsite/bingo/", "erk", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_mynewsite_root3, "/_mynewsite/",  "erk", 404, api, html).  % can't see that page
?tapi(get_mynewsite3, "/_mynewsite/bingo/", "erk", 401, api, html). % no valid hypertag will cause it to

?test(get_validate_root1, "/_validate/",  "erk", 404, loggedinhtml).  % can't see that page
?test(get_validate1, "/_validate/bingo/", "erk", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_validate_root2, "/_validate/",  "erk", 404, loggedouthtml).  % can't see that page
?test(get_validate2, "/_validate/bingo/", "erk", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_validate_root3, "/_validate/",  "erk", 404, api, html).  % can't see that page
?tapi(get_validate3, "/_validate/bingo/", "erk", 401, api, html). % no valid hypertag will cause it to

?test(get_authorize_root1, "/_authorize/",  "erk", 404, loggedinhtml).  % can't see that page
?test(get_authorize1, "/_authorize/bingo/", "erk", 500, loggedinhtml). % no valid hypertag will cause it to

?test(get_authorize_root2, "/_authorize/",  "erk", 404, loggedouthtml).  % can't see that page
?test(get_authorize2, "/_authorize/bingo/", "erk", 500, loggedouthtml). % no valid hypertag will cause it to

?tapi(get_authorize_root3, "/_authorize/",  "erk", 404, api, html).  % can't see that page
?tapi(get_authorize3, "/_authorize/bingo/", "erk", 401, api, html). % no valid hypertag will cause it to

?test(get_logout_root1, "/_logout/?return=" ++ ?SITE, "erk", 200, loggedinhtml).  % can see this page
?test(get_logout1, "/_logout/bingo/", "erk", 404, loggedinhtml). % no valid subpage

?test(get_logout_root2, "/_logout/?return=" ++ ?SITE, "erk", 200, loggedouthtml).  % can see this page
?test(get_logout2, "/_logout/bingo/", "erk", 404, loggedouthtml). % no valid subpage

?tapi(get_logout_root3, "/_logout/?return=" ++ ?SITE, "erk", 401, api, html).  % can see this page
?tapi(get_logout3, "/_logout/bingo/", "erk", 404, api, html). % no valid subpage

%% the generic site and pages stuff
?test(get_site_root1,  "/_site/", "erk", 200, loggedinjson).
?test(get_site_root1a, "/_site/", "erk", 404, loggedinhtml).
?test(get_site_root2,  "/_site/", "erk", 200, loggedoutjson).
?test(get_site_root2a, "/_site/", "erk", 404, loggedouthtml).
?tapi(get_site_root3,  "/_site/", "erk", 404, api, json).
?tapi(get_site_root3a, "/_site/", "erk", 404, api_subdirs, html).
?tapi(get_site_root3b, "/_site/", "erk", 404, api, json).
?tapi(get_site_root3c, "/_site/", "erk", 200, api_subdirs, json).

% same code as _site so don't bother to write lots of tests
?test(get_pages_root1,  "/_pages/", "erk", 200, loggedinjson).
?test(get_pages_root2,  "/_pages/", "erk", 200, loggedoutjson).
?tapi(get_pages_root3,  "/_pages/", "erk", 404, api, json).
?tapi(get_pages_root3a, "/_pages/", "erk", 200, api_subdirs, json).

%% statistics - only logged in admins should be able to see it
%% can't be arsed to right a special I am logged in as admin
%% test - that works, I know...
?test(get_statistics_root1,  "/_statistics/", "erk", 200, loggedinhtml).
?test(get_statistics_root2,  "/_statistics/", "erk", 401, loggedouthtml).
?tapi(get_statistics_root3,  "/_statistics/", "erk", 401, api, json).
?tapi(get_statistics_root3a, "/_statistics/", "erk", 401, api_subdirs, json).

% conventional site types under root
?test(get_underscore_sites_root1,  "/_sites/",                   "erk", 200, loggedinhtml).
?test(get_underscore_sites_root1a, "/_sites/?view=spreadsheet",  "erk", 200, loggedinhtml).
?test(get_underscore_sites_root1b, "/_sites/?view=webpage",      "erk", 200, loggedinhtml).
?test(get_underscore_sites_root1c, "/_sites/?view=wikipage",     "erk", 200, loggedinhtml).
?test(get_underscore_sites_root1d, "/_sites/?view=table",        "erk", 200, loggedinhtml).
?test(get_underscore_sites_root2,  "/_sites/",                   "erk", 401, loggedouthtml).
?test(get_underscore_sites_root2a, "/_sites/?view=spreadsheet",  "erk", 401, loggedouthtml).
?test(get_underscore_sites_root2b, "/_sites/?view=webpage",      "erk", 401, loggedouthtml).
?test(get_underscore_sites_root2c, "/_sites/?view=wikipage",     "erk", 401, loggedouthtml).
?test(get_underscore_sites_root2d, "/_sites/?view=table",        "erk", 401, loggedouthtml).
?tapi(get_underscore_sites_root3,  "/_sites/",                   "erk", 401, api, json).
?tapi(get_underscore_sites_root3a, "/_sites/",                   "erk", 200, api_subdirs, json).

?test(get_underscore_sites1,  "/_sites/blen/", "erk", 404, loggedinhtml).
?test(get_underscore_sites2,  "/_sites/blon/", "erk", 404, loggedouthtml).
?tapi(get_underscore_sites3,  "/_sites/blan/", "erk", 404, api, json).
?tapi(get_underscore_sites3a, "/_sites/blun/", "erk", 404, api_subdirs, json).

?test(get_underscore_audit_root1,  "/_audit/", "erk", 200, loggedinhtml).
?test(get_underscore_audit_root1a, "/_audit/", "erk", 200, loggedinjson).
?test(get_underscore_audit_root2,  "/_audit/", "erk", 401, loggedouthtml).
?tapi(get_underscore_audit_root3,  "/_audit/", "erk", 401, api, json).
?tapi(get_underscore_audit_root3a, "/_audit/", "erk", 200, api_subdirs, json).

?test(get_underscore_audit1,  "/_audit/blah/", "erk", 200, loggedinhtml).
?test(get_underscore_audit1a, "/_audit/blah/", "erk", 200, loggedinjson).
?test(get_underscore_audit2,  "/_audit/bleh/", "erk", 401, loggedouthtml).
?tapi(get_underscore_audit3,  "/_audit/bloh/", "erk", 401, api, json).
?tapi(get_underscore_audit3a, "/_audit/bluh/", "erk", 200, api_subdirs, json).

?test(get_services_phone1,  "/_services/phone/", "erk", 200, loggedinhtml).
?test(get_services_phone1a, "/_services/phone/", "erk", 404, loggedinjson).
?test(get_services_phone2,  "/_services/phone/", "erk", 200, loggedouthtml).
?test(get_services_phone2a, "/_services/phone/", "erk", 404, loggedoutjson).
?tapi(get_services_phone3,  "/_services/phone/", "erk", 404, api_subdirs, json).

%% ?test(get_services_phoneredirect1,  "/_services/phoneredirect/", "erk", 200, loggedinhtml).
%% ?test(get_services_phoneredirect1a, "/_services/phoneredirect/", "erk", 404, loggedinjson).
%% ?test(get_services_phoneredirect2,  "/_services/phoneredirect/", "erk", 401, loggedouthtml).
%% ?test(get_services_phoneredirect2a, "/_services/phoneredirect/", "erk", 401, loggedoutjson).
%% ?tapi(get_services_phoneredirect3,  "/_services/phoneredirect/", "erk", 401, api_subdirs, json).

% reprovision only works on usability site
?test(get_reprovision1,  "/_reprovision/", "erk", 404, loggedinhtml).
?test(get_reprovision1a, "/_reprovision/", "erk", 404, loggedinjson).
?test(get_reprovision2,  "/_reprovision/", "erk", 404, loggedouthtml).
?test(get_reprovision2a, "/_reprovision/", "erk", 404, loggedoutjson).
?tapi(get_reprovision3,  "/_reprovision/", "erk", 404, api_subdirs, json).

% forgotten passport only works on some sites
?test(get_forgotten_pwd_root1,  "/_forgotten_password/", "erk", 404, loggedinhtml).
?test(get_forgotten_pwd_root1a, "/_forgotten_password/", "erk", 404, loggedinjson).
?test(get_forgotten_pwd_root2,  "/_forgotten_password/", "erk", 404, loggedouthtml).
?test(get_forgotten_pwd_root2a, "/_forgotten_password/", "erk", 404, loggedoutjson).
?tapi(get_forgotten_pwd_root3,  "/_forgotten_password/", "erk", 404, api_subdirs, json).

?test(get_forgotten_pwd1,  "/_forgotten_password/blah/", "erk", 404, loggedinhtml).
?test(get_forgotten_pwd1a, "/_forgotten_password/blah/", "erk", 404, loggedinjson).
?test(get_forgotten_pwd2,  "/_forgotten_password/blah/", "erk", 404, loggedouthtml).
?test(get_forgotten_pwd2a, "/_forgotten_password/blah/", "erk", 404, loggedoutjson).
?tapi(get_forgotten_pwd3,  "/_forgotten_password/blah/", "erk", 404, api_subdirs, json).

%% Proper tests - at last - updates
?test(get_update1,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 404, loggedinhtml).
?test(get_update1a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 200, loggedinjson).
?test(get_update2,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 404, loggedouthtml).
?test(get_update2a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 401, loggedoutjson).
?tapi(get_update3,  "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 401, api, json).
?tapi(get_update3a, "/some/page/?updates=23213132&paths=%2Ferkle%2F&view=spreadsheet", "erk", 401, api_subdirs, json).

% twilio recordings
% no valid hypertag will cause the first test to crash - its cool
?test(get_twilio_recording1,  "/some/page/?view=recording", "erk", 500, loggedinhtml).
?test(get_twilio_recording1a, "/some/page/?view=recording", "erk", 401, loggedinjson).
?test(get_twilio_recording2,  "/some/page/?view=recording", "erk", 401, loggedouthtml).
?test(get_twilio_recording2a, "/some/page/?view=recording", "erk", 401, loggedoutjson).
?tapi(get_twilio_recording3,  "/some/page/?view=recording", "erk", 401, api_subdirs, json).

% debug view
?test(get_debug1,  "/some/page/a1?view=debug", "erk", 200, loggedinhtml).
?test(get_debug1x, "/some/page/?view=debug",   "erk", 404, loggedinhtml).
?test(get_debug1a, "/some/page/a1?view=debug", "erk", 401, loggedinjson).
?test(get_debug2,  "/some/page/a1?view=debug", "erk", 401, loggedouthtml).
?test(get_debug2a, "/some/page/a1?view=debug", "erk", 401, loggedoutjson).
?tapi(get_debug3,  "/some/page/a1?view=debug", "erk", 401, api_subdirs, json).

% log view
?test(get_logs1,  "/some/page/a1?view=logs", "erk", 200, loggedinhtml).
?test(get_logs1x, "/some/page/?view=logs",   "erk", 200, loggedinhtml).
?test(get_logs1a, "/some/page/a1?view=logs", "erk", 200, loggedinjson).
?test(get_logs2,  "/some/page/a1?view=logs", "erk", 401, loggedouthtml).
?test(get_logs2a, "/some/page/a1?view=logs", "erk", 401, loggedoutjson).
?tapi(get_logs3,  "/some/page/a1?view=logs", "erk", 200, api_subdirs, json).

% recalc
% some recalcs can wedget the server in the test suite
% not sure how...
?test(get_recalc1a, "/some/page/a1?view=recalc", "erk", 401, loggedinjson).
?test(get_recalc2,  "/some/page/a1?view=recalc", "erk", 401, loggedouthtml).
?test(get_recalc2a, "/some/page/a1?view=recalc", "erk", 401, loggedoutjson).
?tapi(get_recalc3,  "/some/page/a1?view=recalc", "erk", 401, api_subdirs, json).
