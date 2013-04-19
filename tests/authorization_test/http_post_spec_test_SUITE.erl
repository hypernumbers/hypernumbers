%%%-----------------------------------------------------------------------------
%%% File        : http_post_spec_test_suite.erl
%%% Author      : Gordon Guthrie <gordon@vixo.com>
%%% Description : tests http POST authorisation
%%%
%%% Created     : 18th March 2013 by Gordon Guthrie
%%%-----------------------------------------------------------------------------
-module(http_post_spec_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(SITE, "http://tests.hypernumbers.dev:9000").
-define(SOMEJSON, "{\"some\": \"bloody\", \"old\": \"nonsense\"}").

-define(test(Name, Path, Body, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Fn = case Status of
                        loggedinjson          ->
                            post_logged_in_TEST;
                        loggedinjsonnotadmin  ->
                            post_logged_in_notadmin_TEST;
                        loggedoutjson         ->
                            post_logged_out_TEST
                    end,
               Got = test:Fn(URL, Body),
               case Expected of
                   Got    -> {test, ok};
                   _Other -> io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~n",
                                       [Expected, Got]),
                             exit("FAIL: Mismatch in " ++ atom_to_list(Name)
                                  ++ " of " ++ atom_to_list(?MODULE))
               end).

-define(tapi(Name, Path, Body, Expected, Status),
        Name(_Config) ->
               URL  = ?SITE ++ Path,
               Got = test:post_api_TEST(URL, Body, Status),
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
    ].

all2() ->
    [
     % special pages
     post_invite_root1,
     post_invite_root2,
     post_invite2,
     post_invite_root3,
     post_invite3,
     post_mynewsite_root1,
     post_mynewsite1,
     post_mynewsite_root2,
     post_mynewsite2,
     post_mynewsite_root3,
     post_mynewsite3,
     post_validate_root1,
     post_validate1,
     post_validate_root2,
     post_validate2,
     post_validate_root3,
     post_validate3,
     post_authorize_root1,
     post_authorize1,
     post_authorize_root2,
     post_authorize2,
     post_authorize_root3,
     post_authorize3,
     post_logout_root1,
     post_logout1,
     post_logout_root2,
     post_logout2,
     post_logout_root3,
     post_logout3,

     % site structure pages
     post_site_root1,
     post_site_root2,
     post_site_root3,
     post_site_root3a,
     post_pages_root1,
     post_pages_root2,
     post_pages_root3,
     post_pages_root3a,

     % statistics
     post_statistics_root1,
     post_statistics_root2,
     post_statistics_root3,
     post_statistics_root3a,

%% conventional pages like /_sites, /_replies and /_contacts
%% off the main page for forms and phones and stuff
%% normally all pages beginning with '_' under root are
%% not creatable
     post_underscore_sites1,
     post_underscore_sites2,
     post_underscore_sites3,
     post_underscore_sites3a,

     % audit pages
     post_underscore_audit_root1,
     post_underscore_audit_root2,
     post_underscore_audit_root3,
     post_underscore_audit1,
     post_underscore_audit2,
     post_underscore_audit3,
     post_underscore_audit3a,

     % now phone stuff
     % need to work this stuff out before enabling them
     post_services_phone1,
     post_services_phone2,
     post_services_phone3,
     post_services_phone3a,

     % not testing these
     %% post_services_phoneredirect1,
     %% post_services_phoneredirect2,
     %% post_services_phoneredirect3,
     %% post_services_phoneredirect3a,

     % reprovisioning usability site
     post_reprovision1,
     post_reprovision2,
     post_reprovision3,
     post_reprovision3a,

     % forgotten password
     post_forgotten_pwd_root1,
     post_forgotten_pwd_root2,
     post_forgotten_pwd_root3,
     post_forgotten_pwd_root3a,

     post_forgotten_pwd1,
     post_forgotten_pwd2,
     post_forgotten_pwd3,
     post_forgotten_pwd3a,

     % parse expression
     post_root_parse_expression1,
     post_root_parse_expression2,
     post_root_parse_expression3,
     post_root_parse_expression3a,
     post_parse_expression1,
     post_parse_expression2,
     post_parse_expression3,
     post_parse_expression3a,

     post_root_login1,
     post_root_login2,
     post_root_login3,
     post_root_login3a,
     post_login1,
     post_login2,
     post_login3,
     post_login3a
    ].

%% Test cases starts here.
%%------------------------------------------------------------------------------
%% The special pages tests
?test(post_invite_root1, "/_invite/",  ?SOMEJSON, 401, loggedinjson).  % can't see that page
?test(post_invite1, "/_invite/bingo/", ?SOMEJSON, 401, loggedinjson). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(post_invite_root2, "/_invite/",  ?SOMEJSON, 401, loggedoutjson).  % can't see that page
?test(post_invite2, "/_invite/bingo/", ?SOMEJSON, 401, loggedoutjson). % no valid hypertag will cause it to crash (but it has been allowed...)

?tapi(post_invite_root3, "/_invite/",  ?SOMEJSON, 401, api).  % can't see that page
?tapi(post_invite3, "/_invite/bingo/", ?SOMEJSON, 401, api). % no valid hypertag will cause it to crash (but it has been allowed...)

?test(post_mynewsite_root1, "/_mynewsite/",  ?SOMEJSON, 401, loggedinjson).  % can't see that page
?test(post_mynewsite1, "/_mynewsite/bingo/", ?SOMEJSON, 401, loggedinjson). % no valid hypertag will cause it to

?test(post_mynewsite_root2, "/_mynewsite/",  ?SOMEJSON, 401, loggedoutjson).  % can't see that page
?test(post_mynewsite2, "/_mynewsite/bingo/", ?SOMEJSON, 401, loggedoutjson). % no valid hypertag will cause it to

?tapi(post_mynewsite_root3, "/_mynewsite/",  ?SOMEJSON, 401, api).  % can't see that page
?tapi(post_mynewsite3, "/_mynewsite/bingo/", ?SOMEJSON, 401, api). % no valid hypertag will cause it to

?test(post_validate_root1, "/_validate/",  ?SOMEJSON, 401, loggedinjson).  % can't see that page
?test(post_validate1, "/_validate/bingo/", ?SOMEJSON, 401, loggedinjson). % no valid hypertag will cause it to

?test(post_validate_root2, "/_validate/",  ?SOMEJSON, 401, loggedoutjson).  % can't see that page
?test(post_validate2, "/_validate/bingo/", ?SOMEJSON, 401, loggedoutjson). % no valid hypertag will cause it to

?tapi(post_validate_root3, "/_validate/",  ?SOMEJSON, 401, api).  % can't see that page
?tapi(post_validate3, "/_validate/bingo/", ?SOMEJSON, 401, api). % no valid hypertag will cause it to

?test(post_authorize_root1, "/_authorize/",  ?SOMEJSON, 401, loggedinjson).  % can't see that page
?test(post_authorize1, "/_authorize/bingo/", ?SOMEJSON, 401, loggedinjson). % no valid hypertag will cause it to

?test(post_authorize_root2, "/_authorize/",  ?SOMEJSON, 401, loggedoutjson).  % can't see that page
?test(post_authorize2, "/_authorize/bingo/", ?SOMEJSON, 401, loggedoutjson). % no valid hypertag will cause it to

?tapi(post_authorize_root3, "/_authorize/",  ?SOMEJSON, 401, api).  % can't see that page
?tapi(post_authorize3, "/_authorize/bingo/", ?SOMEJSON, 401, api). % no valid hypertag will cause it to

?test(post_logout_root1, "/_logout/?return=" ++ ?SITE, ?SOMEJSON, 401, loggedinjson).  % can see this page
?test(post_logout1, "/_logout/bingo/", ?SOMEJSON, 401, loggedinjson). % no valid subpage

?test(post_logout_root2, "/_logout/?return=" ++ ?SITE, ?SOMEJSON, 401, loggedoutjson).  % can see this page
?test(post_logout2, "/_logout/bingo/", ?SOMEJSON, 401, loggedoutjson). % no valid subpage

?tapi(post_logout_root3, "/_logout/?return=" ++ ?SITE, ?SOMEJSON, 401, api).  % can see this page
?tapi(post_logout3, "/_logout/bingo/", ?SOMEJSON, 401, api). % no valid subpage

%% the generic site and pages stuff
?test(post_site_root1,  "/_site/", ?SOMEJSON, 401, loggedinjson).
?test(post_site_root2,  "/_site/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_site_root3,  "/_site/", ?SOMEJSON, 401, api).
?tapi(post_site_root3a, "/_site/", ?SOMEJSON, 401, api_subdirs).

% same code as _site so don't bother to write lots of tests
?test(post_pages_root1,  "/_pages/", ?SOMEJSON, 401, loggedinjson).
?test(post_pages_root2,  "/_pages/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_pages_root3,  "/_pages/", ?SOMEJSON, 401, api).
?tapi(post_pages_root3a, "/_pages/", ?SOMEJSON, 401, api_subdirs).

?test(post_statistics_root1,  "/_statistics/", ?SOMEJSON, 401, loggedinjson).
?test(post_statistics_root2,  "/_statistics/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_statistics_root3,  "/_statistics/", ?SOMEJSON, 401, api).
?tapi(post_statistics_root3a, "/_statistics/", ?SOMEJSON, 401, api_subdirs).

% conventional site types under root
?test(post_underscore_sites_root1,  "/_sites/",                   ?SOMEJSON, 401, loggedinjson).
?test(post_underscore_sites_root2,  "/_sites/",                   ?SOMEJSON, 401, loggedoutjson).
?tapi(post_underscore_sites_root3,  "/_sites/",                   ?SOMEJSON, 401, api).
?tapi(post_underscore_sites_root3a, "/_sites/",                   ?SOMEJSON, 200, api_subdirs).

?test(post_underscore_sites1,  "/_sites/blen/", ?SOMEJSON, 401, loggedinjson).
?test(post_underscore_sites2,  "/_sites/blon/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_underscore_sites3,  "/_sites/blan/", ?SOMEJSON, 401, api).
?tapi(post_underscore_sites3a, "/_sites/blun/", ?SOMEJSON, 401, api_subdirs).

?test(post_underscore_audit_root1,  "/_audit/", ?SOMEJSON, 401, loggedinjson).
?test(post_underscore_audit_root2,  "/_audit/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_underscore_audit_root3,  "/_audit/", ?SOMEJSON, 401, api).
?tapi(post_underscore_audit_root3a, "/_audit/", ?SOMEJSON, 401, api_subdirs).

?test(post_underscore_audit1,  "/_audit/blah/", ?SOMEJSON, 401, loggedinjson).
?test(post_underscore_audit2,  "/_audit/bleh/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_underscore_audit3,  "/_audit/bloh/", ?SOMEJSON, 401, api).
?tapi(post_underscore_audit3a, "/_audit/bluh/", ?SOMEJSON, 401, api_subdirs).

?test(post_services_phone1,   "/_services/phone/", ?SOMEJSON, 401, loggedinjson).
?test(post_services_phone2,   "/_services/phone/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_services_phone3,   "/_services/phone/", ?SOMEJSON, 401, api).
?tapi(post_services_phone3a,  "/_services/phone/", ?SOMEJSON, 401, api_subdirs).

%% ?test(post_services_phoneredirect1,   "/_services/phoneredirect/", ?SOMEJSON, 200, loggedinjson).
%% ?test(post_services_phoneredirect2,   "/_services/phoneredirect/", ?SOMEJSON, 401, loggedoutjson).
%% ?tapi(post_services_phoneredirect3,   "/_services/phoneredirect/", ?SOMEJSON, 401, api).
%% ?tapi(post_services_phoneredirect3a,  "/_services/phoneredirect/", ?SOMEJSON, 401, api_subdirs).

% reprovision only works on usability site
?test(post_reprovision1,  "/_reprovision/", ?SOMEJSON, 401, loggedinjson).
?test(post_reprovision2,  "/_reprovision/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_reprovision3,  "/_reprovision/", ?SOMEJSON, 401, api).
?tapi(post_reprovision3a, "/_reprovision/", ?SOMEJSON, 401, api_subdirs).

% forgotten passport only works on some sites
?test(post_forgotten_pwd_root1,   "/_forgotten_password/", ?SOMEJSON, 500, loggedinjson).
?test(post_forgotten_pwd_root2,   "/_forgotten_password/", ?SOMEJSON, 500, loggedoutjson).
?tapi(post_forgotten_pwd_root3,   "/_forgotten_password/", ?SOMEJSON, 401, api).
?tapi(post_forgotten_pwd_root3a,  "/_forgotten_password/", ?SOMEJSON, 401, api_subdirs).

?test(post_forgotten_pwd1,   "/_forgotten_password/blah/", ?SOMEJSON, 401, loggedinjson).
?test(post_forgotten_pwd2,   "/_forgotten_password/blah/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_forgotten_pwd3,   "/_forgotten_password/blah/", ?SOMEJSON, 401, api_subdirs).
?tapi(post_forgotten_pwd3a,  "/_forgotten_password/blah/", ?SOMEJSON, 401, api).

?test(post_root_parse_expression1,   "/_parse_expression/", ?SOMEJSON, 401, loggedinjson).
?test(post_root_parse_expression2,   "/_parse_expression/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_root_parse_expression3,   "/_parse_expression/", ?SOMEJSON, 401, api_subdirs).
?tapi(post_root_parse_expression3a,  "/_parse_expression/", ?SOMEJSON, 401, api).

?test(post_parse_expression1,   "/_parse_expression/blah/", ?SOMEJSON, 401, loggedinjson).
?test(post_parse_expression2,   "/_parse_expression/blah/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_parse_expression3,   "/_parse_expression/blah/", ?SOMEJSON, 401, api_subdirs).
?tapi(post_parse_expression3a,  "/_parse_expression/blah/", ?SOMEJSON, 401, api).

% login
?test(post_root_login1,   "/_login/", ?SOMEJSON, 401, loggedinjson).
?test(post_root_login2,   "/_login/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_root_login3,   "/_login/", ?SOMEJSON, 401, api_subdirs).
?tapi(post_root_login3a,  "/_login/", ?SOMEJSON, 401, api).

?test(post_login1,   "/_login/blah/", ?SOMEJSON, 401, loggedinjson).
?test(post_login2,   "/_login/blah/", ?SOMEJSON, 401, loggedoutjson).
?tapi(post_login3,   "/_login/blah/", ?SOMEJSON, 401, api_subdirs).
?tapi(post_login3a,  "/_login/blah/", ?SOMEJSON, 401, api).
