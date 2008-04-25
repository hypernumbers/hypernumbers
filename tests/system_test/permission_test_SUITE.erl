%%%-----------------------------------------------------------------------------
%%% File    : API_test_SUITE.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------
-module(permission_test_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").
-include("../../include/spriki.hrl").

%% Test server callback functions
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
    code:add_path("../../../../ebin"),
    production_boot:start(),
    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) -> bits:clear_db(),Config.
end_per_testcase(_TestCase, _Config) -> ok.

all() -> [].
%    [write_perm,get_perm,get_perm2,get_perm3].

%% Test cases starts here.
%%------------------------------------------------------------------------------
write_perm() -> 
    [{userdata,[{doc,"Set /private to be private"}]}].
write_perm(Config) when is_list(Config) -> 
    test_util:expected("Error: unauthorised",
        hn_util:post(?HN_URL1++"/private/",
        "action=create&gui=spriki.swf&name=private&public=false")).

get_perm() ->     
    [{userdata,[{doc,"Set /private to be private"}]}].
get_perm(Config) when is_list(Config) -> 
    hn_util:post(?HN_URL1++"/private/",
        "action=create&gui=spriki.swf&name=private&public=false"),
    test_util:expected("Error: unauthorised",
        hn_util:req(?HN_URL1++"/private/a1")).

get_perm2() -> 
    [{userdata,[{doc,"Set /private to be private"}]}].
get_perm2(Config) when is_list(Config) -> 
    hn_util:post(?HN_URL1++"/private/",
        "action=create&gui=spriki.swf&name=private&public=false"),
    test_util:expected("Error: unauthorised",
        hn_util:req(?HN_URL1++"/private/nested/a1")).

get_perm3() ->
    [{userdata,[{doc,"Set /private to be private"}]}].
get_perm3(Config) when is_list(Config) -> 
    hn_util:post(?HN_URL1++"/private/",
        "action=create&gui=spriki.swf&name=private&public=false"),
    test_util:expected("0",hn_util:req(?HN_URL1++"/a1")).
