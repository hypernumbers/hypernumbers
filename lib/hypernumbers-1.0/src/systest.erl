-module(systest).
-author(tom@hypernumbers.com).

-export([save/2, restore/2]).

-define(FIXTURE_DIR, "tests/system_test/fixtures").


%% Persist the current page data located at Path as a json Fixture
save(Path, Fixture) ->
    CHeaders = [{"accept", "application/json"}],
    case http:request(get, {Path, CHeaders}, [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Hds, JSON}} ->
            FN = filename:join(?FIXTURE_DIR, Fixture++".json"),
            file:write_file(FN, JSON);
        Other ->
            throw({bad_http_request, Other})
    end.


restore(Path, Fixture) ->
    Type = "application/json",
    Body = "{\"delete\":\"all\"}",
    case http:request(post, {Path, [], Type, Body}, [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Hds, "\"success\""}} ->
            FN = filename:join(?FIXTURE_DIR, Fixture++".json"),
            hn_import:json_file(Path, FN);
        Other ->
            throw({bad_http_request, Other})        
    end.
    
