%%% @copyright (C) 2010, Hypernumbers, Ltd.

-module(hn_net_util).

-export([
         cookie/3,
         kill_cookie/1,
         post/3
        ]).

%% Two years is forever on the internet.
-define(TWO_YEARS, 63113852).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cookies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cookie(string(), string(), integer() | string()) -> string().
cookie(N, V, "session")                -> c_(N, V, []);
cookie(N, V, "never")                  -> c_(N, V, [{max_age, ?TWO_YEARS}]);
cookie(N, V, Age) when is_integer(Age) -> c_(N, V, [{max_age, Age}]).

c_(Name, Value, Opts) ->
    mochiweb_cookies:cookie(Name, Value, [{path, "/"}] ++ Opts).

-spec kill_cookie(string()) -> string().
kill_cookie(N) -> cookie(N, "killitwithfire", 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Url, Data, Format) ->
    {ok, {{_V, _Status,_R},_H,Body}} =
        httpc:request(post,{Url,[],Format,Data},[],[]),
    Body.
