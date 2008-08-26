%%% @doc Wrapper for the Hypernumbers API.
%%%
%%% * Uses XML for now.
%%% * Only numbers, strings and booleans are supported.
%%%
%%% To create an instance:
%%%   hypernumbers:new(#hypernumbers_settings{})

-module(hypernumbers, [Hns]).

-export([set_value/3, get_value/2, settings/0]).
-compile(export_all).

-include("hypernumbers_settings.hrl").

%% @spec set_value(Path, Ref, Value) where
%%   Path = list()
%%   Ref = atom()
%%   Value = number() | string() | bool()
%% @doc Set a value (or formula) for a cell.
set_value(Path, Ref, Value) ->
    Data = ("<create><formula><![CDATA[" ++
            prep_for_post(Value) ++
            "]]></formula></create>"),
    http_post(make_url(Path, Ref), Data).

%% @spec get_value(Path, Ref) -> term()
%% @doc Get value (not formula) of a cell.
get_value(Path, Ref) ->
    Body = http_get(make_url(Path, Ref)),
    conv_from_get(Body).

%% @doc Read the settings for an instance of the module.
settings() ->
    Hns.

%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prep_for_post(true) ->
    "true";
prep_for_post(false) ->
    "false";
prep_for_post(N) when is_integer(N) ->
    integer_to_list(N);
prep_for_post(F) when is_float(F) ->
    float_to_list(F);
prep_for_post(Str) when is_list(Str) ->
    lists:flatten(Str).

conv_from_get("true")  -> true;
conv_from_get("false") -> false;
conv_from_get("TRUE")  -> true;
conv_from_get("FALSE") -> false;
conv_from_get(X) ->
    case lists:member(X, ["#NULL!", "#DIV/0!", "#VALUE!",
                          "#REF!", "#NAME?", "#NUM!", "#N/A"]) of
        true -> % error value
            list_to_atom(X);
        false ->
            case tconv:to_num(X) of
                N when is_number(N) -> % number
                    N;
                {error, nan} -> % string
                    X
            end
    end.

make_url([], Ref) ->
    ("http://" ++ Hns#hypernumbers_settings.host ++
     ":" ++ Hns#hypernumbers_settings.port ++ "/" ++
     string:to_upper(tconv:to_s(Ref)));
make_url(Path, Ref) ->
    ("http://" ++ Hns#hypernumbers_settings.host ++
     ":" ++ Hns#hypernumbers_settings.port ++ "/" ++
     string:join(Path, "/") ++ "/" ++ string:to_upper(tconv:to_s(Ref))).

http_post(Url, Data) ->
    http_post(Url, Data, fun default_handle_return/1).
http_post(Url, Data, HandleReturnFun) ->
    Return = http:request(post,
                          {Url, [], "text/xml", Data},
                          [{timeout, 5000}],
                          []),
    HandleReturnFun(Return).
    
%% TODO: Handle connection refused.
default_handle_return({ok, {{_V, 200, _R}, _H, _Body}}) ->
    ok;
default_handle_return({error, timeout}) ->
    {error, timeout};
default_handle_return({ok, {{_V, _Code, _R}, _H, _Body}}) ->
    {error, post_error}.

http_get(Url) ->
    {ok, {{_V, _Code, _R}, _H, Body}} = http:request(get,
                                                     {Url, []},
                                                     [], []),
    Body.
