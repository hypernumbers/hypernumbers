-module(~s).
-compile(export_all).

-include_lib("hypernumbers/include/hypernumbers.hrl").
-include_lib("hypernumbers/include/spriki.hrl").
-include("ct.hrl").

init_per_suite(Config) ->
    hypernumbers_app:clean_start(),
    code:add_path(~p),
    hn_import:json_file("~s", 
                        "~s"),
    ~s
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

get_val(Ref) ->
    V = case hn_db_api:read_attributes(Ref#refX{site=~p},["value"]) of
            [{_Ref, {"value", Val}}]           -> Val; 
            _Else                              -> "" 
        end.

all() ->
    [~s].

~s

