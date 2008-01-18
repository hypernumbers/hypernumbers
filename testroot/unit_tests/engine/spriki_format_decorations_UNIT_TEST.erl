%%%-----------------------------------------------------------------------------
%%% File    : spriki_format_decorations_UNIT_TEST.erl
%%% Author  : Gordon Guthrie <gordonguthrie@localhost>
%%% Description :
%%%
%%% Created : 20 Oct 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-----------------------------------------------------------------------------

-module(spriki_format_decorations_UNIT_TEST).
-include_lib("eunit/include/eunit.hrl").
-include("spriki.hrl").
-include("yaws.hrl").

-define(Protocol, "http://").
-define(Domain, "127.0.0.1").
-define(IP, {127,0,0,1}).
-define(Port, "9000").
-define(Path, "/xxx/").
-define(Ref, "a1").
-define(Site, ?Protocol ++ ?Domain ++ ":" ++ ?Port).
-define(PathRef, ?Path ++ ?Ref).

spriki_format_decorations_test_() ->
  [
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "incoming")) == {{list},[{incoming}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "incoming&format=xml")) == {{xml},[{incoming}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "incoming&format=xml&nocallback")) == {{xml},[{incoming}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "outgoing")) == {{list},[{outgoing}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "outgoing&format=json")) == {{json},[{outgoing}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "outgoing&format=json&nocallback")) == {{json},[{outgoing}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "format=xml")) == {{xml},[]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "format=json")) == {{list},[]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "format=json&nocallback")) == {{json},[]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "toolbar")) == {{list},[{toolbar}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "toolbar&format=xml")) == {{xml},[{toolbar}]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "toolbar&format=json")) == {{json},[toolbar]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "toolbar&format=json&nocallback")) == {{json},[toolbar]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "last")) == {{list},[last]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "last&format=xml")) == {{xml},[last]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "last&format=json")) == {{json},[last]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "last&format=json&nocallback")) == {{json},[last]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "hypernumber")) == {{list},[hypernumber]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "hypernumber&format=xml")) == {{xml},[hypernumber]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "hypernumber&format=json")) == {{json},[hypernumber]}),
    ?_assert(spriki:format_decorations_TEST(arg(?IP, ?Site, ?PathRef, "hypernumber&format=json&nocallback")) == {{json},[hypernumber]})
  ].
  
arg(IP, Site, PathRef, FormatAndDecos) ->
  test_util:get_Arg(IP, Site, PathRef, FormatAndDecos).