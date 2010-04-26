%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       handles integration with external websites
%%%
%%% @end
%%% Created :  9th April 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(hnfuns_integration).

-export([
         facebook_share/1,
         twitter_button/1
        ]).

-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).

%%
%% Exported functions
%%
facebook_share([])       -> fb_share1("box_count", "");
facebook_share([0])      -> fb_share1("box_count", "");
facebook_share([1])      -> fb_share1("button_count", "");
facebook_share([2])      -> fb_share1("button", "");
facebook_share([3])      -> fb_share1("icon_count", "");
facebook_share([N, URL]) -> Str = ?string(URL, ?default_str_rules),
                            fb_share(N, Str);
facebook_share(_Other)   -> ?ERRVAL_VAL.

fb_share(0, URL) -> fb_share1("box_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(1, URL) -> fb_share1("button_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(2, URL) -> fb_share1("button",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(3, URL) -> fb_share1("icon_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(_, _) -> ?ERRVAL_VAL.


fb_share1(Button, URL) ->
    "<a name=\"fb_share\" type=\"" ++ Button ++ "\" "  ++ URL++ " href=\"http://www.facebook.com/sharer.php\">Share</a><script src=\"http://static.ak.fbcdn.net/connect.php/js/FB.Share\" type=\"text/javascript\"></script>".

twitter_button([UserName]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b1(0, "a", Str);
twitter_button([UserName, Type]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b1(Type, "a", Str);
twitter_button([UserName, N, Colour]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b(N, Colour, Str).

tw_b(N, 0, Str) -> tw_b1(N, "a", Str);
tw_b(N, 1, Str) -> tw_b1(N, "b", Str);
tw_b(N, 2, Str) -> tw_b1(N, "c", Str);
tw_b(_, _, _)   -> ?ERRVAL_VAL.

tw_b1(0, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/follow_me-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(1, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/follow_bird-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(2, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/twitter-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(3, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_logo-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(4, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_small-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(5, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_mini-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(_, _, _) -> ?ERRVAL_VAL.
