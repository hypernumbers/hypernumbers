%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       provides dummy web controls for demo purposes
%%%
%%% @end
%%% Created :  9th April 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(hnfuns_web).

-export([
         button/1,
         radio/1,
         select/1,
         form/1
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
form(Items) ->
    [Items2] = ?string([Items], ?default_str_rules),
    form1(Items2, []).

form1([], [_H | T]) ->
    F = "<form>",
    B = "<br /><br /><input type=\"button\" value=\"submit\" /></form>",
    lists:flatten([F, lists:reverse(T) | B]);
form1([H | T], Acc) ->
    form1(T, ["<br /><br />", H | Acc]).

button([Title]) ->
    [Title2] = ?string([Title], ?default_str_rules),
    "<input type=\"submit\" value=\"" ++ Title2 ++ "\">".

radio(Options) -> radio1(Options, []).

radio1([], Acc)      -> lists:flatten(lists:reverse(Acc));
radio1([H | T], Acc) ->
    H2 = ?string(H, ?default_str_rules),
    NewAcc = ["<p>" ++ H2
              ++ "<input type=\"radio\" name = \""
              ++ H2 ++ "\" value=\""
              ++ H2 ++ "\">" ++ "</p>" | Acc],
    radio1(T, NewAcc).
    

select(Options) -> select1(Options, ["<select>"]).

select1([], Acc)      -> lists:flatten(lists:reverse(["</select>" | Acc]));
select1([H | T], Acc) ->
    H2 = ?string(H, ?default_str_rules),
    NewAcc = ["<option>" ++ H2 ++ "</option>" | Acc],
    select1(T, NewAcc).
    
