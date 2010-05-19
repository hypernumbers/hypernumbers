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
         input/1,
         textarea/1,
         button/1,
         radio/1,
         select/1,
         form/1,
         include/1,
         'google.map'/1
        ]).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).

-define(GMAPS_API_KEY, "ABQIAAAAvc75-UC7iUxZiJTb2GNV8hSNAg2NXpz7d4v"
        ++ "U2BZLLU-LGXynmRSNgEhU9IYj0BQ8iFQIcqjqZV4qRQ").

%%
%% Exported functions
%%
'google.map'([]) ->
    'google.map'([0]);
'google.map'([Long]) ->
    'google.map'([Long, 0]);
'google.map'([_Long, _Lat]) ->
    "<iframe width='100%' height='100%' frameborder='0' scrolling='no' "
        ++ "marginheight='0' marginwidth='0' src='http://maps.google.com"
        ++ "/?ie=UTF8&amp;ll=43.707594,-61.083984&amp;spn=25.89694,61.875"
        ++ "&amp;z=4&amp;output=embed'></iframe>".

form(Items) ->
    [Items2] = ?string([Items], ?default_str_rules),
    form1(Items2, []).

form1([], [_H | T]) ->
    F = "<form>",
    B = "<br /><br /><input type=\"button\" value=\"submit\" /></form>",
    lists:flatten([F, lists:reverse(T) | B]);
form1([H | T], Acc) ->
    form1(T, ["<br /><br />", H | Acc]).

input(_) ->
    "<input type='input' class='hntext' data-name='default' />".

textarea(_) ->
    "<textarea class='hninput' data-name='default'></textarea>".

button([]) ->
    button([""]);
button([Title]) ->
    button([Title, ""]);
button([Title, Content]) ->
    Val = case ?string([Title], ?default_str_rules) of
              [[]]     -> "";
              [Title2] -> " value='" ++ Title2 ++ "'"
          end,    
    "<input type='submit' class='hninput' " ++ Val
        ++ " data-form-name='default' data-response='"
        ++ Content ++ "' \">".

radio(Options) ->
    radio1(Options, []).

radio1([], Acc) ->
    lists:flatten(lists:reverse(Acc));
radio1([H | T], Acc) ->
    H2 = ?string(H, ?default_str_rules),
    NewAcc = [ H2
               ++ "<input type='radio' name = '"
               ++ H2 ++ "' value='"
               ++ H2 ++ "'>" | Acc],
    radio1(T, NewAcc).
    

select(Options) ->
    select1(Options, ["<select class=\"hninput\">"]).

select1([], Acc)      ->
    lists:flatten(lists:reverse(["</select>" | Acc]));
select1([H | T], Acc) ->
    H2 = ?string(H, ?default_str_rules),
    NewAcc = ["<option>" ++ H2 ++ "</option>" | Acc],
    select1(T, NewAcc).


include([CellRef]) when ?is_cellref(CellRef) ->
    #cellref{col={offset, X}, row={offset, Y}, path=Path} = CellRef,
    RelRan = #rangeref{type=finite,
                       path=Path,
                       tl = {{offset, X}, {offset, Y}},
                       br = {{offset, X}, {offset, Y}}},
    include([RelRan]);
include([RelRan]) when ?is_rangeref(RelRan) ->
    %% DIRTY HACK. This forces muin to setup dependencies, and checks
    %% for circ errors.
    _Ret = muin:fetch(RelRan),

    AbsRan = area_util:to_absolute(RelRan, 
                                   muin:context_setting(col),
                                   muin:context_setting(row)),
    #rangeref{path=RelPath, tl = {X1,Y1}, br = {X2,Y2}} = AbsRan,
    Site = muin:context_setting(site),
    Path = muin_util:walk_path(muin:context_setting(path), RelPath),
    Obj = {range, {X1, Y1, X2, Y2}},
    Ref = #refX{site = Site, path = Path, obj = Obj},
    {Html, Width} = hn_render:content(Ref),
    lists:flatten(hn_render:wrap_region(Html, Width)).
