%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       z-order funs
%%%
%%% @end
%%% Created : 25 Mar 2011 by gordon@hypernumbers.com

-module(hnfuns_z).

-export([
         debugz/1
        ]).

debugz([Z]) ->
    muin_collect:col([Z], [fetch, fetch_z_all],
                     [return_errors, {all, fun muin_collect:is_zeds/1}],
                     fun debug1/1).

debug1([{zeds, Matches, NoMatches, Errs}]) ->
    Html = "<div class='hn_debug_top'>Debugging Z Specifications</div>"
        ++ make_matches(Matches) ++ make_no_matches(NoMatches) ++ make_errs(Errs),
    {resize, 5, 10, Html}.

make_matches([]) -> "<div>no pages match</div>";
make_matches(L)  ->
    "<div class='hn_debug_hd'>The following pages match:<div>"
        ++ make_m2(L, []) ++ "</div>".

make_no_matches([]) -> [];
make_no_matches(L)  ->
    "<div class='hn_debug_hd'>The following pages (or page stems) do not match:"
        ++ make_no2(L, []) ++ "<div>".

make_errs([]) -> [];
make_errs(L)  ->
    "<div class='hn_debug_hd'>The following pages throw errors:"
        ++ make_e2(L, []) ++ "</div>".

make_m2([], Acc) -> lists:flatten(lists:reverse(Acc));
make_m2([{{Path, Ref}, Val} | T], Acc) ->
    NewA = "<div><a href='" ++ hn_util:list_to_path(Path)
        ++ "'>" ++ hn_util:list_to_path(Path) ++ "</a> " ++ Ref ++ ": "
        ++ tconv:to_s(Val) ++ "</div>",
    make_m2(T, [NewA | Acc]).

make_no2([], Acc) -> lists:flatten(lists:reverse(Acc));
make_no2([{nomatch, Path} | T], Acc) ->
    NewA = "<div><a href='" ++ hn_util:list_to_path(Path)
        ++ "'>" ++ hn_util:list_to_path(Path) ++ "</a></div>",
    make_no2(T, [NewA | Acc]).

make_e2([], Acc) -> lists:flatten(lists:reverse(Acc));
make_e2([{error, Path, {errval, Err}} | T], Acc) ->
    NewA = "<div><a href='" ++ hn_util:list_to_path(Path)
        ++ "'/>" ++ hn_util:list_to_path(Path) ++ "</a> error is: "
        ++ atom_to_list(Err) ++ "</div>",
    make_e2(T, [NewA | Acc]).
