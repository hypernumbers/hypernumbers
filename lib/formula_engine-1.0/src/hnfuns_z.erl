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

-include("spriki.hrl").

debugz([Z]) ->
    Vals = muin_collect:col([Z], [fetch, fetch_z_all],
                     [return_errors, {all, fun muin_collect:is_zeds/1}]),
    {zcellref, _, {_, _, _, _, Zed}} = Z,
    debug1(Zed, Vals).

debug1(Zed, [{zeds, Matches, NoMatches, Errs}]) ->
    Html = "<div class='hn_debug_z'>"
        ++ "<div class='hn_debug_top'>Debugging Z Queries</div>"
        ++ "<div>z-query is: " ++ Zed ++ "</div>"
        ++ make_matches(Matches) ++ make_no_matches(NoMatches) ++ make_errs(Errs)
        ++ "</div>",
    {resize, {5, 10, #incs{}}, Html}.

make_matches([]) -> "<div class='hn_debug_hd'>no pages match</div>";
make_matches(L)  ->
    "<div class='hn_debug_hd'>The following pages match:</div>"
        ++ "<table>"
        ++ make_m2(L, [])
        ++ "</table>".

make_no_matches([]) -> [];
make_no_matches(L)  ->
    "<div class='hn_debug_hd'>The following pages (or page stems) "
        ++ "do not match:</div>"
        ++ make_no2(L, []).

make_errs([]) -> [];
make_errs(L)  ->
    "<div class='hn_debug_hd'>The following pages throw errors:</div>"
        ++ make_e2(L, []).

make_m2([], Acc) -> lists:flatten(lists:reverse(Acc));
make_m2([{{Path, Ref}, Val} | T], Acc) ->
    V = case Val of
            {errval, Err} -> atom_to_list(Err);
            _             -> tconv:to_s(Val)
        end,
    NewA = "<tr><td><a href='" ++ hn_util:list_to_path(Path)
        ++ "'>" ++ hn_util:list_to_path(Path)
        ++ "</a></td><td>" ++ Ref ++ "</td><td>"
        ++ V ++ "</td></tr>",
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
