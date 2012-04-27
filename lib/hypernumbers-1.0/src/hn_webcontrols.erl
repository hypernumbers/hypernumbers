%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       This module handles the input from web controls
%%%
%%% @end
%%% Created :  1 May 2011 by gordon@hypernumbers.com

-module(hn_webcontrols).

-export([
         make_actions/3,
         pad/1
         ]).

-include("spriki.hrl").
-include("hn_webcontrols.hrl").

-define(FULLSTOP, 46). % Ascii char instead of $. which humps syntax highlighting

% we push a single val of now in to prevent problems if the fn runs over
% a midnight
make_actions(Site, Path, Recs) ->
    {_, Recs2} = lists:unzip(Recs),
    Recs3 = [walk(Path, X) || X <- Recs2],
    {Ts, Perms, Dest, As} = make_a(Site, Recs3, now(), [], [], [], []),
    % now transform the actions from relative to absolute paths
    Fun = fun(X) ->
                  Loc = string:join(X, "/") ++ "/",
                  Loc2 = case Loc of
                             [?FULLSTOP | _Rest] -> Loc;
                             _            -> "/" ++ Loc
                         end,
                  muin_util:walk_path(Path, Loc2)
           end,
    As2 = [{Temp, Fun(X)} || {Temp, X} <- As],

    {Dest2, View} = case Dest of
                        [] -> {get_last(As2), []};
                        _  -> Dest
            end,

    Dest3 = hn_util:strip80(Site) ++ hn_util:list_to_path(Dest2) ++ View,
    {Ts, Perms, Dest3, As2}.

get_last(X) -> {_, Last} = hd(lists:reverse(X)),
               Last.

make_a(_S, [], _Now, Temps, Perm, Dest, Acc) ->
    UniqTemps = hslists:uniq(Temps),
    UniqActions = hslists:uniq(lists:flatten(lists:reverse(Acc))),
    {UniqTemps, Perm, Dest, UniqActions};
make_a(S, [H | T], Now, Ts, Perm, Dest, Acc) ->
    {NT, NewPerm, NewDest, NA} = make_a2(S, H, Now, [], [], Perm, Dest, []),
    make_a(S, T, Now, lists:concat([NT, Ts]), NewPerm, NewDest, [NA | Acc]).

make_a2(_S, [], _Now, Temps, _Htap, Perm, Dest, Acc) ->
    {Temps, Perm, Dest, lists:reverse(Acc)};
make_a2(S, [#segment{page = #plainpath{path = P}} | T], Now, Temps,
        Htap, Perm, Dest, Acc) ->
    make_a2(S, T, Now, Temps, [P | Htap], Perm, Dest, Acc);
make_a2(S, [#segment{page = #numberedpage{template = Tpl, type = "random",
                                          prefix = Pr}} = Spec | T],
        Now, Temps, Htap, Perm, Dest, Acc) ->
    Seg = Pr ++ hex(integer_to_list(util2:get_timestamp())),
    NewHtap = [Seg | Htap],
    NewPath = lists:reverse(NewHtap),
    {NewPerm, NewDest} = parse_rest(Spec, NewPath, Perm, Dest),
    NewAcc = {Tpl, NewPath},
    make_a2(S, T, Now, [Tpl | Temps], NewHtap, NewPerm, NewDest, [NewAcc | Acc]);
make_a2(S, [#segment{page = #numberedpage{template = Tpl, type = "increment",
                                          prefix = Pr}} = Spec | T],
        Now, Temps, Htap, Perm, Dest, Acc) ->
    Pages = new_db_api:read_pages(#refX{site = S}),
    % chuck out ones the wrong length
    Pg2 = [X || X <- Pages, length(X) == length(Htap) + 1],
    Seg = get_seg(lists:reverse(Htap), Pg2, Pr),
    NewHtap = [Seg | Htap],
    NewPath = lists:reverse(NewHtap),
    {NewPerm, NewDest} = parse_rest(Spec, NewPath, Perm, Dest),
    NewAcc = {Tpl, NewPath},
    make_a2(S, T, Now, [Tpl | Temps], NewHtap, NewPerm, NewDest,
            [NewAcc | Acc]);
make_a2(S, [#segment{page = #datedpage{template = Tpl,
                                       format = Fm}} = Spec | T],
        Now, Temps, Htap, Perm, Dest, Acc) ->
    Seg = case Fm of                            % 1/2/2003
              "yy"   -> dh_date:format("y", Now); % 03
              "yyyy" -> dh_date:format("Y", Now); % 2003
              "m"    -> dh_date:format("n", Now); % 2
              "mm"   -> dh_date:format("m", Now); % 02
              "mmm"  -> dh_date:format("M", Now); % Feb
              "mmmm" -> dh_date:format("F", Now); % February
              "d"    -> dh_date:format("j", Now); % 1
              "dd"   -> dh_date:format("d", Now); % 01
              "ddd"  -> dh_date:format("D", Now); % Sat
              "dddd" -> dh_date:format("l", Now)  % Saturday
          end,
    % yup, flatpack all strings to lower case...
    NewHtap = [string:to_lower(Seg) | Htap],
    NewPath = lists:reverse(NewHtap),
    {NewPerm, NewDest} = parse_rest(Spec, NewPath, Perm, Dest),
    NewAcc = {Tpl, NewPath},
    make_a2(S, T, Now, [Tpl | Temps], NewHtap, NewPerm, NewDest, [NewAcc | Acc]);
make_a2(S, [#segment{page = #namedpage{template = Tpl, name = Nm}} = Spec | T],
        Now, Temps, Htap, Perm, Dest, Acc) ->
    NewHtap = [Nm | Htap],
    NewPath = lists:reverse(NewHtap),
    {NewPerm, NewDest} = parse_rest(Spec, NewPath, Perm, Dest),
    NewAcc = {Tpl, NewPath},
    make_a2(S, T, Now, [Tpl | Temps], NewHtap, NewPerm, NewDest, [NewAcc | Acc]).

parse_rest(#segment{redirect = {destination, Redir}, addspreadsheetgroups = AS,
                    addwebpagegroups = AWb, addwikipagegroups = AWi,
                    addtablegroups = AT}, Path, Perm, Dest) ->
    NewDest = case Redir of
                  false         -> Dest;
                  "default"     -> {Path, []};
                  "spreadsheet" -> {Path, "?view=spreadsheet"};
                  "webpage"     -> {Path, "?view=webpage"};
                  "wikipage"    -> {Path, "?view=wikipage"};
                  "table"       -> {Path, "?view=table"}
              end,
    NewPerm = make_perms([{"spreadsheet", AS}, {"webpage", AWb},
                          {"wikipage", AWi}, {"table", AT}],
                    []),
    {[{Path, NewPerm} | Perm], NewDest}.

make_perms([], Acc)                -> Acc;
make_perms([{_Type, []} | T], Acc) -> make_perms(T, Acc);
make_perms([H | T], Acc)           -> make_perms(T, [H | Acc]).

get_seg(Path, Pages, Prefix) ->
    % first get all pages that might match
    Fun = fun(X, Acc) ->
                  {Hd, [T]} = lists:split(length(X) - 1, X),
                  case Hd of
                      Path -> case re:run(T, "^"++Prefix) of
                                  {match, [{0, N}]} ->
                                      % now check if the last seg is an int
                                      Last = hd(lists:reverse(X)),
                                      MebbiesNum = lists:nthtail(N, Last),
                                      case to_num(MebbiesNum) of
                                          {error, nan} -> Acc;
                                          I           -> [I | Acc]
                                      end;
                                  nomatch ->
                                      Acc
                              end;
                      _    -> Acc
                  end
          end,
    Pg2 = lists:foldl(Fun, [], Pages),
    make_s2(Pg2, Prefix).

make_s2([], Prefix)   -> Prefix ++ pad(1);
make_s2(List, Prefix) -> Max = lists:max(List),
                         Prefix ++ pad(Max + 1).

pad(X) ->
    String = integer_to_list(X),
    N = ?padlength - length(String),
    Pad = lists:flatten(lists:duplicate(N, "0")),
    Pad ++ String.

hex(String) -> Salt = salts:randomsalt(),
               mochihex:to_hex(crypto:md5_mac(Salt, String)).

walk(_Path, [#segment{page = #plainpath{path = [Char | _]}} | _T] = Dest)
  when Char =/= $.->
    Dest;
walk(Path, CtrlPath) ->
    Segs = [#segment{page = #plainpath{path = X}} || X <- Path],
    lists:foldl(fun(#segment{page = #plainpath{path = "."}}, Stk)    -> Stk;
                   (#segment{page = #plainpath{path = ".."}}, [])    -> [];
                   (#segment{page = #plainpath{path = ".."}}, Stk)   -> hslists:init(Stk);
                   (Word, Stk) -> lists:append(Stk, [Word])
                end,
                Segs,
                CtrlPath).

to_num(Str) when is_list(Str)   ->
    Str2 = string:strip(Str),
    try list_to_integer(Str2)
    catch
        error:
        _ -> {error, nan};
        exit:
        _ -> {error, nan}
    end.

