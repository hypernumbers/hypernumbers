%%%-----------------------------------------------------------------------------
%%% File       util2.erl
%%% @author    Gordon Guthrie <gordonguthrie@vixo.com>
%%% @doc
%%% @copyright Hypernumbers Ltd 2006-2014
%%% @private
%%%
%%% Created    22 Nov 2006 by Gordon Guthrie <gordonguthrie@vixo.com>
%%%-----------------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(util2).

-export([
         timestamp_to_date/1,
         get_timestamp/0,
         bake_biccie/0,
         chop/1,
         rev_chop/1,
         parse_range/1,
         strip_ref/1,
         make_ref/1,
         make_text/1,
         repath/1,
         pad_list/1,
         mk_str_frm_list/1,
         strip_brackets/1]).

-define(MEGA, 1000000000000).
-define(SEC,  1000000).

%%%-----------------------------------------------------------------------------
%%%
%%% Worker functions for the util2
%%% There are two util files cos you can't load one with try/catch into the
%%% debugger (ie that one)
%%%
%%%-----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% These functions are all utility functions                                %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_timestamp() -> pos_integer().
get_timestamp()->
    {Mega, Sec, Micro} = now(),
    ?MEGA * Mega + ?SEC * Sec + Micro.

timestamp_to_date(Stamp) when is_list(Stamp) ->
    timestamp_to_date(list_to_integer(Stamp));
timestamp_to_date(Stamp) when is_integer(Stamp) ->
    Mega = trunc(Stamp/?MEGA),
    Sec = trunc((Stamp - Mega * ?MEGA)/?SEC),
    Micro = Stamp - Mega * ?MEGA - Sec * ?SEC,
    {Mega, Sec, Micro}.

%%--------------------------------------------------------------------
%% Function:    bake_biccie/0
%% Description: a biccie is like a cookie but we don't store the
%%              session the same way as a cookie
%%--------------------------------------------------------------------
%% @spec bake_biccie() -> Biccie
%% @doc this function bakes a new biccie for when a new hypernumber is being
%% created
%% @todo chocolate biccies ;-)
bake_biccie() ->
    % This is the code that generates a cookie in Yaws-1.70
    % nicked from the module yaws_session_server.erl
    N = crypto:rand_uniform(1, 16#ffffffffffffffff),
    atom_to_list(node()) ++ [$-|integer_to_list(N)].

chop(Path)->
    string:tokens(Path, "/").

rev_chop(Path)->
    lists:reverse(chop(Path)).

parse_range(Range)->
    [Cell1,Cell2]=string:tokens(Range,":"),
    Cell1L=string:to_lower(Cell1),
    Cell2L=string:to_lower(Cell2),
    {X1,Y1}=strip_ref(Cell1L),
    {X2,Y2}=strip_ref(Cell2L),
    if (X1 >= X2) ->
            X1new = X2,
            X2new = X1;
       true ->
            X1new = X1,
            X2new = X2
    end,
    if (Y1 >= Y2) ->
            Y1new = Y2,
            Y2new = Y1;
       true ->
            Y1new = Y1,
            Y2new = Y2
    end,
    {X1new,Y1new,X2new,Y2new}.

strip_ref(Ref) ->
    {X,Y} = lists:partition(
              fun(XX) ->
                      if XX < 97  -> false;
                         XX > 122 -> false;
                         true     -> true
                      end
              end,string:to_lower(Ref)),
    {tconv:to_i(X),list_to_integer(Y)}.

make_ref({X, Y}) -> tconv:to_b26(X)++tconv:to_s(Y).

make_text(Item) ->
    case Item of
        Item when is_integer(Item)-> integer_to_list(Item);
        Item when is_float(Item)  -> float_to_list(Item);
        Item when is_list(Item)   -> lists:flatten(Item);
        Item when is_atom(Item)   -> atom_to_list(Item);
        false                     -> "False";
        true                      -> "True";
        _                         -> Item
    end.

%% this will fuck up the spriki - final slash added to repath!
repath("/")  -> "/";
repath([])   -> "/";
repath(Rest) -> repath(Rest,[]).

repath([],Residuum) ->
    lists:concat([Residuum,"/"]);
repath([{A}|T],Residuum) ->
    repath(T,lists:concat(["/","{",A,"}",Residuum]));
repath([{A,B}|T],Residuum) ->
    repath(T,lists:concat(["/","{",A,",",B,"}", Residuum]));
repath([{A,B,C}|T],Residuum) ->
    repath(T,lists:concat(["/","{",A,",",B,",",C,"}", Residuum]));
repath([H|T],Residuum) ->
    repath(T,lists:concat(["/",H,Residuum])).

pad_list(List)->
    pad_list(List,[]).

pad_list([],[])->
    [];
pad_list([],Residuum)->
    List=lists:reverse(Residuum),
    lists:concat(List);
pad_list([H|T],Residuum) ->
    Fun=fun(X)->lists:append(make_text(X)," ") end,
    List=lists:map(Fun,tuple_to_list(H)),
    pad_list(T,[lists:concat(List)|Residuum]).

mk_str_frm_list(List) when is_list(List) ->
    mk_str_frm_list(List,[]).

mk_str_frm_list([H|T],Residuum)->
    mk_str_frm_list(T,[",","\"",H,"\""|Residuum]);
mk_str_frm_list([],[_H|T]) ->
    lists:flatten(["[",lists:reverse(T),"]"]).

strip_brackets(A1) when is_list(A1) ->
    A2 = re:replace(A1, "^\\(", "", [{return, list}, global]),
    re:replace(A2, "\\)$", "", [{return, list}, global]). %"
