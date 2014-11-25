%%% @author    Gordon Guthrie
%%% @author    Jakub Chlanda
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc
%%%		Implements famous paper "A User-Centred Approach to Functions in Excel"
%%%		by Simon Peyton Jones, Alan Blackwell and Margaret Burnett
%%% @end
%%% Created :  15 Jun 2011


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

-module(curie_arity).

%%% TODO fix up
-compile(export_all).

walk_AST([], Acc)	->
	lists:reverse(Acc);
walk_AST([{cellref, _, _, _, Address} | T], Acc)	->
	case contains(Acc, {cellref, Address}) of
		false	-> walk_AST(T, [{cellref, Address} | Acc]);
		true	-> walk_AST(T, Acc)
	end;
walk_AST([_H | T], Acc)	->
	walk_AST(T, Acc).


%~ apply_function(Params_in_AST, Args, AST, Acc).
apply_function(Params_in_AST, Args, AST)	->
	Dictionary = make_KV_dict(Params_in_AST, Args, []),
	apply_function2(Dictionary, AST, []).


apply_function2(_Dictionary, [], Acc)	->
	lists:reverse(Acc);
apply_function2(Dictionary, [H | T], Acc) when is_list(H)	->
	apply_function2(Dictionary, T, [apply_function2(Dictionary, H, []) | Acc]);
apply_function2(Dictionary, [{cellref, _, _, _, Address} | T], Acc)	->
	Value = kfind(Address, Dictionary),
	apply_function2(Dictionary, T, [Value | Acc]);
apply_function2(Dictionary, [H | T], Acc)	->
	apply_function2(Dictionary, T, [H | Acc]).


kfind(Key, Dictionary) ->
	{Key, Val} = lists:keyfind(Key, 1, Dictionary),
    Val.

contains([], _)	->	false;
contains([Key | _], Key)	-> true;
contains([_ | T], Key)		-> contains(T, Key).

%~ make_KV_dict(Params_in_AST, Args).
make_KV_dict([], [], Acc)	-> lists:reverse(Acc);
make_KV_dict([{cellref, Cell} | T1], [Value | T2], Acc)	->
	make_KV_dict(T1, T2, [{Cell, Value} | Acc]).



