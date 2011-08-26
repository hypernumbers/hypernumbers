-module(curie_arity).

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



