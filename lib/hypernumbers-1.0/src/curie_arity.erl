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

contains([], _)	->	false;
contains([Key | _], Key)	-> true;
contains([_ | T], Key)		-> contains(T, Key).
