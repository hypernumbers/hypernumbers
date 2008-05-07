-module(misc_util).
-export([stdfuns_export_xml/0]).
-include("builtins.hrl").

-define(CATEGORIES, [
    {stdfuns_math,      "Math"},
    {stdfuns_text,      "Text"},
    {stdfuns_logical,   "Logical"},
    {stdfuns_stats,     "Statistical"},
    {stdfuns_info,      "Informational"}
    ]).

stdfuns_export_xml() ->

    Filter = fun
        ({Fun,Ident},Ident) -> true;
        (_,_Ident) -> false
    end,

    Function = fun({Fun,AtomIdent}) ->
        {function,[{label,atom_to_list(Fun)}],[]}
    end,

    Category = fun({AtomIdent,Desc}) ->
    
        Funs = lists:filter(fun
            (X) -> Filter(X,AtomIdent)
        end, ?STDFUNS),
                
        {category,[{name,Desc}],
            lists:map(fun(X) -> Function(X) end,Funs)
        }
    end,

    Cats = lists:map(
        fun(X) -> Category(X) end,
        ?CATEGORIES),

    {function,[],Cats}.
