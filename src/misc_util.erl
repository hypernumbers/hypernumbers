-module(misc_util).
-export([stdfuns_export_xml/0,import_xml_attributes/2]).
-include("builtins.hrl").
-include("spriki.hrl").

-define(CATEGORIES, [
    {stdfuns_math,      "Math"},
    {stdfuns_text,      "Text"},
    {stdfuns_logical,   "Logical"},
    {stdfuns_stats,     "Statistical"},
    {stdfuns_info,      "Informational"}
    ]).

%% Read std_funs and produce a workable 
%% xml document for the functions list
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

import_xml_attributes(File,Url) ->
    {ok,String} = hn_util:read(File),
    {attr,[],Refs} = simplexml:from_xml_string(String),
    lists:map
    (
        fun(X) ->
            case X of
            %% Dont sent value attributes
            {ref,_,[{value,_,_}]} -> ok;
            {ref,[_,{ref,Ref}],[{Name,_,Children}]} ->
                Xml = io_lib:format("<create><~s>~s</~s></create>",
                    [Name,simplexml:to_xml_string(Children),Name]),
                hn_util:post(Url++Ref++"?attr",lists:flatten(Xml),"text/xml")
            end
        end,Refs
    ).
