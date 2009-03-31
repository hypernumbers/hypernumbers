%% @author Dale Harvey <dale@hypernumbers.com>
%% @copyright Hypernumbers Ltd.
-module(hn_db).

-include("spriki.hrl").
-include("handy_macros.hrl").
-include("hypernumbers.hrl").

-export([get_item/1,
         get_item_val/1, 
         get_ref_from_name/1
        ]).

%% API for file import only 
%% writes a whole style in a oner - not on a per attribute basis 
-export([write_style_IMPORT/2]). 


%% @spec get_item(Ref) -> ListofItems
%% @doc  Returns the list of attributes that are contained within 
%%       the range specified by Ref
get_item(#ref{site=Site,path=Path,ref=Ref,name=Name}) ->

    F = fun() ->
                NName = ?COND(Name == undef,'_',Name),
                NAddr = ?COND(element(1,Ref)== cell, Ref, _='_'),
                NRef  = #ref{site=Site, path=Path, name=NName, ref=NAddr},
                Match = #hn_item{addr = NRef, _ = '_'},
                mnesia:match_object(hn_item,Match,read)
        end,
    List = mnesia:activity(transaction,F),
    case Ref of
        {cell,_} -> List;
        {page,_} -> List;
        _        -> Fun = fun(#hn_item{addr=#ref{ref=Item}}) -> 
                                  filter_cell(Ref,Item) 
                          end,
                    lists:filter(Fun,List)
    end.

%% @spec get_item_val(Ref) -> Value
%% @doc  return only the value stored at Ref
get_item_val(Addr) ->
    case get_item(Addr) of
        []                    -> [];
        [#hn_item{val=Value}] -> Value
    end.

%% @spec get_ref_from_name(Name) -> RefList
%% @doc  Returns list of with name set to Name
get_ref_from_name(Name) ->
    Fun = fun() ->
                  Match = #hn_item{addr=#ref{name=name, _ = '_'}, val = Name},
                  mnesia:match_object(hn_item,Match,read)
          end,
    Items = mnesia:activity(transaction, Fun),
    [X#hn_item.addr || X <- Items].

%% write_style will write a style if it doesn't exist and then 
%% return an index pointing to it 
%% If the style already exists it just returns the index 
write_style(Addr, Style) ->
    Ref = Addr#ref{ref = {page, "/"}, name = "style", auth = []},
    {RefX, _} = hn_util:ref_to_refX(Ref, "dont care"),
    Fun1 = fun() -> 
                   Match = #styles{refX = RefX, magic_style = Style, _ = '_'}, 
                   mnesia:match_object(styles, Match, read) 
           end, 
    case mnesia:activity(transaction, Fun1) of 
        []              -> write_style2(RefX, Style); 
        [ExistingStyle] -> #styles{index = NewIndex} = ExistingStyle, 
                           NewIndex 
    end. 

write_style2(RefX, Style) -> 
    NewIndex = mnesia:dirty_update_counter(style_counters, RefX, 1), 
    Fun = fun() -> 
                  mnesia:write(#styles{refX = RefX, index = NewIndex,
                                       magic_style = Style}) 
          end, 
    ok = mnesia:activity(transaction, Fun), 
    NewIndex. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%                                                                            %% 
%% write_style_IMPORT for file import API                                     %% 
%%                                                                            %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
write_style_IMPORT(Addr, Style) -> 
    % first write the Style
    StyleIndex = write_style(Addr, Style),
    % now write the style index for the address
    Ref = Addr#ref{name = "style"},
    Item = #hn_item{addr = Ref, val = StyleIndex},
    Fun  =
        fun()->
                mnesia:write(Item)
        end,
    ok = mnesia:activity(transaction, Fun).

%% @spec filter_cell(Range,Ref) -> true | false
%% @doc  Returns true for Refs that are inside Range
filter_cell(X,X) -> true;
filter_cell({row,Y},{cell,{_,Y}}) -> true;
filter_cell({column,X},{cell,{X,_}}) -> true;
filter_cell({range,{_,Y1,_,Y2}},{row,Y}) 
  when Y > Y1; Y < Y2 -> true;
filter_cell({range,{X1,_,X2,_}},{column,X}) 
  when X > X1; X < X2 -> true;
filter_cell({range,Range},{cell,Cell}) -> 
    hn_util:in_range({range,Range},{cell,Cell});
filter_cell(_,_) -> false.
