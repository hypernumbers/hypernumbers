%%% @author Dale Harvey <dale@hypernumbers.com>
%%% @copyright Hypernumbers Ltd.
%%% @private
-module(hn_main).

-include("hypernumbers.hrl").
-include("spriki.hrl").
-include("regexp.hrl").
-include("handy_macros.hrl").
-include("errvals.hrl").
-include("muin_records.hrl").

-export([get_cell_info/4]).

%%%-----------------------------------------------------------------
%%% Function    : get_cell_info/2
%%% Types       : 
%%% Description : Provides the information required by the
%%%               formula parser about a cell, ie its direct
%%%               parents/ dependency tree, and value
%%%-----------------------------------------------------------------
get_cell_info(Site, Path, X, Y) ->

    Ref   = #ref{site=string:to_lower(Site),path=Path,ref={cell,{X,Y}}},
    Value = hn_db:get_item_val(Ref#ref{name="rawvalue"}),

    DepTree = case hn_db:get_item_val(Ref#ref{name="dependency-tree"}) of
                  {xml,Tree} -> Tree;
                  []         -> []
              end,
    
    Val = case Value of
              []                 -> blank;
              {datetime, _, [N]} -> muin_date:from_gregorian_seconds(N);
              Else               -> Else %% Strip other type tags.
          end,
    
    F = fun({url,[{type,Type}],[Url]}) -> 
                {ok,#ref{site=S,path=P,ref={cell,{X1,Y1}}}} = hn_util:parse_url(Url),
                {Type,{S,P,X1,Y1}}
        end,

    Dep = lists:map(F,DepTree) ++ [{"local",{Site,Path,X,Y}}],
    {Val,Dep,[],[{"local",{Site,Path,X,Y}}]}.

