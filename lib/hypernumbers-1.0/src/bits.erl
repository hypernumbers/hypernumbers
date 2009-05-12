%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Hypernumbers.com
%%% @doc       This module really shouldn't be here - it is a scratch
%%%            debugging module (hence the name!).
%%% @private
-module(bits).
-export([ run_formula/1, log/1]).

run_formula(S) ->

    P = fun(Str) ->
                Cell = {10,10},
                {ok,Tok} = xfl_lexer:lex(Str,Cell),
                xfl_parser:parse(Tok)
        end,

    {ok,Ast} = P(S),
    muin:run_code(Ast,{muin_rti,"http://127.0.0.1:9000",[],10,10,false}).
                

log(String) ->
    File= case os:type() of
	      {win32,nt} -> "c:\\tmp\\hypernumbers_log.txt";
	      _          -> "../../logs/recalc/recalc_logs.txt"
	  end,
    _Return=filelib:ensure_dir(File),
    
    case file:open(File, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "~s~n", [String]),
	    file:close(Id);
	_ ->
	    error
    end.

