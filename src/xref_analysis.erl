%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       This moduule is used to perform cross-reference analysis 
%%%
%%% @end
%%% Created :  5 Feb 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(xref_analysis).

-export([analyse/1]).

-record(no_of_calls,
        {
          resolved   = 0,
          unresolved = 0
         }).

-record(no_of_fn_calls,
        {
          local_calls        = 0,
          resolved_ext_calls = 0,
          unresolved_calls   = 0
          }).

-record(no_of_fns,
        {
          local    = 0,
          exported = 0
         }).

-record(details,
        {
          no_of_calls          = #no_of_calls{},
          no_of_fn_calls       = #no_of_fn_calls{},
          no_of_fns            = #no_of_fns{},
          no_of_inter_fn_calls = 0,
          locals_not_used      = 0,
          exports_not_used     = 0
         }).

analyse([RootDir]) ->
    io:format("in xref_analysis:analyse RootDir is ~p~n", [RootDir]),
    xref:start(s),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    xref:add_directory(s, atom_to_list(RootDir)++"/read_excel-1.0/ebin"),
    xref:add_directory(s, atom_to_list(RootDir)++"/formula-1.0/ebin"),
    xref:add_directory(s, atom_to_list(RootDir)++"/hypernumbers-1.0/ebin"),
    Info = xref:info(s, modules),
    %io:format("Info is ~p~n", [Info]),
    Parsed = parse_info(Info),
    io:format("Parsed is ~p~n", [Parsed]),
    % locals_not_used
    % exports_not_used
    % deprecated_function_calls
    Return = xref:analyze(s, locals_not_used),
    io:format("in analysis Return is ~p~n", [Return]).

parse_info(List)         -> parse_info(List, []).

parse_info([], Acc)      -> Acc;
parse_info([H | T], Acc) -> {FuncName, Attr} = H,
                            {_, {_, Dir}} = lists:keysearch(directory, 1, Attr),
                            ["ebin", Dir2 | _T] = lists:reverse(string:tokens(Dir, "/")),
                            Details = get_details(Attr),
                            parse_info(T, [{FuncName, Dir2, Details} | Acc]).

write_html(List) -> write_html(List, []).

write_html([], Acc)      -> io:format("in write_html Acc is ~p~n", [Acc]),
                            Head = "<html>\n<head></head>\n<body>\n",
                            Tail = "\n</body>\n</html>",
                            HTML = lists:flatten([Head, Acc, Tail]),
                            io:format("in write_html HTML is ~p~n", [HTML]),
                            writeout(HTML),
                            ok;
write_html([H | T], Acc) -> {FuncName, _Attrs} = H,
                            C = "<p>"++atom_to_list(FuncName)++"</p>\n",
                            write_html(T, [C | Acc]).

writeout(HTML) ->
    File= case os:type() of
              {win32,nt} -> "c:\\tmp\\xref.html";
              _          -> "../../logs/xref/xref.html"
          end,
    _Return=filelib:ensure_dir(File),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [HTML]),
            file:close(Id);
        _ ->
            error
    end.

get_details(Attr) -> io:format("In get_details Attr is ~p~n", [Attr]),
                     get_d(Attr, #details{}).

get_d([], D) -> D;
get_d([{no_calls, {Resolved, Unresolved}} | T], D) ->
    A = #no_of_calls{resolved = Resolved, unresolved = Unresolved},
    D2 = D#details{no_of_calls = A},
    get_d(T, D2);
get_d([{no_function_calls, {LocalCalls, ResolvedExtCalls,
                         UnresolvedCalls}} | T], D) ->
    A = #no_of_fn_calls{local_calls = LocalCalls,
                        resolved_ext_calls = ResolvedExtCalls,
                        unresolved_calls = UnresolvedCalls},
    D2 = D#details{no_of_fn_calls = A},
    get_d(T, D2);
get_d([{no_functions, {Local, Exported}} | T], D) ->
    A = #no_of_fns{local = Local, exported = Exported},
    D2 = D#details{no_of_fns = A},
    get_d(T, D2);
get_d([{no_inter_function_calls, NoIntFnCalls} | T], D) ->
    D2 = D#details{no_of_inter_fn_calls = NoIntFnCalls},
    get_d(T, D2);
get_d([{locals_not_used, LocalsNotUsed} | T], D) ->
    D2 = D#details{locals_not_used = LocalsNotUsed},
    get_d(T, D2);
get_d([{exports_not_used, ExportsNotUsed} | T], D) ->
    D2 = D#details{exports_not_used = ExportsNotUsed},
    get_d(T, D2);
get_d([H | T], D) -> get_d(T, D).
