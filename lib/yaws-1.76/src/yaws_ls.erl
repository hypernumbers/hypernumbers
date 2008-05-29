%%%----------------------------------------------------------------------
%%% File    : yaws_ls.erl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created :  5 Feb 2002 by Claes Wikstrom <klacke@hyber.org>
%%% Modified: 13 Jan 2004 by Martin Bjorklund <mbj@bluetail.com>
%%% Modified:    Jan 2006 by S�bastien Bigot <sebastien.bigot@tremplin-utc.net>
%%%----------------------------------------------------------------------

-module(yaws_ls).
-author('klacke@hyber.org').

-compile(export_all).

-include("../include/yaws.hrl").
-include("../include/yaws_api.hrl").
-include("yaws_debug.hrl").

-include_lib("kernel/include/file.hrl").

list_directory(Arg, CliSock, List, DirName, Req, DoAllZip) ->
    {abs_path, Path} = Req#http_request.path,
    {DirStr, Pos, Direction, Qry} = parse_query(Path),
    ?Debug("List=~p Dirname~p~n", [List, DirName]), 

    Descriptions = read_descriptions(DirName),

    L0 = lists:zf(
           fun(F) ->
                   File = DirName ++ [$/|F],
                   FI = file:read_file_info(File),
                   file_entry(FI, DirName, F, Qry,Descriptions)
           end, List),

    L1 = lists:keysort(Pos, L0),

    L2 = if Direction == normal -> L1;
            Direction == reverse -> lists:reverse(L1)
         end,

    L3 = [Html || {_, _, _, _, Html} <- L2],
    
    Body = [ doc_head(DirStr),                          
             dir_header(DirName,DirStr),
             table_head(Direction),             
             parent_dir(),
             if 
                 DoAllZip == true -> 
                     allzip();
                 DoAllZip == true_nozip ->
                     [];
                true ->
                     []
             end,
             
%%              if DoAllGZip == true -> alltgz() end,
%%              if DoAllBZip2 == true -> alltbz2() end,

%%              if DoAllZip == true -> alltgz() end,
%%              if DoAllZip == true -> alltbz2() end,

             L3,
             table_tail(),
             dir_footer(DirName),%yaws:address(),
             doc_tail()
            ],
    
    B = list_to_binary(Body),

    yaws_server:accumulate_content(B),
    yaws_server:deliver_accumulated(Arg, CliSock, decide, undefined, final),
    yaws_server:done_or_continue().

parse_query(Path) ->
    case string:tokens(Path, [$?]) of
        [DirStr, [PosC, $=, DirC] = Q] ->
            Pos = case PosC of
                      $N -> 1; % name
                      $M -> 2; % last modified
                      $S -> 3; % size
                      $D -> 4  % Description                                            
                  end,
            Dir = case DirC of
                      $r -> reverse;
                      _  -> normal
                  end,
            {DirStr, Pos, Dir, "/?"++Q};
        _ ->
            {Path, 1, normal, ""}
    end.

parse_description(Line) ->
    L = string:strip(Line),
    Pos = string:chr(L,$ ),                                      
    Filename = string:substr(L, 1, Pos-1),
    D = string:substr(L,Pos+1),
    Description = string:strip(D,left),
    {Filename,Description}.

read_descriptions(DirName) ->
    File = DirName ++ [$/ | "MANIFEST.txt"],
    case file:read_file(File) of
        {ok,Bin} -> Lines = string:tokens(binary_to_list(Bin),"\n"),
                    lists:map(fun parse_description/1,Lines);
        _ -> []
    end.

get_description(Name,Descriptions) ->
    case lists:keysearch(Name,1,Descriptions) of
        {value, {_,Description}} -> Description;
        _ -> []
    end.

doc_head(DirName) ->
    HtmlDirName = yaws_api:htmlize(yaws_api:url_decode(DirName)),
    ?F("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"       
       "<html>\n"
       " <head>\n"
       "  <title>Index of ~s</title>\n"
       "  <style type=\"text/css\">\n"
       "    img { border: 0; padding: 0 2px; vertical-align: text-bottom; }\n"
       "    td  { font-family: monospace; padding: 2px 3px; text-align:left;\n"
       "          vertical-align: bottom; white-space: pre; }\n"
       "    td:first-child { text-align: left; padding: 2px 10px 2px 3px; }\n"
       "    table { border: 0; }\n"
       "  </style>\n"
       "</head> \n"
       "<body>\n",
       [HtmlDirName]
      ).

doc_tail() ->
"</body>\n"
"</html>\n".

table_head(Direction) ->
    NextDirection = if Direction == normal  -> "r";
                       Direction == reverse -> "n"
                    end,
    ["<table>\n"
     "  <tr>\n"
     "    <td><img src=\"/icons/blank.gif\" alt=\"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\"/><a href=\"?N=",NextDirection,"\">Name</a></td>\n"
     "    <td><a href=\"?M=",NextDirection,"\">Last Modified</a></td>\n"
     "    <td><a href=\"?S=",NextDirection,"\">Size</a></td>\n"
     "    <td><a href=\"?D=",NextDirection,"\">Description</a></td>\n"
     "  </tr>\n"
     "  <tr><th colspan=\"4\"><hr/></th></tr>\n"].

table_tail() ->
"  <tr><th colspan=\"4\"><hr/></th></tr>\n"
"</table>\n".


dir_footer(DirName) ->
    File = DirName ++ [$/ | "README.txt"],
    case file:read_file(File) of
        {ok,Bin} -> "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ -> yaws:address()
    end.

dir_header(DirName,DirStr) ->
    File = DirName ++ [$/ | "HEADER.txt"],
    case file:read_file(File) of
        {ok,Bin} -> "<pre>\n" ++ binary_to_list(Bin) ++ "</pre>\n";
        _ ->     HtmlDirName = yaws_api:htmlize(yaws_api:url_decode(DirStr)),
                 "<h1>Index of " ++ HtmlDirName ++ "</h1>\n"
    end.

parent_dir() ->    
    {Gif, Alt} = list_gif(directory,"."),    
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"..\">Parent Directory</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td></td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt
       ]).

%% FIXME: would be nice with a good size approx.  but it would require
%% a deep scan of possibly the entire docroot, (and also some knowledge
%% about zip's compression ratio in advance...)
allzip() ->
    {Gif, Alt} = list_gif(zip,""),    
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"all.zip\">all.zip</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td>Build a zip archive of current directory</td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt]).

alltgz() ->
    {Gif, Alt} = list_gif(zip,""),
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"all.tgz\">all.tgz</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td>Build a gzip archive of current directory</td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt]).

alltbz2() ->
    {Gif, Alt} = list_gif(zip,""),    
    ?F("  <tr>\n"
       "    <td><img src=~p alt=~p/><a href=\"all.tbz2\">all.tbz2</a></td>\n"
       "    <td></td>\n"
       "    <td>-</td>\n"
       "    <td>Build a bzip2 archive of current directory</td>\n"
       "  </tr>\n",
       ["/icons/" ++ Gif,
        Alt]).

is_user_dir(SP) ->
    case SP of
        [$/,$~ | T] -> User = string:sub_word(T,1,$/),
                       case catch yaws:user_to_home(User) of
                           {'EXIT', _} ->
                               false;
                           Home -> 
                               {true,Home}
                       end;        
        _ -> false
    end.

out(A) ->
    SP = A#arg.server_path,    
    PP = A#arg.appmod_prepath,
    Dir = case is_user_dir(SP) of
              {true,Home} -> Home ++ "/public_html";
              false -> A#arg.docroot
          end ++ PP,
    
%%    {html,?F("<h2>~p</h2>",[Dir])}.

    YPid = self(),
    
    case filename:basename(A#arg.server_path) of                    
        "all.zip" -> spawn_link(fun() -> zip(YPid, Dir) end),
                     {streamcontent, "application/zip", ""};
        "all.tgz" -> spawn_link(fun() -> tgz(YPid, Dir) end),
                     {streamcontent, "application/gzip", ""};                    
        "all.tbz2" -> spawn_link(fun() -> tbz2(YPid, Dir) end),
                      {streamcontent, "application/gzip", ""}
    end.

zip(YPid, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "zip -q -1 -r - ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(YPid, P).

tgz(YPid, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "tar cz ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(YPid, P).

tbz2(YPid, Dir) ->
    process_flag(trap_exit, true),    
    P = open_port({spawn, "tar cj ."},
                  [{cd, Dir},use_stdio, binary, exit_status]),
    stream_loop(YPid, P).

stream_loop(YPid, P) ->
    receive
        {P, {data, Data}} ->
            yaws_api:stream_chunk_deliver_blocking(YPid, Data),
            stream_loop(YPid, P);
        {P, {exit_status, _}} ->
            yaws_api:stream_chunk_end(YPid);
        {'EXIT', YPid, Status} ->
            exit(Status);
        Else ->
            error_logger:error_msg("Could not deliver zip file: ~p\n", [Else])
    end.


%% Removed code that appendended  Qry  to the file name.
%% It might still be a good idea in case  type==directory.
%% Was that the intention?
%% Carsten
%%
%% yes, that was the intention.  fixed now (mbj)
%% ... and maybe we should just remove this conversation in the next checkin :)
file_entry({ok, FI}, _DirName, Name, Qry, Descriptions) ->
    ?Debug("file_entry(~p) ", [Name]),
    Ext = filename:extension(Name),
    {Gif, Alt} = list_gif(FI#file_info.type, Ext),
    QryStr = if FI#file_info.type == directory -> Qry;
                true -> ""
             end,

    Description = get_description(Name,Descriptions),

    Entry = 
        ?F("  <tr>\n"
           "    <td><img src=~p alt=~p/><a href=~p title=~p>~s</a></td>\n"
           "    <td>~s</td>\n"
           "    <td>~s</td>\n"
           "    <td>~s</td>\n"
           "  </tr>\n",
           ["/icons/" ++ Gif,
            Alt,
            yaws_api:url_encode(Name) ++ QryStr,
            Name,
            trim(Name,20),
            datestr(FI), 
            sizestr(FI),
           Description]),
    ?Debug("Entry:~p", [Entry]),
    
    {true, {Name, FI#file_info.mtime, FI#file_info.size, Description, Entry}};

file_entry(_Err, _, _Name, _, _) ->
    ?Debug("no entry for ~p: ~p", [_Name, _Err]),
    false.

trim(L,N) ->
    trim(L,N,[]).
trim([_H|_T], 4, Acc) ->
    lists:reverse(Acc) ++ "..&gt;";
trim([H|T], I, Acc) ->
    trim(T, I-1, [H|Acc]);
trim([], _, Acc) ->
    lists:reverse(Acc).

%% FI -> 16-Jan-2006 23:06
datestr(FI) ->    
    {{Year, Month, Day}, {Hour, Min, _}} = FI#file_info.mtime,
    io_lib:format("~s-~s-~w ~s:~s",
                  [yaws:mk2(Day),yaws:month(Month),Year,
                   yaws:mk2(Hour),yaws:mk2(Min)]).

sizestr(FI) when FI#file_info.size > 1000000 ->
    ?F("~.1fM", [FI#file_info.size / 1000000]);
sizestr(FI) when FI#file_info.size > 1000 ->
    ?F("~wk", [trunc(FI#file_info.size / 1000)]);
sizestr(FI) when FI#file_info.size == 0 ->
    ?F("0k", []);
sizestr(_FI) ->
    ?F("1k", []). % As apache does it...

list_gif(directory, ".") ->
    {"back.gif", "[DIR]"};
list_gif(regular, ".txt") -> 
    {"text.gif", "[TXT]"};
list_gif(regular, ".c") ->
    {"c.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".dvi") ->
    {"dvi.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, ".pdf") ->
    {"pdf.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(regular, _) ->
    {"layout.gif", "[&nbsp;&nbsp;&nbsp;]"};
list_gif(directory, _) ->
    {"dir.gif", "[DIR]"};
list_gif(zip, _) ->
    {"compressed.gif", "[DIR]"};
list_gif(_, _) ->
    {"unknown.gif", "[OTH]"}.
