%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(hn_rb).

-behaviour(gen_server).

%% External exports
-export([start/0, start/1, stop/0, rescan/0, rescan/1]).
-export([list/0, list/1, show/0, show/1, grep/1, start_log/1, stop_log/0]).
-export([h/0, help/0]).

%% Internal exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% Report Browser Tool.
%%% Formats Error reports written by log_mf_h
%%%-----------------------------------------------------------------

-record(state, {dir, data, device, max, type, abort, log}).

%%-----------------------------------------------------------------
%% Interface functions.
%% For available options; see print_options().
%%-----------------------------------------------------------------
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup, 
			   {rb_server, {hn_rb, start_link, [Options]},
			    temporary, brutal_kill, worker, [hn_rb]}).

start_link(Options) ->
    gen_server:start_link({local, rb_server}, hn_rb, Options, []).

stop() -> 
    gen_server:call(rb_server, stop),
    supervisor:delete_child(sasl_sup, rb_server).

rescan() -> rescan([]).
rescan(Options) ->
    gen_server:call(rb_server, {rescan, Options}, infinity).

list() -> list(all).
list(Type) -> gen_server:call(rb_server, {list, Type}, infinity).

show() -> 
    gen_server:call(rb_server, show, infinity).

show(Number) when is_integer(Number) -> 
    gen_server:call(rb_server, {show_number, Number}, infinity);
show(Type) when is_atom(Type) ->
    gen_server:call(rb_server, {show_type, Type}, infinity).

grep(RegExp) -> gen_server:call(rb_server, {grep, RegExp}, infinity).

start_log(FileName) -> gen_server:call(rb_server, {start_log, FileName}).

stop_log() -> gen_server:call(rb_server, stop_log).

h() -> help().
help() ->
    io:format("~nReport Browser Tool - usage~n"),
    io:format("===========================~n"),
    io:format("rb:start()         - start the rb_server with default options~n"),
    io:format("rb:start(Options)  - where Options is a list of:~n"),
    print_options(),
    io:format("rb:h()             - print this help~n"),
    io:format("rb:help()          - print this help~n"),
    io:format("rb:list()          - list all reports~n"),
    io:format("rb:list(Type)      - list all reports of type Type~n"),
    io:format("      currently supported types are:~n"),
    print_types(),
    io:format("rb:grep(RegExp)    - print reports containing RegExp~n"),
    io:format("rb:rescan()        - rescans the report directory with same~n"),
    io:format("                     options.~n"),
    io:format("rb:rescan(Options) - rescans the report directory with new~n"),
    io:format("                     options. Options is same as in start/1.~n"),
    io:format("rb:show(Number)    - print report no Number~n"),
    io:format("rb:show(Type)      - print all reports of type Type~n"),
    io:format("rb:show()          - print all reports~n"),
    io:format("rb:start_log(File) - redirect all reports to file~n"),
    io:format("rb:stop_log()      - close the log file and redirect to~n"),
    io:format("                     standard_io~n"),
    io:format("rb:stop            - stop the rb_server~n").

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% MAKE SURE THESE TWO FUNCTIONS ARE UPDATED!
%%-----------------------------------------------------------------
print_options() ->
    io:format("      {start_log, FileName}~n"),
    io:format("         - default: standard_io~n"),
    io:format("      {max, MaxNoOfReports}~n"),
    io:format("         - MaxNoOfReports should be an integer or 'all'~n"),
    io:format("         - default: all~n"),
    io:format("      {report_dir, DirString}~n"),
    io:format("         - DirString should be a string without trailing~n"),
    io:format("         - directory delimiter.~n"),
    io:format("         - default: {sasl, error_logger_mf_dir}~n"),
    io:format("      {type, ReportType}~n"),
    io:format("         - ReportType should be a supported type, 'all'~n"),
    io:format("         - or a list of supported types~n"),
    io:format("         - default: all~n"),
    io:format("      {abort_on_error, Bool}~n"),
    io:format("         - Bool: true | false~n"),
    io:format("         - default: false~n").

print_types() ->
    io:format("         - crash_report~n"),
    io:format("         - supervisor_report~n"),
    io:format("         - progress~n"),
    io:format("         - error~n").

	
init(Options) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),
    Log = get_option(Options, start_log, standard_io),
    Device = open_log_file(Log, group_leader()),
    Dir = get_report_dir(Options),
    Max = get_option(Options, max, all),
    Type = get_option(Options, type, all),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(Dir ++ "/", Max, Type, group_leader()),
    {ok, #state{dir = Dir ++ "/", data = Data, device = Device,
		max = Max, type = Type, abort = Abort, log = Log}}.

handle_call({rescan, Options}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    {Device,Log1} = 
	case get_option(Options, start_log, {undefined}) of
	    {undefined} -> 
		{State#state.device,State#state.log};
	    Log ->
		close_device(State#state.device),
		{open_log_file(Log, Leader),Log}
	end,
    Max = get_option(Options, max, State#state.max),
    Type = get_option(Options, type, State#state.type),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(State#state.dir, Max, Type, Leader),
    NewState = State#state{data = Data, max = Max, type = Type,
			   device = Device, abort = Abort, log = Log1},
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, #state{data = undefined}) ->
    {reply, {error, no_data}, #state{}};
handle_call({list, Type}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    print_list(State#state.data, Type, Leader),
    {reply, ok, State};
handle_call({start_log, FileName}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    NewDevice = open_log_file(FileName, Leader),
    {reply, ok, State#state{device = NewDevice}};
handle_call(stop_log, _From, State) ->
    close_device(State#state.device),
    {reply, ok, State#state{device = standard_io}};
handle_call({show_number, Number}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_report_by_num(Dir, Data, Number, Device, Abort, Log, Leader),
    {reply, ok, State#state{device = NewDevice}};
handle_call({show_type, Type}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),    
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_typed_reports(Dir, Data, Type, Device, Abort, Log, Leader),
    {reply, ok, State#state{device = NewDevice}};
handle_call(show, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_all_reports(Dir, Data, Device, Abort, Log, Leader),
    {reply, ok, State#state{device = NewDevice}};
handle_call({grep, RegExp}, {Pid, _Ref}, State) ->
    Leader = find_group_leader(Pid),
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    NewDevice = print_grep_reports(Dir, Data, RegExp, Device, Abort, Log, Leader),
    {reply, ok, State#state{device = NewDevice}}.

terminate(_Reason, #state{device = Device}) ->
    close_device(Device).

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: open_log_file/1
%% Args: FileName | standard_io
%% Returns: A Device for later use in call to io:format
%%-----------------------------------------------------------------
open_log_file(standard_io, _Leader) -> standard_io;
open_log_file(FileName, Leader) ->
    case file:open(FileName, [write,append]) of
	{ok, Fd} -> Fd;
	Error -> 
	    io:format(Leader, "rb: Cannot open file '~s' (~w).~n",
		      [FileName, Error]),
	    io:format(Leader, "rb: Using standard_io~n", []),
	    standard_io
    end.

close_device(Fd) when is_pid(Fd) ->
    catch file:close(Fd);
close_device(_) -> ok.

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

get_report_dir(Options) ->
    case lists:keysearch(report_dir, 1, Options) of
	{value, {_Key, RptDir}} -> RptDir;
	_ ->
	    case catch application:get_env(sasl, error_logger_mf_dir) of
		{ok, Dir} -> Dir;
		_ ->
		    exit("cannot locate report directory")
	    end
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max, Type)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%5               to read.
%%       Type ::= atom(), describing which reports to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements 
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max, Type, Leader) ->
    case file:open(RptDir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [LastWritten]} -> 
		    Files = make_file_list(RptDir, LastWritten),
		    scan_files(RptDir, Files, Max, Type, Leader);		
		_ -> exit("cannot read the index file")
	    end;
	_ -> exit("cannot read the index file")
    end.

make_file_list(Dir, FirstFileNo) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    FileNumbers = lists:zf(fun(Name) ->
					   case catch list_to_integer(Name) of
					       Int when is_integer(Int) ->
						   {true, Int};
					       _ ->
						   false
					   end
				   end,
				   FileNames),
	    shift(lists:sort(FileNumbers), FirstFileNo);
	_ -> exit({bad_directory, Dir})
    end.
					  
shift(List, First) -> 
    shift(List, First, []).

shift([H | T], H, Res) ->
    [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
    shift(T, First, [H | Res]);
shift([], _, Res) ->
    Res.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max, Type)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max, Type, Leader) ->
    scan_files(Dir, 1, Files, [], Max, Type, Leader).
scan_files(_Dir, _, [], Res, _Max, _Type, _Leader) -> Res;
scan_files(_Dir, _, _Files, Res, Max, _Type, _Leader) when Max =< 0 -> Res;
scan_files(Dir, No, [H|T], Res, Max, Type, Leader) ->
    Data = get_report_data_from_file(Dir, No, H, Max, Type, Leader),
    Len = length(Data),
    NewMax = dec_max(Max, Len),
    NewNo = No + Len,
    NewData = Data ++ Res,
    scan_files(Dir, NewNo, T, NewData, NewMax, Type, Leader).

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max, Type, Leader) ->	
    Fname = integer_to_list(FileNr),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> read_reports(No, Fd, Fname, Max, Type, Leader);
	_ -> [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max, Type)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max, Type, Leader) ->
    io:format(Leader, "rb: reading report...", []),
    case catch read_reports(Fd, [], Type) of
	{ok, Res} -> 
	    file:close(Fd),
	    io:format(Leader, "done.~n", []),
	    NewRes = 
		if
		    length(Res) > Max ->
			lists:sublist(Res, 1, Max);
		    true ->
			Res
		end,
	    add_report_data(NewRes, No, Fname);
	{error, [Problem | Res]} ->
	    file:close(Fd),
	    io:format(Leader, "Error: ~p~n",[Problem]),
	    io:format(Leader, "Salvaged ~p entries from corrupt report file ~s...~n",
		      [length(Res),Fname]),
	    NewRes = 
		if
		    length([Problem|Res]) > Max ->
			lists:sublist([Problem|Res], 1, Max);
		    true ->
			[Problem|Res]
		end,
	    add_report_data(NewRes, No, Fname);
	Else ->
	    io:format(Leader, "err ~p~n", [Else]),
	    [{No, unknown, "Can't read reports from file " ++ Fname,
		  "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
    add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
    add_report_data(T, No+1, FName,
		    [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], _No, _FName, Res) -> Res.

read_reports(Fd, Res, Type) ->
    {ok, FilePos} = file:position(Fd, cur),
    case catch read_report(Fd) of
	{ok, Report} -> 
	    RealType = get_type(Report),
	    {ShortDescr, Date} = get_short_descr(Report),
	    Rep = {RealType, ShortDescr, Date, FilePos},
	    if
		Type == all->
		    read_reports(Fd, [Rep | Res], Type);
		RealType == Type ->
		    read_reports(Fd, [Rep | Res], Type);
		is_list(Type) ->
		    case lists:member(RealType, Type) of
			true ->
			    read_reports(Fd, [Rep | Res], Type);
			_ ->
			    read_reports(Fd, Res, Type)
		    end;
		true ->
		    read_reports(Fd, Res, Type)
	    end;
	{error, Error} ->
	    {error, [{unknown, Error, [], FilePos} | Res]};
	eof ->
	    {ok, Res};
	{'EXIT', Reason} ->
	    [{unknown, Reason, [], FilePos} | Res]
    end.

read_report(Fd) ->
    case io:get_chars(Fd,'',2) of
        [Hi,Lo] ->
            Size = get_int16(Hi,Lo),
            case io:get_chars(Fd,'',Size) of
                eof ->
                    {error,"Premature end of file"};
                List ->
                    Bin = list_to_binary(List),
		    Ref = make_ref(),
		    case (catch {Ref,binary_to_term(Bin)}) of
			{'EXIT',_} ->
			    {error, "Inclomplete erlang term in log"};
			{Ref,Term} ->
			    {ok, Term}
		    end
	    end;
        eof ->
            eof
    end.
 
get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({_Time, {error_report, _Pid, {_, crash_report, _}}}) ->
    crash_report;
get_type({_Time, {error_report, _Pid, {_, supervisor_report, _}}}) ->
    supervisor_report;
get_type({_Time, {info_report, _Pid, {_, progress, _}}}) ->
    progress;
get_type({_Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({{Date, Time}, {error_report, Pid, {_, crash_report, Rep}}}) ->
    [OwnRep | _] = Rep,
    Name = 
	case lists:keysearch(registered_name, 1, OwnRep) of
	    {value, {_Key, []}} ->
		case lists:keysearch(initial_call, 1, OwnRep) of
		    {value, {_K, {M,_F,_A}}} -> M;
		    _ -> Pid
		end;
	    {value, {_Key, N}} -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date, Time)};
get_short_descr({{Date, Time}, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
    Name =
	case lists:keysearch(supervisor, 1, Rep) of
	    {value, {_Key, N}} when is_atom(N) -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date,Time)};
get_short_descr({{Date, Time}, {_Type, Pid, _}}) ->
    NameStr = lists:flatten(io_lib:format("~w", [Pid])),
    {NameStr, date_str(Date,Time)};
get_short_descr(_) ->
    {"???", "???"}.
    
date_str({Y,Mo,D}=Date,{H,Mi,S}=Time) ->
    case application:get_env(sasl,utc_log) of 
	{ok,true} ->
	    {{YY,MoMo,DD},{HH,MiMi,SS}} = 
		local_time_to_universal_time({Date,Time}),
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w UTC", 
					[YY,MoMo,DD,HH,MiMi,SS]));
	_ ->
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w", 
					[Y,Mo,D,H,Mi,S]))
    end.

local_time_to_universal_time({Date,Time}) ->
    case calendar:local_time_to_universal_time_dst({Date,Time}) of
	[UCT] ->
	    UCT;
	[UCT1,_UCT2] ->
	    UCT1;
	[] -> % should not happen
	    {Date,Time}
    end.


print_list(Data, Type, Leader) ->
    Header = {"No", "Type", "Process", "Date     Time"},
    Width = find_width([Header | Data], 0)+1,
    DateWidth = find_date_width([Header | Data], 0) +1,
    Format = lists:concat(["~4s~20s ~", Width, "s~20s~n"]),
    io:format(Leader, Format, tuple_to_list(Header)),
    io:format(Leader, Format, ["==", "====", "=======", "====     ===="]),
    print_list(Data, Type, Width, DateWidth, Leader).
print_list([], _, _, _, _) -> true;
print_list([H|T], Type, Width, DateWidth, Leader) ->
    print_one_report(H, Type, Width, DateWidth, Leader),
    print_list(T, Type, Width, DateWidth, Leader).

find_width([], Width) -> Width;
find_width([H|T], Width) ->
    Try = length(element(3, H)),
    if
	Try > Width -> find_width(T, Try);
	true -> find_width(T, Width)
    end.
find_date_width([], Width) -> Width;
find_date_width([H|T], Width) ->
    Try = length(element(4, H)),
    if
	Try > Width -> find_date_width(T, Try);
	true -> find_date_width(T, Width)
    end.

print_one_report({No, RealType, ShortDescr, Date, _Fname, _FilePos},
		 WantedType,
		 Width, DateWidth, Leader) ->
    if
	WantedType == all ->
	    print_short_descr(No, RealType, ShortDescr, Date, Width, 
			      DateWidth, Leader);
	WantedType == RealType ->
	    print_short_descr(No, RealType, ShortDescr, Date, Width, 
			      DateWidth, Leader);
	true -> ok
    end.

print_short_descr(No, Type, ShortDescr, Date, Width, DateWidth, Leader) ->
    Format = lists:concat(["~4w~20w ~", Width, "s~", DateWidth,"s~n"]),
    io:format(Leader, Format,
              [No,
               Type, 
		       io_lib:format("~s", [ShortDescr]),
		       Date]).

print_report_by_num(Dir, Data, Number, Device, Abort, Log, Leader) ->
    {_,Device1} = print_report(Dir, Data, Number, Device, Abort, Log, Leader),
    Device1.
    
print_typed_reports(_Dir, [], _Type, Device, _Abort, _Log, _Leader) ->
    Device;
print_typed_reports(Dir, Data, Type, Device, Abort, Log, Leader) ->
    {Next,Device1} =
	case element(2, hd(Data)) of
	    Type -> 
		print_report(Dir, Data, element(1, hd(Data)), Device, Abort,
                     Log, Leader);
	    _ -> 
		{proceed,Device}
	end,
    if Next == abort ->
	    Device1;
       true ->
	    print_typed_reports(Dir, tl(Data), Type, Device1, Abort, Log, Leader)
    end.

print_all_reports(_Dir, [], Device, _Abort, _Log, _Leader) ->
    Device;
print_all_reports(Dir, Data, Device, Abort, Log, Leader) ->
    {Next,Device1} = print_report(Dir, Data, element(1, hd(Data)), 
				  Device, Abort, Log, Leader),
    if Next == abort ->
	    Device1;
       true ->
	    print_all_reports(Dir, tl(Data), Device1, Abort, Log, Leader)
    end.

print_report(Dir, Data, Number, Device, Abort, Log, Leader) ->
    case find_report(Data, Number, Leader) of
	{Fname, FilePosition} ->
	    FileName = lists:concat([Dir, Fname]),
	    case file:open(FileName, [read]) of
		{ok, Fd} -> 
		    read_rep(Fd, FilePosition, Device, Abort, Log, Leader);
		_ -> 
		    io:format(Leader, "rb: can't open file ~p~n", [Fname]),
		    {proceed,Device}
	    end;
	no_report ->
	    {proceed,Device}
    end.

find_report([{No, _Type, _Descr, _Date, Fname, FilePosition}|_T], No, _Leader) ->
    {Fname, FilePosition};
find_report([_H|T], No, Leader) -> 
    find_report(T, No, Leader);
find_report([], No, Leader) ->
    io:format(Leader, "There is no report with number ~p.~n", [No]),
    no_report.
    
print_grep_reports(_Dir, [], _RegExp, Device, _Abort, _Log, _Leader) ->
    Device;
print_grep_reports(Dir, Data, RegExp, Device, Abort, Log, Leader) ->
    {Next,Device1} = print_grep_report(Dir, Data, element(1, hd(Data)), 
				       Device, RegExp, Abort, Log, Leader),
    if Next == abort ->
	    Device1;
       true ->
	    print_grep_reports(Dir, tl(Data), RegExp, Device1, Abort, Log, Leader)
    end.

print_grep_report(Dir, Data, Number, Device, RegExp, Abort, Log, Leader) ->
    {Fname, FilePosition} = find_report(Data, Number, Leader),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> 
	    check_rep(Fd, FilePosition, Device, RegExp, Number, Abort, Log, Leader);
	_ -> 
	    io:format(Leader, "rb: can't open file ~p~n", [Fname]),
	    {proceed,Device}
    end.

check_rep(Fd, FilePosition, Device, RegExp, Number, Abort, Log, Leader) ->
    case read_rep_msg(Fd, FilePosition) of
	{Date, Msg} ->
	    MsgStr = lists:flatten(io_lib:format("~p",[Msg])),
	    case regexp:match(MsgStr, RegExp) of
		{match, _, _} ->
		    io:format(Leader, "Found match in report number ~w~n", [Number]),
		    case catch rb_format_supp:print(Date, Msg, Leader) of
			{'EXIT', _} ->
			    handle_bad_form(Date, Msg, Device, Abort, Log, Leader);
			_ ->
			    {proceed,Device}
		    end;		
		_ ->
		    {proceed,Device}
	    end;
	_ ->
	    io:format(Leader, "rb: Cannot read from file~n", []),
	    {proceed,Device}
    end.

read_rep(Fd, FilePosition, Device, Abort, Log, Leader) ->
    case read_rep_msg(Fd, FilePosition) of
	{Date, Msg} ->
	    case catch rb_format_supp:print(Date, Msg, Leader) of
		{'EXIT', _} ->
		    handle_bad_form(Date, Msg, Device, Abort, Log, Leader);
		_ ->
		    {proceed,Device}
	    end;
	_ -> 
	    io:format(Leader, "rb: Cannot read from file~n", []),
	    {proceed,Device}
    end.
    
handle_bad_form(Date, Msg, Device, Abort, Log, Leader) ->
    io:format(Leader, "rb: ERROR! A report on bad form was encountered. " ++
	      "It can not be printed to the log.~n~n", []),
    io:format(Leader, "Details:~n~p ~p~n~n", [Date,Msg]),
    case {Abort,Device,open_log_file(Log, Leader)} of
	{true,standard_io,standard_io} ->
	    io:format(Leader, "rb: Logging aborted.~n", []),
	    {abort,Device};
	{false,standard_io,standard_io} ->
	    io:format(Leader, "rb: Logging resumed...~n~n", []),
	    {proceed,Device};
	{_,_,standard_io} ->
	    io:format(Leader, "rb: Can not reopen ~p. Logging aborted.~n", [Log]),
	    {abort,Device};
	{true,_,NewDevice} ->
	    io:format(NewDevice,
		      "~n~n************************* RB ERROR ************************~n" ++
		      "A report on bad form was encountered here and the logging~n" ++
		      "process was aborted. Note that there may well be remaining~n" ++
		      "reports that haven't yet been logged. Please see the rb~n" ++
		      "manual for more info.~n" ++
		      "***********************************************************~n", []),
	    io:format(Leader, "rb: Logging aborted.~n", []),
	    {abort,NewDevice};
	{false,_,NewDevice} ->
	    io:format(NewDevice, 
		      "~n   ********* RB: UNPRINTABLE REPORT ********~n~n", []),
	    io:format(Leader, "rb: Logging resumed...~n~n", []),	    
	    {proceed,NewDevice}
    end.

read_rep_msg(Fd, FilePosition) ->
    file:position(Fd, {bof, FilePosition}),
    Res = 
	case catch read_report(Fd) of
	    {ok, Report} ->
		{_ShortDescr, Date} = get_short_descr(Report),
		{Date, Report};
	    _ -> error
	end,
    file:close(Fd),
    Res.

find_group_leader(Pid) when is_pid(Pid) ->
    {group_leader, Leader} =
        case is_alive() of
            true ->
                rpc:call(node(Pid), erlang, process_info, [Pid, group_leader]);
            false ->
                process_info(Pid, group_leader)
        end,
    Leader.
