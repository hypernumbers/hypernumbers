%%% @author Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc gui_generator server.

-module(gui_generator_srv).
-behaviour(gen_server).

%%% API
-export([start/1]).

%%% API for internal use only
-export([tick/1]).

%%% gen_server API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {page}).

-define(SERVER, ?MODULE).
-define(CLOCKTICK, 10000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% gen_server API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Init
%% @hidden
init([]) ->
    {ok, #state{}}.

%% Handle call messages.
%% @hidden
handle_call({start, Page}, _From, State) ->
    NewState = State#state{page = Page},
    spawn_link(?MODULE, tick, [NewState]),
    Reply = ok,
    {reply, Reply, NewState}.

%% Handle cast messages.
%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle non-call/cast messages.
%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% Terminate -- close the logfile.
%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Page) ->
    gen_server:call(?MODULE, {start, Page}).

%%% @hidden
tick(#state{page = Page} = State) -> 
    hn_util:generate_po_CHEATING(Page),
    os:cmd(get_cmd()),
    timer:sleep(?CLOCKTICK),
    tick(State).

get_cmd() ->

    [_File, _Ebin ,_Gui_gen, _Lib | Rest] =
        lists:reverse(string:tokens(code:which(?MODULE), "/")),
    Pre = case os:type() of
              {win32,_} -> "";
              _         -> "/"
          end,
    Pre++string:join(lists:reverse(Rest),"/")++"/hypernumbers clear_po".
