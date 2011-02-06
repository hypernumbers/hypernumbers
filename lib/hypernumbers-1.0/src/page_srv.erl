%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc       The page server keeps a record of pages
%%%
%%% @end
%%% Created : 26 Jan 2011 by gordon@hypernumbers
%%%-------------------------------------------------------------------
-module(page_srv).

-behaviour(gen_server).

-include("keyvalues.hrl").

%% API
-export([start_link/1]).

-export([
         page_written/2,
         page_deleted/2,
         get_pages/1,
         does_page_exist/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {site, pages = []}).

%%%===================================================================
%%% API
%%%===================================================================
page_written(Site, Path) when is_list(Path) ->
    Id = hn_util:site_to_atom(Site, "_pages"),
    gen_server:call(Id, {page_written, Path}).

page_deleted(Site, Path) when is_list(Path) ->
    Id = hn_util:site_to_atom(Site, "_pages"),
    gen_server:call(Id, {page_deleted, Path}).

get_pages(Site) ->
    Id = hn_util:site_to_atom(Site, "_pages"),
    gen_server:call(Id, get_pages).

does_page_exist(Site, Path) ->
    Id = hn_util:site_to_atom(Site, "_pages"),
    gen_server:call(Id, {does_page_exist, Path}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Site) ->
    Id = hn_util:site_to_atom(Site, "_pages"),
    gen_server:start_link({local, Id}, ?MODULE, [Site], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Site]) ->
    [{kvstore, ?pages, Pages}] = hn_db_api:read_kv(Site, ?pages),
    {ok, #state{site = Site, pages = Pages}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, #state{site = Site, pages = Pages} = State) ->
    {Rep, NewP} = case Request of
                      {page_written, P} ->
                          case lists:member(P, Pages) of
                              true  -> {ok, Pages};
                              false -> P2 = [P | Pages],
                                       ok = hn_db_api:write_kv(Site, ?pages, P2),
                                       {ok, P2}
                          end;
                      {page_deleted, P} ->
                          P2 = lists:delete(P, Pages),
                          ok = hn_db_api:write_kv(Site, ?pages, P2),
                          {ok, P2};
                      get_pages ->
                          {Pages, Pages};
                      {does_page_exist, P} ->
                          {lists:member(P, Pages), Pages}
                  end,
    {reply, Rep, State#state{pages = NewP}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
