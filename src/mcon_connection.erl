%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mcon_connection).

-behaviour(gen_server).

%% API
-export([start_link/1, send/2, connect_object/2, set_mode/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {socket, object=undef, mode=object}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).


send(Pid, Text) ->
    gen_server:cast(Pid, {send, Text}). 

connect_object(Pid, ObjID) when is_pid(Pid), is_integer(ObjID)->
    gen_server:cast(Pid, {connect_object, ObjID}).

set_mode(Pid, Mode) ->
    gen_server:cast(Pid, {set_mode, Mode}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Sock]) ->
    inet:setopts(Sock, [{active, true}, {packet, line}]),
    {ok, #state{socket = Sock}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({send, Text}, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, Text),
    {noreply, State};
handle_cast({set_mode, Mode}, State) ->
    {noreply, State#state{mode = Mode}};
handle_cast({connect_object, ObjID}, State) ->
    mushdb:add_connection(ObjID, self()),
    {noreply, State#state{object = ObjID}};
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
handle_info({tcp, _Port, Data}, #state{object = ObjID, mode={repl, global}} = State) ->
  {ok, Pid} = mdb_store:lookup(ObjID),
  try 
    Result = mdb_element:eval(Pid, Data),
    hamush:pemit(self(), "~s\n> ", [Result])
  catch
    _:_ -> hamush:pemit(self(), "Ooops, something went wrong!")
  end
  {noreply, State};
handle_info({tcp, _Port, Data}, #state{object = ObjID, mode=object} = State) ->
    mushcmd:exec({self(), ObjID} , Data),
    {noreply, State};
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
terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
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
