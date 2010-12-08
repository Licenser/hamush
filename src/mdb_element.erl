%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mdb_element).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 add_connection/2,
	 create/1,
	 get/1,
	 get/2,
	 set/3,
	 delete/1,
	 hear/2,
	 get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {id=0, attrs=dict:new(), home=-1, connections=[]}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Value) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

create(Id) ->
    mdb_store_sup:start_child(Id).

get(Pid) ->
    mdb_event:get(Pid),
    gen_server:call(Pid, get).

get_state(Pid) ->
    gen_server:call(Pid, state).


get(Pid, Attr) ->
    mdb_event:get(Pid, Attr),
    gen_server:call(Pid, {get, Attr}).

set(Pid, Attr, Value) ->
    mdb_event:set(Pid, Attr, Value),
    gen_server:cast(Pid, {set, Attr, Value}).

add_connection(Pid, Connection) ->
    gen_server:cast(Pid, {add_connection, Connection}).

delete(Pid) ->
    mdb_event:delete(Pid),
    gen_server:cast(Pid, delete).

hear(Pid, Message) ->
    gen_server:cast(Pid, {hear, Message}).    


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
init([Id]) ->
    {ok, #state{id=Id}}.

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
handle_call(get, _From, State) ->
    Reply = {ok, State},
    {reply, Reply, State};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({get, home}, _From, #state{home = Home} = State) ->
    {reply, {ok, Home}, State};
handle_call({get, Attr}, _From, State) ->
    case dict:find(Attr, State#state.attrs) of
	{ok, [Value]} -> {reply, {ok, Value}, State};
	_ -> {reply, attr_not_found , State}
    end.

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
handle_cast(delete, State) ->
    {stop, State};
handle_cast({add_connection, Connection}, #state{connections = Cons} = State) ->
    Cons1 = [Connection | Cons],
    State1 = State#state{connections = Cons1},
    {noreply, State1};
handle_cast({hear, Message}, #state{connections = Cons} = State) ->
    lists:foreach(fun (Pid) ->
			 mcon_connection:send(Pid, Message)
		 end, Cons),
    {noreply, State};
handle_cast({set, home, ID}, State) when is_integer(ID)->
    {noreply, State#state{home=ID}};
handle_cast({set, Attr, Value}, State) ->
    mdb_backend:insert(State#state.id, Attr, Value),
    {noreply, State#state{attrs=dict:append(Attr, Value, dict:erase(Attr, State#state.attrs))}}.


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
    mdb_store:delete(self()),
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
