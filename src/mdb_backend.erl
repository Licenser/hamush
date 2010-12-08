-module(mdb_backend).
-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, insert/3,count/0, objects/0, get_object/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, attribute).


-record(attribute, {attr_id, attr_value}).
-record(state, {server=node(), table=attribute}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?SERVER, [], []).

insert(ObjID, Name, Value) ->
  gen_server:call(?SERVER, {insert, ObjID, Name, Value}).

objects() ->
  gen_server:call(?SERVER, objects).

get_object(Id) ->
  gen_server:call(?SERVER, {get_object, Id}).

count() ->
  gen_server:call(?SERVER, count).

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
init([]) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:create_table(?TABLE,
      [{disc_copies, [node()]},
        {attributes, record_info(fields,attribute)}]),
    {ok, #state{}}
  catch
    _ -> {ok, #state{}}
  end.

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
handle_call(count, _From, State) ->
  {atomic, Elements} = mnesia:transaction( 
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(?TABLE) ] 
        )) 
    end),
  {reply, length(Elements), State};
handle_call(objects, _From, State) ->
  {atomic, Objects} = mnesia:transaction(
    fun() -> qlc:eval(qlc:q(
      [ Id || {attribute, {Id, _}, _} <- mnesia:table(attribute) ], 
      {unique, true}
    ))end),
  {reply, Objects, State};
handle_call({get_object, Id}, _From, State) ->
  {atomic, Attrs} = mnesia:transaction(
    fun() -> qlc:eval(qlc:q(
      [ {Attr, Value} || {attribute, {ObjId, Attr}, Value} <- mnesia:table(attribute), Id =:= ObjId ]
    ))end),
  {reply, Attrs, State};
handle_call({insert, ObjID, Name, Value}, _From, State) ->
  Fun = fun() ->
          mnesia:write(
            #attribute{attr_id={ObjID, Name},
                       attr_value=Value})
        end,
  case mnesia:transaction(Fun) of
	{atomic,_} -> {reply, ok, State};
	F -> {reply, {failed, F}, State}
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
handle_cast(_, State) ->
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

