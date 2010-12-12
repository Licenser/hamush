%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 29 Aug 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mcmd_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, split_cmd/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {pid, cmd, args}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Pid, Cmd) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Pid, Cmd) ->
    gen_server:start_link(?MODULE, [Pid, Cmd], []).

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
init([Pid, Cmd]) ->
    {Sender, _} = Pid,
    try
	case split_cmd(Cmd) of
	    [C | Args] -> 
		case mcmd_cmd_storage:get(C) of
		    {ok, F} -> {ok, #state{pid = Pid, cmd=F, args=Args}, 0};
		    not_found -> 
			hamush:pemit(Sender, "Huh?  (Type \"help\" for help.)\n"),
			{stop, not_found, #state{pid = Pid, cmd=C, args=Args}}
		end;
	    nomatch -> hamush:pemit(Sender, "Huh?  (Type \"help\" for help.)\n"),
		       {stop, invalid_command, #state{pid = Pid, cmd=Cmd, args=[]}}
	end
    catch
	_Class:_Error -> hamush:pemit(Sender, "Huh?  (Type \"help\" for help.)\n"),
			 {stop, invalid_command, #state{pid = Pid, cmd=Cmd, args=[]}}
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
handle_info(timeout, #state{pid = Pid, cmd = Cmd, args = Args} = State) ->
%    try
	Cmd(Pid, Args),
%    catch
%	_Class:_Error -> {Sender, _}  = Pid,
%			 hamush:pemit(Sender, "Error: Command failed!\n")
%    end,
    {stop, normal, State};
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


split_cmd(Cmd) ->
    case Cmd of
	"\"" ++ Rest ->
	    case re:run(Rest, "(.*?)[\n\r]+", [{capture, [1], list}]) of
		{match, [What]} ->
		    ["say", What]
	    end;
	":" ++ Rest ->
	    case re:run(Rest, "^(.*?)[\n\r]+\$", [{capture, [1], list}]) of
		{match, [What]} ->
		    ["pose", What]
	    end;
	";" ++ Rest ->
	    case re:run(Rest, "^(.*?)[\n\r]+\$", [{capture, [1], list}]) of
		{match, [What]} ->
		    ["semipose", What]
	    end;
	"&" ++ Rest ->
	    case re:run(Rest, "^(\\w+)\\s+(?:(\\w+))(?:\\s*=(.*?))?[\n\r]+\$", [{capture, [1, 2, 3], list}]) of
		{match, [Attr, Object, []]} ->
		    ["set", Attr, Object];
		{match, [Attr, Object, Value]} ->
		    ["set", Attr, Object, Value]
	    end;
	_ ->
	    case re:run(Cmd, "^([@]?\\w+)(?:/(\\w+))?(?:\\s+(?:(.*?)=)?(.*?))?[\n\r]+\$", [{capture, [1, 2, 3, 4], list}]) of
		{match, [CMD |Match]} -> [ string:to_lower(CMD) | [ X || X <- Match, X =/= []]];
		nomatch -> nomatch
	    end
    end.

	    
