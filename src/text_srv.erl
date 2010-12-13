%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net> [http://blog.licenser.net]
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 29 Aug 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(text_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, text/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {texts=dict:new()}).

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
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

text(ID) ->
  gen_server:call(?SERVER, {text, ID}).

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
  {ok, #state{}, 1}.

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
handle_call({text, ID}, _From, #state{texts = Texts} = State) ->
  case dict:find(ID, Texts) of
    {ok, [Value]} -> {reply, Value, State};
    _ -> {reply, {error, not_found}, State}
  end;
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
handle_info(timeout, _State) ->
    {noreply, #state{texts = map_files(list_files("texts"))}};
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



handle_file(File, Name, Dict) ->
  {ok, Cnt} = file:read_file(File),
  Content = erlang:bitstring_to_list(Cnt),
  dict:append(Name, Content, Dict).
  
join_with(A, J, B) ->
  if
    A =:= "" ->
      B;
    true ->
     string:concat(A, string:concat(J, B))
  end.

list_files(Dir) ->
  {ok, Files} = file:list_dir(Dir),
  lists:map(
    fun (File) ->
      NewFile = join_with(Dir, "/", File),
      case file:read_file_info(NewFile) of
        {ok, {file_info, _, directory, _,
          _, _, _,
          _, _, _, _, _, _ ,_}} -> 
          {File, list_files(NewFile)};
        _ -> 
          File
      end
    end, Files).

map_files(Files) ->
  map_files("texts", "", Files, dict:new()).
  
map_files(Path, Prefix, Files, Dict) ->
  lists:foldl(
  fun(File, D) ->
    case File of
      {Directory, Fs} -> 
        map_files(join_with(Path, "/", Directory), join_with(Prefix, ".", Directory), Fs, D);
      File ->
        Name = re:replace(File, "\\.txt$", "",[{return,list}]),
        handle_file(join_with(Path, "/", File), join_with(Prefix, ".", Name), D)
    end
  end, Dict, Files).