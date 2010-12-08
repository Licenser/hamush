%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ham_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    mdb_store:init(),
    case ham_sup:start_link() of
      {ok, Pid} ->
        case mdb_backend:count() of
          0 ->
            RoomOne = hamush:create(room, "Room One"),
            hamush:set(RoomOne, "DESCRIPTION", "This is the first room."),
            God = hamush:create(user, "God", RoomOne),
            RoomTwo = hamush:create(room, "Room Two"),
            hamush:set(RoomTwo, "DESCRIPTION", "This is the second room."),
            Otto = hamush:create(user, "Otto", RoomOne),
            Door12 = hamush:create(exit, "Two", RoomOne),
            Door21 = hamush:create(exit, "One", RoomTwo),
            hamush:fset(God, "PASSWORD", "123"),
            hamush:fset(Otto, "PASSWORD", "123"),
            hamush:set(Door12, home, RoomTwo),
            hamush:set(Door21, home, RoomOne),
            hamush:set(God, home, RoomOne),
            hamush:set(Otto, home, RoomOne);
          _ -> 
            mdb_store:load(),
            ok
        end,
      {ok, Pid};
        Error ->
          Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
