%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(cmds_creation).

-export([init/0]).

init () ->
    mcmd_cmd_storage:register("create", fun create/2),
    mcmd_cmd_storage:register("@dig", fun dig/2).

create ({_Pid, ObjID}, [Name | _]) when is_integer(ObjID), is_pid(_Pid) ->
    NID = hamush:create(object, Name, ObjID),
    hamush:pemit(ObjID, "~s (#~w) has been created.~n", [Name, NID]).

dig ({_Pid, ObjID}, [Name, Exits]) when is_integer(ObjID), is_pid(_Pid) ->
    NID = hamush:create(room, Name),
    Location = hamush:location(ObjID),
    hamush:pemit(ObjID, "Room ~s (#~w) has been created.~n", [Name, NID]),
    case re:run(Exits, "([^,]*)(?:,(.*))?", [{capture, [1, 2], list}]) of
	{match, [To, []]} ->
	    ExitTo = exit_to(To, Location, NID),
	    hamush:pemit(ObjID, "Exit ~s (#~w) to ~s has been created.~n", [To, ExitTo, Name]);
	{match, [To, From]} ->
	    ExitTo = exit_to(To, Location, NID),
	    hamush:pemit(ObjID, "Exit ~s (#~w) to ~s has been created.~n", [To, ExitTo, Name]),
	    ExitFrom = exit_to(From, NID, Location),
	    hamush:pemit(ObjID, "Exit ~s (#~w) from ~s has been created.~n", [To, ExitFrom, Name])
    end;

dig ({_Pid, ObjID}, [Name]) when is_integer(ObjID), is_pid(_Pid) ->
    NID = hamush:create(room, Name),
    hamush:pemit(ObjID, "Room ~s (#~w) has been created.~n", [Name, NID]).


exit_to(Name, From, To) ->
    ToExit = hamush:create(exit, Name, From),
    hamush:set(ToExit, home, To),
    ToExit.
