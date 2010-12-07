%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(cmds_interaction).

-export([init/0]).

init () ->
    mcmd_cmd_storage:register("set",      fun set/2),
    mcmd_cmd_storage:register_many(
      ["@describe", "@desc"],    fun desc/2),
    mcmd_cmd_storage:register_many(["l", "lo", "loo", "look"],     fun look/2),
    mcmd_cmd_storage:register("say",      fun say/2),
    mcmd_cmd_storage:register("pose",     fun pose/2),
    mcmd_cmd_storage:register("semipose", fun semipose/2).


set ({_Pid, ObjID}, [Attr, Obj, Value]) when is_integer(ObjID), is_pid(_Pid) ->
    case hamush:resolve(ObjID, Obj) of
	{ok, ID} -> hamush:set(ID, Attr, Value);
	{not_found, _} -> hamush:pemit(ObjID, "I don't see this here.\n");
	{found_too_many, _} -> hamush:pemit(ObjID, "I don't know what you mean.\n")
    end.

desc ({_Pid, ObjID}, [Obj, Value]) when is_integer(ObjID), is_pid(_Pid) ->
    case hamush:resolve(ObjID, Obj) of
	{ok, ID} -> hamush:fset(ID, "DESCRIPTION", Value);
	{not_found, _} -> hamush:pemit(ObjID, "I don't see this here.\n");
	{found_too_many, _} -> hamush:pemit(ObjID, "I don't know what you mean.\n")
    end.

    
say ({_Pid, ObjID}, [What | _]) when is_integer(ObjID), is_pid(_Pid) ->
    hamush:remit_but(hamush:location(ObjID), [ObjID], "~s says, \"~s\".~n", [hamush:name(ObjID), What]),
    hamush:pemit(ObjID, io_lib:format("You say, \"~s\".~n", [hamush:eval(What, ObjID)])).

pose ({_Pid, ObjID}, [What | _]) when is_integer(ObjID), is_pid(_Pid) ->
    hamush:remit(hamush:location(ObjID), "~s ~s~n", [hamush:name(ObjID), hamush:eval(What, ObjID)]).

semipose ({_Pid, ObjID}, [What | _]) when is_integer(ObjID), is_pid(_Pid) ->
    hamush:remit(hamush:location(ObjID), "~s~s~n", [hamush:name(ObjID), hamush:eval(What, ObjID)]).

look ({_Pid, ObjID}, [What | _]) when is_integer(ObjID), is_pid(_Pid) ->
    case hamush:resolve(ObjID, What) of
	{ok, ID} -> hamush:pemit(ObjID, hamush:desc(ID, ObjID));
	{not_found, _} -> hamush:pemit(ObjID, "I don't see this here.\n");
	{found_too_many, _} -> hamush:pemit(ObjID, "I don't know what you mean.\n")
    end;
look ({_Pid, ObjID}, _) when is_integer(ObjID), is_pid(_Pid) ->
    Room = hamush:location(ObjID),
    hamush:pemit(ObjID, hamush:desc(Room, ObjID)).
