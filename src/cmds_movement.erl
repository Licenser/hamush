%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(cmds_movement).

-export([init/0]).

init() ->
  mcmd_cmd_storage:register("go", fun go/2).

go ({_Pid, ObjID}, [To | _]) when is_integer(ObjID), is_pid(_Pid) ->
  case hamush:resolve(ObjID, To)  of
    {ok, Exit} -> 
      {ok, Dest} = hamush:get(Exit, home),
      hamush:move_to(ObjID, Dest);
    {not_found, _} -> hamush:pemit(ObjID, "You can’t go that way.\n");
    {found_too_many, _} -> hamush:pemit(ObjID, "I don't know what way to go.\n")
  end.
