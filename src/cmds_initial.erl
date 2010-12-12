%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net> [http://blog.licenser.net]
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(cmds_initial).

-export([init/0]).

init() ->
  mcmd_cmd_storage:register("connect", fun connect/2).

connect ({Pid, undef}, [Login | _]) when  is_pid(Pid)->
    [Name, Password] = string:tokens(Login, " "),
    [ObjID] = mushdb:match({user, Name}),
    {ok, Pwd} = hamush:fget(ObjID, "PASSWORD"),
    if 
	Pwd =:= Password -> mcon_connection:connect_object(Pid, ObjID),
			    hamush:pemit(ObjID, "Login successful.~n~s~n", [hamush:desc(hamush:location(ObjID), ObjID)]);
	true -> hamush:pemit(Pid, "Login failed.\n")
    end.

