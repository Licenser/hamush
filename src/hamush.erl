%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(hamush).

-export([move_to/2, 
	 content/1,
	 get/2,
	 fget/2,
	 desc/2,
	 pemit/3,
	 location/1,
	 name/1,
	 exits/1,
	 eval_get/3,
	 eval/2,
	 resolve/2,
	 create/0, create/1, create/2, create/3,
	 pemit/2,
	 set/3,
	 remit/2, remit/3,
	 remit_but/3, remit_but/4,
	 fset/3]).

content(ObjID) when is_integer(ObjID) ->
    mdb_store:match({'_', '_', ObjID}).

exits(ObjID) when is_integer(ObjID) ->
    mdb_store:match({exit, '_', ObjID}).

move_to(ObjID, RoomID) when is_integer(ObjID), is_integer(RoomID)->
    Location = hamush:location(ObjID),
    hamush:remit_but(Location, [ObjID], "~s has left.~n", [hamush:name(ObjID)]),
    mdb_store:location(ObjID, RoomID),
    hamush:pemit(ObjID, desc(RoomID, ObjID)),
    hamush:remit_but(RoomID, [ObjID], "~s has left.~n", [hamush:name(ObjID)]).

location(ObjID) when is_integer(ObjID) ->
    {ok, Location} = mdb_store:location(ObjID),
    Location.

set(ObjID, Attr, Value) when is_integer(ObjID), is_atom(Attr) ->
    mushdb:set(ObjID, Attr, Value);
set(ObjID, Attr, Value) when is_integer(ObjID) ->
    mushdb:set(ObjID, string:to_upper(Attr), Value).


fset(ObjID, Attr, Value) when is_integer(ObjID) ->
    mushdb:set(ObjID, Attr, Value).

get(ObjID, Attr) when is_integer(ObjID), is_atom(Attr) ->
    mushdb:get(ObjID, Attr);
get(ObjID, Attr) when is_integer(ObjID) ->
    mushdb:get(ObjID, string:to_upper(Attr)).

fget(ObjID, Attr) when is_integer(ObjID) ->
    mushdb:get(ObjID, Attr).

name(ObjID) when is_integer(ObjID) ->
    case mdb_store:name(ObjID) of
	{ok, Name} -> Name;
	_ -> ""
    end.

create() ->
    create("Unnamed").
create(Name) ->
    create(object, Name).
create(Type, Name) ->
    create(Type, Name, -1).
create(Type, Name, Location) ->
    mdb_store:create(Type, Name, Location).


pemit(ObjID, Message) when is_integer(ObjID) ->
    if
	ObjID >= 0 ->
	    {ok, Pid} = mdb_store:lookup(ObjID),
	    mdb_element:hear(Pid, Message)
    end;
pemit(Pid, Message) when is_pid(Pid) ->
    mcon_connection:send(Pid, Message).

pemit(To, Format, Arguments) ->
    pemit(To, io_lib:format(Format, Arguments)).
    

remit_but(ObjID, But, Format, Args) when is_integer(ObjID) ->
    remit_but(ObjID, But, io_lib:format(Format, Args)).
remit_but(ObjID, But, Message) when is_integer(ObjID) ->
    if
	ObjID >= 0 ->
	    Content = lists:filter(fun (ID) ->
					   not lists:any(fun (ID1) ->
								 ID =:= ID1
							 end, But)
				   end,
				   hamush:content(ObjID)),
	    lists:foreach(fun (ID) ->
				  pemit(ID, Message)
			  end, Content)
    end.
remit(ObjID, Format, Args) when is_integer(ObjID) ->
    remit(ObjID, io_lib:format(Format, Args)).
remit(ObjID, Message) when is_integer(ObjID) ->
    Content = hamush:content(ObjID),
    lists:foreach(fun (ID) ->
			  pemit(ID, Message) 
		  end, Content).

resolve(Observer, "me") when is_integer(Observer) ->
    {ok, Observer};
resolve(Observer, "here") when is_integer(Observer) ->
    {ok, hamush:location(Observer)};
resolve(Observer, Lookup) when is_integer(Observer) ->
    case  re:run(Lookup,"#(\\d+)|(\\w+)|\\*(\\w+)", [{capture, [1, 2, 3], list}]) of
	{match, [IDs, [], []]} -> 
	    {ID, []} = string:to_integer(IDs),
	    {ok, ID};
	{match, [[], Local, []]} ->
	    Location = hamush:location(Observer),
	    filter_by_name(Local, [Location | hamush:content(Location) ++ hamush:content(Observer)]);
	{match, [[], [], Glob]} -> 
	    filter_by_name(Glob, mdb_store:match({user, '_'}))
    end.
	    
filter_by_name(Name, ObjIDs) ->
    LName = string:to_lower(Name),
    Len = string:len(LName),
    case lists:filter(fun (E) ->
			      string:to_lower(string:substr(hamush:name(E), 1, Len)) =:= LName
		      end,
		      ObjIDs) of
	[Result] -> {ok, Result};
	[] -> {not_found, -1};
	_ -> {found_too_many, -1}
    end.

desc(ObjID, Observer) -> 
    Content = string:join(
		lists:map(fun (ID) ->
				  hamush:name(ID)
			  end, hamush:content(ObjID)), [13,10]),
    io_lib:format("~s~n~s~n~n~s~n", [ hamush:name(ObjID),  hamush:eval_get(ObjID, "DESCRIPTION", Observer), Content]).


eval_get(ObjID, Attribute, Actor) ->
    case hamush:get(ObjID, Attribute) of
	{ok, Value} -> ham_lisp:run_by(Actor, ObjID, Value);
	{error, not_found} -> ""
    end.

eval(Code, Actor) ->
    ham_lisp:run_by(Actor, Code).
