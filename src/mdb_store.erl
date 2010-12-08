%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mdb_store).

-export([init/0,
	 create/0, create/1, create/2, create/3,
	 delete/1, 
	 lookup/1, lookup/2,
	 match/1,
	 name/1,
	 location/1, location/2, 
	 rename/2,
	 insert/5]).

-define(TABLE_ID, ?MODULE).
% {id, type, name, location, PID}
-define(POS_id, 1).
-define(POS_type, 2).
-define(POS_name, 3).
-define(POS_location, 4).
-define(POS_pid, 5).

init() ->
    Table = ets:new(?TABLE_ID, [public, named_table]),
    ets:insert(?TABLE_ID, {'Next ID', 0}),
    Table.

next_id() ->
    [{'Next ID', NextID}] = ets:lookup(?TABLE_ID, 'Next ID'),
    ets:insert(?TABLE_ID, {'Next ID', NextID + 1}),
    NextID.

create() ->
    create("Unnamed").

create(Name) ->
    create(object, Name).

create(Type, Name) ->
    create(Type, Name, 0).

create(Type, Name, Location) ->
    NextID = next_id(),
    mdb_event:create(NextID),
    {ok, Element} = mdb_element:create(NextID),
    insert(NextID, Type, Name, Location, Element),
    NextID.

insert(ID, Type, Name, Location, Pid) ->
    ets:insert(?TABLE_ID, {ID, Type, Name, Location, Pid}).

lookup(ID) when is_integer(ID)->
    case ets:lookup(?TABLE_ID, ID) of
	[{ID, _, _, _, Pid}] ->
	    {ok, Pid};
	[] ->
	    {error, not_found}
    end.

match({Type, Name}) ->
     [ID || [ID] <- ets:match(?TABLE_ID,{'$1', Type, Name, '_', '_'})];
match({Type, Name, Location}) ->
     [ID || [ID] <- ets:match(?TABLE_ID,{'$1', Type, Name, Location, '_'})].

lookup(ID, Attr) ->
    case lookup(ID) of
	{ok, Pid} -> mdb_element:get(Pid, Attr);
	Error -> Error
    end.


delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

location(ObjID) ->
    case ets:lookup(?TABLE_ID, ObjID) of
	[{_, _, _, Location, _}] ->
	    {ok, Location};
	[] ->
	    {error, not_found}
    end.

name(ObjID) ->
    case ets:lookup(?TABLE_ID, ObjID) of
	[{_, _, Name, _, _}] ->
	    {ok, Name};
	[] ->
	    {error, not_found}
    end.


location(ObjID, Room) when is_integer(ObjID), is_integer(Room) ->
    ets:update_element(?TABLE_ID, ObjID, {?POS_location, Room}).


rename(ObjID, Name) ->
    ets:update_element(?TABLE_ID, ObjID, {?POS_name, Name}).
