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
         load/0, load/1,
         update/2,
         create/0, create/1, create/2, create/3,
         delete/1, 
         lookup/1, lookup/2,
         match/1,
         name/1,
         location/1, location/2, 
         rename/2,
         insert/5, insert/2]).

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

load(Id) ->
  [{'Next ID', NextID}] = ets:lookup(?TABLE_ID, 'Next ID'),
  io:format("id: ~w - ~w~n", [Id, NextID]),
  if 
     NextID < Id ->
       ets:insert(?TABLE_ID, {'Next ID', Id + 1});
     NextID =:= Id ->
       ets:insert(?TABLE_ID, {'Next ID', Id + 1});
     true -> ok
  end,
  [{'Next ID', NextID2}] = ets:lookup(?TABLE_ID, 'Next ID'),  
  io:format("id: ~w - ~w~n", [Id, NextID2]),
  {ok, Obj} = mdb_element:create(Id).
%  insert(Id, Obj).
load() ->
    lists:foreach(
      fun load/1,
      mdb_backend:objects()).
                 

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
    hamush:fset(NextID, type, Type),
    hamush:fset(NextID, name, Name),
    hamush:fset(NextID, location, Location),
    NextID.

update(ObjID, {name, Name}) ->
  ets:update_element(?TABLE_ID, ObjID, {?POS_name, Name});
update(ObjID, {type, Type}) ->
  ets:update_element(?TABLE_ID, ObjID, {?POS_type, Type});
update(ObjID, {location, Location}) ->
  ets:update_element(?TABLE_ID, ObjID, {?POS_location, Location});
update(_, {_, _}) -> 
  ok.

insert(ID, Pid) ->
    ets:insert(?TABLE_ID, {ID, object, "Unnamed", 0, Pid}).
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
