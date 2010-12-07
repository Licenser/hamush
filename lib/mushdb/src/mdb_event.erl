%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mdb_event).

-export([start_link/0,
	 get/1,
	 get/2,
	 create/1,
	 set/3,
	 delete/1]).

-define(SERVER, ?MODULE).

start_link() -> 
    gen_event:start_link({local, ?SERVER}).

create(ID) ->
    gen_event:notify(?SERVER, {create, ID}).

get(Pid) ->
    gen_event:notify(?SERVER, {get, Pid}).
get(Pid, Attr) ->
    gen_event:notify(?SERVER, {get, Pid, Attr}).



set(Pid, Attr, Value) ->
    gen_event:notify(?SERVER, {set, Pid, Attr, Value}).

delete(ID) ->
    gen_event:notify(?SERVER, {delete, ID}).
