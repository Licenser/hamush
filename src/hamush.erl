%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net> [http://blog.licenser.net]
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%% This module contains the central interface functions for hamush.
%%% All modules/extensions or other code wanting to interact with 
%%% objects in the virtual world should use the functions provided
%%% here to do so.
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
   text/1,
   fset/3]).



%%--------------------------------------------------------------------
%% @doc
%% Retreives a text from the text server.
%%
%% @spec text(ID::string()) -> string()
%% @end
%%--------------------------------------------------------------------
text(ID) ->
  case text_srv:text(ID) of
    {error, _} -> "";
    V -> V
  end.

%%--------------------------------------------------------------------
%% @doc
%% Lists all objects that are contained ObjID.
%%
%% @spec content(ObjID::integer()) -> [] | [integer()]
%% @end
%%--------------------------------------------------------------------
content(ObjID) when is_integer(ObjID) ->
  mdb_store:match({'_', '_', ObjID}).

%%--------------------------------------------------------------------
%% @doc
%% Lists all exits in a location.
%%
%% @spec exits(ObjID::integer()) -> [] | [ExitID::integer()]
%% @end
%%--------------------------------------------------------------------
exits(ObjID) when is_integer(ObjID) ->
  mdb_store:match({exit, '_', ObjID}).

%%--------------------------------------------------------------------
%% @doc
%% moves ObjID to RoomID.
%%
%% @spec move_to(ObjID::integer(), RoomID::integer()) -> nil()
%% @end
%%--------------------------------------------------------------------
move_to(ObjID, RoomID) when is_integer(ObjID), is_integer(RoomID)->
  Location = hamush:location(ObjID),
  hamush:remit_but(Location, [ObjID], "~s has left.~n", [hamush:name(ObjID)]),
  fset(ObjID, location, RoomID),
  hamush:pemit(ObjID, desc(RoomID, ObjID)),
  hamush:remit_but(RoomID, [ObjID], "~s has left.~n", [hamush:name(ObjID)]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the location of ObjID
%%
%% @spec location(ObjID::integer()) -> LocationID::integer()
%% @end
%%--------------------------------------------------------------------
location(ObjID) when is_integer(ObjID) ->
  {ok, Location} = mdb_store:location(ObjID),
  Location.

%%--------------------------------------------------------------------
%% @doc
%% Sets an Attr on an ObjID to Value. Attr is converted to uppercase,
%% if Attr is an atom it is untouched otherwise it will converted to
%% upercase.
%%
%% @spec set(ObjID::integer(), Attr, Value) -> nil()
%%  Attr = string() | atom()
%%  Value = string() | integer()
%% @end
%%--------------------------------------------------------------------
set(ObjID, Attr, Value) when is_integer(ObjID), is_atom(Attr) ->
  mushdb:set(ObjID, Attr, Value);
set(ObjID, Attr, Value) when is_integer(ObjID) ->
  mushdb:set(ObjID, string:to_upper(Attr), Value).

%%--------------------------------------------------------------------
%% @doc
%% Sets an Attr on an ObjID to Value, without touching Attr.
%%
%% @spec fset(ObjID::integer(), Attr, Value) -> nil()
%%  Attr = string() | atom()
%%  Value = string() | integer()
%% @end
%%--------------------------------------------------------------------
fset(ObjID, Attr, Value) when is_integer(ObjID) ->
  mushdb:set(ObjID, Attr, Value).

%%--------------------------------------------------------------------
%% @doc
%% Gets the Attr from ObjID. Unless Attr is atom it will be turned
%% uppercase.
%%
%% @spec get(ObjID::integer(), Attr) -> string() | integer()
%%  Attr = string() | atom()
%% @end
%%--------------------------------------------------------------------
get(ObjID, Attr) when is_integer(ObjID), is_atom(Attr) ->
    mushdb:get(ObjID, Attr);
get(ObjID, Attr) when is_integer(ObjID) ->
    mushdb:get(ObjID, string:to_upper(Attr)).

%%--------------------------------------------------------------------
%% @doc
%% Gets on Attr from ObjID without modifying Attr.
%%
%% @spec fget(ObjID::integer(), Attr) -> string()
%%  Attr = string() | atom()
%% @end
%%--------------------------------------------------------------------
fget(ObjID, Attr) when is_integer(ObjID) ->
    mushdb:get(ObjID, Attr).

%%--------------------------------------------------------------------
%% @doc
%% Returns the name of ObjID or an empty string of none is exitsant.
%%
%% @spec name(ObjID::integer()) -> string()
%% @end
%%--------------------------------------------------------------------
name(ObjID) when is_integer(ObjID) ->
  case mdb_store:name(ObjID) of
    {ok, Name} -> Name;
    _ -> ""
  end.
%% @equiv create(Type, Name, Location)
create() ->
  create("Unnamed").
%% @equiv create(Type, Name, Location)
create(Name) ->
  create(object, Name).
%% @equiv create(Type, Name, Location)
create(Type, Name) ->
  create(Type, Name, -1).

%%--------------------------------------------------------------------
%% @doc
%% creates a new object of Type with Name in Location
%%
%% @spec create(Type, Name::string(), Location::integer()) -> ID::integer()
%%  Type = thing | room | exit
%% @end
%%--------------------------------------------------------------------
create(Type, Name, Location) ->
  mdb_store:create(Type, Name, Location).

%%--------------------------------------------------------------------
%% @doc
%% Emits Message string to ObjID.
%%
%% @spec pemit(ObjID::integer(), Message::string()) -> nil()
%% @end
%%--------------------------------------------------------------------
pemit(ObjID, Message) when is_integer(ObjID) ->
    if
  ObjID >= 0 ->
      {ok, Pid} = mdb_store:lookup(ObjID),
      mdb_element:hear(Pid, Message)
    end;
pemit(Pid, Message) when is_pid(Pid) ->
    mcon_connection:send(Pid, Message).

%%--------------------------------------------------------------------
%% @doc
%% Emits a message to ObjID using Format and Arguments as in
%% io_lib:format/2.
%%
%% @spec pemit(ObjID::integer(), Format::string(), Arguments::list()) -> nil()
%% @end
%%--------------------------------------------------------------------
pemit(ObjID, Format, Arguments) ->
  pemit(ObjID, io_lib:format(Format, Arguments)).

%%--------------------------------------------------------------------
%% @doc
%% Emits a message to all objects in ObjID but the object named But
%% using Format and Args as in io_lib:format/2.
%%
%% @spec remit_but(ObjID::integer(), But::integer(), Format::string(), Args::list()) -> nil()
%% @end
%%--------------------------------------------------------------------
remit_but(ObjID, But, Format, Args) when is_integer(ObjID) ->
  remit_but(ObjID, But, io_lib:format(Format, Args)).

%%--------------------------------------------------------------------
%% @doc
%% Emits a message to all objects in ObjID but the object named But.
%%
%% @spec remit_but(ObjID::integer(), But::integer(), Message::string()) -> nil()
%% @end
%%--------------------------------------------------------------------
remit_but(ObjID, But, Message) when is_integer(ObjID) ->
  if
    ObjID >= 0 ->
      Content = lists:filter(
        fun (ID) ->
          not lists:any(
            fun (ID1) ->
              ID =:= ID1
            end, But)
        end,
        hamush:content(ObjID)),
        lists:foreach(fun (ID) ->
          pemit(ID, Message)
        end, Content)
  end.

%%--------------------------------------------------------------------
%% @doc
%% Emits a message to all objects in ObjID using Format and Args as 
%% in io_lib:format/2.
%%
%% @spec remit(ObjID::integer(), Format::string(), Args::list()) -> nil()
%% @end
%%--------------------------------------------------------------------
remit(ObjID, Format, Args) when is_integer(ObjID) ->
  remit(ObjID, io_lib:format(Format, Args)).

%%--------------------------------------------------------------------
%% @doc
%% Emits a Message to all objects in ObjID.
%%
%% @spec remit(ObjID::integer(), Message::string()) -> nil()
%% @end
%%--------------------------------------------------------------------
remit(ObjID, Message) when is_integer(ObjID) ->
  Content = hamush:content(ObjID),
  lists:foreach(fun (ID) ->
    pemit(ID, Message) 
  end, Content).

%%--------------------------------------------------------------------
%% @doc
%% Resolves the an string to the object Pid of the Object.<br/>
%% <ul>
%%  <li>"me" returns the Observer.</li>
%%  <li>"here" returns the location of the resolver.</li>
%%  <li>"#&lt;id&gt;" returns the oject with id .</li>
%%  <li>"&lt;name&gt;" returns the object with name in the Observers location.</li>
%%  <li>"*&lt;name&gt;" a global lookup for name.</li>
%% </ul>
%% @spec resolve(Observer::integer(), Lookup::string()) -> {ok, Pid::pid()}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Filters a list of objects by their name
%%
%% @spec filter_by_name(Name::string(), ObjIDs::list()) -> ObjIDs::list()
%%  ObjIDs = [ID::integer()]
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Returns the desc of an object by evaling it's DESCRIPTION
%% attribute.
%%
%% @spec desc(ObjID::integer(), Observer::integer()) -> string()
%% @end
%%--------------------------------------------------------------------
desc(ObjID, Observer) -> 
  Content = string:join(
    lists:map(fun (ID) ->
      hamush:name(ID)
    end, hamush:content(ObjID)), [13,10]),
  io_lib:format("~s~n~s~n~n~s~n", [ hamush:name(ObjID),  hamush:eval_get(ObjID, "DESCRIPTION", Observer), Content]).

%%--------------------------------------------------------------------
%% @doc
%% Gets Attribute from Object and evaluates it from the POV of Actor.
%%
%% @spec eval_get(ObjID::integer(), Attr, Actor::integer()) -> string()
%%  Attr = string() | atom()
%% @end
%%--------------------------------------------------------------------
eval_get(ObjID, Attribute, Actor) ->
  case hamush:get(ObjID, Attribute) of
    {ok, Value} -> ham_lisp:run_by(Actor, ObjID, Value);
    {error, not_found} -> ""
  end.

%%--------------------------------------------------------------------
%% @doc
%% Evaluates Code from the point of view of Actor.
%%
%% @spec eval(Code::string(), Actor::integer()) -> string()
%% @end
%%--------------------------------------------------------------------
eval(Code, Actor) ->
    ham_lisp:run_by(Actor, Code).
