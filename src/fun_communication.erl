%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(fun_communication).


-export([init/0]).

init () ->
    ham_fun_storage:register("pemit", fun pemit/2).

pemit(_Env, [To | Message]) when is_number(To) ->
    hamush:pemit(To, Message).
