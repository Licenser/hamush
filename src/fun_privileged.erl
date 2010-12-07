%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(fun_privileged).


-export([init/0]).

init () ->
    ham_fun_storage:register("defun", {fun defun/2, false, true}).

defun(Env, [Function, LocalForm | Body]) ->
    {subterm, Locals} = LocalForm,
    F = fun (_, Args) ->
		LocaEnv = lists:foldl(fun ({{ident, Name}, Value}, CurEvn) ->
					       dict:append(Name, Value, dict:erase(Name, CurEvn))
				       end, Env, lists:zip(Locals, Args)),
		ham_lisp:eval(LocaEnv, Body)
	end,
    ham_fun_storage:register(Function, F),
    {F, ture, true}.
