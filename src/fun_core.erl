%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(fun_core).


-export([init/0]).

init () ->
    ham_fun_storage:register("get", fun get/2),
    ham_fun_storage:register("+", fun add/2),
    ham_fun_storage:register("-", fun sub/2),
    ham_fun_storage:register("*", fun mul/2),
    ham_fun_storage:register("/", fun divide/2),
    ham_fun_storage:register("u", fun u/2),
    ham_fun_storage:register("cond", {fun lcond/2, false}),
    ham_fun_storage:register("eq", fun leq/2),
    ham_fun_storage:register("str", fun str/2),
    ham_fun_storage:register("map", fun lmap/2),
    ham_fun_storage:register("fold", fun lfold/2),
    ham_fun_storage:register("lambda", {fun lambda/2, false}),
    ham_fun_storage:register("let", {fun llet/2, false}).

add(_Env, Args) ->
    lists:foldl(fun (X, Sum) ->
			X + Sum
		end, 0, Args).
sub(_Env, Args) ->
    case Args of
	[F | Rest] -> lists:foldl(fun (X, Sum) ->
					 Sum - X
				 end, F, Rest);
	[] -> 0
    end.
mul(_Env, Args) ->
    lists:foldl(fun (X, Sum) ->
			X * Sum
		end, 1, Args).
divide(_Env, Args) ->
    case Args of
	[F | Rest] -> lists:foldl(fun (X, Sum) ->
					  Sum / X
				  end, F, Rest);
	[] -> 1
    end.
llet(Env, [DeclsForm, Form]) ->
    {subterm, Decls} = DeclsForm,
    NewEnv = lists:foldl(fun ({subterm, [{ident, N}, T]}, CurEvn) ->
				 dict:append(N, ham_lisp:eval(CurEvn, T), dict:erase(N, CurEvn))
			 end, Env, Decls),
    ham_lisp:eval(NewEnv, Form).

get(Env, [What]) ->
    [Actor] = dict:fetch(actor, Env),
    case re:run(What, "(.*)/(.*)", [{capture, [1, 2], list}]) of
	{match, [Obj, Attr]} -> case hamush:resolve(Actor,Obj) of
				    {ok, Object} -> case hamush:get(Object, Attr) of
							{ok, R} -> R;
							_ -> "#-1 Not Found."
						    end;
				    _ -> "#-1 Not Found."
				end;
	_ -> "#-1 Not foun."
    end.

u(Env, [What]) ->
    [Actor] = dict:fetch(actor, Env),
    case re:run(What, "(.*)/(.*)", [{capture, [1, 2], list}]) of
	{match, [Obj, Attr]} -> 
	    case hamush:resolve(Actor,Obj) of
				    {ok, Object} ->
					hamush:eval_get(Object, Attr, Actor);
				    _ -> "#-1 Not Found."
				end;
	_ -> "#-1 Not foun."
    end.

str(_Env, Args) ->
    string:join(
      lists:map(fun (E) ->
			case E of
			    X when is_integer(X) -> 
				[R] = io_lib:format("~w", [X]),
				R;
			    X -> X
			end
		end, Args), "").

lambda(Env, [LocalForm, Body]) ->
    {subterm, Locals} = LocalForm,
    F = fun (_, Args) ->
		LocaEnv = lists:foldl(fun ({{ident, Name}, Value}, CurEvn) ->
					       dict:append(Name, Value, dict:erase(Name, CurEvn))
				       end, Env, lists:zip(Locals, Args)),
		ham_lisp:eval(LocaEnv, Body)
	end,
    {F, true, false}.

lmap(Env, [Fun | Args]) ->
    {F, _} = Fun,
    MapF = fun (E) -> F(Env, [E]) end,
    lists:map(MapF, Args).

lfold(Env, [Fun, Initial, Args]) ->
    {F, _} = Fun,
    FoldF = fun (E, Acc) -> F(Env, [E, Acc]) end,
    lists:foldl(FoldF, Initial, Args).

leq(_Env, [A, B]) ->
    A =:= B.


lcond(Env, [Cond, True, False]) ->
    case ham_lisp:eval(Env, Cond) of
	[] -> ham_lisp:eval(Env, False);
	_ -> ham_lisp:eval(Env, True)
    end;
lcond(Env, [Cond, True]) ->
    case ham_lisp:eval(Env, Cond) of
	[] -> [];
	_ -> ham_lisp:eval(Env, True)
    end.
