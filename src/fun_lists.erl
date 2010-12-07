%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(fun_lists).


-export([init/0]).

init () ->
    ham_fun_storage:register("first", fun lfirst/2),
    ham_fun_storage:register("rest", fun lrest/2),
    ham_fun_storage:register("cons", fun lcons/2),
    ham_fun_storage:register("quote", {fun lquote/2, false}).

lfirst(_Env, [[]]) ->
   [];
lfirst(_Env, [[E | _]]) ->
    E.

lrest(_Env, [[]]) ->
   [];
lrest(_Env, [[_ | R]]) ->
    R.

lcons(_Env, [E, L]) ->
    [E | L].

lquote(_Env, [L]) ->
    ham_lisp:unroll(L).

