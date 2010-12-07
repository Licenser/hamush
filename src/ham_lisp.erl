%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%% @copyright (C) 2010, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created :  5 Sep 2010 by Heinz N. Gies <heinz@Heinz-N-Giess-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ham_lisp). 

-export([tokenize/1, termize/2, termize/1, eval/1, run/1, run/2, eval/2, run_by/2, run_with/2, run_by/3]).


tokenize(String) ->
    tokenize(String, [], [initial]).
tokenize([], Tokens, _State) ->
    lists:reverse(Tokens);

tokenize(String, Tokens, [initial | _UpperState] = State) ->
    case re:run(String, "^('?\\()?(.*)", [{capture, [1, 2], list}]) of
	{match, [[_|_], _Rest]} -> tokenize(String, Tokens, [code | State]);
	{match, [[], Rest]} -> tokenize(Rest, Tokens, [string | State])
    end;
tokenize(String, Tokens, [string | _UpperState] = State) ->
    case re:run(String, "^(?:((?:\\.|[^[])+)|(\\[))(.*)", [{capture, [1, 2, 3], list}]) of
	{match, [[], "[", Rest]} -> tokenize(Rest, Tokens, [code|State]);
	{match, [Str, [], Rest]} -> tokenize(Rest, [{string, Str} | Tokens], State)
    end;
tokenize(String, Tokens, [substring | UpperState] = State) ->
    case re:run(String, "^(?:((?:\\.|[^[\"])+)|(\\[)|(\"))(.*)", [{capture, [1, 2, 3,4], list}]) of
	{match, [[], "[", [], Rest]} -> tokenize(Rest, Tokens, [code|State]);

	{match, [[], [], "\"", Rest]} -> tokenize(Rest, [{string_end, $"} | Tokens], UpperState);
	{match, [Str, [], [], Rest]} -> tokenize(Rest, [{string, Str} | Tokens], State)
    end;
tokenize(String, Tokens, [code | UpperState] = State) ->
    case re:run(String, "^(?:((?:[a-zA-Z*#-][a-zA-Z0-9+*/-]*|[+/*-]))|(['()])|(\\d+)|(\")|[\\s/m]+|(\\]))(.*)", [{capture, [1, 2, 3, 4, 5, 6], list}]) of
	{match, [[], [], [], [], [], Rest]}  -> tokenize(Rest, Tokens, State);
	{match, [Ident, [], [], [], [], Rest]} -> tokenize(Rest, [{ident, Ident} | Tokens], State);
	{match, [[], [Symbol], [], [], [], Rest]} -> tokenize(Rest, [{symbol, Symbol} | Tokens], State);
	{match, [[], [], Number, [], [], Rest]}  -> 
	    {N, []} =  string:to_integer(Number),
	    tokenize(Rest, [{number,N} | Tokens], State);
	{match, [[], [], [], [], "]", Rest]} -> tokenize(Rest, Tokens, UpperState);
	{match, [[], [], [], "\"", [], Rest]} -> tokenize(Rest, [{string_start, $"} | Tokens], [substring | State])
    end.
    

termize(Tokens) ->
    {Term, []} = termize(Tokens, []),
    Term.
termize([], Term) ->
    {lists:reverse(Term), []};    
termize([{symbol, $(} | Rest], Term) ->
    {Subterm, Rest1} = termize(Rest, []),
    termize(Rest1, [{subterm, Subterm} | Term]);
termize([{symbol, $'}, {symbol, $(} | Rest], Term) ->
    {Subterm, Rest1} = termize(Rest, []),
    termize(Rest1, [{list, Subterm} | Term]);
termize([{symbol, $)} | Rest], Term) ->    
    {lists:reverse(Term), Rest};
termize([{string_start, $"} | Rest], Term) ->
    case termize(Rest, []) of
	{[T], Rest1} -> termize(Rest1, [T | Term]);
	{Subterm, Rest1} -> termize(Rest1, [{subterm, Subterm} | Term])
    end;	
termize([{string_end, $"} | Rest], Term) ->    
    {lists:reverse(Term), Rest};
termize([E | Rest], Term) ->
    termize(Rest, [E | Term]).
    

eval(Elements) ->
    eval(dict:append(actor, 1, dict:new()), Elements).
eval(_Env, []) -> "";
eval(Env, {subterm, [F | Args]}) ->
    {ok, [Privileged]} = dict:find(privileged, Env),
    case eval(Env, F) of
	{C, true, P} when not (P xor Privileged)  -> 
	    As = lists:map(fun (A) -> 
				   eval(Env, A)
			   end, Args),
	    C(Env, As);
	{C, false, P}when  not (P xor Privileged) -> 
	    C(Env, Args)
    end;
eval(Env, {subterm, Elements}) ->
    string:join(
      lists:map(fun (E) ->
			case eval(Env, E) of
			    X when is_integer(X) -> 
				[R] = io_lib:format("~w", [X]),
				R;
			    X -> X
			end			  
		end, Elements), "");
eval(Env, {ident, I}) -> 
    case ham_fun_storage:get(I) of
	{ok, C} -> C;
	_ -> case dict:find(I, Env) of
		 {ok, [X]} -> X;
		 _ -> case re:run(I, "^.*/.*\$") of
			  {match, _} -> I;
			  nomatch -> "#-1 Not found."
		      end
	     end
    end;
eval(_Env, {list, L}) -> lists:map(fun (E) -> unroll(E) end, L);
eval(_Env, {number, N}) -> N;
eval(_Env, {string, S}) -> S;
eval(Env, Elements) -> 
    string:join(
      lists:map(fun (E) ->
			case eval(Env, E) of
			    X when is_integer(X) -> 
				[R] = io_lib:format("~w", [X]),
				R;
			    X -> X
			end			  
		end, Elements), "").

unroll({list, L}) -> lists:map(fun (E) -> unroll(E) end, L);
unroll({subterm, L}) -> lists:map(fun (E) -> unroll(E) end, L);
unroll({number, N}) -> N;
unroll({ident, S}) -> S;
unroll({string, S}) -> S.


run_by(Actor, String) ->
    run_by(Actor, Actor, String).
run_by(Actor, This, String) ->
    run_with(String, [{actor, Actor}, {this, This}]).

run_with(String, Values) ->
    Env = lists:foldl(fun ({K, V}, E) ->
			      dict:append(K, V, E)
		      end, dict:new(), Values),
    run(Env, String).

run(String) ->
    eval(termize(tokenize(String))).
run(Env, String) ->
    eval(Env, termize(tokenize(String))).
