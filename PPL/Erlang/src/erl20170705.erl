% @Author: marioZhou
% @Date:   2017-12-24 16:36:29
% @Last Modified by:   marioZhou
% @Last Modified time: 2017-12-24 17:11:14

-module(erl20170705).
-compile(export_all).

leafy(V) ->
	receive
		{ask, P} -> P ! {self(), V}
	end.

branchy(L, R, F, Ready, Dad) ->
	receive
		{ask, Pid} -> L ! {ask, self()},
					R ! {ask, self()},
					branchy(L, R, F, Ready, Pid);
		{L, V} -> 
			case Ready of
				true -> Dad ! {self(), F(V, R)};
				false -> branchy(V, R, F, true, Dad)
			end;
		{R, V} ->
			case Ready of
				true -> Dad ! {self(), F(L, V)};
				false -> branchy(L, V, F, true, Dad)
			end
	end.

activate({leaf, V}, _) ->
	spawn(?MODULE, leafy, [V]);
activate({branch, L, R}, F) ->
	spawn(?MODULE, branchy, [activate(L, F), activate(R, F), F, false, self()]).


test() ->
T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
A1 = activate(T1, fun min/2),
A1 ! {ask, self()},
receive
{A1, V} -> V end.