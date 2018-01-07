% @Author: marioZhou
% @Date:   2017-12-24 15:34:18
% @Last Modified by:   marioZhou
% @Last Modified time: 2017-12-24 16:15:56

-module(erl20170720).
-compile(export_all).

cell(Value) ->
	receive
		{set, V} -> cell(V);
		{get, Pid} -> Pid ! {self(), Value},
					  cell(Value)
	end.

create_dlist(0) -> [];
create_dlist(N) -> [spawn(?MODULE, cell, [0]) | create_dlist(N-1)].

dlist_to_list([]) -> [];
dlist_to_list([X|Xs]) ->
	X ! {get, self()},
	L = dlist_to_list(Xs),
	receive
		{_, V} -> [V] ++ L
	end.

dmap(_, []) -> ok;
dmap(F, [X|Xs]) ->
	X ! {get, self()},
	receive
		{_, V} -> X ! {set, F(V)}
	end,
	dmap(F, Xs).


