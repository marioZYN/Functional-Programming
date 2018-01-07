% @Author: marioZhou
% @Date:   2017-12-24 11:32:26
% @Last Modified by:   marioZhou
% @Last Modified time: 2017-12-24 12:01:07

-module(erl20170901).
-compile(export_all).

worker(Dad, List, X) ->
	case lists:member(X, List) of
		true -> Dad ! {found, List};
		false -> Dad ! nay
	end.

get_result(0) -> false;
get_result(N) -> 
	receive
		nay -> get_result(N-1);
		{found, L} -> L
	end.

parfind(LofL, X) ->
	lists:foreach(fun(L) -> 
					spawn(?MODULE, worker, [self(), L, X])
		 			end,
		 		LofL),
	get_result(length(LofL)).
