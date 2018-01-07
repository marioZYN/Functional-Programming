% @Author: marioZhou
% @Date:   2017-12-24 17:26:59
% @Last Modified by:   marioZhou
% @Last Modified time: 2017-12-25 13:53:15

-module(erl20170227).
-compile(export_all).

runit(From, P, X) ->
	From ! {self(), P(X)}.

pfilter(P, List) ->
	lists:map(fun(X) -> runit(self(), P, X) end, List),
	lists:foldl(fun(X1, X2) ->
					receive
						{_, true} -> X2 ++ [X1];
						{_, false} -> X2
					end
				end,
				[],
				List).

% the above implementation is not correct because the order of the list can not be guaranteed.

pfilter2(P, List) ->
	W = lists:map(fun(X) -> runit(self(), P, X) end, List),
	lists:foldl(fun(X1, X2) ->
					receive
						{X1, true} -> X2 ++ [X1];
						{X1, false} -> X2
					end
				end,
				[],
				W).



