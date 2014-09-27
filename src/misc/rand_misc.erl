-module(rand_misc).

-export([rand/2, rand/1, 
         rand_n/2, rand_np/2,
         rand_p10000/1, rand_p/1,
         rand_1_out_of_n/1,
         rand_n_out_of_10000/1,
         shuffle/1]).


rand(Max) when is_integer(Max)->
    mod_rand:seed(),
    random:uniform(Max);
%% 优化
rand([]) ->
    [];
rand([V]) ->
    V;
rand(List) when is_list(List)->
    [V] = rand_n(1, List),
    V.


%% 产生一个介于Min到Max之间的随机整数
%% rand(1, Max-Min+1) + (Min-1)
%% 随机数+偏移量
rand(Min, Max) 
  when Min =< Max->
    rand(Max - (Min - 1)) + (Min - 1).

rand_n(N, List) when is_list(List) ->
    Len = length(List),
    Ns = rand_n(N, {1, Len}),
    [lists:nth(Index, List) || Index <- Ns];
%% 从[min , max] 中取出 N个数，不重复
rand_n(Count, {Min, Max}) 
  when (Max - Min)+1 > Count->
	rand_n2(Count, Min, Max, []);
rand_n(_Count, {Min, Max}) ->
    shuffle(lists:seq(Min, Max)).
	
rand_n2(0, _Min, _Max, List) ->
	List;
rand_n2(Count, Min, Max, List) ->
	Num = rand(Min, Max),
	case lists:member(Num, List) of
		false->
			rand_n2(Count - 1, Min, Max, [Num|List]);
		true ->
			rand_n2(Count, Min, Max, List)
	end.

%% 优化
rand_p([{V, _P}]) ->
    V;
rand_p(List) ->
    rand_p(List, full).

%% 优化
rand_p10000([{V, P}]) 
  when P >= 10000 ->
    V;
rand_p10000(List) ->
    rand_p(List, 10000).

rand_np(N, List) 
  when length(List) =< N ->
    [Item || {Item, _} <- List];
rand_np(N, List) ->
    FullPower = lists:sum([P || {_, P} <- List]),
    rand_np2(N, List, FullPower).

rand_np2(0, _, _) ->
    [];
rand_np2(N, List, FullPower) -> 
    Rand = rand(FullPower),
    {Item, Power} = inner_select(Rand, 0, List),
    [Item | 
     rand_np2(N-1, 
              lists:delete({Item, Power}, List), 
              FullPower - Power)].



rand_p(List, full) ->
    FullPower = lists:sum([P || {_, P} <- List]),
    rand_p(List, FullPower);
rand_p(List, Base) ->
    Rand = rand(Base),
    %% 过滤掉小于随机值的数据
    case inner_select(Rand, 0, List) of
        [] ->
            [];
        {Item, _} ->
            Item
    end.
    %% {Item, _} = inner_select(Rand, 0, List),
    %% Item.


inner_select(Rand, Current, [{Item, Power} | Tail]) ->
    NewCurrent = Current + Power,
    if
        NewCurrent >= Rand ->
            {Item, Power};
        true ->
            inner_select(Rand, NewCurrent, Tail)
    end;
inner_select(_, _, []) ->
    %% 到最后还没有选取到，那么返回 []
    [].

rand_1_out_of_n(N) ->
    rand(N) =:= 1.

rand_n_out_of_10000(N) ->
    rand(10000) =< N.

shuffle(L) ->        
    List1 = [{rand(10000000), X} || X <- L], 
    List2 = lists:keysort(1, List1), 
    [E || {_, E} <- List2]. 

%% list_rand(N, List) ->
%%     mod_rand:seed(),    
%%     list_rand(N, List, [], 1, get(random_seed)).

%% 遍历的时候性能上不去，算法理论最优
%% %% 连续随机的话，可以绕开进程字典来优化
%% list_rand(_N, [], SelectList, _, Seed) ->
%%     put(random_seed, Seed),
%%     SelectList;
%% list_rand(N, [H|List], SelectList, Index, Seed) when Index =< N ->
%%     list_rand(N, List, [H|SelectList], Index+1, Seed);
%% list_rand(N, [H|List], SelectList, Index, Seed) ->
%%     {IndexRand, Seed1} =  random:uniform_s(Index, Seed),
%%     %% N/Index 取，否则不取
%%     case IndexRand > N of
%%         true ->
%%             %% 不取
%%             list_rand(N, List, SelectList, Index+1, Seed1);
%%         false ->
%%             %% 取
%%             {Pos, Seed2} =  random:uniform_s(N, Seed1),
%%             {SelectList2, _} = 
%%                 lists:mapfoldl(fun(E, Acc) ->
%%                                        if
%%                                            Acc =:= Pos ->
%%                                                {H, Acc+1};
%%                                            true ->
%%                                                {E, Acc+1}
%%                                        end
%%                                end, 1, SelectList),
%%             % io:format("~p ~p ~p~n", [SelectList, SelectList2, Pos]),
%%             list_rand(N, List, SelectList2, Index+1, Seed2)
%%     end.


    
