-module(ds_misc).
%% 封装stdlib的数据结构操作
-export([dict_cons/3, orddict_cons/3, gb_trees_cons/3, ordd_store/2]).

-export([
         list_split/2, %% 将[]分割成N份[[]..[]]，预处理多进程的数据分块
         list_add_unique_element/2, %% 在List里面添加唯一的元素
         list_shuffle/1,
         list_dualmap/3,
         nth/2
        ]).

-export([term_to_string/1, string_to_term/1, binstring_to_term/1]).

-export([to_hex/1, bytes2int/2, bytes2int/4]).

-export([rand_str/0, is_string/1]).

-export([short_record/1, record_modified/2, pr/1]).

-export([to_32bit_ip/1, to_ip_str/1]).

-export([rec_to_pl/2, rec_to_pl/3]).

-export([get_int/2]).

-include("common.hrl").

%% data structure misc

dict_cons(Key, Value, Dict) ->
    dict:update(Key, fun (List) -> [Value | List] end, [Value], Dict).

orddict_cons(Key, Value, Dict) ->
    orddict:update(Key, fun (List) -> [Value | List] end, [Value], Dict).

gb_trees_cons(Key, Value, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Values} -> 
            gb_trees:update(Key, [Value | Values], Tree);
        none -> 
            gb_trees:insert(Key, [Value], Tree)
    end.



list_split(L, N) -> 
    list_split0(L, [[] || _ <- lists:seq(1, N)]).

list_split0([], Ls) -> 
    Ls;
list_split0([I | Is], [L | Ls]) -> 
    list_split0(Is, Ls ++ [[I | L]]).

list_add_unique_element(Add, List) ->
    %% Notice: 不用lists:usort([Add|List]). 是因为若List是有序的，则[Add | List]就变乱序了，这个时候调用usort会开销大很多
    %% ordsets:to_list(ordsets:add_element(Add, ordsets:from_list(List))).
    %% 等价于ordsets:add_element(Add, ordsets:from_list(List))，ordsets:add_element是O(N)的
    %% 一般，这个调用返回会存起来，再作为List传进来（有序的），所以相对来说后者做法会比较高效
    %% 乱序的话，两者复杂度一样。
    ordsets:to_list(ordsets:add_element(Add, ordsets:from_list(List))).

list_shuffle(L) ->
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

list_dualmap(_F, [], []) ->
    [];
list_dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | list_dualmap(F, R1, R2)].

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    list_to_binary(to_hex(binary_to_list(Bin)));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].


to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

%% 获取HEX格式的随机字串
rand_str() ->
    to_hex(crypto:rand_bytes(4)).


is_string([C | Cs]) when is_integer(C), C >= 0, C =< 16#10ffff ->
    is_string(Cs);
is_string([_ | _]) ->
    false;
is_string([]) ->
    true;
is_string(_) ->
    false.



bytes2int(N1, N0) when 0 =< N1, N1 =< 255,
		       0 =< N0, N0 =< 255 ->
    (N1 bsl 8) bor N0.
bytes2int(N3, N2, N1, N0) when 0 =< N3, N3 =< 255,
			       0 =< N2, N2 =< 255,
			       0 =< N1, N1 =< 255,
			       0 =< N0, N0 =< 255 ->
    (N3 bsl 24) bor (N2 bsl 16) bor (N1 bsl 8) bor N0.


pr(Record) ->    
    RecordName = element(1, Record),
    case all_record:get_fields(RecordName) of
        [] ->
            Record;
        RecordFields ->                                        
            lists:zip(RecordFields, tl(tuple_to_list(Record)))
    end.

short_record(undefined) ->
    [];
short_record([]) ->
    [];
%% for repeat field, it is a tuple list
short_record(RecordList) 
  when is_list(RecordList),
       is_tuple(hd(RecordList)) ->
    [short_record(Record) || Record <- RecordList];
short_record(Record) 
  when is_tuple(Record) ->
    case pr(Record) of
        Record ->
            Record;
        ValList ->
            {element(1, Record), 
             lists:reverse(lists:foldl(fun({Field, Val}, Acc) ->
                                               case short_record(Val) of
                                                   [] ->
                                                       Acc;
                                                   ShortVal ->
                                                       [{Field, ShortVal}|Acc]
                                               end
                                       end, [], ValList))}
    end;
short_record(Other) ->
    Other.


ordd_store(List, Dict) when is_list(List)->
    lists:foldl(fun({K, V}, AccDict) ->
                        orddict:store(K, V, AccDict)
                end, Dict, List);
ordd_store({K, V}, Dict) ->
    orddict:store(K, V, Dict).


record_modified(R1, R2)
  when is_tuple(R1) andalso
       is_tuple(R2) ->
    L1 = tuple_to_list(R1),
    L2 = tuple_to_list(R2),
    record_modified({[], false}, L1, L2).
record_modified({[], InFlag}, [Head | Rest1], [Head | Rest2]) ->
    record_modified({[Head], InFlag}, Rest1, Rest2);
record_modified({ResultList, InFlag}, [Head | Rest1], [Head | Rest2]) ->
    record_modified({[undefined | ResultList], InFlag}, Rest1, Rest2);
record_modified({ResultList, _}, [_H1 | Rest1], [H2 | Rest2]) ->
    record_modified({[H2 | ResultList], true}, Rest1, Rest2);
record_modified({ResultList, true}, [], []) ->
    %% 将最终的结果反转成 record
    list_to_tuple(lists:reverse(ResultList));
record_modified({_, false}, [], []) ->
    %% 没有任何变更
    [];
record_modified(RList, L1, L2) ->
    ?DEBUG("record compare failed: ~p~n~p~n~p~n", [RList, L1, L2]),
    [].


nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T);
nth(_, _) ->
    [].

to_32bit_ip({A, B, C, D}) ->
    A * (1 bsl 24) + B * (1 bsl 16) + C * (1 bsl 8) + D.

to_ip_str(Integer) ->
    list_to_binary(lists:concat([Integer bsr 24, ".",
                                 (Integer band 16711680) bsr 16, ".",
                                 (Integer band 65280) bsr 8, ".",
                                 Integer band 255])).

%% Convert record to proplist
%%
%% RecInfo is from record_info(fields, record_name)
%%     Be care record_name should be an 'atom', not a Variable
%% see http://erlang.org/pipermail/erlang-questions/2007-January/024666.html

rec_to_pl(RecInfo, Record) ->
    rec_to_pl(RecInfo, Record, all).

rec_to_pl2([H|T], Record, NeedFields, N, Acc) ->
    if
        NeedFields =:= all ->
            Acc1 = [{H, erlang:element(N, Record)}|Acc],
            rec_to_pl2(T, Record, NeedFields, N+1, Acc1);
        true ->
            case lists:member(H, NeedFields) of
                false ->
                    rec_to_pl2(T, Record, NeedFields, N+1, Acc);
                true ->
                    Acc1 = [{H, erlang:element(N, Record)}|Acc],
                    rec_to_pl2(T, Record, NeedFields, N+1, Acc1)
            end
    end;
rec_to_pl2([], _Record, _, _N, Acc) ->
    Acc.

rec_to_pl(RecInfo, Record, NeedFields) ->
    rec_to_pl2(RecInfo, Record, NeedFields, 2, []).

term_to_string(Term) ->
    list_to_binary(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
binstring_to_term(BinString) ->
    string_to_term(binary_to_list(BinString)).

string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> 
                    Term;
                Error -> 
                    throw(Error)                        
            end;
        Error ->
            throw(Error)
    end.


get_int(Key, KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        false ->
            false;
        {_, <<>>} ->
            <<>>;
        {_, Int} when is_integer(Int) ->
            Int;
        {_, Bin} when is_binary(Bin) ->
            erlang:binary_to_integer(Bin);
        {_, List} when is_list(List) ->
            erlang:list_to_integer(List)
    end.
