-module(sensitive_word_misc).

-export([compile_pattern_init/0, 
         sensitive_word_filter/1,
         is_sensitive_word/1
        ]).

compile_pattern_init() ->
    WordList = data_words:get_words_verlist(),
    BinWordList = [list_to_binary(Word) || Word <- WordList],
    CP = binary:compile_pattern(BinWordList),
    cache_misc:set(sensitive_word_cp, CP, infinity).

compile_pattern() ->
    cache_misc:get(sensitive_word_cp).
                
%% 角色名敏感词检查，小于255的字符串
is_sensitive_word(Words) 
  when is_list(Words) ->
    is_sensitive_word(list_to_binary(Words));
is_sensitive_word(Words) when is_binary(Words)->
    case binary:match(Words, compile_pattern()) of
        nomatch ->
            false;
        _ ->
            true
    end.

sensitive_word_filter(Words) 
  when is_list(Words) ->
    sensitive_word_filter(list_to_binary(Words));
sensitive_word_filter(Bin) ->
    binary:replace(Bin, compile_pattern(), <<"*">>, [global]).


%% test() ->
%%     io:format("~ts~n", [sensitive_word_filter("神奇的共产党")]),
%%     io:format("~p~n", [is_have_sensitive_word("神奇的共产党")]).

