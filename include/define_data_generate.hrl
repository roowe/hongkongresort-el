-ifndef(DEFINE_DATA_GENERATE_HRL).	
-define(DEFINE_DATA_GENERATE_HRL, true).	

-record(generate_conf, {
          fun_name,
          record_conf, %% single, all, {more, FilterKey} FilterKey is Field atom or list,必须是原子或者整形的字段
          args_count = 1, %% 为了支持(Key1, Key2)的旧代码，建议使用1个参数，也就是默认值，这样就({Key1, Key2}) record_conf=all会不管这个值
          handle_args_fun, %% 只允许原子或者整形的字段作为key(除非有特殊普通需求，我就考虑加下)，多key会统一变成{Key1, Key2}
          handle_result_fun, %% 返回需要的字段List或者字段或者自定义串
          default = [], %%默认值          
          filter = fun(A) -> A end, %%对所有数据进行预筛选
          warning_not_find = true
         }).


%% record info fields help
-define(RECORD_FIELDS(TableName), record_info(fields, TableName)).

-endif.	
