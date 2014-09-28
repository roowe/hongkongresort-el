%% Warning:本文件由make_record自动生成，请不要手动修改
-ifndef(DB_COMMENT_HRL).
-define(DB_COMMENT_HRL, true).
%% comment => comment
-record(comment, {
          id,                                   %% 
          content,                              %% 
          from,                                 %% 
          to = -1,                              %% 
          activity_id,                          %% 
          predecessor_id = -1,                  %% 
          generated_time,                       %% 
          parent_id = -1                        %% 
         }).
-endif.
