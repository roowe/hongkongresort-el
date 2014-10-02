%% Warning:本文件由make_record自动生成，请不要手动修改
-ifndef(DB_USER_ACTIVITY_RELATION_HRL).
-define(DB_USER_ACTIVITY_RELATION_HRL, true).
%% user_activity_relation => user_activity_relation
-record(user_activity_relation, {
          user_id,                              %% 
          activity_id,                          %% 
          relation,                             %% 
          generated_time,                       %% 
          last_applying_time,                   %% 
          last_accepted_time,                   %% 
          last_rejected_time                    %% 
         }).
-endif.
