%% Warning:本文件由make_record自动生成，请不要手动修改
-ifndef(DB_NOTIFICATION_HRL).
-define(DB_NOTIFICATION_HRL, true).
%% notification => notification
-record(notification, {
          id,                                   %% 
          is_read = 0,                          %% 
          from,                                 %% 
          to,                                   %% 
          content,                              %% 
          activity_id,                          %% 
          comment_id,                           %% 
          assessment_id,                        %% 
          cmd,                                  %% 
          relation,                             %% 
          status                                %% 
         }).
-endif.
