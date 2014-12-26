%% Warning:本文件由make_record自动生成，请不要手动修改
-ifndef(DB_ACTIVITY_HRL).
-define(DB_ACTIVITY_HRL, true).
%% activity => activity
-record(activity, {
          id,                                   %% 
          title,                                %% 
          content,                              %% 
          created_time,                         %% 
          begin_time,                           %% 
          application_deadline,                 %% 
          capacity = 0,                         %% 
          status = 0,                           %% 
          host_id,                              %% 
          num_applied = 0,                      %% 
          num_selected = 0,                     %% 
          last_accepted_time,                   %% 
          last_rejected_time,                   %% 
          address = <<""/utf8>>                 %% 
         }).
-endif.
