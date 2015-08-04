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
          status,                               %% 
          generated_time                        %% 
         }).
-endif.
