-ifndef(DB_ACTIVITY_HRL).
-define(DB_ACTIVITY_HRL, true).
%% activity => activity
-record(activity, {
          id,                                   %% 
          title = <<""/utf8>>,                  %% 
          content = <<""/utf8>>,                %% 
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
