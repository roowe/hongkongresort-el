-ifndef(DB_USER_HRL).
-define(DB_USER_HRL, true).
%% user => user
-record(player, {
          id,                                   %% 
          name,                                 %% 
          display_name,                         %% 
          password,                             %% 
          email,                                %% 
          group_id = 0,                         %% 
          authentication_status = 0,            %% 
          gender = 0,                           %% 
          avatar = 0,                           %% 
          created_time,                         %% 
          verification_code,                    %% 
          unread_count = 0,                     %% 
          password_reset_code,                  %% 
          salt,                                 %% 
          unassessed_count = 0                  %% 
         }).
-endif.
