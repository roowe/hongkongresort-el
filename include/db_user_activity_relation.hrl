-ifndef(DB_USER_ACTIVITY_RELATION_HRL).
-define(DB_USER_ACTIVITY_RELATION_HRL, true).
%% user_activity_relation => user_activity_relation
-record(player_activity_relation, {
          player_id,                              %% 
          activity_id,                          %% 
          relation,                             %% 
          generated_time,                       %% 
          last_applying_time,                   %% 
          last_selected_time                    %% 
         }).
-endif.
