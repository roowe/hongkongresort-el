-ifndef(DB_COMMENT_HRL).
-define(DB_COMMENT_HRL, true).
%% comment => comment
-record(comment, {
          id,                                   %% 
          content,                              %% 
          from,                                 %% 
          to,                                   %% 
          activity_id,                          %% 
          predecessor_id = -1,                  %% 
          generated_time,                       %% 
          parent_id = -1,                       %% 
          num_children = 0                      %% 
         }).
-endif.
