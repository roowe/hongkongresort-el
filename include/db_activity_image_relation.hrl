-ifndef(DB_ACTIVITY_IMAGE_RELATION_HRL).
-define(DB_ACTIVITY_IMAGE_RELATION_HRL, true).
%% activity_image_relation => activity_image_relation
-record(activity_image_relation, {
          activity_id,                          %% 
          image_id,                             %% 
          generated_time                        %% 
         }).
-endif.
