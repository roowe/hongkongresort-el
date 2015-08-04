-ifndef(DB_IMAGE_HRL).
-define(DB_IMAGE_HRL, true).
%% image => image
-record(image, {
          id,                                   %% 
          url,                                  %% 
          meta_id,                              %% 
          meta_type,                            %% 
          generated_time = 0                    %% 
         }).
-endif.
