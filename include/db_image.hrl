%% Warning:本文件由make_record自动生成，请不要手动修改
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
