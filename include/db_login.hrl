-ifndef(DB_LOGIN_HRL).
-define(DB_LOGIN_HRL, true).
%% login => login
-record(login, {
          player_id,                              %% 
          token,                                %% 
          timestamp                             %% 
         }).
-endif.
