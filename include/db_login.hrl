-ifndef(DB_LOGIN_HRL).
-define(DB_LOGIN_HRL, true).
%% login => login
-record(login, {
          user_id,                              %% 
          token,                                %% 
          timestamp                             %% 
         }).
-endif.
