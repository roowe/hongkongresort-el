-ifndef(DEFINE_MYSQL_HRL).
-define(DEFINE_MYSQL_HRL, true).

-include("mysql/include/mysql.hrl").
%% record操作MySQL一些基本信息
-record(record_mysql_info,{
          db_pool,
          table_name,
          fields,
          record_name,
          in_db_hook,
          out_db_hook    
         }).



-endif.
