
-ifdef(OLD_QUEUE_TYPE).
-define(QUEUE_TYPE(), queue()).
-define(DICT_TYPE(), dict()).
-else.
-define(QUEUE_TYPE(), queue:queue()).
-define(DICT_TYPE(), dict:dict()).
-endif.

-define(DBI_SUP, dbi_app).
