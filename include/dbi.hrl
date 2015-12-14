
-ifdef(NEW_QUEUE_TYPE).
-define(QUEUE_TYPE, (queue:queue())).
-else.
-define(QUEUE_TYPE, (queue())).
-endif.
