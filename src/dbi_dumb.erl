-module(dbi_dumb).
-author('manuel@altenwald.com').

-behaviour(dbi).

-export([
    init/8,
    terminate/1,
    do_query/3,
    check_migration/1,
    get_migrations/1,
    add_migration/3,
    rem_migration/2
]).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(_Host, _Port, _User, _Pass, _Database, _Poolname, _Poolsize, _Extra) ->
    ok.

-spec terminate(Poolname :: atom()) -> ok.

terminate(_Poolname) ->
    case ets:info(dbi_dumb_testing) of
        undefined -> ok;
        _ -> ets:delete(dbi_dumb_testing)
    end,
    case ets:info(dbi_dumb_users) of
        undefined -> ok;
        _ -> ets:delete(dbi_dumb_users)
    end,
    case ets:info(dbi_dumb_migrations) of
        undefined -> ok;
        _ -> ets:delete(dbi_dumb_migrations)
    end,
    ok.

-spec do_query(
    PoolDB :: atom(),
    SQL :: binary() | string(),
    [Params :: any()]) ->
    {ok, integer(), [term()]} | {error, any()}.

do_query(PoolDB, SQL, Params) when is_list(SQL) ->
    do_query(PoolDB, list_to_binary(SQL), Params);

do_query(_PoolDB, <<"CREATE TABLE testing", _/binary>>, _Params) ->
    dbi_dumb_testing = ets:new(dbi_dumb_testing, [public, named_table, set]),
    {ok, 0, []};

do_query(_PoolDB, <<"CREATE TABLE users", _/binary>>, _Params) ->
    dbi_dumb_users = ets:new(dbi_dumb_users, [public, named_table, set]),
    {ok, 0, []};

do_query(_PoolDB, <<"CREATE TABLE comments", _/binary>>, _Params) ->
    dbi_dumb_comments = ets:new(dbi_dumb_comments, [public, named_table, set]),
    {ok, 0, []};

do_query(_PoolDB, <<"INSERT INTO testing", _/binary>>, Params) ->
    lists:foreach(fun(Id) ->
        ets:insert_new(dbi_dumb_testing, {Id, Id})
    end, Params),
    {ok, length(Params), []};

do_query(_PoolDB, <<"INSERT INTO users", _/binary>>, Params) ->
    {A, B} = lists:partition(fun(A) -> is_integer(A) end, Params),
    Data = lists:zip(A, B),
    lists:foreach(fun(Userdata) ->
        ets:insert_new(dbi_dumb_users, Userdata)
    end, Data),
    {ok, length(Data), []};

do_query(_PoolDB, <<"INSERT INTO comments", _/binary>>, Params) ->
    {A, B} = lists:partition(fun(A) -> is_integer(A) end, Params),
    Data = lists:zip(A, B),
    lists:foreach(fun(Userdata) ->
        ets:insert_new(dbi_dumb_comments, Userdata)
    end, Data),
    {ok, length(Data), []};

do_query(_PoolDB, <<"UPDATE testing", _/binary>>, _Params) ->
    ets:delete_all_objects(dbi_dumb_testing),
    ets:insert(dbi_dumb_testing, {2, 2}),
    {ok, 1, [{2}]};

do_query(_PoolDB, <<"SELECT * FROM testing", _/binary>>, _Params) ->
    case ets:info(dbi_dumb_testing) of
        undefined ->
            {error, {sqlite_error,"no such table: testing"}};
        _ ->
            Data = ets:tab2list(dbi_dumb_testing),
            {ok, length(Data), [ {Id} || {Id, _} <- Data ]}
    end;

do_query(_PoolDB, <<"SELECT * FROM users", _/binary>>, _Params) ->
    Data = ets:tab2list(dbi_dumb_users),
    {ok, length(Data), Data};

do_query(_PoolDB, <<"SELECT * FROM comments", _/binary>>, _Params) ->
    Data = ets:tab2list(dbi_dumb_comments),
    {ok, length(Data), Data}.

-spec check_migration(PoolDB :: atom()) ->
      {ok, integer(), [binary()]}.

check_migration(_PoolDB) ->
    case ets:info(dbi_dumb_migrations) of
        undefined ->
            ets:new(dbi_dumb_migrations, [public, named_table, set]);
        _ ->
            ok
    end,
    ok.

-spec get_migrations(Poolname :: atom()) ->
      {ok, Count :: integer(), [{binary()}]}.

get_migrations(_Poolname) ->
    Data = ets:tab2list(dbi_dumb_migrations),
    {ok, length(Data), Data}.

-spec add_migration(Poolname :: atom(), Code :: binary(), File :: binary()) ->
      ok | {error, Reason :: any()}.

add_migration(_Poolname, Code, File) ->
    ets:insert_new(dbi_dumb_migrations, {Code, File}),
    ok.

-spec rem_migration(Poolname :: atom(), Code :: binary()) ->
      ok | {error, Reason :: any()}.

rem_migration(_Poolname, Code) ->
    ets:delete(dbi_dumb_migrations, Code),
    ok.
