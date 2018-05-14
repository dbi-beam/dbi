-module(dbi_mysql).
-author('manuel@altenwald.com').

-behaviour(dbi).

-export([
    start_link/1,
    init/8,
    terminate/1,
    do_query/3,
    check_migration/1,
    get_migrations/1,
    add_migration/3,
    rem_migration/2
]).

-include("dbi.hrl").

-define(DEFAULT_POOLSIZE, 10).
-define(DEFAULT_MAX_OVERFLOW, 10).
-define(DEFAULT_PORT, 3306).

-spec start_link([term()]) -> {ok, pid()}.

start_link(ConnData) ->
    {ok, _} = apply(p1_mysql_conn, start_link, ConnData).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(Host, Port, User, Pass, Database, Poolname, Poolsize, Extra) ->
    application:start(p1_mysql),
    MaxOverflow = proplists:get_value(max_overflow, Extra, ?DEFAULT_MAX_OVERFLOW),
    ConnData = [Host, dbi_query:default(Port, ?DEFAULT_PORT),
                User, Pass, Database, undefined],
    PoolArgs = [{name, {local, Poolname}}, {worker_module, ?MODULE},
                {size, dbi_query:default(Poolsize, ?DEFAULT_POOLSIZE)},
                {max_overflow, MaxOverflow}],
    ChildSpec = poolboy:child_spec(Poolname, PoolArgs, ConnData),
    supervisor:start_child(?DBI_SUP, ChildSpec),
    ok.

-spec terminate(Poolname :: atom()) -> ok.

terminate(_Poolname) ->
    ok.

-spec do_query(
    PoolDB :: atom(),
    SQL :: binary() | string(),
    [Params :: any()]) ->
    {ok, integer(), [term()]} | {error, any()}.

do_query(PoolDB, SQL, Params) when is_list(SQL) ->
    do_query(PoolDB, list_to_binary(SQL), Params);

do_query(PoolDB, RawSQL, Params) when is_binary(RawSQL) ->
    SQL = dbi_query:resolve(RawSQL),
    poolboy:transaction(PoolDB, fun(PID) ->
        case p1_mysql_conn:squery(PID, SQL, self(), Params) of
            {data, Result} ->
                Rows = p1_mysql:get_result_rows(Result),
                {ok, length(Rows), [ list_to_tuple(Row) || Row <- Rows ]};
            {updated, Result} ->
                Count = p1_mysql:get_result_affected_rows(Result),
                {ok, Count, []};
            {error, Result} ->
                Error = p1_mysql:get_result_reason(Result),
                {error, Error}
        end
    end).

-spec check_migration(PoolDB :: atom()) ->
      {ok, integer(), [binary()]}.

check_migration(PoolDB) ->
    Create = <<"CREATE TABLE IF NOT EXISTS schema_migrations("
               "id integer primary key auto_increment, "
               "code varchar(1024), "
               "filename varchar(1024));">>,
    {ok, _, _} = do_query(PoolDB, Create, []),
    ok.

-spec get_migrations(Poolname :: atom()) ->
      {ok, Count :: integer(), [{binary()}]}.

get_migrations(Poolname) ->
    SQL = <<"SELECT code, filename "
            "FROM schema_migrations "
            "ORDER BY id ASC">>,
    do_query(Poolname, SQL, []).

-spec add_migration(Poolname :: atom(), Code :: binary(), File :: binary()) ->
      ok | {error, Reason :: any()}.

add_migration(Poolname, Code, File) ->
    Insert = <<"INSERT INTO schema_migrations(code, filename) "
               "VALUES ($1, $2)">>,
    case do_query(Poolname, Insert, [Code, File]) of
        {ok, 1, []} ->
            ok;
        Error ->
            Error
    end.

-spec rem_migration(Poolname :: atom(), Code :: binary()) ->
      ok | {error, Reason :: any()}.

rem_migration(Poolname, Code) ->
    Delete = <<"DELETE FROM schema_migrations "
               "WHERE code = $1">>,
    case do_query(Poolname, Delete, [Code]) of
        {ok, _, _} -> ok;
        Error -> Error
    end.
