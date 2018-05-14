-module(dbi_sqlite).
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

init(_Host, _Port, _User, _Pass, Database, Poolname, _Poolsize, _Extra) ->
    case whereis(dbi_sqlite_server) of
        undefined ->
            ChildSpec = dbi_app:child(dbi_sqlite_server, []),
            supervisor:start_child(dbi_app, ChildSpec);
        PID when is_pid(PID) ->
            ok
    end,
    application:start(esqlite),
    dbi_sqlite_server:open_database(Poolname, Database),
    ok.

-spec terminate(Poolname :: atom()) -> ok.

terminate(Poolname) ->
    dbi_sqlite_server:close_database(Poolname),
    ok.

-spec do_query(
    PoolDB :: atom(),
    SQL :: binary() | string(),
    [Params :: any()]) ->
    {ok, integer(), [string() | binary()]} | {error, any()}.

do_query(PoolDB, SQL, Params) when is_list(SQL) ->
    do_query(PoolDB, list_to_binary(SQL), Params);

do_query(PoolDB, RawSQL, Params) when is_binary(RawSQL) ->
    SQL = dbi_query:resolve(RawSQL),
    {ok, Conn} = dbi_sqlite_server:get_database(PoolDB),
    case dbi_query:sql_type(SQL) of
        dql ->
            case catch esqlite3:q(SQL, Params, Conn) of
                Rows when is_list(Rows) -> {ok, length(Rows), Rows};
                {error, Error} ->
                    {error, Error};
                {'EXIT', Error} ->
                    {error, Error}
            end;
        dml ->
            case catch esqlite3:exec(SQL, Params, Conn) of
                '$done' ->
                    {ok, AffectedRows} = esqlite3:changes(Conn),
                    {ok, AffectedRows, []};
                {error, Error} ->
                    {error, Error};
                {'EXIT', Error} ->
                    {error, Error}
            end
    end.

-spec check_migration(PoolDB :: atom()) ->
      {ok, integer(), [binary()]}.

check_migration(PoolDB) ->
    Create = <<"CREATE TABLE IF NOT EXISTS schema_migrations("
               "id integer primary key autoincrement, "
               "code varchar, "
               "filename varchar);">>,
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
        {ok, _, _} ->
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
