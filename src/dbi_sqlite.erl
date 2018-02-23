-module(dbi_sqlite).
-author('manuel@altenwald.com').

-behaviour(dbi).

-export([
    init/8,
    terminate/1,
    do_query/3
]).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(_Host, _Port, _User, _Pass, Database, Poolname, _Poolsize, _Extra) ->
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
