-module(dbi_pgsql).
-author('manuel@altenwald.com').

-behaviour(dbi).

-export([
    start_link/1,
    init/8,
    terminate/1,
    do_query/3,
    check_migration/1,
    transaction/3
]).

-include("dbi.hrl").

-define(DEFAULT_POOLSIZE, 10).
-define(DEFAULT_MAX_OVERFLOW, 10).
-define(DEFAULT_PORT, 5432).

-spec start_link([term()]) -> {ok, pid()}.

start_link([Host, User, Pass, Opts]) ->
    {ok, _} = epgsql:connect(Host, User, Pass, Opts).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(Host, Port, User, Pass, Database, Poolname, Poolsize, Extra) ->
    application:start(epgsql),
    MaxOverflow = proplists:get_value(max_overflow, Extra, ?DEFAULT_MAX_OVERFLOW),
    DataConn = [Host, User, Pass,
                [{database, Database},
                 {port, dbi_query:default(Port, ?DEFAULT_PORT)}] ++ Extra],
    PoolArgs = [{name, {local, Poolname}}, {worker_module, ?MODULE},
                {size, dbi_query:default(Poolsize, ?DEFAULT_POOLSIZE)},
                {max_overflow, MaxOverflow}],
    ChildSpec = poolboy:child_spec(Poolname, PoolArgs, DataConn),
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

do_query(PID, SQL, Params) when is_pid(PID) ->
    case epgsql:equery(PID, SQL, Params) of
        {ok, _Columns, Rows} -> {ok, length(Rows), Rows};
        {ok, Count} -> {ok, Count, []};
        {ok, Count, _Columns, Rows} -> {ok, Count, Rows};
        {error, Error} -> {error, Error}
    end;

do_query(PoolDB, SQL, Params) when is_atom(PoolDB) ->
    poolboy:transaction(PoolDB, fun(PID) ->
        do_query(PID, SQL, Params)
    end).

-spec check_migration(PoolDB :: atom()) ->
      {ok, integer(), [binary()]}.

check_migration(PoolDB) ->
    Create = <<"CREATE TABLE IF NOT EXISTS schema_migrations("
               "id serial primary key not null, "
               "code text, "
               "filename text);">>,
    {ok, _, _} = do_query(PoolDB, Create, []),
    ok.

-spec transaction(PoolDB :: atom(), function(), Opts :: term()) ->
      {ok, integer(), [term()]} | {error, any()}.

transaction(PoolDB, Fun, Opts) ->
    poolboy:transaction(PoolDB, fun(PID) ->
        QTran = fun(C) ->
            Q = fun(Query, Args) ->
                do_query(C, Query, Args)
            end,
            Fun(Q)
        end,
        epgsql:with_transaction(PID, QTran)
    end, Opts).
