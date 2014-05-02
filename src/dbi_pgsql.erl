-module(dbi_pgsql).

-export([
    init/8,
    run/0,
    do_query/3
]).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(Host, Port, User, Pass, Database, Poolname, Poolsize, Extra) ->
    application:load(epgsql_pool),
    Conf = application:get_all_env(epgsql_pool),
    Poolnames = proplists:get_value(pools, Conf, []),
    ok = application:set_env(epgsql_pool, pools, [Poolname|Poolnames]),
    DataConn = [
        {database, Database},
        {host, Host},
        {port, Port},
        {username, User},
        {password, Pass}
    ] ++ Extra,
    ok = application:set_env(epgsql_pool, Poolname, {Poolsize, DataConn}),
    ok.

-spec run() -> ok.

run() ->
    ok = application:start(epgsql),
    ok = application:start(epgsql_pool),
    ok.

-spec do_query(
    PoolDB :: atom(), 
    SQL :: binary() | string(), 
    [Params :: any()]) -> 
    {ok, integer(), [string() | binary()]} | {error, any()}.

do_query(PoolDB, SQL, Params) ->
    {ok, C} = pgsql_pool:get_connection(PoolDB),
    lager:debug("executing query [~s] with params ~p~n", [SQL, Params]),
    Result = case pgsql:equery(C, SQL, Params) of
        {ok, _Columns, Rows} -> {ok, length(Rows), Rows};
        {ok, Count} -> {ok, Count, []};
        {ok, Count, _Columns, Rows} -> {ok, Count, Rows};
        {error, Error} -> {error, Error}
    end,
    pgsql_pool:return_connection(PoolDB, C),
    Result.
