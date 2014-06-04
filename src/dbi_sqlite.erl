-module(dbi_sqlite).

-export([
    init/8,
    run/0,
    do_query/3
]).

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(_Host, _Port, _User, _Pass, Database, Poolname, _Poolsize, _Extra) ->
    dbi_sqlite_server:open_database(Poolname, Database),
    ok.

-spec run() -> ok.

run() ->
    ok = application:start(esqlite),
    ok.

-spec do_query(
    PoolDB :: atom(), 
    SQL :: binary() | string(), 
    [Params :: any()]) -> 
    {ok, integer(), [string() | binary()]} | {error, any()}.

do_query(PoolDB, SQL, Params) when is_list(SQL) ->
    do_query(PoolDB, list_to_binary(SQL), Params);

do_query(PoolDB, RawSQL, Params) when is_binary(RawSQL) ->
    SQL = dbi_utils:resolve(RawSQL),
    {ok, Conn} = dbi_sqlite_server:get_database(PoolDB),
    Result = case catch esqlite3:q(SQL, Params, Conn) of
        Rows when is_list(Rows) ->
            {ok, length(Rows), Rows};
        {error, Error} ->
            {error, Error};
        Error ->
            {error, Error}
    end,
    Result.

%%-----------------------------------------------------------------------------
%% Internal functions

