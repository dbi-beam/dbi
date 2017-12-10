-module(dbi_mysql).
-author('manuel@altenwald.com').

-behaviour(dbi).

-export([
    start_link/1,
    init/8,
    terminate/1,
    do_query/3
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
    ConnData = [Host, dbi_utils:default(Port, ?DEFAULT_PORT),
                User, Pass, Database, undefined],
    PoolArgs = [{name, {local, Poolname}}, {worker_module, ?MODULE},
                {size, dbi_utils:default(Poolsize, ?DEFAULT_POOLSIZE)},
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
    {ok, integer(), [string() | binary()]} | {error, any()}.

do_query(PoolDB, SQL, Params) when is_list(SQL) ->
    do_query(PoolDB, list_to_binary(SQL), Params);

do_query(PoolDB, RawSQL, Params) when is_binary(RawSQL) ->
    SQL = dbi_utils:resolve(RawSQL),
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
