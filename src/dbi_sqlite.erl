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
    SQL = resolve(RawSQL),
    {ok, Conn} = dbi_sqlite_server:get_database(PoolDB),
    Result = case esqlite3:q(SQL, Params, Conn) of
        Rows when is_list(Rows) ->
            {ok, length(Rows), Rows};
        Error ->
            {error, Error}
    end,
    Result.

%%-----------------------------------------------------------------------------
%% Internal functions

resolve(SQL) ->
    resolve(SQL, <<>>).

resolve(<<>>, SQL) ->
    SQL;
resolve(<<"$$",Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, "$">>);
resolve(<<"$",A:1/binary,Rest/binary>>, SQL) when A >= $0 andalso A =< $9 ->
    resolve(Rest, <<SQL/binary, "?", A/binary>>);
resolve(<<C:1/binary,Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, C/binary>>).
