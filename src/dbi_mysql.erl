-module(dbi_mysql).

-export([
    init/8,
    run/0,
    do_query/3
]).

-include_lib("emysql/include/emysql.hrl").

-spec init(
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

init(Host, OPort, User, Pass, Database, Poolname, OPoolsize, Extra) ->
    Encoding = proplists:get_value(encoding, Extra, utf8),
    Port = default(OPort, 3306),
    Poolsize = default(OPoolsize, 10),
    emysql:add_pool(Poolname, Poolsize, User, Pass, Host, Port, 
        Database, Encoding), 
    ok.

-spec run() -> ok.

run() ->
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
    Result = case emysql:execute(PoolDB, SQL, Params) of
        #result_packet{rows=Rows} ->
            {ok, length(Rows), [ list_to_tuple(Row) || Row <- Rows ]};
        #ok_packet{affected_rows=Count} ->
            {ok, Count, []};
        #error_packet{msg=Error} ->
            {error, Error}
    end,
    Result.

%%-----------------------------------------------------------------------------
%% Internal functions

default(undefined, Default) -> Default;
default(Value, _Default) -> Value.
