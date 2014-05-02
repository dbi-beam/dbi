-module(dbi).

-export([
    do_query/2,
    do_query/3,
    connect/9,
    connect/7
]).

-spec do_query(Pool::atom(), Query::binary() | string()) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query) ->
    do_query(Pool, Query, []).

-spec do_query(Pool::atom(), Query::binary() | string(), Args::[any()]) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query, Args) ->
    case application:get_env(dbi, Pool) of
        {ok, Conf} ->
            Type = proplists:get_value(type, Conf),
            Module = list_to_atom("dbi_" ++ atom_to_list(Type)),
            Module:do_query(Pool, Query, Args);
        undefined ->
            {error, enopool}
    end.

-spec connect( Type::atom(),
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom(),
    Poolsize :: integer(), Extra :: [term()]) -> ok.

connect(Type, Host, Port, User, Pass, Database, Poolname, Poolsize, Extra) ->
    Module = list_to_atom("dbi_" ++ atom_to_list(Type)),
    DBConf = [
        {type, Type}, {host, Host}, {port, Port},
        {user, User}, {pass, Pass},
        {database, Database}, {poolsize, Poolsize} | Extra
    ],
    application:set_env(dbi, Poolname, DBConf),
    Module:init(Host, Port, User, Pass, Database, Poolname, Poolsize, Extra).

-spec connect( Type::atom(),
    Host :: string(), Port :: integer(), User :: string(),
    Pass :: string(), Database :: string(), Poolname :: atom()) -> ok.

connect(Type, Host, Port, User, Pass, Database, Poolname) ->
    connect(Type, Host, Port, User, Pass, Database, Poolname, 10, []).
