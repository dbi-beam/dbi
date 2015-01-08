-module(dbi_cache).
-author('manuel@altenwald.com').

-export([
    do_query/2,
    do_query/3,
    do_query/4
]).

-define(DBI_CACHE_DEFAULT_TTL, 5).

-spec do_query(Pool::atom(), Query::binary() | string()) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query) ->
    TTL = default_ttl(Pool),
    do_query(Pool, Query, [], TTL).

-spec do_query(Pool::atom(), Query::binary() | string(), Args::[any()]) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query, Args) ->
    TTL = default_ttl(Pool),
    do_query(Pool, Query, Args, TTL).

-spec do_query(Pool::atom(), Query::binary() | string(), Args::[any()],
    TTL::non_neg_integer()) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query, Args, TTL) ->
    case cache:get(Pool, {Query, Args}) of
    undefined ->
        case application:get_env(dbi, Pool) of
        {ok, Conf} ->
            Type = proplists:get_value(type, Conf),
            Module = list_to_atom("dbi_" ++ atom_to_list(Type)),
            Res = Module:do_query(Pool, Query, Args),
            cache:put(Pool, {Query, Args}, Res, TTL),
            Res;
        undefined ->
            {error, enopool}
        end;
    Res ->
        Res
    end.

-spec default_ttl(Pool::atom()) -> non_neg_integer().

default_ttl(Pool) ->
    DBConf = application:get_env(dbi, Pool, []),
    proplists:get_value(cache, DBConf, ?DBI_CACHE_DEFAULT_TTL).
