-module(dbi_config).
-author('manuel@altenwald.com').

-export([apply_configs/0,
         to_charlist/1]).

-define(SUPERVISOR, dbi_app).

to_charlist(Str) when is_list(Str) -> Str;
to_charlist(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_charlist(Int) when is_integer(Int) -> integer_to_list(Int);
to_charlist(Float) when is_float(Float) -> float_to_list(Float);
to_charlist(Atom) when is_atom(Atom) -> atom_to_list(Atom).

apply_configs() ->
    Conf = application:get_all_env(dbi),
    lists:foldl(fun
        ({included_applications, []}, Set) ->
            Set;
        ({PoolName, DBConf}, Set) ->
            Module = module_init(PoolName, DBConf),
            maybe_delayed(PoolName, DBConf),
            maybe_cache(PoolName, DBConf),
            ordsets:add_element({Module, PoolName}, Set)
    end, ordsets:new(), Conf).

module_init(PoolName, DBConf) ->
    Type = proplists:get_value(type, DBConf),
    Module = list_to_atom("dbi_" ++ atom_to_list(Type)),
    Host = proplists:get_value(host, DBConf),
    Port = proplists:get_value(port, DBConf),
    User = proplists:get_value(user, DBConf),
    Pass = proplists:get_value(pass, DBConf),
    DBName = proplists:get_value(database, DBConf),
    Poolsize = proplists:get_value(poolsize, DBConf),
    Module:init(Host, Port, User, Pass, DBName, PoolName, Poolsize, DBConf),
    case  proplists:get_value(migrations, DBConf, undefined) of
        undefined ->
            ok;
        AppName ->
            Module:check_migration(PoolName),
            dbi_migrations:update(PoolName, AppName)
    end,
    Module.

child_delayed(I, C) ->
    {I, {dbi_delayed, start_link, [I, C]},
        transient,
        5000,
        worker,
        [dbi_delayed]}.

maybe_delayed(PoolName, DBConf) ->
    case proplists:get_value(delayed, DBConf) of
        undefined ->
            ok;
        DelayName ->
            ChildSpecD = child_delayed(DelayName, PoolName),
            supervisor:start_child(?SUPERVISOR, ChildSpecD)
    end.

child_cache(I, C) ->
    {I, {cache, start_link, [I, C]},
        transient,
        5000,
        worker,
        [cache]}.

maybe_cache(PoolName, DBConf) ->
    case proplists:get_value(cache, DBConf) of
        undefined ->
            ok;
        TTL ->
            CachePool = dbi_cache:cache_name(PoolName),
            ChildSpecC = child_cache(CachePool, [{n, 10}, {ttl, TTL}]),
            supervisor:start_child(?SUPERVISOR, ChildSpecC)
    end.
