-module(dbi_app).
-author('manuel@altenwald.com').

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1, child/2]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Conf = application:get_all_env(),
    Modules = lists:foldl(fun
        ({included_applications, []}, Set) ->
            Set;
        ({PoolName, DBConf}, Set) ->
            Module = module_init(PoolName, DBConf),
            maybe_delayed(PoolName, DBConf),
            maybe_cache(PoolName, DBConf),
            ordsets:add_element({Module, PoolName}, Set)
    end, ordsets:new(), Conf),
    {ok, PID, Modules}.

stop(Modules) ->
    Ends = fun({Module, PoolName}) ->
        spawn(fun() ->
            Module:terminate(PoolName)
        end)
    end,
    [ Ends(M) || M <- Modules ],
    ok.

child_cache(I, C) ->
    {I, {cache, start_link, [I, C]},
        transient,
        5000,
        worker,
        [cache]}.

child_delayed(I, C) ->
    {I, {dbi_delayed, start_link, [I, C]},
        transient,
        5000,
        worker,
        [dbi_delayed]}.

child(I, A) ->
    {I, {I, start_link, A}, permanent, 5000, worker, [I]}.

%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% Internal functions

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

maybe_delayed(PoolName, DBConf) ->
    case proplists:get_value(delayed, DBConf) of
        undefined ->
            ok;
        DelayName ->
            ChildSpecD = child_delayed(DelayName, PoolName),
            supervisor:start_child(?MODULE, ChildSpecD)
    end.

maybe_cache(PoolName, DBConf) ->
    case proplists:get_value(cache, DBConf) of
        undefined ->
            ok;
        TTL ->
            CachePool = dbi_cache:cache_name(PoolName),
            ChildSpecC = child_cache(CachePool, [{n, 10}, {ttl, TTL}]),
            supervisor:start_child(?MODULE, ChildSpecC)
    end.
