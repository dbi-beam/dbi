-module(dbi_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD_DELAYED(I, C), {I, 
    {dbi_delayed, start_link, [I, C]}, 
    transient, 5000, worker, [dbi_delayed]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Conf = application:get_all_env(),
    Modules = lists:foldl(fun
        ({included_applications, []}, Set) -> Set;
        ({PoolName, DBConf}, Set) ->
            Type = proplists:get_value(type, DBConf),
            Module = list_to_atom("dbi_" ++ atom_to_list(Type)),
            Host = proplists:get_value(host, DBConf),
            Port = proplists:get_value(port, DBConf),
            User = proplists:get_value(user, DBConf),
            Pass = proplists:get_value(pass, DBConf),
            DBName = proplists:get_value(database, DBConf),
            Poolsize = proplists:get_value(poolsize, DBConf),
            Module:init(Host, Port, User, Pass, DBName, PoolName, Poolsize, DBConf),
            case proplists:get_value(delayed, DBConf) of
                undefined -> ok;
                DelayName ->
                    ChildSpec = ?CHILD_DELAYED(DelayName, PoolName),
                    supervisor:start_child(?MODULE, ChildSpec)
            end,
            ordsets:add_element({Module,PoolName}, Set)
    end, ordsets:new(), Conf),
    [ M:run() || {M,_} <- Modules ],
    {ok, PID, Modules}.

stop(Modules) ->
    Ends = fun({Module,PoolName}) ->
        spawn(fun() ->
            Module:terminate(PoolName)
        end)
    end,
    [ Ends(M) || M <- Modules ],
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(dbi_sqlite_server, [])
    ]} }.
