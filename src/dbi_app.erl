-module(dbi_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
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
    		ordsets:add_element(Module, Set)
    end, ordsets:new(), Conf),
    [ M:run() || M <- Modules ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
    	?CHILD(dbi_sqlite_server, worker)
    ]} }.
