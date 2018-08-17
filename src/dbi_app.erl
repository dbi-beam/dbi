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
    Modules = dbi_config:apply_configs(),
    {ok, PID, Modules}.

stop(Modules) ->
    Ends = fun({Module, PoolName}) ->
        spawn(fun() ->
            Module:terminate(PoolName)
        end)
    end,
    [ Ends(M) || M <- Modules ],
    ok.

child(I, A) ->
    {I, {I, start_link, A}, permanent, 5000, worker, [I]}.

%% Supervisor callbacks

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
