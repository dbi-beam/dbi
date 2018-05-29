-module(dbi).
-author('manuel@altenwald.com').

-export([
    start/0,
    get_backend/1,
    do_query/2,
    do_query/3,
    connect/9,
    connect/7,
    get_migrations/1,
    add_migration/3,
    rem_migration/2,
    update_seq/3
]).

-callback init(Host :: string(), Port :: integer(), User :: string(),
               Pass :: string(), Database :: string(), Poolname :: atom(),
               Poolsize :: integer(), Extra :: [term()]) -> ok.

-callback do_query(PoolDB :: atom(), SQL :: binary() | string(),
                   [Params :: any()]) ->
          {ok, integer(), [term()]} | {error, any()}.

%% TODO:
% -callback transaction(PoolDB :: atom(), function()) -> ok | {error, any()}.

-callback check_migration(PoolDB :: atom()) ->
          {ok, integer(), [binary()]}.

-callback terminate(Poolname :: atom()) -> ok.

-callback get_migrations(Poolname :: atom()) ->
          {ok, Count :: integer(), [{binary()}]}.

-callback add_migration(Poolname :: atom(), Code :: binary(), File :: binary()) ->
          ok | {error, Reason :: any()}.

-callback rem_migration(Poolname :: atom(), Code :: binary()) ->
          ok | {error, Reason :: any()}.

-callback update_seq(Poolname :: atom(), SeqId :: binary(),
                     SeqNum :: pos_integer()) ->
          ok | {error, Reason :: any()}.

-spec start(App::atom()) -> ok.

-optional_callbacks([update_seq/3]).

start(App) ->
    case application:start(App) of
        {error, {already_started, App}} -> ok;
        {error, {not_started, DepApp}} ->
            start(DepApp),
            start(App);
        Other -> Other
    end.

-spec get_backend(Pool::atom()) -> module().

get_backend(Pool) ->
    {ok, Conf} = application:get_env(dbi, Pool),
    Type = proplists:get_value(type, Conf),
    list_to_atom("dbi_" ++ atom_to_list(Type)).

-spec start() -> ok.

start() ->
    start(dbi).

-spec do_query(Pool::atom(), Query::binary() | string()) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query) ->
    do_query(Pool, Query, []).

-spec do_query(Pool::atom(), Query::binary() | string(), Args::[any()]) ->
    {error, Reason::atom()} | {ok, Count::integer(), Rows::[term()]}.

do_query(Pool, Query, Args) ->
    Module = get_backend(Pool),
    Module:do_query(Pool, Query, Args).

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

-spec get_migrations(Poolname :: atom()) ->
      {ok, Count :: integer(), [{binary()}]}.

get_migrations(Poolname) ->
    Module = get_backend(Poolname),
    Module:get_migrations(Poolname).

-spec add_migration(Poolname :: atom(), Code :: binary(), File :: binary()) ->
      ok | {error, Reason :: any()}.

add_migration(Poolname, Code, File) ->
    Module = get_backend(Poolname),
    Module:add_migration(Poolname, Code, File).

-spec rem_migration(Poolname :: atom(), Code :: binary()) ->
      ok | {error, Reason :: any()}.

rem_migration(Poolname, Code) ->
    Module = get_backend(Poolname),
    Module:rem_migration(Poolname, Code).

-spec update_seq(Poolname :: atom(), SeqId :: binary(),
                 SeqNum :: pos_integer()) ->
      ok | {error, Reason :: any()}.

update_seq(Poolname, SeqId, SeqNum) ->
    Module = get_backend(Poolname),
    case lists:member({update_seq, 2}, Module:module_info(exports)) of
        true -> Module:update_seq(Poolname, SeqId, SeqNum);
        false -> {error, notimpl}
    end.
