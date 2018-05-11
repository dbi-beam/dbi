-module(dbi_migrations).
-author('manuel@altenwald.com').

-export([update/2, downgrade/2]).

-define(BASE_PATH, "migrations").

update(PoolName, AppName) ->
    SQL = <<"SELECT code, filename "
            "FROM schema_migrations "
            "ORDER BY id ASC">>,
    {ok, _Count, Codes} = dbi:do_query(PoolName, SQL, []),
    error_logger:info_msg("database migrations: ~p", [Codes]),
    Mask = filename:join(?BASE_PATH, "*_create.sql"),
    Wildcard = filename:join(code:priv_dir(AppName), Mask),
    error_logger:info_msg("searching migrations: ~s", [Mask]),
    Files = lists:map(fun(File) ->
        Filename = filename:basename(File),
        [Code|_] = string:tokens(Filename, "_"),
        error_logger:info_msg("generated: ~p", [{Code, Filename}]),
        {list_to_binary(Code), list_to_binary(Filename)}
    end, lists:sort(filelib:wildcard(Wildcard))),
    BaseDir = filename:join(code:priv_dir(AppName), ?BASE_PATH),
    case Files -- Codes of
        [] ->
            error_logger:info_msg("Already done.");
        ToApply ->
            lists:foreach(fun(Tuple) ->
                apply_migration(PoolName, BaseDir, Tuple)
            end, ToApply),
            error_logger:info_msg("~b migration(s) applied.",
                                  [length(ToApply)])
    end.

apply_migration(PoolName, BaseDir, {Code, File}) ->
    Filename = filename:join(BaseDir, File),
    error_logger:info_msg("applying migration ~s", [Code]),
    {ok, Queries} = eql:compile(Filename),
    %% FIXME: we'll need to add transactions in near future:
    lists:foreach(fun({_Name, SQL}) ->
        {ok, _, _} = dbi:do_query(PoolName, SQL)
    end, lists:reverse(Queries)),
    Insert = <<"INSERT INTO schema_migrations(code, filename) "
               "VALUES ($1, $2)">>,
    {ok, 1, _} = dbi:do_query(PoolName, Insert, [Code, File]),
    ok.

downgrade(PoolName, AppName) ->
    SQL = <<"SELECT code, filename "
            "FROM schema_migrations "
            "ORDER BY id DESC">>,
    {ok, _Count, Codes} = dbi:do_query(PoolName, SQL, []),
    error_logger:info_msg("database migrations: ~p", [Codes]),
    Mask = filename:join(?BASE_PATH, "*_drop.sql"),
    Wildcard = filename:join(code:priv_dir(AppName), Mask),
    error_logger:info_msg("searching migrations: ~s", [Mask]),
    Files = lists:map(fun(File) ->
        Filename = filename:basename(File),
        [Code|_] = string:tokens(Filename, "_"),
        CreatedRaw = re:replace(Filename, "_drop.sql$", "_create.sql"),
        Created = iolist_to_binary(CreatedRaw),
        error_logger:info_msg("generated: ~p", [{Code, Created}]),
        {list_to_binary(Code), Created}
    end, lists:sort(filelib:wildcard(Wildcard))),
    BaseDir = filename:join(code:priv_dir(AppName), ?BASE_PATH),
    case Codes -- (Codes -- Files) of
        [] ->
            error_logger:info_msg("Already clean.");
        ToApply ->
            lists:foreach(fun({Code, File}) ->
                DropRaw = re:replace(File, "_create.sql$", "_drop.sql"),
                Drop = iolist_to_binary(DropRaw),
                rollback_migration(PoolName, BaseDir, {Code, Drop})
            end, ToApply),
            error_logger:info_msg("~b migration(s) removed.",
                                  [length(ToApply)])
    end.

rollback_migration(PoolName, BaseDir, {Code, File}) ->
    Filename = filename:join(BaseDir, File),
    error_logger:info_msg("cleaning migration ~s", [Code]),
    {ok, Queries} = eql:compile(Filename),
    %% FIXME: we'll need to add transactions in near future:
    lists:foreach(fun({_Name, SQL}) ->
        {ok, _, _} = dbi:do_query(PoolName, SQL)
    end, lists:reverse(Queries)),
    Insert = <<"DELETE FROM schema_migrations "
               "WHERE code = $1">>,
    {ok, 1, _} = dbi:do_query(PoolName, Insert, [Code]),
    ok.
