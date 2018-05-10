-module(dbi_migrations).
-author('manuel@altenwald.com').

-export([update/2]).

-define(BASE_PATH, "migrations").

update(PoolName, AppName) ->
    SQL = <<"SELECT code, filename "
            "FROM schema_migrations "
            "ORDER BY id">>,
    {ok, _Count, Codes} = dbi:do_query(PoolName, SQL, []),
    Mask = filename:join(?BASE_PATH, "*_create.sql"),
    Wildcard = filename:join(code:priv_dir(AppName), Mask),
    Files = lists:map(fun(File) ->
        Filename = filename:basename(File),
        [Code|_] = string:tokens(Filename, "_"),
        {Code, Filename}
    end, lists:sort(filelib:wildcard(Wildcard))),
    BaseDir = filename:join(code:priv_dir(AppName), ?BASE_PATH),
    case Files -- Codes of
        [] ->
            error_logger:info_msg("Already done.");
        ToApply ->
            lists:foreach(fun(Tuple) ->
                apply_migration(PoolName, BaseDir, Tuple)
            end, Files -- Codes),
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
    end, Queries),
    Insert = <<"INSERT INTO schema_migrations(code, filename) "
               "VALUES ($1, $2)">>,
    {ok, 1, _} = dbi:do_query(PoolName, Insert, [Code, File]),
    ok.
