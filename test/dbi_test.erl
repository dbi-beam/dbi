-module(dbi_test).
-compile([debug_info, export_all]).

-include_lib("eunit/include/eunit.hrl").

dbi_basic_test() ->
    application:set_env(dbi, testdb, [
        {type, dumb}
    ]),
    ok = dbi:start(),
    {ok,0,[]} = dbi:do_query(
        testdb,
        "CREATE TABLE testing ( id int primary key );"),
    {ok,1,[]} = dbi:do_query(
        testdb,
        "INSERT INTO testing(id) VALUES ($1)", [1]),
    {ok,4,[]} = dbi:do_query(
        testdb,
        "INSERT INTO testing(id) VALUES ($1),($2),($3),($4)",
        lists:seq(2, 5)),
    {ok,5,_} = dbi:do_query(testdb, "SELECT * FROM testing"),
    ok = application:stop(dbi),
    ok.

dbi_migrations_test() ->
    application:set_env(dbi, testdb, [
        {type, dumb},
        {migrations, dbi}
    ]),
    ok = dbi:start(),
    {ok,1,[]} = dbi:do_query(
        testdb,
        "INSERT INTO users(id, name) VALUES ($1, $2)",
        [1, <<"alice">>]),
    {ok,4,[]} = dbi:do_query(
        testdb,
        "INSERT INTO users(id, name) VALUES ($1, $2), "
        "($3, $4), ($5, $6), ($7, $8)",
        [2, <<"bob">>, 3, <<"charlie">>, 4, <<"darcy">>,
         5, <<"elliott">>]),
    {ok, 5, _} = dbi:do_query(testdb, "SELECT * FROM users"),
    ok = application:stop(dbi),
    ok.

dbi_delayed_test() ->
    application:set_env(dbi, testdb, [
        {type, dumb},
        {delayed, mydelayed}
    ]),
    ok = dbi:start(),
    ok = dbi_delayed:do_query(
        mydelayed,
        "CREATE TABLE testing ( id int primary key );"),
    ok = dbi_delayed:do_query(
        mydelayed,
        "INSERT INTO testing(id) VALUES ($1)", [1]),
    ok = dbi_delayed:do_query(
        mydelayed,
        "INSERT INTO testing(id) VALUES ($1),($2),($3),($4)",
        lists:seq(2, 5)),
    {error,{sqlite_error,"no such table: testing"}} =
        dbi:do_query(testdb, "SELECT * FROM testing"),
    timer:sleep(1000),
    {ok,5,_} = dbi:do_query(testdb, "SELECT * FROM testing"),
    ok = application:stop(dbi),
    ok.

dbi_cache_test() ->
    application:set_env(dbi, testdb, [
        {type, dumb},
        {cache, 3}
    ]),
    ok = dbi:start(),
    {ok,0,[]} = dbi:do_query(
        testdb,
        "CREATE TABLE testing ( id int primary key );"),
    {ok,1,[]} = dbi:do_query(
        testdb,
        "INSERT INTO testing(id) VALUES ($1)", [1]),
    {ok,1,[{1}]} = dbi_cache:do_query(testdb, "SELECT * FROM testing"),
    dbi:do_query(testdb, "UPDATE testing SET id = 2"),
    {ok,1,[{1}]} = dbi_cache:do_query(testdb, "SELECT * FROM testing"),
    timer:sleep(3000),
    {ok,1,[{2}]} = dbi_cache:do_query(testdb, "SELECT * FROM testing"),
    ok = application:stop(dbi),
    ok.

dbi_args_test() ->
    application:set_env(dbi, testdb, [
        {type, dumb}
    ]),
    ok = dbi:start(),
    {ok,0,[]} = dbi:do_query(
        testdb,
        "CREATE TABLE users ( id int primary key, username varchar(100) );"),
    lists:foreach(fun(I) ->
        {ok,1,[]} = dbi:do_query(
            testdb,
            "INSERT INTO users(id, username) VALUES ($1, $2)",
            [I, <<"Data that requires sanitization! ' --">>])
    end, lists:seq(1,10)),
    {ok,10,[{_,_}|_]} = dbi:do_query(testdb, "SELECT * FROM users"),
    ok = application:stop(dbi),
    ok.
