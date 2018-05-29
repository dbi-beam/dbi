DBI for Erlang
==============

[![Build Status](https://api.travis-ci.org/altenwald/dbi.png)](https://travis-ci.org/altenwald/dbi)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/dbi.svg)](https://codecov.io/gh/altenwald/dbi)
[![License: LGPL 2.1](https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20v2.1-blue.svg)](https://raw.githubusercontent.com/altenwald/dbi/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/dbi.svg)](https://hex.pm/packages/dbi)

Database Interface for Erlang and Elixir. This is an abstract implementation to use the most common database libraries ([p1_mysql][1], [epgsql][2] and [esqlite][4], and others you want) to use with standard SQL in your programs and don't worry about if you need to change between the main databases in the market.

**IMPORTANT** Note that you'll need to include one (or several) of the other dependencies in the (DBI-BEAM)(https://github.com/dbi-beam) group to use SQLite, MySQL, PostgreSQL or other database supported.

### Install (rebar3)

To use it, with rebar, you only need to add the dependency to the rebar.config file:

```erlang
{deps, [
    {dbi, "1.1.0"}
]}
```

### Install (mix)

To use it, with mix, you only need to add the dependency to the mix.exs file:

```elixir
{:dbi, "~> 1.1.0"}
```

### Configuration

The configuration is made in the configuration file (`sys.config` or `app.config`) so, you can add a new block for config the database connection as follow:

```erlang
{dbi, [
    {mydatabase, [
        {type, mysql},
        {host, "localhost"},
        {user, "root"},
        {pass, "root"},
        {database, "mydatabase"},
        {poolsize, 10}
    ]},
    {mylocaldb, [
        {type, sqlite},
        {database, ":memory:"}
    ]},
    {mystrongdb, [
        {type, pgsql},
        {host, "localhost"},
        {user, "root"},
        {pass, "root"},
        {database, "mystrongdb"},
        {poolsize, 100}
    ]}
]}
```

The available types in this moment are: `mysql`, `pgsql` and `sqlite`.

In case you're using Elixir, you can define the configuration for your project in this way:

```elixir
confg :dbi, mydatabase: [
              type: :mysql,
              host: 'localhost',
              user: 'root',
              pass: 'root',
              database: 'mydatabase',
              poolsize: 10
            ],
            mylocaldb: [
              type: :sqlite,
              database: ':memory'
            ],
            mystrongdb: [
              type: :pgsql,
              host: 'localhost',
              user: 'root',
              pass: 'root',
              database: 'mystrongdb',
              poolsize: 100
            ]
```

### Using DBI

To do a query (Erlang):

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase, "SELECT * FROM users", []),
```

Elixir:

```elixir
{:ok, count, rows} = DBI.do_query(:mydatabase, "SELECT * FROM users", [])
```

Or with params (Erlang):

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase,
    "SELECT * FROM users WHERE id = $1", [12]),
```

Elixir:

```elixir
{:ok, count, rows} = DBI.do_query(:mydatabase,
    "SELECT * FROM users WHERE id = $1", [12])
```

Rows has the format: `[{field1, field2, ..., fieldN}, ...]`

**IMPORTANT** the use of $1..$100 in the query is extracted from pgsql, in mysql and sqlite is converted to the `?` syntax so, if you write this query:

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase,
    "UPDATE users SET name = $2 WHERE id = $1", [12, "Mike"]),
```

Elixir:

```elixir
{:ok, count, rows} = DBI.do_query(:mydatabase,
    "UPDATE users SET name = $2 WHERE id = $1", [12, "Mike"])
```

That should works well in pgsql, but **NOT for mysql and NOT for sqlite**. For avoid this situations, the best to do is **always keep the order of the params**.

### Delayed or Queued queries

If you want to create a connection to send only commands like INSERT, UPDATE or DELETE but without saturate the database (and run out database connections in the pool) you can use `dbi_delayed` (Erlang):

```erlang
{ok, PID} = dbi_delayed:start_link(delay_myconn, myconn),
dbi_delayed:do_query(delay_myconn,
    "INSERT INTO my tab VALUES ($1, $2)", [N1, N2]),
```

Elixir:

```elixir
{:ok, pid} = DBI.Delayed.start_link(:delay_myconn, :myconn)
DBI.Delayed.do_query(:delay_myconn,
    "INSERT INTO my tab VALUES ($1, $2)", [n1, n2])
```

This use only one connection from the pool `myconn`, when the query ends then `dbi_delayed` gets another query to run from the queue. You get statistics about the progress and the queue size (Erlang):

```erlang
dbi_delayed:stats(delay_myconn).
[
    {size, 0},
    {query_error, 0},
    {query_ok, 1}
]
```

Elixir:

```elixir
DBI.Delayed.stats(:delay_myconn)
[
    size: 0,
    query_error: 0,
    query_ok: 1
]
```

The delayed can be added to the configuration:

```erlang
{dbi, [
    {mydatabase, [
        {type, mysql},
        {host, "localhost"},
        {user, "root"},
        {pass, "root"},
        {database, "mydatabase"},
        {poolsize, 10},
        {delayed, delay_myconn}
    ]}
]}
```

Elixir:

```elixir
config :dbi, mydatabase: [
               type: :mysql,
               host: 'localhost',
               user: 'root',
               pass: 'root',
               database: 'mydatabase',
               poolsize: 10,
               delayed: :delay_myconn
             ]
```

### Cache queries

Another thing you can do is use a cache for SQL queries. The cache store the SQL as `key` and the result as `value` and keep the values for the time you specify in the configuration file:

```erlang
{dbi, [
    {mydatabase, [
        {type, mysql},
        {host, "localhost"},
        {user, "root"},
        {pass, "root"},
        {database, "mydatabase"},
        {poolsize, 10},
        {cache, 5}
    ]}
]}
```

Elixir:

```elixir
config :dbi, mydatabase: [
               type: :mysql,
               host: 'localhost',
               user: 'root',
               pass: 'root',
               database: 'mydatabase',
               poolsize: 10,
               cache: 5
             ]
```

The cache param is in seconds. The ideal time to keep the cache values depends on the size of your tables, the data to store in the cache and how frequent are the changes in that data. For avoid flood and other issues due to fast queries or a lot of queries in little time you can use 5 or 10 seconds. To store the information about constants or other data without frequent changes you can use 3600 (one hour) or more time.

To use the cache you should to use the following function from `dbi_cache`:

```erlang
dbi_cache:do_query(mydatabase, "SELECT items FROM table"),
```

Elixir:

```elixir
DBI.Cache.do_query(:mydatabase, "SELECT items FROM table")
```

You can use `do_query/2` or `do_query/3` if you want to use params. And if you want to use a specific TTL (time-to-live) for your query, you can use `do_query/4`:

```erlang
dbi_cache:do_query(mydatabase,
    "SELECT items FROM table", [], 3600),
```

Elixir:

```elixir
DBI.Cache.do_query(:mydatabase,
    "SELECT items FROM table", [], 3600)
```

### Migrations

You can add a configuration for each connection to configure migrations for a specific application:

```erlang
{dbi, [
    {mydatabase, [
        {type, mysql},
        {host, "localhost"},
        {user, "root"},
        {pass, "root"},
        {database, "mydatabase"},
        {poolsize, 10},
        {migrations, myapp}
    ]}
]},
```

For your application you'll need to create the path `priv/migrations` and locate there the files. The files must to have the format: `code_description_create.sql`. It's mandatory to have the code (it's recomended an incremental code like 001, 002, and so on) and the suffix `_create`. The extension must to be `sql`.

The content of the file is a plain SQL file with the following format:

```sql
-- :create_users_table
CREATE TABLE users (
    id serial primary key not null,
    name varchar
);
```

The table `schema_migrations` will be created then to store the migrations applied to avoid to apply them twice.

### Fixtures

When you're testing your software and using a real database connection you'll need to insert some specific data in a easy way. Fixtures is a mechanism to do it. You only need to format the data in the following way:

```erlang
Data = [{users,
         ["id", "name", "surname"],
         [{1, <<"Manuel">>, <<"Rubio">>},
          {2, <<"Marga">>, <<"Ortiz">>}]}].
```

This will auto-generate `INSERT` queries to the table `users` with the name of the columns (id, name, surname) and the columns.

You can run this as:

```erlang
dbi_fixtures:populate(my_connection, Data)
```

**IMPORTANT** The only restriction is the first column has to be an ID (numeric one). The system will call to `dbi:update_seq/3` when the data is inserted to update the ID to the last ID inserted.

Enjoy!

[1]: https://github.com/processone/p1_mysql
[2]: https://github.com/wg/epgsql
[4]: https://github.com/mmzeeman/esqlite
