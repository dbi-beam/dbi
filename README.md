DBI for Erlang
==============

[![Build Status](https://api.travis-ci.org/altenwald/dbi.png)](https://travis-ci.org/altenwald/dbi)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/dbi.svg)](https://codecov.io/gh/altenwald/dbi)
[![License: LGPL 2.1](https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20v2.1-blue.svg)](https://raw.githubusercontent.com/altenwald/dbi/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/dbi.svg)](https://hex.pm/packages/dbi)

Database Interface for Erlang. This is an abstract implementation to use the most common database libraries ([p1_mysql][1], [epgsql][2] and [esqlite][4], and others you want) to use with standard SQL in your programs and don't worry about if you need to change between the main databases in the market.

### Install (rebar3)

To use it, with rebar, you only need to add the dependency to the rebar.config file:

```erlang
{deps, [
    {dbi, "0.1.0"}
]}
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

### Using DBI

To do a query:

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase, "SELECT * FROM users", []),
```

Or with params:

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase,
    "SELECT * FROM users WHERE id = $1", [12]),
```

Rows has the format: `[{field1, field2, ..., fieldN}, ...]`

**IMPORTANT** the use of $1..$100 in the query is extracted from pgsql, in mysql and sqlite is converted to the `?` syntax so, if you write this query:

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase,
    "UPDATE users SET name = $2 WHERE id = $1", [12, "Mike"]),
```

That should works well in pgsql, but **NOT for mysql and NOT for sqlite**. For avoid this situations, the best to do is **always keep the order of the params**.

### Delayed or Queued queries

If you want to create a connection to send only commands like INSERT, UPDATE or DELETE but without saturate the database (and run out database connections in the pool) you can use `dbi_delayed`:

```erlang
{ok, PID} = dbi_delayed:start_link(delay_myconn, myconn),
dbi_delayed:do_query(delay_myconn,
    "INSERT INTO my tab VALUES ($1, $2)", [N1, N2]),
```

This use only one connection from the pool `myconn`, when the query ends then `dbi_delayed` gets another query to run from the queue. You get statistics about the progress and the queue size:

```erlang
dbi_delayed:stats(delay_myconn).
[
    {size, 0},
    {query_error, 0},
    {query_ok, 1}
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

The cache param is in seconds. The ideal time to keep the cache values depends on the size of your tables, the data to store in the cache and how frequent are the changes in that data. For avoid flood and other issues due to fast queries or a lot of queries in little time you can use 5 or 10 seconds. To store the information about constants or other data without frequent changes you can use 3600 (one hour) or more time.

To use the cache you should to use the following function from `dbi_cache`:

```erlang
dbi_cache:do_query(mydatabase, "SELECT items FROM table"),
```

You can use `do_query/2` or `do_query/3` if you want to use params. And if you want to use a specific TTL (time-to-live) for your query, you can use `do_query/4`:

```erlang
dbi_cache:do_query(mydatabase,
    "SELECT items FROM table", [], 3600),
```

Enjoy!

[1]: https://github.com/processone/p1_mysql
[2]: https://github.com/wg/epgsql
[4]: https://github.com/mmzeeman/esqlite
