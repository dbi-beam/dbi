DBI for Erlang
--------------

Database Interface for Erlang. This is an abstract implementation to use the most common database libraries ([emysql][1], [epgsql_pool][2] and [epgsql][3], [esqlite][4], and others you want) to use with standard SQL in your programs and don't worry about if you need to change between the main databases in the market.

To use it, with rebar, you only need to add the dependency to the rebar.config file:

```erlang
{deps, [
    {dbi, ".*", {git, "https://github.com/altenwald/dbi.git", master}}
]}
```

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

That should works well in pgsql, but **NOT for mysql and NOT for sqlite**. For avoid this situations, the best to do is always keep the order of the params.

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

Enjoy!

[1]: https://github.com/Eonblast/Emysql
[2]: https://github.com/wg/epgsql
[3]: https://github.com/josephwecker/epgsql_pool
[4]: https://github.com/mmzeeman/esqlite
