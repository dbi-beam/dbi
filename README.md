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

**IMPORTANT** the use of $1..$100 in the query is extracted from pgsql, in mysql is converted to the `?` syntax so, if you write this query:

```erlang
{ok, Count, Rows} = dbi:do_query(mydatabase, 
    "UPDATE users SET name = $2 WHERE id = $1", [12, "Mike"]),
```

That should works well in pgsql, but not for mysql. For avoid this situations, the best to do is always keep the order of the params.

**IMPORTANT** for mysql and postgresql, when you send an INSERT, UPDATE or DELETE statement, you receive `{ok, Count, []}` where `Count` is the number of affected rows. SQLite has not this feature implemented.

Enjoy!

[1]: https://github.com/Eonblast/Emysql
[2]: https://github.com/wg/epgsql
[3]: https://github.com/josephwecker/epgsql_pool
[4]: https://github.com/mmzeeman/esqlite
