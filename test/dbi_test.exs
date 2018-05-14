defmodule DBITest do
  use ExUnit.Case

  test "basic" do
    assert :ok == DBI.start()
    assert {:ok, 0, []} == DBI.do_query(:testdb1,
                                        "CREATE TABLE testing ( id int primary key );")
    assert {:ok, 1, []} == DBI.do_query(:testdb1,
                                        "INSERT INTO testing(id) VALUES ($1)", [1])
    assert {:ok, 4, []} == DBI.do_query(:testdb1,
                                        "INSERT INTO testing(id) VALUES ($1),($2),($3),($4)",
                                        [2, 3, 4, 5])
    {:ok, 5, _} = DBI.do_query(:testdb1, "SELECT * FROM testing")
    :ok = Application.stop(:dbi)
  end

  test "migrations" do
    assert :ok == DBI.start()
    assert {:ok, 1, []} == DBI.do_query(:testdb4,
                                        "INSERT INTO users(id, name) VALUES ($1, $2)",
                                        [1, "alice"])
    assert {:ok, 4, []} == DBI.do_query(:testdb4,
                                        "INSERT INTO users(id, name) VALUES ($1, $2), " <>
                                        "($3, $4), ($5, $6), ($7, $8)",
                                        [2, "bob", 3, "charlie", 4, "darcy", 5, "elliott"])
    {:ok, 5, _} = DBI.do_query(:testdb4, "SELECT * FROM users")
    :ok = Application.stop(:dbi)
  end

  test "delayed" do
    assert :ok == DBI.start()
    assert :ok == DBI.Delayed.do_query(:mydelayed,
                                       "CREATE TABLE testing ( id int primary key );")
    assert :ok == DBI.Delayed.do_query(:mydelayed,
                                       "INSERT INTO testing(id) VALUES ($1)", [1])
    assert :ok == DBI.Delayed.do_query(:mydelayed,
                                        "INSERT INTO testing(id) VALUES ($1),($2),($3),($4)",
                                        [2, 3, 4, 5])
    assert {:error, {:sqlite_error, 'no such table: testing'}} ==
           DBI.do_query(:testdb3, "SELECT * FROM testing")
    :timer.sleep(:timer.seconds(1))
    {:ok, 5, _} = DBI.do_query(:testdb3, "SELECT * FROM testing")
    :ok = Application.stop(:dbi)
  end

  test "cache" do
    assert :ok == DBI.start()
    assert {:ok, 0, []} == DBI.do_query(:testdb2,
                                        "CREATE TABLE testing ( id int primary key );")
    assert {:ok, 1, []} == DBI.do_query(:testdb2,
                                        "INSERT INTO testing(id) VALUES ($1)", [1])
    assert {:ok, 1, [{1}]} == DBI.Cache.do_query(:testdb2, "SELECT * FROM testing")
    DBI.do_query(:testdb2, "UPDATE testing SET id = $1", [2])
    assert {:ok, 1, [{1}]} == DBI.Cache.do_query(:testdb2, "SELECT * FROM testing")
    :timer.sleep(:timer.seconds(3))
    {:ok, 1, [{2}]} = DBI.Cache.do_query(:testdb2, "SELECT * FROM testing")
    :ok = Application.stop(:dbi)
  end

  test "args" do
    assert :ok == DBI.start()
    assert {:ok, 0, []} == DBI.do_query(:testdb1,
                                        "CREATE TABLE comments ( id int primary key, comment varchar(100) );")
    Enum.each(1..10, fn(i) ->
        assert {:ok, 1, []} == DBI.do_query(:testdb1,
                                           "INSERT INTO comments(id, comment) VALUES ($1, $2)",
                                           [i, "Data that requires sanitization! ' --"])
    end)
    {:ok, 10, [{_,_}|_]} = DBI.do_query(:testdb1, "SELECT * FROM comments")
    :ok = Application.stop(:dbi)
  end
end
