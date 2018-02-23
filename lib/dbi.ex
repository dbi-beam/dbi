defmodule DBI do
  use Application

  # wrap from dbi_app.erl
  def start(type, args), do: :dbi_app.start(type, args)
  def stop(modules), do: :dbi_app.stop(modules)

  # wrap from dbi.erl
  def start(), do: :dbi.start()
  def do_query(pool, query), do: :dbi.do_query(pool, query)
  def do_query(pool, query, args), do: :dbi.do_query(pool, query, args)
  def connect(type, host, port, user, pass, database, poolname) do
    :dbi.connect(type, host, port, user, pass, database, poolname)
  end
  def connect(type, host, port, user, pass, db, poolname, poolsize, extra) do
    :dbi.connect(type, host, port, user, pass, db, poolname, poolsize, extra)
  end

  defmodule Cache do
    def do_query(ref, query), do: :dbi_cache.do_query(ref, query)
    def do_query(ref, query, args), do: :dbi_cache.do_query(ref, query, args)
    def do_query(ref, query, args, ttl) do
      :dbi_cache.do_query(ref, query, args, ttl)
    end
  end

  defmodule Delayed do
    def start_link(ref, conn), do: :dbi_delayed.start_link(ref, conn)
    def do_query(ref, query, args), do: :dbi_delayed.do_query(ref, query, args)
    def do_query(ref, query), do: :dbi_delayed.do_query(ref, query)
    def stats(ref), do: :dbi_delayed.stats(ref)
  end
end
