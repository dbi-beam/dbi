defmodule DBI.Mixfile do
  use Mix.Project

  def project do
    [app: :dbi,
     version: get_version(),
     name: "DBI",
     description: "DataBase Interface for Erlang",
     package: package(),
     source_url: "https://github.com/dbi-beam/dbi",
     elixir: "~> 1.6",
     compilers: Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    env = if Mix.env == :test do
      [testdb1: [type: :dumb],
       testdb2: [type: :dumb,
                 cache: 3],
       testdb3: [type: :dumb,
                 delayed: :mydelayed],
       testdb4: [type: :dumb,
                 migrations: :dbi]
      ]
    else
      []
    end
    [applications: [:crypto, :public_key, :asn1, :ssl],
     mod: {DBI, []},
     env: env]
  end

  defp deps do
    [{:cache, "~> 2.2.0"},
     {:eql, "~> 0.1.2"},
     {:ex_doc, ">= 0.0.0", only: :dev}]
  end

  defp package do
    [files: ["lib", "src", "rebar.*", "include", "mix.exs", "README*", "COPYING*"],
     maintainers: ["Manuel Rubio"],
     licenses: ["LGPL 2.1"],
     links: %{"GitHub" => "https://github.com/dbi-beam/dbi"}]
  end

  defp get_version do
    retrieve_version_from_git()
    |> String.split("-")
    |> case do
      [tag] -> tag
      [tag, _num_commits, commit] -> "#{tag}-#{commit}"
    end
  end

  defp retrieve_version_from_git do
    System.cmd("git", ["describe", "--always", "--tags"])
    |> Tuple.to_list()
    |> List.first()
    |> String.trim()
  end
end
