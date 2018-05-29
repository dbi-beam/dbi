-module(dbi_fixtures).
-author('manuel@altenwald.com').

-export([populate/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-else.
-define(debugFmt(_, _), ok).
-define(assertEqual(A, B), A = B).
-endif.

populate(PoolName, Data) ->
    lists:foreach(fun({Name, Columns, Rows}) ->
        ColNum = length(Columns),
        ParamNumbers = [ [$$|integer_to_list(X)] || X <- lists:seq(1, ColNum) ],
        ParamNumberTexts = list_to_binary(string:join(ParamNumbers, ", ")),
        Params = list_to_binary(string:join(Columns, ", ")),
        NameBin = atom_to_binary(Name, utf8),
        Query = <<"INSERT INTO ", NameBin/binary, "(", Params/binary,
                  ") VALUES (", ParamNumberTexts/binary, ")">>,
        GetMax = fun([Id|_], MaxId) -> erlang:max(Id, MaxId) end,
        SeqId = lists:foldl(GetMax, 0, Rows),
        lists:foreach(fun(Row) ->
                          ?debugFmt("Query: ~s ~999p", [Query, Row]),
                          ?assertEqual({ok, 1, []},
                                       dbi:do_query(PoolName, Query, Row))
                      end, Rows),
        ok = dbi:update_seq(PoolName, SeqId + 1)
    end, Data).
