-module(dbi_utils).

-export([
    resolve/1
]).

resolve(SQL) ->
    resolve(SQL, <<>>).


resolve(<<>>, SQL) ->
    SQL;

resolve(<<"$$",Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, "$">>);

resolve(<<"$",A:1/binary,Rest/binary>>, SQL) when A >= $0 andalso A =< $9 ->
    resolve(drop_numbers(Rest), <<SQL/binary, "?">>);

resolve(<<C:1/binary,Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, C/binary>>).


drop_numbers(<<A:1/binary,Rest/binary>>) when A >= $0 andalso A =< $9 ->
    drop_numbers(Rest);

drop_numbers(Rest) ->
    Rest.
