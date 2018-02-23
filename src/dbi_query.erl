-module(dbi_query).
-author('manuel@altenwald.com').

-export([
    resolve/1,
    sql_type/1,
    default/2
]).

resolve(SQL) ->
    resolve(SQL, <<>>).


resolve(<<>>, SQL) ->
    SQL;

resolve(<<"$$",Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, "$">>);

resolve(<<"$",A:8,Rest/binary>>, SQL) when A >= $0 andalso A =< $9 ->
    resolve(drop_numbers(Rest), <<SQL/binary, "?">>);

resolve(<<C:1/binary,Rest/binary>>, SQL) ->
    resolve(Rest, <<SQL/binary, C/binary>>).


drop_numbers(<<A:8,Rest/binary>>) when A >= $0 andalso A =< $9 ->
    drop_numbers(Rest);

drop_numbers(Rest) ->
    Rest.


sql_type(<<" ",Rest/binary>>) -> sql_type(Rest);
sql_type(<<"\r", Rest/binary>>) -> sql_type(Rest);
sql_type(<<"\t", Rest/binary>>) -> sql_type(Rest);
sql_type(<<"\n", Rest/binary>>) -> sql_type(Rest);
sql_type(<<S:8,E1:8,L:8,E2:8,C:8,T:8," ",_/binary>>) when
    (S =:= $S orelse S =:= $s) andalso
    (E1 =:= $E orelse E1 =:= $e) andalso
    (L =:= $L orelse L =:= $l) andalso
    (E2 =:= $E orelse E2 =:= $e) andalso
    (C =:= $C orelse C =:= $c) andalso
    (T =:= $T orelse T =:= $t) -> dql;
sql_type(<<S:8,H:8,O:8,W:8," ",_/binary>>) when
    (S =:= $S orelse S =:= $s) andalso
    (H =:= $H orelse H =:= $h) andalso
    (O =:= $O orelse O =:= $o) andalso
    (W =:= $W orelse W =:= $w) -> dql;
sql_type(_) -> dml.


default(undefined, Default) -> Default;
default(Value, _Default) -> Value.
