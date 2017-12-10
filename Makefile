all: compile

compile:
	./rebar3 compile

test:
	-epmd -daemon
	./rebar3 do xref, eunit, cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname dbi \
		-output cobertura.xml > /dev/null

clean:
	./rebar3 clean

.PHONY: test
