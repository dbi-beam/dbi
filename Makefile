all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

test: compile
	./rebar eunit skip_deps=true

clean:
	./rebar clean

.PHONY: test

