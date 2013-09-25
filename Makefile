all: compile

deps:
	@rebar get-deps
	@rebar compile

compile: deps
	@rebar compile skip_deps=true

tests: clean compile
	@rebar skip_deps=true eunit

clean:
	@rm -Rf ebin

dist-clean: clean
	@rm -Rf deps
