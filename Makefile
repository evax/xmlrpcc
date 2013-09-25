all: compile

deps:
	@rebar get-deps
	@rebar compile

compile: deps
	@rebar compile skip_deps=true

clean:
	@rm -Rf ebin
	@rm -Rf deps
