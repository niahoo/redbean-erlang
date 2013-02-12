

all: get-deps vm-args compile-all

remake: clean-deps clean all

compile: vm-args
	- rm ebin -r
	@(rebar skip_deps=true compile)

compile-all:
	- rm ebin -r
	@(rebar compile)

get-deps:
	@(GPROC_DIST=true rebar get-deps)

clean:
	@(rebar clean)

clean-deps:
	@(rebar delete-deps)

tests:
	@(rebar skip_deps=true verbose=0 eunit)


xref:
	@(rebar skip_deps=true xref)

vm-args:

	cat priv/vm-args.src > priv/vm-args

	for i in `find -name ebin -type d`; do \
		echo '-pa' $$i >> priv/vm-args; \
	done

dial:
	dialyzer \
		--src -r src \
		-pa $(HOME)/src/erlbean \
		-pa $(HOME)/src/erlbean/deps/epgsql \
		--verbose

doc:
	@(rebar skip_deps=true doc)
