all: get-deps vm-args compile-all

remake: clean-deps clean all

compile: vm-args
	- rm ebin -r
	@(rebar skip_deps=true compile)

compile-all:
	- rm ebin -r
	@(rebar compile)

get-deps:
	@(rebar get-deps)

clean:
	@(rebar clean)

clean-deps:
	@(rebar delete-deps)

xref:
	@(rebar skip_deps=true xref)

vm-args:

	cat priv/vm-args.src > priv/vm-args

	for i in `find -name ebin -type d`; do \
		echo '-pa' $$i >> priv/vm-args; \
	done

