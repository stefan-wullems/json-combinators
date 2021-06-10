.PHONY: build
build:
	idris2 --build jsoncombinators.ipkg

clean:
	idris2 --clean jsoncombinators.ipkg
	rm -r build

repl:
	rlwrap idris2 --repl jsoncombinators.ipkg

testbin:
	@${MAKE} -C tests testbin

# usage: `make test only=messages001`
test-only:
	${MAKE} -C tests only=$(only)

test: build testbin test-only
