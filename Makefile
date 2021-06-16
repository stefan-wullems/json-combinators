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

install:
	idris2 --install jsoncombinators.ipkg

test-only:
	${MAKE} -C tests only=$(only)

test: build install testbin test-only
