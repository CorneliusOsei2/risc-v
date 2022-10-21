.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

processor:
	OCAMLRUNPARAM=b dune exec bin/processor.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh


