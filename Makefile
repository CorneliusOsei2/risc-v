.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

install:
	opam switch create cs3110-2022fa ocaml-base-compiler.4.14.0
	eval $(opam env)
	opam switch list
	opam install -y utop ounit2 ocamlformat ANSITerminal

utop:
	OCAMLRUNPARAM=b dune utop src

processor:
	dune clean
	OCAMLRUNPARAM=b dune exec bin/system.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
	rm -f risc_v_processor_generator.zip

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f risc_v_processor_generator.zip
	zip -r risc_v_processor_generator.zip . -x@exclude.lst




