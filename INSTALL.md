# Install instructions for risc-v-processor-generator

opam switch create cs3110-2022fa ocaml-base-compiler.4.14.0
eval $(opam env)
opam switch list
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc

2.Install ANSITerminal
opam update
opam upgrade
opam install AnSITerminal

3.Start the RISC-V-processor-generator in root directory
make processor
