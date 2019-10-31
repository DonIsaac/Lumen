build: src/*.ml bin/calculator.ml
	dune build bin/calculator.exe

.PHONY: utop run

utop:
	dune utop src

run:
	./_build/default/bin/calculator.exe
