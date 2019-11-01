OBJECTS = src/*.ml* bin/*.ml*

build: $(OBJECTS)
	dune build bin/calculator.exe

.PHONY: utop run install test

install:
	opam install dune ANSITerminal merlin utop ounit2

# Opens a utop shell
utop:
	dune utop src

# Launches the calculator program
run:
	./_build/default/bin/calculator.exe

test:
	dune runtest -f
