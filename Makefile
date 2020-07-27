OBJECTS = src/*.ml* bin/*.ml*
TARGET = calculator

build: $(OBJECTS)
	dune build bin/calculator.exe

.PHONY: utop run install test debug build-debug coverage

install:
	opam install dune ANSITerminal merlin utop ounit2 bisect_ppx

# Launches the calculator program
run:
	./_build/default/bin/calculator.exe

test:
	dune runtest -f


# ------------------ DEBUGGING COMMANDS ------------------

# Opens a utop shell
utop:
	dune utop src

debug: build-debug
	ocamldebug _build/default/bin/$(TARGET).bc

build-debug: $(OBJECTS)
	dune build bin/$(TARGET).bc

coverage: test
	bisect-ppx-report html
