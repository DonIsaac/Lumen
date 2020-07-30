OBJECTS = src/*.ml* bin/*.ml*
TARGET = lumen

build: $(OBJECTS)
	dune build bin/lumen.exe

.PHONY: utop run install test debug build-debug coverage

install:
	opam install dune ANSITerminal merlin utop ounit2 bisect_ppx

# Launches the Lumen interpreter
run:
	./_build/default/bin/lumen.exe

test:
	BISECT_ENABLE=yes dune runtest -f


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
