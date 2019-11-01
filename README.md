# Arithmetic Interpreter

This project is an arithmetic expression interpreter. It works by sending user
input through a lexer and parser, and then evaluating the resulting AST.
This interpreter has the following features:

- Addition, subtraction, multiplication, and division
- logarithms and exponents
- Variable declaration, assignment, and usage
- Negative and positive integers
- Right-associative calculations

## Installation

Make sure you have [opam](https://opam.ocaml.org/) and [ocaml](https://ocaml.org/)
installed before continuing

```sh
git clone https://github.com/DonIsaac/Arithmetic-Interpreter.git
cd Arithmetic-Interpreter
make install # Install necessary dependencies
make # Build the interpreter using dune
make run # execute the binary
```

## Example Usage

```
> let x = 5 * (4 + 3)
= 35
> let y = x / 3
= 11
> (x * y) / (x + y)
= 8
> log base 2 of 64
= 6
> 2^4-1
= 15
> log base 10 of (2^5 * 9 - 1)
= 2
> exit 0
```

## Grammar

The language used by this interpreter is described by the following
[CFG](https://en.wikipedia.org/wiki/Context-free_grammar):

```
D -> let ID = A | A | exit N
A -> ID = A | S 
S -> M + S | M - S | M 
M -> E * M | E / M | E
E -> U ^ E | log base A of U | U
U -> +N | -N | N
N -> n | ID | (A)
where n is any integer
```

## Available Tasks

A set of [make](https://www.gnu.org/software/make/) tasks have been provided for convenience purposes. You must have
make installed in order to use them. The available commands are as follows:

- __make build__ - Default command. Compiles the interpreter. This task will fail
if the required dependencies are not installed.
- __make install__ - Installs dependencies required for calculation
- __make run__ - Executes the compiled interpreter
- __make utop__ - Launches `utop` for testing purposes
- __make test__ - Runs the tests found in `test/test.ml`

## Features TODO

- Add summations
- Add support for floating point numbers
