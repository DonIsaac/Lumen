# Arithmetic Interpreter

This project is an interpreter for a language that supports some basic mathematical
operations. It works by sending user input through a lexer and parser, and then
evaluating the resulting AST.

The language used by this interpreter supports the following features:

- Integer and boolean types
- Addition, subtraction, multiplication, and division
- Logical `and`, `or`, and `not` operations
- Equality/inequality checking
- Logarithms and exponents
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

```ebnf
  PROG -> STMT_CHAIN "EOF"
  TERM -> ";" | "\n" (* TODO Should newline act as a terminator? *)

  (* Statements and loops
  ==================================== *)
  STMT_CHAIN -> STMT_BLOCK STMT_BLOCK | STMT_BLOCK;
  STMT_BLOCK -> "do" STMT_CHAIN "end" | STMT;
  STMT ->       STMT_CTL | EXPR TERM;
  STMT_CTL ->   STMT_IF
  STMT_IF ->    "if" STMT_BLOCK "then" STMT_BLOCK ["else" STMT_BLOCK]

  (* Variable assignment and declaration
  ==================================== *)
  EXPR -> D
  D -> "let" ID "=" A | A | "exit" [N];
  A -> ID "=" A | S

  (* TODO block statements should go here *)

  (* Mathematical operations
  ==================================== *)
  EXPR_LOR -> EXPR_LAND | EXPR_LAND "or" EXPR_LOR
  EXPR_LAND -> EXPR_EQ | EXPR_EQ "and" EXPR_LAND
  EXPR_EQ -> EXPR_ADD | EXPR_ADD OP_EQ EXPR_EQ
    OP_EQ -> "==" | "===" | "!=" | "!=="
  EXPR_ADD -> M + EXPR_ADD | M - EXPR_ADD | M
  M -> E * M | E / M | E
  E -> U ^ E | "log base" A "of" U | U
  U -> "+"N | "-"N | N
  N -> n | ID | BOOL | (A)
  BOOL -> "true" | "false"
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
- __make debug__ - Run the interpreter in debug mode
- __made build-debug__ - build the interpreter to bytecode in debug mode

## Features TODO

- Add summations
- Add support for floating point numbers
- Add compound statements (statement blocks, statement chains)
- Add logical comparison operators
- Add statements and loops (I want to tackle `if` next)
