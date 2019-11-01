# Arithmetic Interpreter

This project is an arithmetic expression interpreter. It works by sending user
input through a lexer and parser, and then evaluating the resulting AST.
This interpreter has the following features:

- Addition, subtraction, multiplication, and division
- Variable declaration, assignment, and usage
- Negative and positive integers
- Right-associative calculations

## Installation

```sh
git clone https://github.com/DonIsaac/Arithmetic-Interpreter.git
cd Arithmetic-Interpreter
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
> exit 0
```

## Grammar

The language used by this interpreter is described by the following CFG:

```
D -> let ID = A | A | exit N
A -> ID = A | S
S -> M + S | M - S | M
M -> U * M | U / M | U
U -> +N | -N | N
N -> n | ID | (A)
where n is any integer
```
