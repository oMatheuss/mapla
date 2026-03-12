# Mapla Lang

This repository contains the source code for the Mapla compiler.

## Overview

Mapla is a programming language designed with simplicity in mind. Its syntax is inspired by c, [lua](https://www.lua.org/) and a touch of other languages, it aims to be a low-level, system language.

## Features

There are 4 primitive types:

- `int`: 32-bit integer number (dword size);
- `real`: 32-bit floating precision number (dword size);
- `bool`: boolean value (word size);
- `char`: utf-8 code point (word size);

Support for arrays and pointers are also implemented with the following syntax:

- `int[10]`: a type for creating a 32-bit * 10 sized array;
- `int*`: a type that represents a pointer to an integer;
- `&x`: creates a pointer to the variable x;
- `x[10]`: access the value at the given position;

The following control-flow statements are supported:

- `if-else`: conditional blocks;
- `while`: conditional block loop;
- `for`: conditional block loop with integer variable;

Functions can be created using the `func` keyword, they also can take `N` arguments between parentheses separeted by comma. Below is an example for the fibonacci recursive algorithm:

```
func fib(num: int): int do
    if num < 2 then
        return num;
    end
    return fib(num - 1) + fib(num - 2);
end
```

## Building

To build the compiler, first you need to have [Rust](https://www.rust-lang.org) installed, then you can use the following command:

```shell
cargo build --bin mapla
```

## How to use

The mapla program can be compiled in a three step way:

1. Translating the source code into assembly instructions;
2. Generating an object file;
3. Linking into an executable;

For this steps you will need the following tools:

- [The Netwide Assembler (NASM)](https://www.nasm.us/) for assembling
- [GNU Compiler Collection (GCC)](https://gcc.gnu.org/) for linking

After downloading and installing (make sure that you have `nasm` and `cc` binaries in your environment), you can compile any program with the following commands:

```shell
target/debug/mapla examples/helloworld.txt
```

If you want to immediately run the program after compilation, you can use the flag `--run`. To emit object/assembly files instead, you can use `--obj`/`--asm` flags respectively.

## Examples

For more examples on how the syntax works, you should look at [`examples`](/examples/) folder.
