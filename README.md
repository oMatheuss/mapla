# My Lang

This repository contains the source code for the My Lang compiler.

## Overview

My Lang is a toy language, designed for simplicity and low-level operations. Its syntax is highly inspired by [lua](https://www.lua.org/) for its simplicity, but instead of being interpreted, it aims to be a low-level, system language.

## Features

The following types are supported for creating variables:

- `int`: 32-bit integer number (dword size);
- `real`: 32-bit floating precision number (dword size);
- `bool`: boolean value (word size);
- `char`: utf-8 code point (word size);

Intial support for arrays and pointers are also implemented with the following syntax:

- `int[10]`: a type for creating a 32-bit * 10 sized array (the result is a int*);
- `int*`: a type that represents a pointer to an integer;
- `&x`: creates a pointer to the variable x;
- `x[10]`: access the value at the given position;

The following statements are supported:

- `if`: conditional block (else block is not yet supported);
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
cargo build --bin my-lang
```

## How to use

The my-lang program can be compiled in a three step way:

1. Translating the source code into assembly instructions;
2. Generating an object file (bytecode);
3. Linking with the GCC Runtime (executable);

For this steps you will need the following tools:

- [The Netwide Assembler (NASM)](https://www.nasm.us/)
- [GNU Compiler Collection (GCC)](https://gcc.gnu.org/)

After downloading and installing, you can compile any program with the following commands:

```shell
target/debug/my-lang.exe -t windows examples/helloworld.txt
nasm -fwin64 examples/helloworld.asm
cc examples/helloworld.obj -o examples/helloworld.exe
```

After compiling the code into an executable, you can run the result program:

```shell
examples/helloworld.exe
```

## Examples

For more examples on how the syntax works, you should look at [`examples`](/examples/) folder.