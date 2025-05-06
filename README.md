# My Lang

This repository contains the source code for the My Lang compiler. The current version can only output assembly code for the [Netwide Assembler (NASM)](https://www.nasm.us/).

## Features

The following types are supported for creating variables:

- `int`: 32-bit integer number (dword size);
- `real`: 32-bit floating precision number (dword size);
- `bool`: boolean value (word size) (not yet supported);
- `char`: utf-8 code point (word size) (not yet supported)

The following statements are supported:

- `if`: conditional block (else block is not yet supported);
- `while`: conditional block loop (do loops are not yet supported);
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

The first step, is to compile your code into assembly using the following command:

```shell
target/debug/my-lang examples/helloworld.txt
```

From this point on, you will need nasm to assemble your code into machine code. The Netwide Assembler (NASM) can be found [here](https://www.nasm.us/).

### Linux
If you is running linux, you can assemble the code directly, using the following command:

```shell
nasm out.asm
```

### Windows
On windows you will not be able to assemble the code directly, instead you can generate an object and link it using [gcc](https://gcc.gnu.org/). Using the commands below will generate an exe file on windows:

```shell
nasm -fwin64 out.asm
```

```shell
gcc out.obj -o out.exe
```