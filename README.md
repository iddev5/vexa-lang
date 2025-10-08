# vexa-lang

This is an attempt to create a statically-typed, compiled programming language which compiles to Web Assembly bytecode. The project is implemented in Zig `0.3.0`. The compiler generates valid `.wasm` objects in most of the cases and can be verified using wasm tools.

## History

The project originally started as a Lua implementation which compiles to WASM. However, as Lua is a dynamically typed language and its table-oriented object system needs significant runtime support, the idea was shortly scraped. Thus, I modified the code to be its own language, somewhat inspired by Python and Odin.

## Stages of Compiler
- Tokenizer: It uses a standard state-machine based tokenizer
- Parser: Arranges tokens into an AST
- analysis: Converts AST into IR and performs type checking
- WasmGen: Responsible for actually generating the WASM bytecode

## Types in language
Currently the following types are implemented in the language:
- void: void
- bool: i32
- float: f64
- func: i32, stored in an IR node

## Features
- [x] Numeric and boolean literals
- [x] Binary expressions
- [x] Unary expressions
- [x] If-elseif-else statements
- [x] While loops
- [x] Diagnostics and error messages
- [x] Function
- [x] Function parameters
- [x] Function return values
- [x] Multi-return values (using WASM extensions)
- [ ] Multi-return values using globals 
- [ ] Sequence types (arrays, lists)
- [ ] For-loops for sequence types
- [ ] Hash types (dictionary, set)
- [ ] User defined types (structs) - Work in progress
- [ ] Enum and union types

## References

- https://github.com/ziglang/zig

- https://github.com/hexops/mach/tree/main/src/sysgpu/shader

- https://webassembly.org and https://webasembly.github.io/spec/core/binary/index.html

- https://github.com/WebAssembly/wabt

- https://developer.mozilla.org/en-US/docs/WebAssembly/Reference

- https://craftinginterpreters.com
