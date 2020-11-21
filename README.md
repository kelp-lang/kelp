# <img src="kelp.svg" width=120>elp programming language

> bevare kelp is under heavy development. all is subject to change

# What is Kelp?
Kelp is a programming language.
## How does it look like?
```kelp
let main = []: [] (
    "Hello world!" > println
)
```
## Why should I use Kelp?
> Right now, you shouldn't, it's under heavy development.

Because, it's different from other languages, and may lend some benefits. It was inspired by purely functional languages like Haskell, but also with Rust.

Advantages of Kelp:
- allows for higher order functions (functions that return functions)
- no explicit memory management (yet no GC)
- automatic parallelization
- custom operators (Kelp is built arround operators)
- simple grammar (right now, grammar has only 29 lines, at it won't grow that bigger)
- compiled and interpreted (thanks to Agar)

## What is this Agar that I heard of?
Agar is the intermediate language used by Kelp in it's build chain. It's somewhat similiar to LLVM IR or Webassembly, yet more geared towards Kelp.

# Getting started
Open the [documentation](https://kelp-lang.github.io/docs/).

# Installing from source
> This isn't currently recommended, as kelp is highly unstable
## Requirements
- cargo
- rust stable
## Process
```bash
git clone https://github.com/kelp-lang/kelp
cd kelp

cargo build
```

# Contributing
Contributions are welcome! If you don't know what to work on, check out open issues!