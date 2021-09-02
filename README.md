# <img src="assets/kelp-title.svg" width="100%">

> bevare kelp is under heavy development. all is subject to change

- [<img src="assets/kelp-title.svg">](#)
- [<img src="assets/kelp-leaf.svg"> What is Kelp?](#-what-is-kelp)
  - [How does it look like?](#how-does-it-look-like)
  - [Why should I use Kelp?](#why-should-i-use-kelp)
  - [What is this Agar that I heard of?](#what-is-this-agar-that-i-heard-of)
- [<img src="assets/kelp-leaf.svg"> Getting started](#-getting-started)
- [<img src="assets/kelp-leaf.svg"> Installing from source](#-installing-from-source)
  - [Requirements](#requirements)
  - [Process](#process)
- [<img src="assets/kelp-leaf.svg"> Contributing](#-contributing)

# <img src="assets/kelp-leaf.svg"> What is Kelp?
Kelp is a programming language.
## How does it look like?
```kelp
"Hello world!" -> println
```

For more checkout the [examples directory](examples)

## Why should I use Kelp?
> Right now, you shouldn't, it's under heavy development.

Because, it's different from other languages, and may lend some benefits. It was inspired by purely functional languages like Haskell, but also with Rust.

Advantages of Kelp:
- allows for higher order functions (functions that return functions)
- no explicit memory management (~~yet no GC~~ - maybe)
- purely functional (monads yay!)
- custom operators (Kelp is built arround operators)
- simple grammar (the parser has just 500 LOCs)
- compiled and interpreted (thanks to Agar)
- lazy ~~automatic parallelization~~ (planned)

## What is this Agar that I heard of?
Agar is the intermediate language used by Kelp in it's build chain. It's somewhat similiar to LLVM IR or Webassembly, yet more geared towards Kelp. And also higher level, as Kelp uses LLVM in it's compiler toolchain.

# <img src="assets/kelp-leaf.svg"> Getting started
Open the [documentation](https://kelp-lang.github.io/docs/).

# <img src="assets/kelp-leaf.svg"> Installing from source
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
This produces a few executables namely: `kelpc` - the kelp compiler, `kelpi` - the kelp interpreter

# <img src="assets/kelp-leaf.svg"> Contributing
Contributions are welcome! If you don't know what to work on, check out open issues!
