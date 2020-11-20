# Hello world
This will be your first program in kelp, so let's start as every language does. With `hello world`!

Everything in kelp is a value, even functions, so we want to create a variable with a value of our function.
```kelp
let main = []: [] (
  // some code
)
```
`let` is used to denote that we are defining a variable. We call it `main` as this is a variable that is called when the program starts. We follow it by the assignment operator `=`. After that we add the group of arguments `[]` followed by a return type `: []`.

>As you may see, it's kind of weird that we write `[]: []`, this actually means that we don't take any arguments and we don't return anything (or properly: we return `void`).

After our function declaration, we add function body denoted by `()`. Any code will be executed, it doesn't matter if the code is inside of a function or not. But function bodies allow us to structure code and create so-called scopes.

To print anything we need one function `print`, it takes any number of arguments and returns `void`. We only feed it one argument, which is our string `"Hello world!"`. To do that we use the function application operator `>`.
```kelp
let main = []: [] (
    "Hello world!" > print
)
```

You can now compile the code and run it!
```bash
kelpc hello_world.klp
./hello_world
```