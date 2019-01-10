Getting a workflow going on ghci
A code demo/tutorial doc

------

Assuming your download of ghci went okay, can you open ghci and 
see the Prelude> prompt?

------

If so, let's start off by defining some functions:

Prelude> mult3 n = n * 3

We can invoke this function interactively:

Prelude> mult3 3
Prelude> 9

And define a function with an if-then-else, written all on one line for now:

Prelude> profitOrNo income expense = if income > expense then "profit" else "no profit"

Check out the error of applying too few or too many arguments.

------

Interactively in the interpreter, you'll have to set the multi-line mode 
and use some special syntax to define anything across multiple lines:

Prelude> :set +m
Prelude> let fib 0 = 0
Prelude|     fib 1 = 1
Prelude|     fib n = fib(n-1) + fib(n-2)
Prelude|
Prelude>

For this reason, you may find the best work-flow for you is to keep an
editor open with a Haskell file, like this one, and load it into
an instance of ghci, either by opening ghci with the filename as an argument
or loading it into a running session with the following command

Prelude> :l fileName.lhs

Then when you make changes in the file, you can reload the file with the same 
command or reload the module it represents with the `reload` command 
described below.

For now, let's start out with this document as our first .lhs file. In here, 
every line that isn't prefixed by a `>` is a comment. This ends up being 
handing for taking notes, and is the standard format for Haskell files 
in CS 557.

If you want to use a regular Haskell file, where every line is code unless 
marked as comments, you can use the .hs file extension, but we'll use .lhs
files for everything in this lab.

Here you can add multi-line definitions directly:

> fib 0 = 0
> fib 1 = 1
> fib n = fib(n-1) + fib(n-2)

------

Every module in the runtime environment has a name. At first, the Prelude is
the only one, but when you load your file, it's given the name "Main". 

You can see the list of all the modules loaded (aside from those, like the
Prelude, which are loaded automatically) with:

Prelude> :show modules

You can reload a given module with: 

Prelude> :reload [given module name]
or
Prelude> :re [given module name]

And browse all the functions and constants in scope in a given 
module with:

Prelude> :browse [given module name]
or
Prelude> :bro [given module name]

And to quite ghci you can type

Prelude> :quit
or
Prelude> :q

You can find more information about ghci commands here: https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/ghci-commands.html

At this point, you should be well set to take on lab number 1.

