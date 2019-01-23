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
handy for taking notes, and is the standard format for Haskell files 
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

If you haven't already, load this file into the interperter. 
You should see the prompt change from `Prelude>` to `*Main>`.

You can see the list of all the modules loaded (aside from those, like the
Prelude, which are loaded automatically) with:

*Main> :show modules

You can reload a given module with: 

*Main> :reload [given module name]
or
*Main> :re [given module name]

You can browse all the functions and constants in scope in a given 
module with:

*Main> :browse [given module name]
or
*Main> :bro [given module name]

You can get the type of an object with:

*Main> :type [name of object]
or
*Main> :t [name of object]

And you can quite ghci with:

*Main> :quit
or
*Main> :q

More information about ghci commands is available here:
 https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/ghci-commands.html

------

Before starting the lab exercises, let's try out some type conversion.

Haskell has two types of division, `div` which performs integer division
on Integral types and the `/` function that evaluates the ratio to return 
a Fractional types. In the interpreter we can see their types and test 
them out:

*Main> :t div
div :: Integral a => a -> a -> a

You can read this type signature "for all `a` where `a` is an Integral type,
the function `div` will take a value of type `a`, then take another value of 
type `a`, and then return a value of type `a`"

*Main> div 3 2 
*Main> 1

Try the same for the `/` function.

So say we've got this function:

> fizz x y = div (x + 1) (y - 3)

We can see the type of `fizz` is:

*Main> :t fizz
fizz :: Integral a => a -> a -> a

Say that we want to pass the two values (sqrt 4) and (sqrt 16) to `fizz`.

For Haskell the type of the `sqrt` function is:

*Main> :t sqrt
sqrt :: Floating a => a -> a

That is, even if result has no fractional component, `sqrt` will still give us
a result with a type in the Floating typeclass. So when we try:

*Main> fizz (sqrt 4) (sqrt 16)

We get a type error.

We need to convert the results of `sqrt 4` and `sqrt 16` from the
Floating typeclass to a type in the Integral typeclass or above (in the 
typeclass tree)

That is, we want to either round or truncate the result. In this case,
either will work. Try:

*Main> fizz (round (sqrt 4)) (truncate (sqrt 16))

Success!

Finally, sometimes we want to go the other way. For instance, if you try
to add 1.5 to the value we just found:

*Main> fizz (round (sqrt 4)) (truncate (sqrt 16)) + 1.5

Let's check out the type of `+` to see why:

*Main> :t (+)
(+) :: Num a => a -> a -> a

It appears to take any numeric type, but both inputs must have the same type.

We know the result of `fizz` will be an int or an integer, so 
we can change its type to a Floating type without losing any precision.

To do that, the `fromIntegral` function is the way to go.

*Main> fromIntegral (fizz (round (sqrt 4)) (truncate (sqrt 16))) + 1.5

Again, sweet.

------

At this point, you should be well set to take on lab number 1.

