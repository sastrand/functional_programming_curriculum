New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   1: Higher Order Functions 

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part1.lhs

------
Currying
------

We've seen in Haskell's type signatures that a function that takes two
arguments and returns a single value doesn't obviously distinguish between
parameters and return values.

For example the type of `div`, the integer division function is:

  div :: Integral a => a -> a -> a

So far we've read this that `div` takes two `a`s and returns an `a`.

But `div` with only one argument given will still work. It is now a function
that takes one integer and returns an integer. 

It's a prefix function, but by wrapping it in backticks (note ` not ') we can
use it in an infix placement. So the result of dividing 9 by 3 with `div` can
be written both:

  div 9 3
  9 `div` 3 

So let's partially apply this. If we want to find the maximum number of 
quarters that could be used to render the following dollar amounts (in cents), 
we could give `div` its second argument, 25, and map this partial application
of the function onto our input.

> someChange = [32,64,89,21]

  map (`div` 25) someChange


------
Lambdas
------

Like comprehensions, lambdas began as an artifact of functional languages but
have gained very widespread use outside the paradigm because of their
flexibility and concision.

A lambda is a function that is not named.

By comparison, the result of 1/0.099 is a value that is not named.
Anywhere we want to use that value, we can substitute the expression (1/0.099). 
If you need to type it multiple times, it's probably better to name it, but
if you only need to use this value once, giving it a name only makes your code
a little longer and a little harder to understand.

The same is true of functions, if we want a function that will test if a string
is a palindrome, we could write it:

> isPalindrome x = x == reverse x

Then we could write a function that takes a list of strings and returns those
that match our test with the `filter` function.

> securityDeposit = ["tenant", "spackle", "sconce", "repaper"]
> palindromes x = filter isPalindrome x

But if we only needed to do this once, we might as well not bother naming
`isPalindrome`. We can write it as a lambda with the following syntax:

  (\x -> x == reverse x)

This makes our palindromes expression:

> palindromes' x = filter (\x -> x == reverse x) x

Did we save a lot of keystrokes, not really, but we can understand
`palindromes'` without needing to look up the one-line function `isPalindrome`.

Another handy thing about lambdas is that they maintain the scope of the
function in which they're called. This means that if you use a lambda in the
definition of a larger function and there are some variables in that larger
function, you can use those variables in your lambda.

For example, a function that will add a given value to every element in a list:

> someVals = [1,2,3]
> addThis n xs = map (\x -> x + n) xs

  addThis 3 someVals => [4,5,6]

Another term for a partially applied function is a curried function, named
after the logician Haskell Curry.

------
Function Composition
------

Functions in Haskell can be composed just like functions in math. We've done
this extensively already just by nesting function calls. For example, the last
element in a list `xs` is equal to `head (reverse xs)`.

> last xs = head (reverse xs)

There is some special syntax for function composition that echos the small,
in-line circle used in mathematical notation to represent function composition.
This expresses the idea of function composition somewhat differently, instead
of applying one function and then applying another function to the result,
we're creating a new function that is the composition of the two of them.

> last' xs = (head . reverse) xs

`last` and `last'` will have the same result and (likely) the same evaluation,
but this additional syntax suggests a slightly different way of thinking about
function composition that may feel more natural when used with higher order and
partially applied functions.

For example, we can map `sqrt` and `abs` (the absolute value function) together 
over a list to find the square roots of the elements' absolute values:

> realSquares = map (sqrt . abs) [1.1,2.2,3.3]


------
Exercises
------

01. Write a lambda that when given a string, will check to see if it is the
string "sasquatch", returning True or False depending. Drop your lambda
directly into the test by replacing `undefined`.

> prob1Test = (\x -> x == "sasquatch") "sasquatch" == True
> prob1 = putStrLn ("Test => " ++ if prob1Test then "PASS" else "FAIL")


02. Use your lambda from the previous exercise to write a function, 
    `squatchCount`, that will take a list and count the number of instances of 
    "sasquatch" present.

> squatchCount :: [[Char]] -> Int
> squatchCount x = length $ filter (\x -> x == "sasquatch") x

> nwCampSite = ["tree", "rain", "camp fire", "sasquatch"]
> neCampSite = ["tree"]
> prob2Test1 = squatchCount nwCampSite == 1
> prob2Test2 = squatchCount neCampSite == 0
> prob2 = do
>         putStrLn ("Test 1 => " ++ show (squatchCount nwCampSite))
>         putStrLn ("Test 2 => " ++ show (squatchCount neCampSite))
>         putStrLn ("Test = " ++ if prob2Test1 && prob2Test2 then "PASS" else "FAIL")


03. Using a lambda, write a function, `remove`, that will take a value and 
    remove every element of a list that matches that value.

> remove x xs = filter (\y -> x/=y) xs

> littleData = [256, 100, 32]
> prob3Test1 = remove 100 littleData    == [256,32]
> prob3Test2 = remove "rain" nwCampSite == ["tree", "camp fire", "sasquatch"]
> prob3 = do
>         putStrLn ("Test 1 => " ++ show (remove 100 littleData))
>         putStrLn ("Test 2 => " ++ show (remove "rain" nwCampSite))
>         putStrLn ("Test = " ++ if prob3Test1 && prob3Test2 then "PASS" else "FAIL")

04. Replace `undefined` in the function below with a partially applied 
    version of your `remove` implementation composed with the `length` function
    to finish the function `drySeason` that takes a list of lists of strings
    and returns the quantity that are not equal to "rain".

> drySeason xss = sum $ map (length . remove "rain") xss

> nwCampStrs = [["rain","camp fire"],["headwaters","ghost stories", 
>   "omgWasThatABear"],["oregon","washington","rain","british columbia"]]
> prob4Test = drySeason nwCampStrs == 7
> prob4 = do
>         putStrLn $ "Test1 => " ++ show (drySeason nwCampStrs)
>         putStrLn $ "Test  = " ++ if prob4Test then "PASS" else "FAIL"

