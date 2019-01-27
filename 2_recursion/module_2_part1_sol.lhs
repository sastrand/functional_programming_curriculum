New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   1: Guards and Pattern Matching 

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part1.lhs

------

In this module, we'll see some new syntax in the way of guards
for expressing conditionals and make our first foray into recursion 
with pattern matching.

------
First off, some namespace management
------

> import Prelude hiding (lookup)
> import Data.Char
> import Data.List hiding (insert, delete, lookup)
> import Control.Exception

------
Guards
------

Guards are some extra syntax that allow us to define conditional expressions
without the sometimes cumbersome trio of an if-the-else. In a guard
we define what something is based on some boolean choice. Like a long line
of if-then-else statements, the first one to evaluate to true will be the 
only one chosen. 

At the very end, instead of an else, we use the keyword `otherwise`.

For instance, we can re-write the helper function from fizzBuzz that 
determines the fizzBuzz string of a single value:

> fizzBuzzHelper' x
>   | mod x 15 == 0 = "FizzBuzz"
>   | mod x 3  == 0 = "Fizz"
>   | mod x 5  == 0 = "Buzz"
>   | otherwise     = show x

Guards provide an extra bit of syntax support in the form of the `where` 
statement. This way, when the conditional evaluation does the same work and 
the guards branch based on the result, we can write the expression to do the 
evaluation once.

For example, a function to determine the letter grade of a CS grad student's 
GPA could calculate the average of the input list of grades once and then
compare the result to determine the letter grade in the guards.

> gradPass grades
>   | gpa == 4.0 = "Pass: A"
>   | gpa >= 3.0 = "Pass: B"
>   | otherwise  = "No pass"
>   where gpa = sum grades / fromIntegral (length grades)

Note: the first guard that matches is chosen. As long as our data is 
well-behaved, we don't need an explicit upper bound on the guard `gpa >= 3.0`,
but what would happen if you entered a list of 4.1 grades?

------
Pattern matching
------

In situations where a function has a clear base case and recursive case,
defining the function in multiple instances works well. This is like a 
switch statement in C. The first case that matches the input will be the 
one that runs.

Unlike a switch statement in C, we can match *patterns* as well as values to
determine which case of the function to use.

For instance, every list is either an empty list or an element appended to 
another list. This definition is itself recursive. To see it in action,
remember `:` is the "cons" operator that appends an element to the front of a 
list. 

For example, you may hear that a good algorithm is correct, efficient, and 
simple (in that order). We could put those qualities in a list directly: 

> goodAlgs = ["correct", "efficient", "simple"]

Or we could start with an empty list and cons each quality onto the front.

> goodAlgs' = "correct" : "efficient" : "simple" : []
 
The result is the same.

Viewing lists as single elements appended to the front of an empty list 
provides the two patterns that describe all lists: the empty list `[]` and 
something appended to the front of a list `x:xs`.

These patterns become useful right away. For instance, if we want a function 
that will multiply every element of a list together:

> prod [] = 1
> prod (x:xs) = x * prod xs

We can define the two cases of the function as operating on each of these two
patterns of a list. The first, `prod []` is our base case, and by splitting up
the input in the recursive case `prod (x:xs)` we can rebuild the list by
recursing through it, recombining the elements of the list however we want.

In the case of `prod` we're taking a list and combining the contents by 
multiplying them together, but we could also combine them back into a list
using the concatenation operator for lists `++`.

For example, we could capitalize every letter in a string with the `toUpper` 
function from the `Data.Char` module:

> allCaps []     = []
> allCaps (x:xs) = toUpper x : allCaps xs

Take a moment to make sure you're comfortable with what's going on with these
recursive functions. This is the big leap in thinking. Everything after this
is learning how this tool can solve different problems.



------
Exercises
------

01. Write a function `factorial` that takes an Integral value n and calculates
    its factorial. We'll see next week there's a higher-order function that will
    allow you to do this over a range, but for now, try recursion with pattern 
    matching.

    Note that in the type signature of `factorial` our input must be a member of 
    the Numeric typeclass as well as the Ord typeclass. In order to identify the base
    case we have to do a comparison with 1--the terminating point in the process
    of calculating the factorial of n. In order to do this comparison, we can't allow
    numbers, like non-terminating reals or irrational numbers, that can't be compared.

> factorial :: (Ord p, Num p) => p -> p
> factorial 1 = 1
> factorial n = n * factorial (n-1)

> prob1Test1 = factorial 1 == 1
> prob1Test2 = factorial 6 == 720
> prob1 = do
>           putStrLn ("factorial 1 = " ++ show (factorial 1))
>           putStrLn ("factorial 6 = " ++ show (factorial 6))
>           putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" else "FAIL")



02. Use pattern matching and guards to write a function, `lookup` that takes 
    a list of tuples representing key-value pairs like those in 
    `phs` below and a String key value. If the key is the first element of any
    key-value tuple in the list, return its corresponding value. 
    Otherwise return the result of invoking the special function `error` with 
    the input of an error message. Error shuts the application down. eg:

> wishICouldDivByZero numerator denominator
>  | denominator == 0 = error "alas, you cannot divide by zero"
>  | otherwise        = numerator / denominator

    Here's some data used in the unit tests:

> phs = [("Ava","280-994-7832"), ("Bax","525-646-3563"), 
>        ("Mel","629-692-4398"), ("Kai","839-560-0099")]

> lookup :: Eq t => t -> [(t, p)] -> p
> lookup k [] = error "no key"
> lookup k (kv:kvs)
>   | k == fst kv = snd kv
>   | otherwise   = lookup k kvs

> prob2Test1 = lookup "Kai" phs == "839-560-0099"
> prob2Test2 = catch (print $ lookup "Baz" phs) handler
>   where
>     handler :: SomeException -> IO ()
>     handler ex = putStrLn "PASS\""

> prob2 = do
>           putStrLn ("lookup \"Kai\" phs = " ++ show (lookup "Kai" phs))
>           if prob2Test1
>             then prob2Test2
>             else putStrLn "FAIL"



03. Write a function `delete` to take a list of key-value tuples and
    a key and remove all instances of the tuple containing that key from the 
    list.

> delete :: Eq t => t -> [(t, b)] -> [(t, b)]
> delete k [] = []
> delete k (kv:kvs)
>   | k == fst kv = delete k kvs
>   | otherwise   = kv : delete k kvs

> prob3Test1 = delete "Baz" phs == phs
> prob3Test2 = delete "Kai" phs == init phs
> prob3 = do
>           putStrLn ("delete \"Baz\" phs = " ++ show (delete "Baz" phs))
>           putStrLn ("delete \"Kai\" phs = " ++ show (delete "Kai" phs))
>           putStrLn ("Test = " ++ if prob3Test1 && prob3Test2 then "PASS" else "FAIL")



04. Write a function `insert` to take a list of kv-pairs and a new kv-pair. 
    If the key of the new kv-pair is already in the list, overwrite the old
    kv-pair with the new kv-pair, otherwise add the new kv-pair to the head
    of the list.

    For example, insert ("Abe","713-539-4825") to phs, should produce:

> phs' = [("Abe","713-539-4825"), ("Ava","280-994-7832"), 
>         ("Bax","525-646-3563"), ("Mel","629-692-4398"),
>         ("Kai","839-560-0099")]

    And insert ("Bax","000-000-0000") to phs, should produce something like:

> phs'' = [("Bax","000-000-0000"), ("Ava","280-994-7832"),  
>         ("Mel","629-692-4398"), ("Kai","839-560-0099")]

> insert kv kvs = kv : delete (fst kv) kvs

> prob4Test1 = insert ("Abe","713-539-4825") phs == phs'
> prob4Test2 = insert ("Bax","000-000-0000") phs == phs''
> prob4 = do
>           putStrLn ("insert (\"Abe\",\"713-539-4825\") phs = " ++ show (insert ("Abe","713-539-4825") phs))
>           putStrLn ("insert (\"Bax\",\"000-000-0000\") phs = " ++ show (insert ("Bax","000-000-0000") phs))
>           putStrLn ("Test = " ++ if prob4Test1 && prob4Test2 then "PASS" else "FAIL")

05. What is the time asymptotic complexity for each of these operations? 
    
06. With a hash table, we could get their time down to O(1) for each of 
    these operations. Without a hash table, how could we improve their 
    efficiency?


------
Sources
------

For catching errors in problem 2's tests: 
https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
