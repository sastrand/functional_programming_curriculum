New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   1: Guards and Pattern Matching 

------
Name:
------

In this module, we'll see some new syntax in the way of guards
for expressing conditionals and make our first foray into recursion 
with pattern matching.

------
First off, some namespace management
------

> import Prelude hiding (lookup)
> import Data.Char

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

01. Use pattern matching to write a function, `firsts`, that takes a list of 
    tuples and returns the first element from each one in a list.

    Here's some data used in the unit tests:

> phs = [("Ava","280-994-7832"), ("Bax","525-646-3563"), 
>        ("Mel","629-692-4398"), ("Kai","839-560-0099")]

> firsts :: [(a, b)] -> [a]
> firsts [] = []
> firsts (x:xs) = fst x : firsts xs

> prob1Test = firsts phs == ["Ava","Bax","Mel","Kai"]
> prob1 = do
>           putStrLn ("unFizz 20 = " ++ show (firsts phs))
>           putStrLn ("Test = " ++ if prob1Test then "PASS" else "FAIL")


02. Noticing the key-value pairs in the last question, maybe you get the
    sense that we're making a dictionary. Indeed, we are.

    We'll see later how we can generalize this data structure to hold 
    values of different types. For now, all our keys and values will be Strings.

    Use pattern matching and guards to write a function that takes 
    a list of tuples representing key-value pairs like those in 
    `phs` and if the key is in the dictionary, returns its value and 
    otherwise returns an empty string.

> lookup k [] = ""
> lookup k (kv:kvs)
>   | k == fst kv = snd kv
>   | otherwise   = lookup k kvs

> prob2Test1 = lookup "Kai" phs == "839-560-0099"
> prob2Test2 = lookup "Baz" phs == ""
> prob2 = do
>           putStrLn ("lookup \"Kai\" phs = " ++ show (lookup "Kai" phs))
>           putStrLn ("lookup \"Baz\" phs = " ++ show (lookup "Baz" phs))
>           putStrLn ("Test = " ++ if prob2Test1 && prob2Test2 then "PASS" else "FAIL")


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

**. 

