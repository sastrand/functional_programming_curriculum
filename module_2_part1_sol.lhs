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

**. Write a function, `firsts`, that takes a list of tuples and returns the 
    first element from each one in a list. You can use pattern matching or a
    comprehension for this.

    Here's some test data:

> phs = [("Ava","280-994-7832"), ("Bax","525-646-3563"), 
>        ("Mel","629-692-4398"), ("Kai","839-560-0099")]

> firsts [] = []
> firsts (x:xs) = fst x : firsts xs

**. Noticing the key-value pairs in the last question, maybe you get the
    sense that we're making a dictionary. We are :-)

    Use pattern matching and guards to write a function that takes a key as a 
    string and a list of tuples representing key-value pairs like those in 
    `phs` and a key and returns the first key-value pair with a matching key. 
    If the key isn't in the dictionary, return a tuple of empty strings.

> lookup k [] = ("","")
> lookup k (kv:kvs)
>   | k == fst kv = kv
>   | otherwise   = lookup k kvs

**. Write a function `insert` to take a list of String key-value tuples and 
    a new key-value tuple and add the new tuple to the front of the list.

    For example, insert ("Abe","713-539-4825") to phs, should produce:

> phs' = [("Abe","713-539-4825"), ("Ava","280-994-7832"), 
>         ("Bax","525-646-3563"), ("Mel","629-692-4398"),
>         ("Kai","839-560-0099")]

> insert kv kvs = kv : kvs

**. Write a function `delete` to take a list of String key-value tuples and
    a key and remove all instances of the tuple containing that key from the 
    list.

> delete k [] = []
> delete k (kv:kvs)
>   | k == fst kv = delete k kvs
>   | otherwise   = kv : delete k kvs

