New Beginnings Winter 2019
Haskell Lab

Module 1 Part 2
List comprehensions and ranges

------
List comprehensions
------

We saw list comprehensions in Python, and these have the same expressive power.
There we saw them as an alternative to loops, written sometimes more concisely 
and without the need for a sentry variable to count iterations. 

We can also think about them as sets. Their name comes from this idea
and the Axiom of Comprehensions, which can be summarized:

    Given a set A and a predicate P, there is a subset of A whose members are 
    those elements in A that satisfy P.

This view that all sets can be built by sub-setting a larger set with respect to some predicate allowed for a new notion of set theory that avoids Russel's Paradox.
(The problem that you can define a set as containing every element that is not 
contained in that set.) In this alternative view, every set is not the result of just describing what things are in it, but the result of applying some predicate to the set of all things and then only keeping the values that return true. [1]

We can build sets this way using the filter function we saw in the last lab.
But, in programming and in set notation, we may want to build a set not just by
sub-setting some larger one but also by applying some operation to each element 
in the result, and this is what a comprehension provides.

For example, if we want a set that is the square of every even, natural number 
less than 10, we could describe the set:

    tenSquares = { x^x | x ∈ [1,2,3,...,10] ∧ x is even}

We can do exactly the same with a comprehension:

> tenSquares = [ x^x | x <- [1..10], even x ]

This works over multiple variables. If we want the cross product of two
lists, we can write a list comprehension to do it:

> crossProd xs ys = [(x,y) | x <- xs, y <- ys]

Here we've created every tuple that has an element of x in its first position
and an element of y in its second position. If we further want all values of x 
to be even and all values of y to be odd, we can write:

> evenOddCrossProd xs ys = [ (x, y) | x <- xs, y <- ys, even x, odd y ]

In Haskell the sets we draw from are called *generators* and the predicates
applied to them are called *guards*. So the general format is:

[ <function application> | <generator(s)>, <guard(s)> ]

Say we're given two sets of points representing the placement of rotors in a
device similar to an Enigma Machine. Every rotor in the first set must be 
directly connected to every rotor in the second set. How much wire would be 
needed to create these connections?

Using the Cartesian distance formula to find the shortest distance between two points (x1,y1), (x2,y2):

> cartDist ((x1,y1), (x2,y2)) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

we can model this solution using a list comprehension and the `sum` function
over lists.

> topR = [(1.0,0.0), (1.0,1.5), (1.0,3.0), (1.0,3.5)]
> btmR = [(0.0,0.5), (0.0,2.0), (0.0,2.5), (0.0,4.0)]

> wire = sum [ cartDist ((x1,y1), (x2,y2)) | (x1,y1) <- topR, (x2,y2) <- btmR ]


------
Ranges
------

We can create a list of sequenced values by writing out the first part of the 
sequence, adding two dots, and then the end of the sequence (though be
warned, this does not always do what you expect with floating point values)

> someEvens = [2,4..42]

Because of Haskell's laziness, these lists can be infinite.

> allEvens = [2,4..]

Try evaluating `allEvens` in ghci.

(CTRL+C will interrupt a running evaluation)

Because ranges are a handy way to represent infinite lists, we'll
also talk about a few ways to manage infinite lists. 

If we want just the first so many elements from a list, we can use `take`.

> fiveEvens = take 5 [2,4..] 

We can create a new infinite list by repeating a value

> infiniteTwos = repeat 2

Or use `replicate` to repeat a value a set number of times:

> answers = replicate 4 42

------
Exercises
------

**. Using a list comprehension, write a function `areaCodes` that takes a list of
    phone numbers in the format "123-456-7890" and returns a list of the area codes
    as strings.

    Here is some test data: 

> phs = ["371-836-3310", "805-834-9912", "851-246-9844", "210-599-4050"]

> areaCodes = [take 3 x | x <- phs]

** Using a list comprehension and a range, write a function `unFizz` that
   will return all the multiples of 5 that are not also multiples of 3 up 
   to a given point.

   So `unFizz 20` should return: [5,10,20]

> unFizz n = [ x | x <- [1..n], x `mod` 5 == 0, x `mod` 3 /= 0 ]

**. Using a list comprehension, write a function `radar`, that will take a set of
    points and a reference point and return all the points that are within 4 units 
    of distance from that point.

    Here is some test data you can use:

> ourBoat    = (2.0,2.0)
> otherBoats = [(-2.0,1.5),(-1,4.8),(1.2,3.7),(4.1,3.4),(5.9,5.0),(6.1,1.7),
>               (7.3,8.5),(3.0,-1.5),(-1.1,-2.6)]

> radar pnts ref = [ (x1,y1) | (x1,y1) <- pnts, cartDist ((x1,y1), ref) < 4]

------
Sources
------

[1] https://www.quora.com/How-was-Russells-paradox-resolved