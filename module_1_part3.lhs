New Beginnings Winter 2019
Haskell Lab

Module 1: Weeks 1 and 2

------
List comprehensions and ranges
------

**. Write a range named `evens` that is every positive even number.

> evens = [2,4..]

**. Write a list comprehension `evensMult4` that is every positive even number
    that is a multiple of 4.

> evensMult4 = [x | x <- evens, mod x 4 == 0]

**. Pass your FizzBuzz function the range that is every number divisible by 3
    from 1 to 100 and print the result.
