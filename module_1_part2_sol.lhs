New Beginnings Winter 2019
Haskell Lab

Module 1: Weeks 1 and 2


We can create a list of sequenced values by writing out the first part of the 
sequence, adding two dots, and then the end of the sequcene (though be
adviced, this can get strange with floating point values)

> someEvens = [2,4..42]

Because of Haskell's lazyness, these lists can be infinite.

> allEvens = [2,4..]

Try evaluating `allEvens` in ghci.

(CTRL+C will interrupt a running evaluation)

------
List comprehensions and ranges
------

**. Write a range named `evens` that is every positive even number.

> evens = [2,4..]

**. Write a list comprehension `evensMult4` that is every positive even number
    that is a multiple of 4.

> evensMult4 = [x | x <- evens, mod x 4 == 0]

**. Import your instance of the `module_1_part2.lhs` file into this file, to 
use your FizzBuzz function, and pass it the range that is every number 
divisible by 3 from 1 to 100. Print the result.

