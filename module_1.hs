-- New Beginnings Winter 2019
-- Haskell Lab

-- Module 1: Weeks 1 and 2

-- ------
-- Part I: Functions over numeric types
-- ------

-- 01. Write a function that multiplies any number it receives by 3.

-- 02. Write a function that takes a number and returns the square of that number. 

-- 03. Write a function that takes two arguments, squares them both, and returns 
--     the sum of the two squares.

-- **. Write a function that converts an ____ to an integer type.

-- **. Write a function that determines if a given number is a perfect square.

-- **. Write a function that returns the additive inverse of a given ratio (...).

-- **. Write a function that takes three arguments `a`, `b`, and `c` representing 
--     the three coefficients in a second degree algebraic equation, 
--     `a*(x^2) + b*x + c = 0`, and returns the possible values of x in a tuple.

-- ------
-- Functions over lists and tuples
-- ------

-- **. Write a function that takes two strings and determines if they are 
--     anagrams of one another.

-- **. Write a function that takes a string and determines if it's a palindrome.

-- **. Write a function that takes a list of numbers and returns their sum.

-- **. Write a function that takes a list of integers and checks if it is the 
--     Fibonacci sequence. 

--     hint: the following function will generate the Fibonacci sequence up to a
--     given value n.

-- **. Write a function that takes a string and makes it a palindrome if it's not
--     already, so 'hannah' would stay 'hannah' but 'cab' would become 'cabbac'.

-- **. Write a function that takes two lists of numbers and adds their respective 
--     values to one another until the shorter list is out of numbers, 
--     so `foo [1, 2, 3] [4, 5, 6, 7]` would return `[5, 7, 9]`.

-- **. Write a function that takes a list of numbers and returns a new list 
--     containing only the perfect squares in the original list.
--     (Where a "perfect square" is the square of a whole number).

-- **. Write a function that takes a list of tuples of integers and returns the 
--     largest value from each tuple in a new list. 

-- **. Write a function that takes a list of tuples of integers and returns 
--     a list representing a set of largest values from each tuple respectively.
--     That is, the function in the above question but with duplicates removed
--     from its result.

-- **. Write a function that takes a list of tuples of integers and returns the
--     largest value present in all the tuples in the original list.

--------
-- List comprehensions and ranges
--------

-- **. Write a range that is every positive even number.

-- **. Write a list comprehension that is every positive even number.

-- **. Implement FizzBuzz as a list comprehension. That is, write a function that
--     takes a list of numbers and returns a new list in which each value of the 
--     original list is replaced with "Fizz" if its is divisible by 3, "Buzz" if 
--     it is divisible by 5, "FizzBuzz" if it is divisible by 3 and 5, and the
--     original value as a string if it is divisible by neither.

--     So an input of [1..15] would return:

--     ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz",
--      "11", "Fizz", "13", "14", "FizzBuzz"]

-- **. Pass your FizzBuzz function the range that is every number divisible by 3 
--     from 1 to 100 and record the result as a comment here.
