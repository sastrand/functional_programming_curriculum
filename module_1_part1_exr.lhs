New Beginnings Winter 2019
Haskell Lab

Module 1 Part 1
Functions over lists

------
Name:
------

------
library code we'll use
------

> import Data.List

------
Lists
------

We can create lists using the standard syntax

> goodList = [1,1,2,3,5,8]

and concatenate two lists with the `++` operator.

> betterList = goodList ++ [13,21,33]

Strings are themselves lists of characters, so they use similar syntax

> bigCat = "Panthera leo"
> sound = ['r','a'] ++ ['r','r']

Load this module into ghci and see how the constant `sound` evaluates. 

Haskell provides a few built-in functions for manipulating lists:

`reverse` will reverse a list

> countdown = reverse [0,1,2,3,4,5]

`head` will return the first element of a list
`tail` will return everything after the head
`last` will return the last element of a list
`init` will return everything before the last

head "caterpillar" ==> 'c'
tail "caterpillar" ==> "aterpillar"
last "caterpillar" ==> 'r'
init "caterpillar" ==> "caterpilla"

And we can index into a list using the `!!` operator

> someCountries = ["Argentina", "Bolivia", "Columbia"]

someCountries!!1 ==> "Bolivia"

But we've got to stick with C-style indexes. No fancy wrapping is 
built in.

someCountries!!3 ==> Exception: Prelude.!!: index too large
someCountries!!(-1) ==> Exception: Prelude.!!: negative index

------
Higher-order Functions Over Lists
------

A higher-order function is a function that takes another function as
an argument. We've already seen this in Python, and they're super
useful here in Haskell, too.

For instance, if we want to apply the function `mult3` to every value
from 1 to 10, we can use the `map` function:

> multsOf3 = map mult3 [1,2..10]

Likewise, if we want to apply a Boolean function to every element in a 
list and return the elements for which that function returned True,
we can `filter` the list with that function.

Remember the mod function in Haskell is the prefix function `mod`. So we
could write a function that checks if a value is evenly divisible by 8:

> multOf8 x = if mod x 8 == 0 then True else False

and use this to filter a list of elements:

> multsOf8 = filter multOf8 [256, 382, 512, 770, 1024]

Finally, we can `zip` two lists together to create a list of tuples of 
corresponding elements. Because tuples can contain elements of different
types (unlike lists) these two lists can be of any types.

> zippedVals = zip [1,2,3] ["one", "two", "three"] 

------
Exercises
------

This first exercise is already completed as an example.
For each exercise, there's a debugging print statement and a test just
below. If you run these tests these without defining the function in the
exercise, you'll get an error, and that's okay.

Before moving on, our goal is to get everyone past `fizzBuzz` in problem 4.

01. (Eg) Write a function, `mult3`, that takes a number and multiples it by 3.

> mult3 :: Num a => a -> a
> mult3 x = x * 3

> prob1Test = mult3 3 == 9
> prob1 = do
>           putStrLn ("mult3 3 = " ++ show (mult3 3))
>           putStrLn ("Test = " ++ if prob1Test then "PASS" else "FAIL")


02. Write a function, `square`, that takes a number and returns the square of 
    that number. 

> square :: Num a => a -> a
> square x = undefined

> prob2Test = square 4 == 16
> prob2 = do
>           putStrLn ("square 5 = " ++ show (square 5))
>           putStrLn ("Test = " ++ if prob2Test then "PASS" else "FAIL")


03. Write a function, `squareSum`, that takes two arguments, squares them both,
    and returns the sum of the two squares.

> squareSum :: Num a => a -> a -> a
> squareSum = undefined

> prob3Test = squareSum 2 4 == 20
> prob3 = do
>           putStrLn ("squareSum 2 4 = " ++ show (squareSum 2 4))
>           putStrLn ("Test = " ++ if prob3Test then "PASS" else "FAIL")

04. Implement FizzBuzz over lists. That is, write a function, `fizzBuzz`, that
    takes a list of numbers and returns a new list in which each value of the 
    original list is replaced with "Fizz" if its is divisible by 3, "Buzz" if 
    it is divisible by 5, "FizzBuzz" if it is divisible by 3 and 5, and the
    original value as a string if it is divisible by neither. If you haven't 
    seen this problem before, check the test case for an example.

    Hint: consider writing two functions: one that takes a single numeric 
    input and determines its fizz-buzz string and one that applies that 
    function over every value in a given list. 

    Remember, the function `show` will convert a value to the String type.

> fizzBuzz :: (Show a, Integral a) => [a] -> [[Char]]
> fizzBuzz = undefined

> prob4Test = fizzBuzz [1..15] == ["1", "2", "Fizz", "4", "Buzz", "Fizz",
>                                   "7", "8", "Fizz", "Buzz", "11", "Fizz",
>                                   "13", "14", "FizzBuzz"]
> prob4 = do
>            putStrLn ("fizzBuzz [1..15] = " ++ show (fizzBuzz [1..15]))
>            putStrLn ("Test = " ++ if prob4Test then "PASS" else "FAIL")

05. Write a function, `isPalindrome`, that takes a string and determines 
    if it's a palindrome.

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome = undefined

> prob5Test = isPalindrome "tacocat" && not (isPalindrome "taco")
> prob5 = do 
>           putStrLn ("isPalindrome \"tacocat\" = " ++ show (isPalindrome "tacocat"))
>           putStrLn ("isPalindrome \"taco\" = " 
>                     ++ show (isPalindrome "taco"))
>           putStrLn ("Test = " ++ if prob5Test then "PASS" else "FAIL")


06. Write a function, `toPalindrome`, that takes a string and makes it 
    a palindrome if it's not
    already, so 'hannah' would stay 'hannah' but 'cab' would become 'cabbac'.

> toPalindrome :: Eq a => [a] -> [a]
> toPalindrome = undefined

> prob6Test = toPalindrome "hannah" == "hannah" 
>             && toPalindrome "cab" == "cabbac"
> prob6 = do
>           putStrLn ("toPalindrome \"hannah\" = " ++ show (toPalindrome "hannah"))
>           putStrLn ("toPalindrome \"cab\" = " ++ show (toPalindrome "cab"))
>           putStrLn ("Test = " ++ if prob6Test then "PASS" else "FAIL")


07. Write a function, `findPalindromes`, that takes a list of words and 
returns those that are palidromes.

> findPalindromes :: Eq a => [[a]] -> [[a]]
> findPalindromes = undefined

> prob7Test = findPalindromes ["civic", "shahs", "zorro"] 
>               == ["civic", "shahs"]
> prob7 = do
>           putStrLn ("findPalindromes [\"civic\", \"shahs\", \"zorro\"] = " 
>                    ++ show (findPalindromes ["civic", "shahs", "zorro"]))
>           putStrLn ("Test = " ++ if prob7Test then "PASS" else "FAIL")

