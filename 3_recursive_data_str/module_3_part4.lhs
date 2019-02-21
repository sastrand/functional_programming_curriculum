New Beginnings Winter 2019
Haskell Lab

Module 3
Part   4: Syntax Review

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part4.lhs

------
Module 1: Lists and Functions
Learn You a Haskell Chapters 1 & 2
------

 Applying a function
---------------------

> countdown = reverse [0,1,2,3,4,5]

 Accessing elements in a list
------------------------------

> someCountries = ["Argentina", "Bolivia", "Columbia"]

    someCountries!!1     ==> "Bolivia"
    someCountries!!3     ==> Exception: Prelude.!!: index too large
    someCountries!!(-1)  ==> Exception: Prelude.!!: negative index

    length someCountries ==> 3

    head someCountries   ==> "Argentina"
    tail someCountries   ==> ["Bolivia","Columbia"]
    init someCountries   ==> ["Argentina","Bolivia"]
    last someCountries   ==> "Columbia"

    take 2 someCountries ==> ["Argentina","Bolivia"]
    drop 2 someCountries ==> ["Columbia"]

 Defining a function
---------------------

-- if-then-else

Remember Haskell requires every if-then to also have an else

> multOf8 x = if mod x 8 == 0 then True else False

If broken into multiple lines, the `then` and `else` must be at least as far
right as the `if`.

> multOf8' x = if mod x 8 == 0
>                 then True
>                 else False

If you're nesting multiple if-else statements, you can seperate them with white
space, though at this point, you and your readers may find guards preferable.

> fizzBuzz x = if mod x 5 == 0
>                then 
>                  if mod x 3 == 0
>                    then
>                      "FizzBuzz"
>                    else
>                      "Fizz"
>                else
>                  if mod x 3 == 0
>                    then
>                      "Buzz"
>                    else
>                      "-"

-- Guards

Allow you to define the body of a function multiple times and match each with a
condition over the input. Guards can't be nested, but you can use an
if-then-else inside a branch of the guard statement.

> fizzBuzz' x 
>   | x mod 5 == 0 = if x mod 3 == 0 then "FizzBuzz" else "Fizz"
>   | x mod 3 == 0 = "Buzz"
>   | otherwise    = "-"


 List comprehensions
---------------------

These are a set operation in which you define a set or sets from which you want
to take elements, "generators", a predicate through which you want to filter
the elements you take, "guards" (same idea but way different syntax than
guards used in function definition), and a function you want to apply to each
new element of your set, to the left of the pipe.

[ <function application> | <generator(s)>, <guard(s)> ]


> evenSquares = [ x^x | x <- [1..10], even x ]
> evenOddCrossProd xs ys = [ (x, y) | x <- xs, y <- ys, even x, odd y ]

The result of a list comprehension is a new list.


------
Module 2: Recursion
Learn You a Haskell Chapters 4 and 5
------

-- Pattern matching

Every list is the empty list or one element prepended to another list:

> goodAlgs = ["correct", "efficient", "simple"]
> goodAlgs' = "correct" : "efficient" : "simple" : []
> simpleAlgs = "simple" : []
> noAlgs     = []

To match these two patterns of lists, we can definine a function twice--once
for each pattern. Note, the syntax here is different from guards in which we
define the body of a function multiple times based on a predicate. In this
case, we're matching an instance of data type to a pattern that describes some
instance of the same data type.

The most common pattern we'll use is recognizing an empty list compared to a
list that contains at least one element.

> sumOfSquares []     = 0
> sumOfSquares (x:xs) = x^2 + sumOfSquares xs

-- Case statements

Pattern matching through re-defining a function works alright, though you may
find it clearer to define your function in one place and use something like a
special-purpose guard to match the patterns of input within the single function
definition. These are case statements.

> sumOfSquares' l = case l of
>                     []     -> 0
>                     (x:xs) -> x^2 + sumOfSquares' xs


------
Module 3: Recursive Data Structures and Higher-Order Functions
Learn You a Haskell Chapter 6
------

-- filter

> multsOf8 = filter multOf8 [256, 382, 512, 770, 1024]

-- zip

> zippedVals = zip [1,2,3] ["one", "two", "three"] 

-- map

> someChange = [32,64,89,21]

  map (`div` 25) someChange

-- filter and lambdas

> housingWords = ["tenant",  "sconce", "repaper"]
> housingPalindromes = filter (\x -> x == reverse x) housingWords

-- Function composition

> last xs  = head (reverse xs)
> last' xs = (head . reverse) xs
> realSquares = map (sqrt . abs) [1.1,-2.2,3.3]


------
Exercises
------

If there are any exercises in module 3 you haven't finished, particularly
exerciese on trees, I recommend going back to those. 

Otherwise the exercises below are taken/adapted from the Haskell Wiki and
should provide some practice using a range of Haskell syntax. Each can be
solved in at least a few ways. 

You can find more like them and example solutions at: 
https://wiki.haskell.org/99_questions

32. Using Euclid's Algorithmi (en.wikipedia.org/wiki/Euclidean_algorithm)
    write a function to find the gcd of two numbers.

> myGCD :: Integral a => a -> a -> a
> myGCD = undefined

> prob32Test = [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] == [9,3,3]
> prob32 = do
>         putStrLn ("Test = " ++ if prob33bTest then "PASS" else "FAIL")

33. Determine whether two positive integer numbers are coprime. Two numbers are 
    coprime if their greatest common divisor equals 1.

> coprime :: Integral a => a -> a -> Bool
> coprime = undefined

> prob33Test1 = (coprime 35 64)
> prob33Test2 = (coprime 6 9) == False
> prob33 = do
>         putStrLn ("Test = " ++ if prob33Test1 && not prob33Test2 then "PASS" else "FAIL")

33b. Given two lists of numbers of the same length, return a list of the pairs
    of numbers at corresponding positions in the lists that are co-primes of 
    one another

> coprimes :: Integral a => [a] -> [a] -> [(a,a)]
> coprimes = undefined

> prob33bTest = coprimes [6,12,35,99] [9,7,64,11] == [(12,7),(35,64)]
> prob33b = do
>         putStrLn ("Test = " ++ if prob33bTest then "PASS" else "FAIL")

17. Split a list into two parts; the length of the first part is given.

> split :: [a] -> Int -> ([a], [a])
> split = undefined

> prob15Test = split "abcdefghik" 3 == ("abc", "defghik")
> prob15 = do
>         putStrLn ("Test = " ++ if prob15Test then "PASS" else "FAIL")

16. Drop every N'th element from a list.

> dropEvery :: [a] -> Int -> [a]
> dropEvery = undefined

> prob16Test = dropEvery "abcdefghik" 3 == "abdeghk"
> prob16 = do
>         putStrLn ("Test = " ++ if prob16Test then "PASS" else "FAIL")

