**. Write a function that takes a list of numbers and returns their sum.

**. Write a function, `isFibs`, that takes a list of integers and checks if 
    it is a part of the Fibonacci sequence, starting with 0. 

    To help, the following list, `fibs` is the entire Fibonacci sequence, 
    forever. And the function `subList` will check if its first argument 
    is a sublist of its second argument.

> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

> subList :: Eq a => [a] -> [a] -> Bool
> subList [] [] = True
> subList _ []    = False
> subList [] _    = True
> subList (x:xs) (y:ys) 
>     | x == y    = subList xs ys   
>     | otherwise = subList (x:xs) ys

**. Write a function, `addLists`, that takes two lists of numbers and adds
 their respective values to one another until the shorter list is out of
 numbers, 
    so `foo [1, 2, 3] [4, 5, 6, 7]` would return `[5, 7, 9]`.


**. Write a function, `findAllLarge`, that takes a list of tuples of integers
    and returns the largest value from each tuple in a new list. 

findAllLarge :: [(Int)] -> [Int]

findAllLarge x = map (map max) x

prob11 = putStrLn ("findAllLarge [(1,2,3), (4,5,6)] = " ++ show (findAllLarge [(1,2,3), (4,5,6)]))

**. Write a function, `findLargest`, that takes a list of tuples of integers
    and returns the largest value across all the tuples in the original list.


**. Write a function, `isAnagram`, that takes two strings and determines 
    if they are anagrams of one another.

> isAnagram :: Ord a => [a] -> [a] -> Bool
> isAnagram x y = if sort x == sort y
>                    then True
>                    else False

> prob6Test = isAnagram "the morse code" "here come dots" 
>             && not (isAnagram "foo" "nope")
> prob6 = do
>         putStrLn ("isAnagram \"the morse code\" \"here come dots\" = " 
>                   ++ show (isAnagram "silent" "listen"))
>         putStrLn ("isAnagram \"foo\" \"nope\" = " 
>                   ++ show (isAnagram "foo" "nope"))
>         putStrLn ("Test = " ++ if prob6Test then "PASS" else "FAIL")


10. Write a function, `findRevs`, that takes two lists of words,
    and returns a list of Boolean values indicating which pairs of 
    words corresponding across the lists are the reverse of one another.

> findRevs :: Eq a => [[a]] -> [[a]] -> [Bool]
> findRevs x y = map isPalindrome (zipWith (++) x y)

> prob10Test = findRevs ["cab", "tac", "eye"] ["cab", "cat", "eye"]
>                == [False, True, True]
> prob10 = do
>            putStrLn ("findRevs [\"cab\", \"tac\", \"eye\"]" 
>                       ++ " [\"cab\", \"cat\", \"eye\"] = " 
>                       ++ show (findRevs ["cab", "tac", "eye"] ["cab", "cat", "eye"]))
>            putStrLn ("Test = " ++ if prob10Test then "PASS" else "FAIL")


11. Write a function, `quadEq`, that takes three arguments `a`, `b`, and `c`
    representing the three coefficients in a second degree algebraic equation,
    `a*(x^2) + b*x + c = 0`, and returns the possible values of x in a tuple.

> quadEq :: Floating b => b -> b -> b -> (b, b)
> quadEq a b c = (((-1 * b) + sqrt(b^2 - 4*a*c))/(2*a), 
>                  ((-1 * b) - sqrt(b^2 - 4*a*c))/(2*a))

> prob11Test = quadEq 1 2 (-3) == (1.0,-3.0)
> prob11 = do
>            putStrLn ("quadEq 1 2 (-3) = " ++ show (quadEq 1 2 (-3)))
>            putStrLn ("Test = " ++ if prob11Test then "PASS" else "FAIL")

**. Import your instance of the `module_1_part1.lhs` file into this file, to 
use your FizzBuzz function, and pass it the range that is every number 
divisible by 3 from 1 to 100. Print the result.

----------
2019-01-16
----------

**. Using these functions on lists, write the following functions over sets, where
    sets are represented as lists.

> union as bs = dupOut (as ++ bs)

> intersection as bs = [x | x <- as, x `elem` bs, x `elem` as]

------
Linear Algebra Library
------

In this module, let's start building a linear algebra library.

**. Define a function `dotProduct` that takes two lists of numbers of the same
    length and returns their dot product.

You could do this in many ways, including with guards and pattern matching
or in a solution that includes the `zipWith` function.

> dotProduct [] [] = 0
> dotProduct (v1:v1s) (v2:v2s) = (v1*v2) + dotProduct v1s v2s

> dotProduct' v1 v2 = sum $ zipWith (*) v1 v2

**. Define a function `matrixMult` that performs matrix multiplication over two 
    matrices represented as lists of lists.


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

--------
blah
--------
**. Write a function `maxElem` that will take a list of Ints from -999 to 999
    and return the largest element in that list. If the list is empty, return -1000. 

> maxElem [] = -1000
> maxElem (x:xs) = max x (maxElem xs)


**. Given two linked lists, check to see if one is a strict subset of the other.

    That is, every element in A must be in B and A may not equal B.

> strictSubset [] [] = False
> strictSubset xs ys 
>   | sort xs == sort ys = False 
>   | otherwise          = subset xs ys