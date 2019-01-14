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


