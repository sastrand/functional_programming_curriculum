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