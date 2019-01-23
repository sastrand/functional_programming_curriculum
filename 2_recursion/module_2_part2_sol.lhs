New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   2: Guards and Pattern Matching Continued

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part2.lhs

------

In this lab we're reviewing some of the topics from last lab and trying out some 
additional exercises.

------

> import Data.List

------

01. Given two linked lists, check to see if one is a non-strict subset of the other.

    That is, every element in A must be in B and A may equal B.

> subset :: (Foldable t, Eq a) => [a] -> t a -> Bool
> subset [] ys = True
> subset (x:xs) ys
>   | x `elem` ys = True && subset xs ys
>   | otherwise   = False

> prob1Test1 = subset [1,2,3] [1,2,3,4] == True
> prob1Test2 = subset [] [1,2] == True
> prob1Test3 = subset [1,2] [5,6,7]  == False
> prob1 = do
>           putStrLn ("subset [1,2,3] [1,2,3,4] = " ++ show (subset [1,2,3] [1,2,3,4]))
>           putStrLn ("subset [] [1,2]  = " ++ show (subset [] [1,2] ))
>           putStrLn ("subset [1,2] [5,6,7] = " ++ show (subset [1,2] [5,6,7]))
>           putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 && prob1Test3 then "PASS" else "FAIL")



02. Write a function, `dupOut`, to remove duplicates from a list. The input
    list may be sorted or unsorted. The values in the result can be in any order.

    So dupOut [1,2,3,2,1,4] should return [1,2,3,4]

I found this solution form Mateusz Piotrowski on SO [1] and liked it 
better than mine.

> dupOut :: Eq a => [a] -> [a]
> dupOut [] = []
> dupOut (x:xs)   
>   | x `elem` xs   = dupOut xs
>   | otherwise     = x : dupOut xs

> prob2Test1 = dupOut [1,2,1,2,1,1] == [1,2] || dupOut [1,2,1,2,1,1] == [2,1]
> prob2Test2 = dupOut "red fox" == "red fox"
> prob2 = do
>           putStrLn ("dupOut [1,2,1,2,1,1] = " ++ show (dupOut [1,2,1,2,1,1]))
>           putStrLn ("dupOut \"red fox\" = " ++ show (dupOut "red fox"))
>           putStrLn ("Test = " ++ if prob2Test1 && prob2Test2 then "PASS" else "FAIL")



03. In building cyclic data structures, like those used in round-robin scheduling,
    it can be useful to insert a value at the second to last position in a linked 
    list. Write a function `insertSecondLast` to do this. In the case of an empty 
    list, return a list containing just the argument. 

> insertSecondLast :: a -> [a] -> [a]
> insertSecondLast x []     = [x]
> insertSecondLast x (a:[]) = x:a:[]
> insertSecondLast x (a:b:xs) = a : insertSecondLast x (b:xs) 

> prob3Test1 = insertSecondLast 42 [1,2,3,4,5] == [1,2,3,4,42,5]
> prob3Test2 = insertSecondLast 'o' [] == "o"
> prob3Test3 = insertSecondLast 'J' "o" == "Jo"
> prob3 = do
>           putStrLn ("insertSecondLast 42 [1,2,3,4,5] = " ++ show (insertSecondLast 42 [1,2,3,4,5]))
>           putStrLn ("insertSecondLast 'o' [] = " ++ show (insertSecondLast 'o' []))
>           putStrLn ("insertSecondLast 'J' \"o\" = " ++ show (insertSecondLast 'J' "o"))
>           putStrLn ("Test = " ++ if prob3Test1 && prob3Test2 && prob3Test3 then "PASS" else "FAIL")



04. Sorting an entire list takes O(n lg n) time but inserting a value into a sorted list 
    and maintaing the sorted order in the result is O(n) time. In some divide and conquer
    algorithms, this complexity savings will compound on each recursive call, making it
    a excellent overall optimization.

    Write a function `insertSorted` that inserts an element into a pre-sorted list
    in O(n) time and returns a list that is also sorted.

> insertSorted :: Ord t => t -> [t] -> [t]
> insertSorted x [] = [x]
> insertSorted x (a:xs) 
>   | x <= a = x : a : xs
>   | x >  a = a : insertSorted x xs


> prob4Test1 = insertSorted 8 [1,2,3,5,13] == [1,2,3,5,8,13]
> prob4Test2 = insertSorted 1 [] == [1]
> prob4 = do
>           putStrLn ("insertSorted 8 [1,2,3,5,13] = " ++ show (insertSorted 8 [1,2,3,5,13]))
>           putStrLn ("insertSorted 1 [] = " ++ show (insertSorted 1 []))
>           putStrLn ("Test = " ++ if prob4Test1 && prob4Test2 then "PASS" else "FAIL")

------
Sources
------

[1] https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell