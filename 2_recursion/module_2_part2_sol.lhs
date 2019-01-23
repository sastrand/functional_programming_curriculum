New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   2: Guards and Pattern Matching Continued

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part2.lhs

------



**. Write a function `maxElem` that will take a list of Ints from -999 to 999
    and return the largest element in that list. If the list is empty, return -1000. 

> maxElem [] = -1000
> maxElem (x:xs) = max x (maxElem xs)

**. In building cyclic data structures, like those used in round-robin scheduling,
    it can be useful to insert a value at the second to last position in a linked 
    list. Write a function `insertSecondLast` to do this. In the case of an empty 
    list, return a list containing just the argument. 

> insertSecondLast x []     = [x]
> insertSecondLast x (a:[]) = x:a:[]
> insertSecondLast x (a:b:xs) = a : insertSecondLast x (b:xs) 

**. Also in schedulers, it may be useful to insert a value into a pre-sorted list
    without re-sorting the entire list. Sorting an entire list takes O(n lg n) time
    and sorted insertion into a pre-sorted list takes O(n) time.

    Write a function `insertSorted` that inserts an element into a pre-sorted list
    in O(n) time and returns a list that is also sorted.

> insertSorted x [] = [x]
> insertSorted x (a:xs) 
>   | x <= a = x : a : xs
>   | x >  a = a : insertSorted x xs

07. Write a function, `dupOut`, to remove duplicates from a list. The input
    list may be sorted or unsorted. The values in the result should be in 
    the same order as when they first appear in the input list.

    So dupOut [1,2,3,2,1,4] should return [1,2,3,4]

> dupOutHelper [] buf = buf
> dupOutHelper (x:xs) buf
>   | x `elem` buf = dupOutHelper xs buf
>   | otherwise    = dupOutHelper xs (x:buf)

> dupOut' xs = reverse $ dupOutHelper xs []

A better, more functional solution I found on SO [1]:

> dupOut :: Eq a => [a] -> [a]
> dupOut [] = []
> dupOut (x:xs)   
>   | x `elem` xs   = dupOut xs
>   | otherwise     = x : dupOut xs

> prob7Test1 = dupOut [1,2,3,2,1,4] == [1,2,3,4]
> prob7Test2 = dupOut "red fox" == "red fox"
> prob7 = do
>           putStrLn ("dupOut [1,2,3,2,1,4] = " ++ show (dupOut [1,2,3,2,1,4]))
>           putStrLn ("dupOut \"red fox\" = " ++ show (dupOut "red fox"))
>           putStrLn ("Test = " ++ if prob7Test1 && prob7Test2 then "PASS" else "FAIL")

**. Given two linked lists, check to see if one is a non-strict subset of the other.

    That is, every element in A must be in B and A may equal B.

> subset :: (Foldable t, Eq a) => [a] -> t a -> Bool
> subset [] ys = True
> subset (x:xs) ys
>   | x `elem` ys = True && subset xs ys
>   | otherwise   = False

**. Given two linked lists, check to see if one is a strict subset of the other.

    That is, every element in A must be in B and A may not equal B.

> strictSubset [] [] = False
> strictSubset xs ys = if sort xs == sort ys then False else subset xs ys



[1] https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell