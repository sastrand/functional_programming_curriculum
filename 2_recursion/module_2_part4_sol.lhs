New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   4: Exercises

> import Data.Char

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part4.lhs

------

In the next module we'll start working with trees and other recursive data
structures (beyond the list). There solutions with the other two types of
recursion we're interested in--multiple and mutual recursion--will shine. Until
then, let's get more practice with head and tail recursion on lists and linear
series.

A reminder of the head and tail recursive functions we saw last time:

Head recursive:

> zipAdd [] ys = []
> zipAdd xs [] = []
> zipAdd (x:xs) (y:ys) = x+y : zipAdd xs ys

Tail recursive:

> revAndCap [] = []
> revAndCap (x:xs) = revAndCap xs ++ [toUpper x]

------

**. The following Python function takes a positive integer and returns its 
    binary representation as a list of 1s and 0s.

    def to_bin(n):
      ret = []
      while n > 0:
        ret.append(n % 2)
        n = n // 2 
      ret.reverse()
      return ret

    Without using the `reverse`, write a head-recursive function in Haskell, 
    toBin, to do the same work:

> toBin 0 = []
> toBin n = toBin (n `div` 2) ++ [(n `mod` 2)]

> prob1Test = toBin 42 == [1,0,1,0,1,0]
> prob1 = do
>          putStrLn ("toBin 42 = " ++ show(toBin 42))
>          putStrLn ("Test = " ++ if prob1Test then "PASS" else "FAIL")

**. Given the following function that will take a number between 0 and 15 and
    return the hex equivalent of that number, write a new function, `toHex`
    that will create the hexadecimal string of a positive base-10 integer.

> toHexChar d
>   | d < 0 || d > 15 = error "nope"
>   | d < 10          = head $ show d
>   | otherwise       = "ABCDEF"!!(d-10)

> toHex 0 = []
> toHex n = toHex (n `div` 16) ++ [toHexChar (n `mod` 16)]

> prob2Test = toHex 49374 == "C0DE"
> prob2 = do
>         putStrLn ("toHex 49374 = " ++ show(toHex 49374))
>         putStrLn ("Test = " ++ if prob2Test then "PASS" else "FAIL")

**. Write a function, fromBin, that will take a list of 1s and 0s and
    convert that list into the base-10 integer it represents.

> fromBin [] = 0
> fromBin (x:xs) = (2^(length(x:xs)-1) * x) + fromBin xs 

**. Write a function, fromHexChar, that will take a hex character 0-F and 
    convert it into a base-10 Integral value.

    To do this, you may find the Prelude function `elem` helpful. To interpret
    its type signature, know that the only foldable type we've seen is a list.

        elem :: (Eq a, Foldable t) => a -> t a -> Bool

    Also, from the Data.Char package:

        digitToInt :: Char -> Int        

> fromHexChar d
>   | d `elem` "1234567890" = digitToInt d
>   | d == 'A' = 10
>   | d == 'B' = 11
>   | d == 'C' = 12
>   | d == 'D' = 13
>   | d == 'E' = 14
>   | d == 'F' = 15

> probxTest1 = fromHexChar 'D' == 13
> probxTest2 = fromHexChar '7' == 7
> probx = do
>         putStrLn ("fromHexChar \'D\' = " ++ show(fromHexChar 'D'))
>         putStrLn ("fromHexChar \'7\' = " ++ show(fromHexChar '7'))
>         putStrLn ("Test = " ++ if probxTest1 && probxTest2 then "PASS" 
>           else "FAIL") 

**. Write a tail recursive function, `fromHex` that given a positive hexadecimal 
    string, will return its base 10 Integral representation.

> fromHex [] = 0
> fromHex (x:xs) = (16^(length(x:xs)-1) * fromHexChar x) + fromHex xs 



