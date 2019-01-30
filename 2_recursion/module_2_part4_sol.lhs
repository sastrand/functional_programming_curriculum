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

