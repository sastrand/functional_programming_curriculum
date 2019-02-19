New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   3: Folding

 import module_3_part2_sol

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part3.lhs

------
Folding
------

Today we are just looking at one set of higher order functions that take a
recursive data structure, an operation that will connect--that is "reduce"--the
elements in the list, and a base case to be applied at one end of the list.

This sounds something like a recursive traversal, and indeed, the `foldl` and 
`foldr` functions provide the linear recursion we saw in module 2. In general,
these two operators are referred to as folds.

For instance, in module 2 you wrote a function to find the product of a list 
of numbers:

> prod :: Num p => [p] -> p 
> prod []     = 1
> prod (x:xs) = x * prod xs

prod could be re-written with a fold:

> prod' :: Num p => [p] -> p
> prod' xs = foldl (*) 1 xs


Likewise, the fold of a list of integers over the `+` is its sum. The
fold of a list over the `||` operator will tell you if any of its values are
True.

The base case Miran Lipovača calls an "accumulator" in *Learn You a Haskell*. 
If we want to pair-wise evaluate the list from left to right, starting with the
accumulator and using the result of one evaluation as the right value of the
next, we can use `foldl`.

`foldr` works the same way but we start the accumulator at the right and move
from right to left through the list. We can see the difference accumulating
with an operation that isn't associative like substraction:

> subLtoR = foldl (-) 0 [1,2,3,4] --      ((((0 - 1) - 2) - 3) - 4)
> subRtoL = foldr (-) 0 [1,2,3,4] --      (1 - (2 - (3 - (4 - 0))))

Let's go back to `bstInsert` and `bstCreateFromList` in the last module. 
`bstInsert` took a tree and a value and returned a new three that was the
result of placing the value in the given tree following the BST property.
 
  bstInsert :: (Ord a) => BTree a -> a -> BTree a
  bstInsert EmptyLeaf a = Node EmptyLeaf a EmptyLeaf
  bstInsert (Node l a r) b 
      | b == a = Node l a r
      | b > a  = Node l a (bstInsert r b)
      | b < a  = Node (bstInsert l b) a r

`bstCreateFromList` started off with an empty tree as its accumulator and
processed through a list adding each element to the accumulator tree as it
went. The end result is a BST containing every element in the list.
 
  bstCreateFromList :: (Ord a) => [a] -> BTree a
  bstCreateFromList l = foldl bstInsert EmptyLeaf l

You can get more creative by defining a function passed to fold to modify each
element it sees before reducing it into the accumulator. Some simple
modifications can be expressed clearly with a lambda:

> sumOfSquares xs = foldl (\acc x -> acc + x^2) 0 xs

  sumOfSquares [1,2,3] => 14      -- (((0 + 1^2) + 2^2) + 3^2)

------
Exercises
------

**. Write a function `bayes` that takes a list of probabilities of independent
    events and returns the probability that they will all happen. (Beware
    floating point errors showing up when you test your code.)

> bayes :: (Foldable t, Num b) => t b -> b
> bayes xs = foldl (*) 1 xs

> prob1Test1 = bayes [0.5, 0.1, 0.8] < 0.041 && bayes [0.5, 0.1, 0.8] > 0.039
> prob1 = do
>         putStrLn ("Test = " ++ if prob1Test1 then "PASS" else "FAIL")


**. Write a function `contraBayes` that takes a list of probabilities of
    independent events and returns the probability that none of them will happen.

> contraBayes :: (Foldable t, Num a) => t a -> a
> contraBayes xs = foldl (\acc x -> acc * (1-x)) 1 xs

> prob2Test1 = contraBayes [0.6, 0.4, 0.2] == 0.192
> prob2 = do
>         putStrLn ("Test = " ++ if prob2Test1 then "PASS" else "FAIL")


**. Write a function `unLine` that takes a string and returns a list of 
    strings in which each original character is now in its own sublist.

> unLine :: Foldable t => t a -> [[a]]
> unLine s = foldr (\x acc -> [x] : acc) [] s

> prob3Test1 = unLine "hello" == ["h","e","l","l","o"]
> prob3 = do
>         putStrLn ("Test = " ++ if prob3Test1 then "PASS" else "FAIL")


**. Write a function `sumBool` that takes a list of Booleans and returns the
    count of how many are True.

> sumBool :: (Foldable t, Num a) => t Bool -> a
> sumBool xs = foldl (\acc x -> if x then 1+acc else acc) 0 xs

> prob4Test1 = sumBool [True, True, False, True] == 3
> prob4 = do
>         putStrLn ("Test = " ++ if prob4Test1 then "PASS" else "FAIL")

