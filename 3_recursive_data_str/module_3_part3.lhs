New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   2: Folding

> import module_3_part2

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part2.lhs

------
Folding
------

Today we are just looking at higher order function and we'll see how we can use
it to abstract away much of the recursive work we saw in the last module.

In module 2 you got some practice recursing through a list, applying a function, 
to each element, accumulating values into a single result, and terminating at a 
base case with a fixed result.

For instance, a function to find the product of a list of numbers:

> prod :: Num p => [p] -> p 
> prod []     = 1
> prod (x:xs) = x * prod xs

This patten is so often used, that it has its own pair of higher-order
functions, `foldl` and `foldr`.

These fold operations take a list and apply a binary function between every
pair of elements. The fold of a list of integers over the `+` is its sum. The
fold of a list over the `||` operator will tell you if any of its values are
True.

To use the fold functions all we need is a list, a binary function, and a base
case that Miran Lipovača calls an "accumulator" in *Learn You a Haskell*. 
If we want to pair-wise evaluate the list from left to right, starting with the
accumulator and using the result of one evaluation as the right value of the
next, we can use `foldl`.

For instance, to rewrite `prod` using `foldl`:

> prod' xs = foldl (*) 1 xs

`foldr` works the same way but we start the accumulator at the right and move
from right to left through the list. We can see the difference accumulating
with an operation that isn't associative like substraction:

> subLtoR = foldl (-) 0 [1,2,3,4] --      ((((0 - 1) - 2) - 3) - 4)
> subRtoL = foldr (-) 0 [1,2,3,4] --      (1 - (2 - (3 - (4 - 0))))

Let's go back to `bstCreateFromList` in the last module. We see here that we
defined what we want the function to do in two base cases for the list--when
the list contains one element and when the list is empty. And then we build a
tree from all the elements in the list by starting at the left and inserting
the head of the list into the tree, then inserting every subsequent value into
the result of this initial insertion operation.

  bstCreateFromList :: (Ord a) => [a] -> BTree a
  bstCreateFromList l = 
    case l of
      []   -> EmptyLeaf
      x:[] -> Node EmptyLeaf x EmptyLeaf
      x:xs -> foldl bstInsert (Node EmptyLeaf x EmptyLeaf) xs


------
Exercises
------

**. Write a function, `minBinTree`, that will traverse a binary tree without
    knowing if it has the binary search tree property, and return its minimum
    element.

> minBinTree 

**. Write a function `maxBinTree`, that will traverse a binary tree without 
    knowing if it has the binary search tree property, and return its maximum
    element.

**. Write a function, `isBST`, that will check if a given binary tree is a 
    binary search tree.

    You may find it helpful to write two helper functions, one that returns the
    minimum value in a tree and one that returns the maximum value in a tree.

