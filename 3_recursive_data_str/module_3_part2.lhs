New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   2: Folding

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part2.lhs

------
Folding
------

------
Exercises
------

**. Write a function, `isBST`, that will check if a given binary tree is a 
    binary search tree.

    You may find it helpful to write two helper functions, one that returns the
    minimum value in a tree and one that returns the maximum value in a tree.

> minBST t = 
>   case t of
>     Node l v EmptyLeaf -> min v $ minBST l
>     Node EmptyLeaf v r -> min v $ minBST r
>     Node EmptyLeaf v EmptyLeaf -> v
>     Node l v r ->
>       case () of 
>         () | minBST l < minBST r =
>              | minBST l < v = minBST l
>              | otherwise    = v
>            | otherwise = 
>              | minBST r < v = minBST r
>              | otherwise    = v
