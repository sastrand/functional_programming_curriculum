ew Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   2: Trees

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part2.lhs

------
Trees and Multiple Recursion
------

Directed, acyclic graphs model everything from a git repositories branching
and changing over time, to the exploration of all possible proof states in an
automated theorem prover, to the blockchains used in cryptocurrency. DAGs are
everywhere, and a binary tree is just a special case of this ubiquitous data
structure.

In this module we'll traverse and modify binary trees with recursion.

A binary tree is either a leaf (a node with no children) or a node connected
to a left tree and a right tree, and a binary search tree is a binary tree that
maintains the *binary search property* in which every node has a value from an
ordered set, every value in the left sub-tree of a node is less than its value
and every value in the right sub-tree of a node is greater than its value.

We can define a binary tree in Haskell:

> data BTree a = EmptyLeaf
>            | Node (BTree a) a (BTree a) deriving Show

The `data` keyword here defines a new data type. We haven't seen this
explicitly in a language yet. (Despite the name, C's `typedef` keyword only
allows one to add a new name to an existing type.) But in an OOP context, every
new class is a new data type. And it is in this sense that we're creating
`BTree`.

The pipe in the declaration of the `BTree` type can be read as an "or", showing
that a `BTree` can be either an `EmptyLeaf` or given some type `a`, it can be
two `BTree`s of type `a` surrounding a value of type `a`.

To create a new instance of `BTree` we can create an empty leaf, give it a
value, or give it a value and a sub-tree:

    sadTree = EmptyLeaf
    betterTree = Node EmptyLeaf 42 EmptyLeaf
    twoTierTree = Node (Node EmptyLeaf 7 EmptyLeaf) 42 EmptyLeaf


The `deriving Show` at the end of the definition asks the type system to work
out a way to reasonably print the data type we've just defined.

We can add a value to the tree using pattern matching and guards to recursively
traverse the tree until we find a location to insert that value that will
maintain the binary search property for the whole tree.

> bstInsert :: (Ord a) => BTree a -> a -> BTree a
> bstInsert EmptyLeaf a = Node EmptyLeaf a EmptyLeaf
> bstInsert (Node l a r) b 
>     | b == a = Node l a r
>     | b > a  = Node l a (bstInsert r b)
>     | b < a  = Node (bstInsert l b) a r

When working with a single instance of a tree, we can break its pattern up into
(Node l a r) just like we could break the pattern of a list up into (x:xs).
This indicates that the BTree we're interested in isn't an `EmptyLeaf` and gives
us some variable names to use in the rest of the function to define its
component parts.

Also, to help with testing, here is a function that will take a list and 
generate a tree from that list by applying the `bstInsert` function to
each element. We'll come back to this function when we look at the fold
functions in the next part of this module.

> bstCreateFromList :: (Ord a) => [a] -> BTree a
> bstCreateFromList l = foldl bstInsert EmptyLeaf l


------
Exercises
------

01. Write a function, `bstSearch`, that will search for a value in a BTree with
    the binary search tree property, returning True if the value is present and 
    False otherwise.

    Hint/challenge: can you do this in O(lg n) time, for n = the number of 
    elements in the tree?

> bstSearch :: (Ord a) => BTree a -> a -> Bool
> bstSearch EmptyLeaf a = False
> bstSearch (Node l a r) b
>   | b == a    = True
>   | b <  a    = bstSearch l b
>   | otherwise = bstSearch r b

> prob1Test1 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 3 == True
> prob1Test2 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 7 == False
> prob1 = do
>         putStrLn ("Test 1 = " ++ show (bstSearch (bstCreateFromList 
>           [3,1,4,1,5,9]) 3))
>         putStrLn ("Test 2 = " ++ show (bstSearch (bstCreateFromList
>           [3,1,4,1,5,9]) 7))
>         putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" else "FAIL")


02. Regardless of the order in which elements were added and the resulting
    shape of the tree, the in-order traversal of a binary search tree returns 
    a sorted list of the elements in that tree.

    Write a function, `bTreeInOrder` that takes a tree and returns its in-order
    traversal as a list of elements.

    Note that in order to do this, you'll need to make two recursive calls of
    `bTreeInOrder` each time you invoke the function. This is what is sometimes
    called "multiple recursion".

> bTreeInOrder EmptyLeaf = []
> bTreeInOrder (Node l a r) = bTreeInOrder l ++ [a] ++ bTreeInOrder r

> prob2Test1 = bTreeInOrder (bstCreateFromList [4,9,1,1,7,5]) == [1,4,7,5,9]
> prob2 = do
>         putStrLn ("Test 1 = " ++ show (bTreeInOrder (bstCreateFromList 
>           [4,9,1,1,7,5])))
>         putStrLn ("Test = " ++ if prob1Test1 then "PASS" else "FAIL")


------
Aside: Mutual recursion
------

One additional type of recursion we're not covering here is mutual recursion,
in which two or more functions call one another recursively. The definition for
a tree in which each node contains arbitrary many children is a good example
of mutual recursion. To read more about this example and mutual recursion, you
can check out the sources linked at the bottom of the file.


------
Sources
------

--- mutual recursion
https://en.wikipedia.org/wiki/Mutual_recursion
https://stackoverflow.com/questions/28431125/mutual-recursion-in-odd-even-functions-in-haskell
