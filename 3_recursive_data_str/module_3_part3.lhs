New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   3: Trees

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part1.lhs

------
Trees and Multiple Recursion
------

Directed, acyclic graphs can model everything from a git repository branching
and changing over time, to the exploration of all possible proof states in an
automated theorem prover, to the blockchains used in cryptocurrency. DAGs are
everywhere, and a tree is just a special case of this ubiquitous data
structure.

In this module we'll work with a binary search tree seeing how to modify and
traverse it recursively.

A binary tree is either a leaf (a node with no children) or a node connected
to a left tree and a right tree, and a binary search tree is a binary tree that
maintains the *binary search property* in which every node has a value from an
ordered set, every value in the left sub-tree of a node is less than its value
and every value in the right sub-tree of a node is greater than its value.

We can define this data structure in Haskell:

> data BST a = EmptyLeaf
>            | Node (BST a) a (BST a) deriving Show

The `deriving Show` at the end of the definition asks the type system to work
out a way to reasonably print the data type we've just defined.

We can add a value to the tree using pattern matching and guards to recursively
traverse the tree until we find a location to insert that value that will
maintain the binary search property for the whole tree.

> bstInsert :: (Ord a) => BST a -> a -> BST a
> bstInsert EmptyLeaf a = Node EmptyLeaf a EmptyLeaf
> bstInsert (Node l a r) b 
>     | b == a = Node l a r
>     | b > a  = Node l a (bstInsert r b)
>     | b < a  = Node (bstInsert l b) a r

When working with a single instance of a tree, we can break its pattern up into
(Node l a r) just like we could break the pattern of a list up into (x:xs).
This indicates that the BST we're interested in isn't an `EmptyLeaf` and gives
us some variable names to use in the rest of the function to define its
component parts.

We can notice here that `bstInsert` calls itself twice in its own definition.
This is an example of multiple recursion, and will allow us to traverse a tree
with the same recursive patterns we'd use to traverse a list.

To do much more, we see that we'll have more than two patterns we want to 
match. For example to perform an in-order traversal of the tree and return the 
result as a list, we'll have two base cases and three recursive cases. We can
define each of these as an instance of the the function we're writing, as we've
been doing or use a new, more concise piece of syntax, the case expression.

In a case expression, each pattern on which we want to match gets its own line,
followed by a `->`. We apply the case onto a more general parameter name to the
entire function.


> bstInOrder :: (Ord a) => BST a -> [a]
> bstInOrder t =
>   case t of 
>     EmptyLeaf -> []
>     Node EmptyLeaf a EmptyLeaf -> [a]
>     Node l a EmptyLeaf -> bstInOrder(l) ++ [a]
>     Node EmptyLeaf a r -> bstInOrder(r) ++ [a]
>     Node l a r -> bstInOrder(l) ++ [a] ++ bstInOrder(r)

Also, to help with testing out your trees, here is a function that will take a
list and generate a list from that tree by applying the `bstInsert` function to
each element. We'll come back to this function in the next part of this moudle
when we talk more about how `foldl` works.

> bstCreateFromList :: (Ord a) => [a] -> BST a
> bstCreateFromList l = 
>   case l of
>     []   -> EmptyLeaf
>     x:[] -> Node EmptyLeaf x EmptyLeaf
>     x:xs -> foldl bstInsert (Node EmptyLeaf x EmptyLeaf) xs

------
Exercises
------

**. Write a function, `bstSearch`, that will search for a value in a BST, 
    returning True if the value is present and False otherwise.

> bstSearch :: (Ord a) => BST a -> a -> Bool
> bstSearch EmptyLeaf a = False
> bstSearch (Node l a r) b
>   | b == a = True
>   | b /= a = (bstSearch l b) || (bstSearch r b)

> prob1Test1 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 3 == True
> prob1Test2 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 7 == False
> prob1 = do
>         putStrLn ("Test 1 = " ++ show (bstSearch (bstCreateFromList 
>           [3,1,4,1,5,9]) 3))
>         putStrLn ("Test 2 = " ++ show (bstSearch (bstCreateFromList
>           [3,1,4,1,5,9]) 7))
>         putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" else "FAIL")

**. Modify `bstInOrder` to create `bstPreOrder`, a function that performs a 
    pre-order traversal of a given tree and return the path of this traversal 
    as a list.

> bstPreOrder :: (Ord a) => BST a -> [a]
> bstPreOrder t =
>   case t of 
>     EmptyLeaf -> []
>     Node EmptyLeaf a EmptyLeaf -> [a]
>     Node l a EmptyLeaf -> [a] ++ bstPreOrder(l)
>     Node EmptyLeaf a r -> [a] ++ bstPreOrder(r)
>     Node l a r -> [a] ++ bstPreOrder(l) ++ bstPreOrder(r)

> prob2Test1 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 3 == True
> prob2Test2 = bstSearch (bstCreateFromList [3,1,4,1,5,9]) 7 == False
> prob2 = do
>         putStrLn ("Test 1 = " ++ show (bstSearch (bstCreateFromList 
>           [3,1,4,1,5,9]) 3))
>         putStrLn ("Test 2 = " ++ show (bstSearch (bstCreateFromList
>           [3,1,4,1,5,9]) 7))
>         putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" else "FAIL")





------
Aside: Mutual recursion
------

One additional type of recursion we're not covering here is mutual recursion,
in which two or more functions call one another recursively. The definition for
a tree in which each node contains arbitarily many children is a good example
of mutual recursion. To read more about this example and mutual recursion, you
can check out the sources linked at the bottom of the file.


------
Sources
------

--- mutual recursion
https://en.wikipedia.org/wiki/Mutual_recursion
https://stackoverflow.com/questions/28431125/mutual-recursion-in-odd-even-functions-in-haskell
