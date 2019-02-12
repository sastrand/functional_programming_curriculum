New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   2: Parametric Types

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part2.lhs

------
Parametric Types
------

* Maybe, Just, and Nothing
* Parametric polymorphism

* Example of polymorphic stack

> data Stack a = Stack [a] deriving (Show)

> push :: a -> Stack a -> Stack a
> push x s =
>     case s of 
>       Stack [] -> Stack [x]
>       Stack (x':xs) -> Stack (x:x':xs)

--------< aside >--------

Why does the following not work as a definition for `push`?

  push x s = Stack (x:s)

Creates the following error:
"Couldn't match expected type ‘[a]’ with actual type ‘Stack a’"

--------< end aside >--------

> pop :: Stack a -> (Maybe a, Stack a)
> pop s = 
>     case s of
>         Stack [] -> (Nothing, Stack [])
>         Stack (x:xs) -> (Just x, Stack xs)

------
Exercises
------

** Write a `peek` and `isEmpty` function for `Stack`.

** Develop a polymorphic priority queue (?)

