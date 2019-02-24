New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   3: Parametric Types

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part3.lhs

------
Parametric Types
------

A value is polymorphic if it can different types in different contexts.

We've seen this already with methods like `show` that are defined within a
typeclass and have a different function body for every type instance of that
type class.

This is referred to as "ad-hoc" polymorphism as the value with a changing type
changes only as is necessary in context and is limited to changing to a
pre-defined set of types.

This differs from *parametric polymorphism* in which a value is defined in
terms of at least one unconstrained type variable that can be supplied by any 
developer using this value.

Parametric polymorphism is particuarly useful for types that contain other
types, like a list. 

The key difference between the two is if the type variable is constrained. For
example, the type of `show`:

    show :: Show a => a -> String

describes taking any element whose type is a member of the Show typeclass.
While the type of the emtpy list:

    [] :: [a]

Is an empty list of anything at all.

In this module, we'll work on a polymorphic stack:


> data Stack a = Stack [a] deriving (Show)

> push :: a -> Stack a -> Stack a
> push a (Stack s) = Stack (a : s)

Here we're not using record syntax to define the type, so instead of retrieving
the list inside an instance of Stack with a special function, we can retrieve
it with pattern matching. 

> isEmpty :: Stack a -> Bool
> isEmpty (Stack s) 
>   | length s > 0 = False
>   | otherwise    = True


> pop :: Stack a -> (Maybe a, Stack a)
> pop s = 
>     case s of
>         Stack [] -> (Nothing, Stack [])
>         Stack (x:xs) -> (Just x, Stack xs)

------
Exercises
------

09. Write a function, `toList`, that takes a stack and converts it to a list
    with the top of the stack in the 0th position of the list.

10. Write a pop function, `pop`, that takes an instance of the Stack type and
    if there is an element on the stack, returns the element on the top of the 
    stack and a modified stack with this element removed, and if there is not,
    returns nothing.

11. Write a peek function, `peek`, that takes an instance of the Stack type and
    if there are at least two elements on the stack, returns the element below
    the top of the stack, and otherwise returns nothing.



------
Sources
------

