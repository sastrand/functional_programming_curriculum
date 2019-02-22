New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   2: Typeclass Membership

> import Books
> import Data.List

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part2.lhs

------
Deriving Typeclass Membership
------

> instance Show Library where
>   show (Library name books) = concat $ map (\x -> "\t" ++ (show x) ++ "\n") books

> instance Show Book where
>   show (Book title auth yr avail) = (title ++ " by " ++ concat auth ++ " (" 
>         ++ show avail ++ ")")

It may be useful to sort a list of books by title.

> instance Ord Book where
>   compare x y = compare (title x) (title y)

To try it out:

> sortLib lib = Library (name lib) (sort $ books lib)

------
Exercises
------

**. Extend the Show and Ord typeclasses to include Person.


    other exercieses








------
Sources:

deriving membership in the Ord typeclass:
http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Ord
https://stackoverflow.com/questions/23257915/sorting-a-list-of-custom-data-types-by-certain-attribute-in-haskell
