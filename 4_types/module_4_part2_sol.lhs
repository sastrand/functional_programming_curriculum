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

Instead of using the derived `show` method, we can explicitly define one with 
the `instance` keyword for the `Show` typeclass. Within this instance, we're
just defining the `show` method.

> instance Show Library where
>   show (Library name books) = concat $ map (\x -> "\t" ++ (show x) ++ "\n") books

> instance Show Book where
>   show (Book title auth yr avail) = (title ++ " by " ++ concat auth)

Likewise, the Ord typeclass contains the `compare` method used by sorting
functions.

> instance Ord Book where
>   compare x y = compare (title x) (title y)

To try it out:

> sortLib lib = Library (name lib) (sort $ books lib)

------
Exercises
------

**. Extend the Show to include Person so that by converting an instance of
    Patron to a string, the list of books they have checked out print as a 
    bulleted list.


**. Write a function, `mostTitles`, that takes a library and returns the 
    author with the most titles (irrespective of available copies) in the library.


------
Sources:

deriving membership in the Ord typeclass:
http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Ord
https://stackoverflow.com/questions/23257915/sorting-a-list-of-custom-data-types-by-certain-attribute-in-haskell
