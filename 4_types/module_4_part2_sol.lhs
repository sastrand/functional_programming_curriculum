New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   2: Typeclass Membership

> import Books_sol
> import Data.List

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part2.lhs

------
Deriving Typeclass Membership
------

Instead of using the derived `show` method, we can explicitly define how an
instance of a type should be converted to a string by one with the `instance` 
keyword. The definitions below define the `show` methods for the `Library` and 
`Book` class by defining the types `Library` and `Book` as instances of the 
`Show` typeclass. 

> instance Show Library where
>   show (Library addr books) = addr ++ "\n" ++ 
>     (concat $ map (\x -> "\t" ++ (show x) ++ "\n") books)

> instance Show Book where
>   show (Book title auth yr) = title ++ " (" ++ show yr ++ ") by " ++ unwords auth

Likewise, the Ord typeclass contains the `compare` method used by sorting
functions. Defining the `compare` method for a type allows a collection of
those types to be sorted.

> instance Ord Book where
>   compare x y = compare (title x) (title y)

To try it out:

> sortLentOut pat = Patron (name pat) (ph pat) (sort $ lentOut pat)

------
Exercises
------

05b. Extend the Show typeclass to include Patron so that by converting an 
    instance of Patron to a string, the list of books they have checked out 
    print as a bulleted list.

    To try it out, uncomment the print statement below:

> instance Show Patron where
>   show (Patron name ph lentOut) = 
>         "Patron: " ++ name ++ "\n" ++
>         "Phone:  " ++ ph ++ "\n" ++
>         "Books:  \n" ++ (concat $ map (\x -> "  - " ++ show x ++ "\n") lentOut)

> eve = Patron "Eve" "(503) 725-3000" [book1, book2, book3]
> prob5b = putStrLn(show eve)


06. Write a function, `checkout`, that takes a library, a book, and a patron as 
    parameters. If there is a copy of the book available in the library, return 
    a tuple of the Library with the book's count decremented and the Patron with 
    the book added to their checkout list. If the book is not in the library, 
    return Nothing.

> checkout :: Library -> Book -> Patron -> Maybe (Library, Patron)
> checkout lib bk pat
>   | quantAvailable lib (title bk) == Nothing = Nothing
>   | quantAvailable lib (title bk) == Just 0  = Nothing
>   | otherwise = Just (decrementAvail lib bk, lentToPatron pat bk)

> prob6Test1 = checkout libB emma ada == Just (libD, ada2)
> prob6Test2 = checkout libB wool ada == Nothing
> prob6 = do
>         putStrLn ("Test = " ++ if prob6Test1 && prob6Test2 
>                    then "PASS" else "FAIL")


07. Write a method `checkIn` that takes a library, a book, and a patron as
    parameters, increments the quantity available of the book in the library, 
    removes the book from the patron's `lentOut` list and returns the library
    and patron in a tuple.

> checkIn :: Library -> Book -> Patron -> (Library, Patron)
> checkIn lib bk pat = (incrementAvail lib bk, patronReturnsBk pat bk)

> incrementAvail lib bk = 
>   let incrAvail lib bk = map (\(bk', qnt) -> if bk == bk'
>         then (bk', qnt+1) else (bk', qnt)) (books lib)
>   in Library (addr lib) (incrAvail lib bk)

> patronReturnsBk pat bk = 
>   let getFromPat pat bk = [b | b <- lentOut pat, title b /= title bk]
>   in Patron (name pat) (ph pat) (getFromPat pat bk)

> prob7Test = checkIn libD emma ada2 == (libB, ada)
> prob7 = do 
>         putStrLn ("Test = " ++ if prob7Test then "PASS" else "FAIL")


08. Write a function, `mostTitles`, that takes a library and returns the 
    author with the most titles (irrespective of available copies or co-authors) 
    in the library. Ties can be arbitrarily broken.

    For example, one author who has written three of her own books and
    co-authored two others will count as having written five books, regardless
    of how many copies the library has on hand.

    To do this, you may find the `groupBy` function in Data.List and `maximum`
    function in the Prelude helpful. You can find more info on each at:
    http://hackage.haskell.org/package/groupBy-0.1.0.0/docs/Data-List-GroupBy.html
    http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:maximum

> mostTitles :: Library -> String
> mostTitles lib = 
>   let grpByAuth lib = groupBy (==) (getAuths lib)
>   in head $ snd $ maximum $ map (\x -> (length x, x)) (grpByAuth lib)

> getAuths lib = sort $ concat $ map (\(bk, qnt) -> authors bk) (books lib)

> prob8Test1 = mostTitles library1 == "William Gibson"
> prob8Test2 = mostTitles libC     == "Hugh Howey"
> prob8 = do
>         putStrLn ("Test = " ++ if prob8Test1 && prob8Test2 then "PASS" 
>           else "FAIL")


------
Sources:

deriving membership in the Ord typeclass:
http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#t:Ord
https://stackoverflow.com/questions/23257915/sorting-a-list-of-custom-data-types-by-certain-attribute-in-haskell
https://stackoverflow.com/questions/4708028/how-to-find-the-longest-word-in-list
