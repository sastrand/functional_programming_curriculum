New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   1: Algebraic Data Types

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part1.lhs

------
Algebraic Data Types
------

> module Books where
> import qualified Data.Map as Map  

> data Book = Book { title     :: String,
>                    authors   :: [String],
>                    year      :: Int
>                  } deriving (Eq, Show)

> data Library = Library { addr  :: String,
>                          books :: [(Book, Int)]
>                        } deriving (Eq, Show)

Creating a record

> book1 = Book "The Difference Engine: A Novel" 
>              ["William Gibson", "Bruce Sterling"] 2011

> book2 = Book "Parable of the Sower" ["Octavia E. Butler"] 2000

> book3 = Book "Neuromancer" ["William Gibson"] 1986

> library1 = Library "801 SW 10th Ave, Portland, OR" 
>              [(book1, 3), (book2, 1), (book3, 2)]


Accessing a value in a record

  title book1          ==> "The Difference Engine: A Novel"
  last (authors book1) ==> "Bruce Sterling"

------
Maybe: Just and Nothing
------

To return nothing from a function, we need a new type that includes a value for
nothing. Then every return will be of this type--wrapped in its constructors.
Return values that are a thing, x, will be `Just x` and otherwise will be
`Nothing`. The function will now return a `Maybe` type.

From the Prelude:

  data Maybe a = Nothing | Just a

> secondAuthor book
>   | length as > 1 = Just $ as!!1
>   | otherwise     = Nothing
>   where as = authors book

------
Exercises
------

01. Write a function, `buildLibrary`, that will take an address and a list of 
    (Title, [Author], year) tuples and generate a library from it. We can assume
    there is only one copy of each book in the library at first.

> buildLibrary :: Foldable t => String -> t (String, [String], Int) -> Library
> buildLibrary addr bks = foldl (\lib bk -> Library addr ((makeBook bk) 
>  : (books lib))) (Library addr []) bks

> someBooks = [("Emma", ["Jane Austen"], 1815), ("Fences", ["August Wilson"], 1983)]

> makeBook (tl, athrs, yr) = (Book tl athrs yr, 1)

> prob1Test = buildLibrary "123 B St" someBooks 
>   == Library "123 B St" (map makeBook someBooks)
>   || buildLibrary "123 B St" someBooks
>   == Library "123 B St" (map makeBook $ reverse someBooks)

> prob1 = do
>         putStrLn ("Test = " ++ if prob1Test then "PASS" else "FAIL")

** Write a function, `quantAvailable`, that takes a Library and a book's title 
   and if the book is in the library, returns just the quantity of the title 
   available. If the book is not in the library, `quantAvailable` should return 
   nothing.

> quantAvailable lib t
>   | length bk > 0 = Just ((sum . (map snd)) bk)
>   | otherwise     = Nothing
>   where bk = filter (\(bk, qnt) -> title bk == t) (books lib)

** Define a patron data type, `Patron`, that is a name, a phone number, and a 
   list of books currently checked out. 

> data Patron = Patron { name    :: String,
>                        ph      :: String,
>                        lentOut :: [Book]
>                      } 

** Define a function, `lentToPatron`, that takes a patron and a book and adds
   that book to the list of the books lent out to the patron.

> lentToPatron pat bk = Patron (name pat) (ph pat) (bk : lentOut pat)

** Write a function, `decrementAvail`, that takes a library and a book and
   reduces the quantity of that book available in the library by 1.

> decAvail lib bk = map (\(bk', qnt) -> if bk == bk' 
>     then (bk, qnt-1) else (bk, qnt)) (books lib)

> decrementAvail lib bk = Library (addr lib) (decAvail lib bk)


** Write a function, `checkout`, that takes a library, a book, and a patron as 
   parameters. If there is a copy of the book available in the library, return 
   a tuple of the Library with the book's count decremented and the Patron with 
   the book added to their checkout list. If the book is not in the library, 
   return Nothing.

> checkout lib bk pat
>   | quantAvailable lib (title bk) == Nothing = Nothing
>   | quantAvailable lib (title bk) == Just 0  = Nothing
>   | otherwise = Just (decAvail lib bk, lentToPatron pat bk)

**. Write a method `checkIn` that takes a library, a book, and a patron as
    parameters, increments the quantity available of the book in the library, 
    removes the book from the patron's `lentOut` list and returns the library
    and patron in a tuple.


