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
>                  } deriving (Eq)

> data Library = Library { addr  :: String,
>                          books :: [(Book, Int)]
>                        } 

Creating a record

> book1 = Book "The Difference Engine: A Novel" 
>              ["William Gibson", "Bruce Sterling"] 2011

> book2 = Book "Parable of the Sower" ["Octavia E. Butler"] 2000

> book3 = Book "Neuromancer" ["William Gibson"] 1986

> library1 = Library "801 SW 10th Ave, Portland, OR" 
>              [(book1, 3), (book2, 1), (book3, 2)]


Accessing a value in a record

  title book1          ==> "Wildflowers of the Pacific Northwest"
  last (authors book1) ==> "Phyllis Gustafson"

------
Maybe: Just and Nothing
------

From the Prelude:

  data Maybe a = Nothing | Just a

> secondAuthor book
>   | length as > 1 = Just $ as!!1
>   | otherwise     = Nothing
>   where as = authors book

------
Exercises
------

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

** Write a function, `mostTitles`, that takes a library and returns the 
   author with the most titles (irrespective of available copies) in the library.

   For this exercise, I was thinking a dictionary would be a good tool. If
   you'd like to try out the dictionary package, you can place the following
   line below the module declaration at the top of this file:

   import qualified Data.Map as Map  
   
   For some examples of how to use the library, Learn You a Haskell chptr 7
   provides some good notes.

> incrAuth auth_qnts bk 
>   | present   = Map.insert (authors bk) 1 auth_qnts
>   | otherwise = Map.insert (authors bk)  (auth_qnts Map.! (authors bk) + 1) 
>                   auth_qnts
>   where present = Map.member (authors bk) auth_qnts

> mostTitles lib = foldl (\acc x -> incrAuth acc (fst x))  Map.empty (books lib) 
