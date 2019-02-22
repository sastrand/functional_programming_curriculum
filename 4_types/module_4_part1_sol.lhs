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

> data Book = Book { title     :: String,
>                    authors    :: [String],
>                    year      :: Int,
>                    isbn      :: String,
>                    available :: Int
>                  } deriving (Show)

> data Library = Library { name  :: String,
>                          books :: [Book]
>                        } deriving (Show)

Creating a record

> book1 = Book "The Difference ENgine: A Novel" 
>             ["William Gibson", "Bruce Sterling"] 2011 "9780440423621" 0

> book2 = Book "Parable of the Sower" ["Octavia E. Butler"] 2000
>              "9780446675505" 1

> book3 = Book "Neuromancer" ["William Gibson"] 1986 "9780143111603" 2

> library1 = Library "Multnomah County" [book1, book2, book3]


Accessing a value in a record

  title book1         ==> "Wildflowers of the Pacific Northwest"
  last (authors book1) ==> "Phyllis Gustafson"

------
Maybe: Just and Nothing
------

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
>   | length bk > 0 = Just (available (head bk))
>   | otherwise    = Nothing
>   where bk = filter (\x -> title x == t) (books lib)

** Write a function, `bestSeller`, that takes a library and returns the author 
   with the most titles (irrespective of available copies) in the library.

** Define a patron data type, `Patron`, that is a name, a phone number, and a 
   list of books currently checked out.

** Write a function, `checkout`, that takes a library, a book, and a patron as 
   parameters. If there is a copy of the book available in the library, return 
   a copy of the Library with the book's count decremented and a copy of the
   Patron with the book added to their checkout list. If the book is not in the 
   library, return the Library and Patron as they are.

