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
>                    author    :: String,
>                    year      :: Int,
>                    ISBN      :: String,
>                    available :: Int
>                  } deriving (Show)

> data Library = Library { name  :: String,
>                          books :: [Book]
>                        } deriving (Show)

------
Exercises
------

** Write a function, `quantAvailable`, that takes a Library and a Book and
   if the book is in the library, returns the quantity of the title available. 
   If the book is not in the library, `quantAvailable` should return 0.

** Write a function, `bestSeller`, that takes a library and returns the author 
   with the most titles (irrespective of available copies) in the library.

** Define a patron data type, `Patron`, that is a name, a phone number, and a 
   list of books currently checked out.

** Write a function, `checkout`, that takes a library, a book, and a patron as 
   parameters. If there is a copy of the book available in the library, return 
   a copy of the Library with the book's count decremented and a copy of the
   Patron with the book added to their checkout list. If the book is not in the 
   library, return the Library and Patron as they are.

