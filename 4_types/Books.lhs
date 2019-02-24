New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   1: Algebraic Data Types

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/Books.lhs

------
Algebraic Data Types
------

> module Books where

> data Book = Book { title     :: String,
>                    authors   :: [String],
>                    year      :: Int
>                  } deriving (Eq)

> data Library = Library { addr  :: String,
>                          books :: [(Book, Int)]
>                        } deriving (Eq)

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

Note: `where` works outside of guards as well to define an inner function

> getAllAuths lib = map getAuth (books lib)
>   where getAuth bkTup = authors (fst bkTup)

------
Exercises
------

01. Write a function, `buildLibrary`, that will take an address and a list of 
    (Title, [Author], year) tuples and generate a library from it. We can assume
    there is only one copy of each book in the library at first.

> someBooks = [("Emma", ["Jane Austen"], 1815), 
>              ("Rent", ["Jonathan Larson"], 1996)]

> buildLibrary :: String -> [(String, [String], Int)] -> Library
> buildLibrary = undefined

> emma = Book "Emma" ["Jane Austen"] 1815
> rent = Book "Rent" ["Jonathan Larson"] 1996
> wool = Book "Wool" ["Hugh Howey"] 2004
> libA = Library "123 B St" [(emma, 1), (rent, 1)]
> libB = Library "123 B St" [(rent, 1), (emma, 1)]
> libC = Library "456 C St" [(wool, 1)]
> prob1Test1 = buildLibrary "123 B St" someBooks == libA
>           || buildLibrary "123 B St" someBooks == libB
> prob1Test2 = buildLibrary "456 C St" [("Wool", ["Hugh Howey"], 2004)] == libC
> prob1 = do
>         putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 
>           then "PASS" else "FAIL")


02. Write a function, `quantAvailable`, that takes a Library and a book's title 
    and if the book is in the library, returns just the quantity of the title 
    available. If the book is not in the library, `quantAvailable` should return 
    nothing.

> quantAvailable :: Library -> String -> Maybe Int
> quantAvailable = undefined

> libD = Library "123 B St" [(rent, 1),(emma, 0)]
> prob2Test1 = quantAvailable libB "Rent" == Just 1
> prob2Test2 = quantAvailable libC "Emma" == Nothing
> prob2Test3 = quantAvailable libD "Emma" == Just 0
> prob2 = do
>         putStrLn ("Test = " ++ if foldl (&&) True [prob2Test1, prob2Test2,
>           prob2Test3] then "PASS" else "FAIL")


03. Replace the definition of Patron below using record syntax so you can
    access individual fields by their names and derive its membership in the
    Eq typeclass.

> data Patron = Patron String String [Book] deriving (Eq)


04. Define a function, `lentToPatron`, that takes a patron and a book and adds
    that book to the list of the books lent out to the patron.

> lentToPatron :: Patron -> Book -> Patron
> lentToPatron = undefined

> ada = Patron "Ada" "(503) 823-4000" []

> ada2 = Patron "Ada" "(503) 823-4000" [emma]
> prob4Test1 = lentToPatron ada emma == ada2
> prob4 = do
>         putStrLn ("Test = " ++ if prob4Test1 then "PASS" else "FAIL")


05. Write a function, `decrementAvail`, that takes a library and a book and
    reduces the quantity of that book available in the library by 1.

> decrementAvail :: Library -> Book -> Library
> decrementAvail = undefined

> prob5Test = decrementAvail libB emma == libD
> prob5 = do
>         putStrLn ("Test = " ++ if prob5Test then "PASS" else "FAIL")


