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
> buildLibrary addr bks = Library addr (map makeBook bks)
>   where makeBook (tl, athrs, yr) = (Book tl athrs yr, 1)

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
> quantAvailable lib t
>   | length bk > 0 = Just ((sum . (map snd)) bk)
>   | otherwise     = Nothing
>   where bk = filter (\(bk, qnt) -> title bk == t) (books lib)

> libD = Library "123 B St" [(rent, 1),(emma, 0)]
> prob2Test1 = quantAvailable libB "Rent" == Just 1
> prob2Test2 = quantAvailable libC "Emma" == Nothing
> prob2Test3 = quantAvailable libD "Emma" == Just 0
> prob2 = do
>         putStrLn ("Test = " ++ if foldl (&&) True [prob2Test1, prob2Test2,
>           prob2Test3] then "PASS" else "FAIL")


03. Replace the definition of Patron below using record syntax so you can
    access individual fields by their names.

 data Patron = Patron String String [Book]

> data Patron = Patron { name    :: String,
>                        ph      :: String,
>                        lentOut :: [Book]
>                      } deriving (Eq)


04. Define a function, `lentToPatron`, that takes a patron and a book and adds
    that book to the list of the books lent out to the patron.

> lentToPatron :: Patron -> Book -> Patron
> lentToPatron pat bk = Patron (name pat) (ph pat) (bk : lentOut pat)

> ada = Patron "Ada" "(503) 823-4000" []

> ada2 = Patron "Ada" "(503) 823-4000" [emma]
> prob4Test1 = lentToPatron ada emma == ada2
> prob4 = do
>         putStrLn ("Test = " ++ if prob4Test1 then "PASS" else "FAIL")


05. Write a function, `decrementAvail`, that takes a library and a book and
    reduces the quantity of that book available in the library by 1.

> decrementAvail :: Library -> Book -> Library
> decrementAvail lib bk = Library (addr lib) (decAvail lib bk) 
>   where decAvail lib bk = map (\(bk', qnt) -> if bk == bk' 
>         then (bk', qnt-1) else (bk', qnt)) (books lib)

> prob5Test = decrementAvail libB emma == libD
> prob5 = do
>         putStrLn ("Test = " ++ if prob5Test then "PASS" else "FAIL")


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
> checkIn lib bk pat = (incrementAvail lib bk, bkFromPat pat bk)

> incrementAvail lib bk = Library (addr lib) (incrAvail lib bk)
>   where incrAvail lib bk = map (\(bk', qnt) -> if bk == bk'
>         then (bk', qnt+1) else (bk', qnt)) (books lib)

> bkFromPat pat bk = Patron (name pat) (ph pat) (getFromPat pat bk)
>   where getFromPat pat bk = [b | b <- lentOut pat, title b /= title bk]

> prob7Test = checkIn libD emma ada2 == (libB, ada)
> prob7 = do 
>         putStrLn ("Test = " ++ if prob7Test then "PASS" else "FAIL")

