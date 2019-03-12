New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   4: I/O

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part4.lhs

> import qualified Data.Text    as Text
> import qualified Data.Text.IO as Text
> import qualified Data.Set     as Set

------
An introduction to I/O in Haskell
------

Interactive mode can only take us so far in Haskell. 

In particular, if we want a program that can *do something* in the world, it
will be helpful to run it directly so a user of the program doesn't have to
interact with every binding in the namespace to find a run() method.

This matter of doing something takes us to the edge of the material we've set
out to cover. For a purely non-side effecting language, introducing the ability
to side-effect safely and explicitly entails a new set of language features. 

I/O is one instance of doing something, and of all the kinds of I/O we can do,
we'll start with reading from a writing to a file.

------
File I/O
------

Like the Maybe type that can be thought of as `Nothing` or a value wrapped in
`Just`. The IO type wraps values from the outside world. 

Both Maybe types and IO types have the potential to be something unable to be
used in calculation, so they must be isolated from the "pure" parts of the
program in which everything has a definite type that is known at compile time.

In order to do something with a maybe, we carefully unpacked it, providing the
compiler a backup option every time so that it would have something to do if
there were nothing there:

> maybeAdd3 Nothing = Nothing
> maybeAdd3 (Just x) = Just (x+3)

This works okay for Maybe, given it has only two potential patterns. To read
from a file, for instance, we'll reach out into the world, pull in a bunch of
binary (unless the file can't be opened, or is empty, or doesn't exist) and
then convert that binary to an expected type (unless the parsing fails).

Just like with Maybe, we can wrap up this possibility of typing failure in a
container called a monad. The result of a read or write to a file will then be
a value wrapped in the IO monad, so it will have type IO of something.

------
example
------

To read a file, we can use the `readFile` function in the prelude. Likewise, to
write a file we can use `writeFile`.

In this directory is a file of new-line separated 32-bit IP addresses at
`whtlist.txt`.

> whiteList = readFile "whtlist.txt"
> hi = "hello, file"
> writeHello str = writeFile "hello.txt" str

Let's check out the types of these functions:

> whiteList :: IO String
> writeHello :: String -> IO ()

To do something with whiteList, there is syntax that allows instances of the IO 
type to be lined up one after another and executed in sequence. (gasp).

Within a do block, each statement is essentially imperative. There are some
restrictions: every assignment must be the result of an IO action and there
must be a statement at the end that either performs an IO action without
assigning it or returns a value to be used in another function.

For instance, below is function to read two lists and return their dot product.
The first three functions are helpers used in the fourth function with a do
block.

> dotProd :: [Int] -> [Int] -> Int
> dotProd xs ys = sum $ zipWith (*) xs ys

> getDotProdFmLsts :: [[Int]] -> Int
> getDotProdFmLsts xss = dotProd (head xss) (last xss)

> mklsts :: String -> [[Int]]
> mklsts x = map read $ lines x

> getDotProd :: IO Int
> getDotProd = do
>                input <-readFile "someVectors.txt"
>                return $ getDotProdFmLsts $ mklsts input

Type signatures become important here, as we must tell the compiler what to
expect when it tries to parse input with `read`.

Alternatively, to write to a file in a do block:

> writeDotProd = do
>                  input <- getDotProd
>                  writeFile "dot_prod_output.txt" (show input)

The `show` is necessary because `getDotProd` returns an Int and `writeFile`
expects a String.

The do block itself serves as a wrapper for all the IO activity inside of it,
so inside the do block, each assigned IO value can be treated like its
underlying data type. If you read a string in from a file, its type in the
larger program will be IO String, but inside the do block, you can assign it to
a variable and use it as type String.

------
Notes on helpful functions
------

All of these functions are in the prelude except the functions prefixed by
`Set` which are in the `Set` module that's loaded into this file.

-- problem 1
Set.size
Set.difference
Set.fromList
lines

-- problem 2
lines
words
unlines
unwords

------
Exercises
------


01. Write a function, `whitelist`, that reads in all the IPs in the file
    `traffic.txt` and all the IPs in the file `white_list.txt` and returns the 
    number of IP addresses in the traffic file that were not in the white list 
    file.

> mkset :: String -> Set.Set String
> mkset s = Set.fromList (lines s)

> whitelist :: IO Int 
> whitelist = do
>               traffic <- readFile "traffic.txt"
>               whtlist <- readFile "whtlist.txt"
>               return $ Set.size $ Set.difference (mkset traffic) (mkset whtlist)


02. Write a function, `redactor`, that takes a file name and a list of 
    strings to redact from that file and replaces all the confidential strings in
    the original file with the string `REDACTED`, writing the result to
    `output.txt`.

> toRedact = ["Discs", "disc", "Circular", "Flying", "elliptical", "domed", 
>             "Metallic", "object", "objects"]

> redactWords :: [String] -> [String] -> [String]
> redactWords rs [] = []
> redactWords rs (w:ws)
>   | w `elem` rs = "REDACTED" : redactWords rs ws
>   | otherwise = w : redactWords rs ws

> redactFromStr :: String -> [[String]]
> redactFromStr s = map (redactWords toRedact) (map words (lines s))

> redactFromFile :: String -> String
> redactFromFile s = unlines $ map unwords (redactFromStr s)

> redactor :: FilePath -> IO ()
> redactor fpath = do
>              input <- readFile fpath
>              writeFile "output.txt" $ redactFromFile input

