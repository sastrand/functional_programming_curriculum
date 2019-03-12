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
to side-effect safely and explicity entails a new set of language features. 

I/O is one instance of doing something, and of all the kinds of I/O we can do,
we'll start with reading from a writing to a file.

------
File I/O
------

* do notation
* the I/O monad

> getWhitelist = fmap Text.lines $ Text.readFile "whtlist.txt"

------
Exercises
------

**. Write a function, `getDotProd` that opens the file `someVectors.txt` and
    returns the dot product of the two vectors inside of it.

> dotProd :: [Int] -> [Int] -> Int
> dotProd xs ys = sum $ zipWith (*) xs ys

> getDotProdFmLsts :: [[Int]] -> Int
> getDotProdFmLsts xss = dotProd (head xss) (last xss)

> mklsts :: String -> [[Int]]
> mklsts x = map read $ lines x

> getDotProd = do
>                input <-readFile "someVectors.txt"
>                return $ getDotProdFmLsts $ mklsts input

**. Write a function, `whitelist`, that reads in all the IPs in the file
    `traffic.txt` and all the IPs in the file `white_list.txt` and returns the 
    number of IP addresses in the traffic file that were not in the white list 
    file.

> mkset :: String -> Set.Set String
> mkset s = Set.fromList (lines s)

> whitelist' = do
>                traffic <- readFile "traffic.txt"
>                whtlist <- readFile "whtlist.txt"
>                return $ Set.size $ Set.difference (mkset traffic) (mkset whtlist)


**. Write a function, `redactor`, that takes a file name and a list of 
    strings to redact from that file and replaces all the confidential strings in
    the original file with the string `REDACTED`, writing the result to
    `output.txt`.

> toRedact = ["Discs", "disc", "Circular", "Flying", "elliptical", "domed", 
>             "Metallic", "object", "objects"]
   
> redactWords rs [] = []
> redactWords rs (w:ws)
>   | w `elem` rs = "REDACTED" : redactWords rs ws
>   | otherwise = w : redactWords rs ws

> redactFromStr s = map (redactWords toRedact) (map words (lines s))

> redactFromFile :: String -> String
> redactFromFile s = unlines $ map unwords (redactFromStr s)

> redactor fpath = do
>              input <- readFile fpath
>              writeFile "output.txt" $ redactFromFile input

