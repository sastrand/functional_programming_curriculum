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

------
Exercises
------

**. Write a function, `whitelist`, that reads in all the IPs in the file
    `traffic.txt` and all the IPs in the file `white_list.txt` and returns the 
    number of IP addresses in the traffic file that were not in the white list 
    file.

> whitelist = do 
>               let mkset = fmap Set.fromList . fmap Text.lines
>               traffic <- mkset (Text.readFile "traffic.txt")
>               whtlist <- mkset (Text.readFile "whtlist.txt")
>               return $ Set.size $ Set.difference traffic whtlist


**. Write a function, `redactor`, that takes a list of file names and a list of 
    strings to redact from those files and writes over the original files with
    the redacted strings replaced with the string `[REDACTED]`.

 redactor = do
              input <- fmap Set.f 


------
References
------

https://stackoverflow.com/questions/5891140/difference-between-mod-and-rem-in-haskell
