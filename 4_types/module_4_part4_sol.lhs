New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   4: I/O

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part4.lhs

> import Network.HTTP

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

I/O is one instance of doing something, and we'll work with it here 

    * read from and write to a file
    * interacting with other parts of the system (eg. the network)
    * create an interactive program that can run without the interpreter

In this module, we'll build an application that does both of those things.

------
Reading from a file
------

Say you have an API key and--reponsibly--you don't want to store it in your
code, but instead you choose to save it in a `keys.txt` file somewhere else on
your system.

To access the values in `keys.txt` you need to read them into a data strucutre.
For now, assuming there is only one key in keys.txt, just pulling it out of the
file and saving it as a string will work.

> gotBack = getResponseBody (getRequest "http://api.adviceslip.com/advice")


------
Exercises
------

** The Euclidean definition of the integer modulus operation differs from the
   C-style or "Knuth division" mod when one of the arguments in negative.
 
   The Euclidean definition of mod says the result should always be non-
   negative:

        a mod b = r, where a = b * q + r and (0 <= r < b)

   So with this convention -7 % 3 = 2

        as -7 = 3 * q + r
        which, given (0 <= r < b), must be -7 = 3 * -3 + 2.

   The C99 Standard defines the mod operation differently:

   "Modular division returns the remainder produced after performing integer 
   division on the two operands. The operands must be of a primitive integer 
   type."

        So for C -7 % 3 = -1

   Haskell uses the Euclidean definition in its `mod` function and the Knuth
   division definition in its `rem` function.

   Haskell is not the only language to make this distinction, and when working
   with both, it can be helpful to have a tool to compare the results as a
   sanity check.

   Write a command line calculator that takes two arguments separated by a
   space and returns the result of both of these operations. 


** Write a function, `clearance`, that takes a file path to a document and
   returns True if the document contains the string "SECRET" or the string
   "CONFIDENTIAL" and False otherwise.


------
References
------

https://stackoverflow.com/questions/5891140/difference-between-mod-and-rem-in-haskell
