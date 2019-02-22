New Beginnings Winter 2019
Haskell Lab

Module 4: Types
Part   3: I/O

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_4_part4.lhs


------
An introduction to I/O in Haskell
------


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
