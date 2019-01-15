------
Aside
------

An additional syntax note:

The `$` that you'll see in Haskell is a shorthand for normal function application 
over everything from the `$` to the end of the line.

For example:

> last' = head $ reverse [1,2,3,4]

is "syntactic sugar" for using more parentheses:

> last'' = head ( reverse [1,2,3,4] )

Both last' and last'' will give us the last element in the list.