New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   2: Head, Tail, and Mutual Recursion

------
Name:
------

To download this document directly

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part3.lhs

You can find more info on `wget` and what these flags do at note [1].

------


mututal recursion: delete every other node...

https://stackoverflow.com/questions/33923/what-is-tail-recursion



------
End Notes
------
[1] https://gist.github.com/sastrand/4d282b2a29d39805ff1a207fb0687408

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
