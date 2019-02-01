New Beginnings Winter 2019
Haskell Lab

Module 3: Recursive data structures
Part   1: Higher Order Functions 

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_3_part2.lhs

------
Lambdas
------

Like comprehensions, lambdas began as an artificat of functional languages but
have gained very widespread use outside the paradigm because of their
flexibility and concision.

A lambda is a function that is not named.

By comparision, the result of 1/0.099 is a value that is not named.
Anywhere we want to use that value, we can substitute the expression (1/0.099). 
If you need to type it multiple times, it's probably better to name it, but
if you only need to use this value once, giving it a name only makes your code
a little longer and a little harder to understand.

The same is true of functions, if we want a function that will test if a string
is a palindrome, we could write it:

> isPalindrome x = x == reverse x

Then we could write a function that takes a list of strings and returns those
that match our test with the `filter` function.

> securityDeposit = ["tenent", "spackle", "sconce", "repaper"]
> palindromes x = filter isPalindrome x

But if we only needed to do this once, we might as well not bother naming
`isPalindrome`. We can write it as a lambda with the following syntax:

  (\x -> x == reverse x)

This makes our palindromes expression:

> palindromes' x = filter (\x -> x == reverse x) x

Did we save a lot of keystrokes, not really, but we can understand
`palindromes'` without needing to look up the one-line function `isPalindrome`.





