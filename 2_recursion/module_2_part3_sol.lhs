New Beginnings Winter 2019
Haskell Lab

Module 2: Recursion
Part   3: Head and Tail Recursion

> import Data.Char

------
Name:
------

    wget -np -nH --cut-dirs 2  http://web.cecs.pdx.edu/~sastrand/module_2_part2.lhs

------
Recursion
------

At the theoretical level, recursion is a way of defining an operation
inductively over the range of its potential inputs. 

Inductive proofs work well to show that a quality will hold over every value in
an infinite range that has a defined start. Such as all the positive integers,
in which case our defined start is 0 or 1. Every other positive integer comes
after this point. Likewise to prove a quality over every string, we can define 
every possible string as being the empty string or a single character, plus any 
number of additional characters. 

Identifying that we want to do work over an infinite series with a defined
start is the first step to designing a recursive algorithm.

From there, we're interested in the work that needs to be done on one element
relative to all the other elements between it and the base case. 

We can think of this like building a railroad. We have some terminus--a station
somewhere--at which point our railroad will end. Under every piece of track are 
52 railroad ties, so our process of building the railroad is to lay 52 ties, 
lay two tracks, do some welding, and do it again.

To build a railroad iteratively, we ask where the railroad should start and 
where it should stop. We calculate how many times we will need to do our
5e-tie-track-weld operation between the two, and then we do exactly that 
exactly that many times.

To build a railroad recursively, we don't require this calculation. Instead 
we can say 'drop me anywhere in the world, tell me in which direction is the 
terminus, and I will perform the 52-tie-track-weld operation continuously in 
that direction until I look up and have arrived at the terminus'.

The work that is being done is identical. In recursion, we're recognizing that
our work has the property of being a potentially infinite sequence with a
defined start, and we're leveraging that property to describe the work that will
be done in (generally) simpler terms.

Because the computational work that is being done in equivalent recursive and 
iterative algorithms is identical except for how we describe it, we can readily 
translate one type of algorithm into the other.

Let's look at the factorial function from module 3 part 1 for an example:

> factorial :: (Ord p, Num p) => p -> p
> factorial 1 = 1
> factorial n = n * factorial (n-1)

We have the base case that the factorial of 1 is 1. And we define the factorial
for all other positive integers as that integer multiplied by the factorial of
one less than that integer. We will do this again and again until we look up
and have arrived at the base case.

A recursive function gives us a clear base case, and we don't need to know how
many times we will do our single unit of work between our starting point and
this base case. On the other hand, an iterative function gives us the count of
how many times we need to do this work, but doesn't give us the base case. And
converting one of these facts to another can be done with arithmetic.

Here we see that we will perform our multiplication work n times for any given
n. So we can re-write this function (in Python)

    def fact(n):
      ret = 1
      for i in range(n, 1, -1):
        ret = ret * i
      return ret

Likewise, we can go in the other direction and transform a loop into an
instance of recursion.

Here's a function that pair-wise adds the elements of two lists together:

    def zip_add(xs, ys):
      ret = []
      for i in range(min(len(xs), len(ys))):
        ret.append(xs[i] + ys[i])   
      return ret

We see we're going to move through each element of the two lists,

The first thing we do is work out how many loop iterations we'll need. Then we
define what we'll do for each loop and how we will relate that work to the work
that came before. All the information we need to move from this iterative code
to equivalent recursive code is present.

To do this, we identify the base case, that is, how we will know when we're
done. In the for loop we see that it will finish when it has exhausted the
elements in one of the two input lists. So once we encounter the end of one of
the two input lists, we know we're done.

The work that's being done inside the append statement is clear: we're going to
take two elements that correspond to the same positions in the input list and
add them together. And finally, the relationship between one iteration and
the previous iterations we see in the `append` method of the return list. We'll
add our results on to the end of a new list and keep moving forward.

Let's re-write these ideas in code:

> zipAdd [] ys = []
> zipAdd xs [] = []
> zipAdd (x:xs) (y:ys) = x+y : zipAdd xs ys

------
Recursion optimization
------

So far we've considered recursion at the theoretical level and seen it allows
us to reason about and describe work from a different perspective. This is
valuable in its own right. Compared to loops, recursive algorithms tend to
require fewer variables--with no need to store intermediate results or sentry
values--and having fewer variables means there is less opportunity for
programmer error and the code tends to be more readable.

Unfortunately, implemented literally, recursion comes at a significant
performance cost. In the `fact` and `rev` functions above, we make n recursive
calls for an input of size n. If each of these calls is given a full stack
frame, as one might expect it would, our function will require O(n) space on 
the stack.

The iterative implementation of `fact` requires O(1) space and while the
iterative implementation of `rev` uses O(n) space, we could improve this if we
were allowed to modify the input list. In both cases, we have no need for more 
than one stack frame.

We just saw that we can convert a recursive function to an iterative function
by doing some arithmetic and re-writing. In general, we would like the
machinery of our language implementation (our compiler or interpreter) to do 
this for us, but to do this transformation efficiently, it will need some help.

------
Tail recursion
------

A tail recursive function is one in which there is no work done in the function
after the recursive call. Most of the recursion we have seen so far follows 
this pattern. For example, in our zipAdd example from earlier: 

 zipAdd [] ys = []
 zipAdd xs [] = []
 zipAdd (x:xs) (y:ys) = x+y : zipAdd xs ys

The recursive case of the function leaves the recursive call to be the last
piece of the last expression. If you picture the partially computed return list
in a stack, each stack frame holds one element of this return list. When each
frame's recursive call to `zipAdd` returns, it prepends its one element onto
that return list and returns the result. There's no computational work left to
be done.

Because all the computational work of a tail recursive function is done as it
progresses through its input, you can use the recursive call chain to store
return values as if they were in a list. If your algorithm needs to move 
forward and backward through its input, doing computation when it sees a value
the first time, going through the rest of the values, and then passing back
over the results a second time to do some more work, "traditional" or head
recursion will serve you better.


------
Head recursion
------

In tail recursion, as we progress through a recursive call, we use the space in
the call chain behind us as a list or running total that can be returned once 
the recursive call hits the end of the input. In head recursion, we use this 
space like a stack. Once the recursive call hits the base case, it turns
around, moving back through the stack chain and doing more work at each frame. 

Let's check out an example:

> revAndCap [] = []
> revAndCap (x:xs) = revAndCap xs ++ [toUpper x]

In this case, after the recursive call to `revAndCap` returns we still have the
work to do of capitalizing the element popped off the head of the list at each
call.

Head recursion can be converted into a loop as well, generally by making use of
a stack as an intermediate buffer to hold the intermediate values generated from
the first call to the base case. For example:

    def to_upper(c):
      return str(c).upper()[0]

    def rev_and_cap(s):
      stack = ""
      for i in range(len(s)):
        stack = to_upper(s[i]) + stack
      return stack

Every head recursive call can also be converted to a tail recursive call by 
adding an extra parameter to the recursive function that takes a procedure to 
be invoked on the return value. This is called continuation passing style, and
is well-supported in Haskell, but outside our scope here.

While any head recursive call can still be converted into a loop, this
optimization requires more overhead and is not as common in language
implementations as tail recursive optimizations.


------
Exercises
------

01. The following Python function exhaustively searches a list for one element.

    def ex_search(n, xs):
      for x in xs:
        if x == n:
          return True
      return False
    
    Write a function, `exhaustiveSearch`, in Haskell to do the same work:

> exhaustiveSearch :: Eq t => t -> [t] -> Bool  
> exhaustiveSearch n [] = False
> exhaustiveSearch n (x:xs)
>   | n == x    = True
>   | otherwise = False || exhaustiveSearch n xs


> prob1Test1 = exhaustiveSearch 4 [1,7,2,5] == False
> prob1Test2 = exhaustiveSearch 'l' "hello" == True
> prob1 = do
>         putStrLn ("exhaustiveSearch 4 [1,7,2,5] = " ++ show 
>           (exhaustiveSearch 4 [1,7,2,5]))
>         putStrLn ("exhaustiveSearch \'l\' \"hello\" = " ++ show
>           (exhaustiveSearch 'l' "hello"))
>         putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" 
>           else "FAIL")


02. One could approximate the base-2 log of a number by counting how many times
    it can be divided by 2 with its remainder disregarded until the result of 
    that division is 1.

    Write a function `floorLog2` to do this. 

> floorLog2 :: (Num p, Integral t) => t -> p
> floorLog2 1 = 0
> floorLog2 n = 1 + floorLog2(n `div` 2)

> prob2Test1 = floorLog2 32 == 5
> prob2Test2 = floorLog2 256 == 8
> prob2 = do
>          putStrLn ("floorLog2 32" ++ show(floorLog2 32))
>          putStrLn ("floorLog2 256" ++ show(floorLog2 32)
>          putStrLn ("Test = " ++ if prob1Test1 && prob1Test2 then "PASS" 
>            else "FAIL")


------
Aside
------

An additional syntax note:

The `$` that you'll see in Haskell is a shorthand for normal function application 
over everything from the `$` to the end of the line.

For example:

> last' = head $ reverse [1,2,3,4]

This is "syntactic sugar" for using more parentheses:

> last'' = head ( reverse [1,2,3,4] )


------
End Notes
------

For more info on the `wget` command at the top of the file:
https://gist.github.com/sastrand/4d282b2a29d39805ff1a207fb0687408

[1] More information about the "Divide By Two" algorithm:
    http://interactivepython.org/courselib/static/pythonds/BasicDS/
    ConvertingDecimalNumberstoBinaryNumbers.htmlu

