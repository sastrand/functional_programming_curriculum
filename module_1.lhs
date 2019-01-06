New Beginnings Winter 2019
Haskell Lab

------
Name: 
------

Module 1: Weeks 1 and 2

To run this program with ghci, use the command `ghci module_1.lhs`.

------
Part I: Functions over numeric types
------

01. (example) Write a function, `mult3`, that multiplies any number it receives by 3.

> mult3 x = x * 3
> prob1 = print ("mult3 3 = " ++ show (mult3 3))

02. Write a function, `square`, that takes a number and returns the square of 
    that number. 

[FILL IN HERE]

> prob2 = print ("square 5 = " ++ show (square 5))

03. Write a function, `squareSum`, that takes two arguments, squares them both,
    and returns the sum of the two squares.

[FILL IN HERE]

> prob3 = print ("squareSum 2 4 = " ++ show (squareSum 2 4))

04. Write a function, `perfSquare`, that determines if a given number is a 
    perfect square, where a perfect square is the square of an integer.

[FILL IN HERE]

> prob4 = do
>          print ("perfSquare 64 = " ++ show (perfSquare 64))
>          print ("perfSquare 2 = " ++ show (perfSquare 2))

04. In order to avoid the bias from rounding floating points values always 
    away from zero, Haskell uses "unbiased" or "statistician's" rounding in 
    which a value halfway between two integers will be rounded to the 
    nearest even integer. 
    
    So `round 1.5` = 2 and
       `round 2.5` = 2.

    Write a function, `roundAway`, that will round floating point numbers to 
    the nearest value away from zero.

    So `roundAway 1.5` = 2 and
       `roundAway 2.5` = 3.

[FILL IN HERE]

> prob5 = do
>    print("roundAway 1.5 = " ++ show (roundAway 1.5))
>    print("roudnAway 2.5 = " ++ show (roundAway 2.5))
