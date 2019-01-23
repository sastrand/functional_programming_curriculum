New Beginnings Winter 2019
Haskell Lab

Module 1 Extra
Practice with Type Conversions

------
Name:
------

01. Write a function, `perfSquare`, that determines if a given number is a 
    perfect square, where a perfect square is the square of an integer.

    Note: floating point values in Haskell are subject to the same rounding
    errors as they are in any language. It's therefore possible that for some
    perfect squares `sqrt` will return a value with an erroneous factional
    value. The test cases provided here should work for you, but be aware. 

> perfSquare :: (Floating a, RealFrac a) => a -> Bool
> perfSquare = undefined

> prob1Test = perfSquare 64 == True && perfSquare 2 == False
> prob1 = do
>          putStrLn ("perfSquare 64 = " ++ show (perfSquare 64))
>          putStrLn ("perfSquare 2 = " ++ show (perfSquare 2))
>          putStrLn ("Test = " ++ if prob4Test then "PASS" else "FAIL")


02. In order to avoid the bias from rounding floating points values always 
    away from zero, Haskell uses "unbiased" or "statistician's" rounding in 
    which a value halfway between two integers will be rounded to the 
    nearest even integer. 
    
    So `round 1.5` = 2 and
       `round 2.5` = 2.

    Write a function, `roundAway`, that will round floating point numbers to 
    the nearest value away from zero.

    So `roundAway 1.5` = 2 and
       `roundAway 2.5` = 3.

> roundAway :: Integral p => Rational -> p
> roundAway = undefined

> prob2Test = roundAway 1.5 == 2 && roundAway 2.5 == 3
> prob2 = do
>           putStrLn ("roundAway 1.5 = " ++ show (roundAway 1.5))
>           putStrLn ("roudnAway 2.5 = " ++ show (roundAway 2.5))
>           putStrLn ("Test = " ++ if prob5Test then "PASS" else "FAIL")
