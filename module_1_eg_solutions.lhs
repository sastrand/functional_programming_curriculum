New Beginnings Winter 2019
Haskell Lab

Module 1: Weeks 1 and 2

------
Part I: Functions over numeric types
------

01. Write a function, `mult3`, that multiplies any number it receives by 3.

> mult3 :: Num a => a -> a
> mult3 x = x * 3

> prob1 = putStrLn ("mult3 3 = " ++ show (mult3 3))

02. Write a function, `square`, that takes a number and returns the square of 
    that number. 

> square :: Num a => a -> a
> square x = x * x

> prob2Test = square 4 == 16
> prob2 = do
>           putStrLn ("square 5 = " ++ show (square 5))
>           putStrLn ("Test = " ++ if prob2Test then "PASS" else "FAIL")

03. Write a function, `squareSum`, that takes two arguments, squares them both,
    and returns the sum of the two squares.

> squareSum :: Num a => a -> a -> a
> squareSum x y = x * x + y * y

> prob3Test = squareSum 2 4 == 20
> prob3 = do
>           putStrLn ("squareSum 2 4 = " ++ show (squareSum 2 4))
>           putStrLn ("Test = " ++ if prob3Test then "PASS" else "FAIL")

04. Write a function, `perfSquare`, that determines if a given number is a 
    perfect square, where a perfect square is the square of an integer.

> perfSquare :: (Floating a, RealFrac a) => a -> Bool
> perfSquare x = if sqrt x - (fromIntegral (truncate (sqrt x))) == 0
>                    then True
>                    else False

> prob4Test = perfSquare 64 == True && perfSquare 2 == False
> prob4 = do
>          putStrLn ("perfSquare 64 = " ++ show (perfSquare 64))
>          putStrLn ("perfSquare 2 = " ++ show (perfSquare 2))
>          putStrLn ("Test = " ++ if prob4Test then "PASS" else "FAIL")

05. In order to avoid the bias from rounding floating points values always 
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
> roundAway x = if x - (toRational (truncate x)) == 0.5 
>                   then if even (truncate x)
>                       then truncate x + 1
>                       else round x
>                   else round x

> prob5Test = roundAway 1.5 == 2 && roundAway 2.5 == 3
> prob5 = do
>           putStrLn ("roundAway 1.5 = " ++ show (roundAway 1.5))
>           putStrLn ("roudnAway 2.5 = " ++ show (roundAway 2.5))
>           putStrLn ("Test = " ++ if prob5Test then "PASS" else "FAIL")
