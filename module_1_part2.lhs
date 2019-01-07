New Beginnings Winter 2019
Haskell Lab

Module 1: Weeks 1 and 2

------
library code we'll use
------

> import Data.List


------
Functions over lists and tuples
------

06. Write a function, `isAnagram`, that takes two strings and determines 
    if they are anagrams of one another.

> isAnagram :: Ord a => [a] -> [a] -> Bool
> isAnagram x y = if sort x == sort y
>                    then True
>                    else False

> prob6Test = isAnagram "the morse code" "here come dots" 
>             && not (isAnagram "foo" "nope")
> prob6 = do
>         putStrLn ("isAnagram \"the morse code\" \"here come dots\" = " 
>                   ++ show (isAnagram "silent" "listen"))
>         putStrLn ("isAnagram \"foo\" \"nope\" = " 
>                   ++ show (isAnagram "foo" "nope"))
>         putStrLn ("Test = " ++ if prob6Test then "PASS" else "FAIL")


07. Write a function, `isPalindrome`, that takes a string and determines 
    if it's a palindrome.

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome x = if reverse x == x
>                   then True
>                   else False

> prob7Test = isPalindrome "tacocat" && not (isPalindrome "taco")
> prob7 = do 
>           putStrLn ("isPalindrome \"tacocat\" = " ++ show (isPalindrome "tacocat"))
>           putStrLn ("isPalindrome \"taco\" = " 
>                     ++ show (isPalindrome "taco"))
>           putStrLn ("Test = " ++ if prob7Test then "PASS" else "FAIL")


08. Write a function, `toPalindrome`, that takes a string and makes it 
    a palindrome if it's not
    already, so 'hannah' would stay 'hannah' but 'cab' would become 'cabbac'.

> toPalindrome :: Eq a => [a] -> [a]
> toPalindrome x = if isPalindrome x 
>                     then x
>                     else x ++ reverse x

> prob8Test = toPalindrome "hannah" == "hannah" 
>             && toPalindrome "cab" == "cabbac"
> prob8 = do
>           putStrLn ("toPalindrome \"hannah\" = " ++ show (toPalindrome "hannah"))
>           putStrLn ("toPalindrome \"cab\" = " ++ show (toPalindrome "cab"))
>           putStrLn ("Test = " ++ if prob8Test then "PASS" else "FAIL")


09. Write a function, `findPalindromes`, that takes a list of words and 
returns those that are palidromes.

> findPalindromes :: Eq a => [[a]] -> [[a]]
> findPalindromes x = filter isPalindrome x

> prob9Test = findPalindromes ["civic", "shahs", "zorro"] 
>               == ["civic", "shahs"]
> prob9 = do
>           putStrLn ("findPalindromes [\"civic\", \"shahs\", \"zorro\"] = " 
>                    ++ show (findPalindromes ["civic", "shahs", "zorro"]))
>           putStrLn ("Test = " ++ if prob9Test then "PASS" else "FAIL")


10. Write a function, `findRevs`, that takes two lists of words,
    and returns a list of Boolean values indicating which pairs of 
    words corresponding across the lists are the reverse of one another.

> findRevs :: Eq a => [[a]] -> [[a]] -> [Bool]
> findRevs x y = map isPalindrome (zipWith (++) x y)

> prob10Test = findRevs ["cab", "tac", "eye"] ["cab", "cat", "eye"]
>                == [False, True, True]
> prob10 = do
>            putStrLn ("findRevs [\"cab\", \"tac\", \"eye\"]" 
>                       ++ " [\"cab\", \"cat\", \"eye\"] = " 
>                       ++ show (findRevs ["cab", "tac", "eye"] ["cab", "cat", "eye"]))
>            putStrLn ("Test = " ++ if prob10Test then "PASS" else "FAIL")


11. Write a function, `quadEq`, that takes three arguments `a`, `b`, and `c`
    representing the three coefficients in a second degree algebraic equation,
    `a*(x^2) + b*x + c = 0`, and returns the possible values of x in a tuple.

> quadEq :: Floating b => b -> b -> b -> (b, b)
> quadEq a b c = (((-1 * b) + sqrt(b^2 - 4*a*c))/(2*a), 
>                  ((-1 * b) - sqrt(b^2 - 4*a*c))/(2*a))

> prob11Test = quadEq 1 2 (-3) == (1.0,-3.0)
> prob11 = do
>            putStrLn ("quadEq 1 2 (-3) = " ++ show (quadEq 1 2 (-3)))
>            putStrLn ("Test = " ++ if prob11Test then "PASS" else "FAIL")


12. Implement FizzBuzz over lists. That is, write a function, `fizzBuzz`, that
    takes a list of numbers and returns a new list in which each value of the 
    original list is replaced with "Fizz" if its is divisible by 3, "Buzz" if 
    it is divisible by 5, "FizzBuzz" if it is divisible by 3 and 5, and the
    original value as a string if it is divisible by neither.

    So an input of [1..15] would return:

    ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz",
     "11", "Fizz", "13", "14", "FizzBuzz"]

> fizzBuzzHelper :: (Integral a, Show a) => a -> [Char]
> fizzBuzzHelper x = if mod x 15 == 0
>                       then "FizzBuzz"
>                       else if mod x 5 == 0
>                         then "Buzz"
>                         else if mod x 3 == 0
>                           then "Fizz"
>                           else show x

> fizzBuzz :: (Show a, Integral a) => [a] -> [[Char]]
> fizzBuzz x = map fizzBuzzHelper x

> prob12Test = fizzBuzz [1..15] == ["1", "2", "Fizz", "4", "Buzz", "Fizz",
>                                   "7", "8", "Fizz", "Buzz", "11", "Fizz",
>                                   "13", "14", "FizzBuzz"]
> prob12 = do
>            putStrLn ("fizzBuzz [1..15] = " ++ show (fizzBuzz [1..15]))
>            putStrLn ("Test = " ++ if prob12Test then "PASS" else "FAIL")

