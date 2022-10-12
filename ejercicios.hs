-------EJERCICIO 1-----------
--Ejercicio average3Numbers--
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
average3Numbers ::(Fractional a)=> a -> a -> a -> a
average3Numbers x y z = (x+y+z)/3

-------EJERCICIO 2-----------
--Ejercicio isLastDigit3--
isLastDigit3 :: (Integral a) => a ->Bool
isLastDigit3 3=True
isLastDigit3 num
 |ultimo num ==3=True
 |otherwise=False
 where ultimo num=rem num 10

-------EJERCICIO 3-----------
--Ejercicio has3Digits--
has3Digits :: (Integral a)=>a->Bool
has3Digits x
 | x>99 && x<1000 = True
 | otherwise= False

-------EJERCICIO 4-----------
--Ejercicio isNegative--
isNegative :: (Integral a)=>a->Bool
isNegative num
 |num<0=True
 |otherwise=False

-------EJERCICIO 5-----------
--Ejercicio sum2Digits--
sum2Digits:: Int->Int
sum2Digits x
 | x>9 && x<100 = (x `mod` 10)+((x `div` 10) `mod` 10)
 | x>99 = error "El numero no tiene dos digitos "

-------EJERCICIO 6-----------
--Ejercicio even2Digits--
even2Digit:: Int->Bool
even2Digit x
 | x>9 && x<100 = even (x `mod` 10) && even((x `div` 10) `mod` 10)
 | x>99 = error "El numero no es de dos digitos "

-------EJERCICIO 7-----------
--Ejercicio isPrimeNumber--
factores :: Integer -> [Integer]
factores n = [x | x <- [1..19], mod n x == 0]
isPrimeNumber :: Integer -> Bool
isPrimeNumber x = factores x == [1,x]

-------EJERCICIO 8-----------
--8. Ejercicio isEvenAndPrimeNumber
--ejercicio 8
isEvenAndPrimeNumber :: Int -> Bool
isEvenAndPrimeNumber  x = isPrimeNumber x && even(x)

-------EJERCICIO 9-----------
--9. Ejercicio isMultiple
isMultiple :: (Integral a)=> a->a-> Bool
isMultiple x y
 |  mod x y == 0 = True
 |otherwise = False

-------EJERCICIO 10-----------
--Ejercicio isEqual2Digits--
equal2digits :: (Integral a)=> a->Bool
equal2digits x
 |x<10 = False
 |x <=99 = div x 10 == mod x 10

-------EJERCICIO 11-----------
--11. Ejercicio higher
higher:: Int->Int->Int->Int
higher a b c 
 | a>b && a>c = a
 | b>a && b>c = b
 | c>a && c>b = c

-------EJERCICIO 12-----------
--Ejercicio isEvenSum2Number--
isEvenSum2Number ::(Integral a)=> a ->a ->Bool
isEvenSum2Number a b = even (a+b)


------- EJERCICIO 13 -------
--Ejercicio sum2Digit2Number--
sum2Digit2Number :: (Integral a) => a -> a -> a
sum2Digit2Number x y
    |x > 9 && x <100 && y > 9 && y <100 = (div x 10 + mod x 10) + (div y 10 + mod y 10)
    |otherwise = error "Algún número no tiene 2 dígitos"

-------EJERCICIO 14-----------
--Ejercicio sum3Digits--
sum3Digits:: Int->Int
sum3Digits x
 | x>100 && x<1000 =sum [read [y] | y <- show x]
 | x>999= error "El numero no tiene tres digitos"

-------EJERCICIO 15-----------
--Ejercicio equal3Digits--
equals3Digits :: (Integral a) => a -> Bool
equals3Digits num 
 |num >= 100 && num <= 999 = dig1 == dig2 || dig2 == dig3 || dig1 == dig3
 |otherwise = error "El numero no tiene 3 digitos"
 where 
    dig1 = div num 100
    dig2 = mod (div num 10)10
    dig3 = mod num 10
    suma =  dig1 + dig2 + dig3

------- EJERCICIO 16 -------
--Ejercicio positionHigher3Digits--
positionHigher3Digits :: Integer -> String
positionHigher3Digits x
    |(x > 99 && x <1000) && ((max x1 x2 == x1) && (max x1 x3 == x1))= "El mayor se encuentra en la pos 1"
    |(x > 99 && x <1000) && ((max x1 x2 == x2) && (max x2 x3 == x2))= "El mayor se encuentra en la pos 2"
    |(x > 99 && x <1000) && ((max x1 x3 == x3) && (max x2 x3 == x3))= "El mayor se encuentra en la pos 3"
    |otherwise = error "El número no tiene 3 dígitos"
    where
        x1 = div x 100
        x2 = div (mod x 100) 10
        x3 = mod (mod x 100) 10

-------EJERCICIO 17-----------
--Ejercicio palindrome--
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

-------EJERCICIO 18-----------
--Ejercicio safeDivision--
safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then error"No es posible dividir entre cero"
 else Just (n `div` m)

-------EJERCICIO 19-----------
--Ejercicio xor--
xorl :: Bool -> Bool -> Bool
xorl True True = False
xorl True False = True
xorl False True = True
xorl False False = True

-------EJERCICIO 20-----------
--Ejercicio distance--
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
 where
    x' = x1 - x2
    y' = y1 - y2

