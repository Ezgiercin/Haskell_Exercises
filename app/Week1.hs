module Week1 where

import Test.QuickCheck
import Data.Char

maxi :: Int -> Int -> Int
maxi x y = if x >= y then x else y

mini :: Int -> Int -> Int
mini x y = if x < y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Int, Int, Int) -> Int
max3Tupled (x, y, z) = maxi x (maxi y z)

med :: Int -> Int -> Int -> Int
med x y z = if (x >= y && x <= z) || (x <= y && x >= z) then x
             else if (y >= x && y <= z) || (y <= x && y >= z) then y
             else z

med2 :: Int -> Int -> Int -> Int
med2 x y z = (x + y + z) - max3 x y z - mini x (mini y z)