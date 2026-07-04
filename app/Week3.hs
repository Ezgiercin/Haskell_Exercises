module Week3 where

import Test.QuickCheck
import Data.Char

--undup keeps only the first occurrence of each element and removes all later duplicates.
--elemanları karşılaştırılabilir(Eq a) bir türdeki listeyi alır ve tekrar eden elemanları tek bir kopya bırakacak şekilde siler
undup' :: Eq a => [a] -> [a]
undup' [] = []
undup' (x:xs) = x : undup' (filter (/= x) xs)

--smallestFactor computes the smallest factor of a given Integer.
smallestFactor' :: Integer -> Integer
smallestFactor' n = head (filter divides_n [2..n])
    where
        divides_n k = n `mod` k == 0



facto :: Integer -> Integer
facto n = head $ filter divides_n [2..n]  -- head $ filter divides_n [2..n]    head (filter divides_n [2..n]) anlamına gelir
    where
        divides_n k = mod n k == 0

factor :: Integer -> Integer
factor n = head $ filter (\k -> mod n k == 0) [2..n]


factor' :: Integer -> Integer
factor' n = factor'' n [2,3..n]

factor'' :: Integer -> [Integer] -> Integer 
factor'' n [] = n
factor'' n (k:ks)
        | mod n k == 0 = k
        | otherwise = factor'' n ks

