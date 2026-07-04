module Week4 where

import Test.QuickCheck
import Data.Char

filter' :: (a -> Bool) -> [a] -> [a]
--filter' f = foldr (\x xs -> if f x then x : xs else xs) []
filter' f = foldr g []
        where
            g x xs 
                    | f x = x : xs
                    | otherwise = xs



--removes consecutive duplicates
remdup' :: Eq a => [a] -> [a]
remdup' = foldr f []
        where
            f x [] = [x]
            f x (y:ys) 
                        | x == y = y : ys
                        | otherwise = x : y : ys


-- computes the average of a list of Double in a single pass.
avg' :: [Double] -> Double
avg' [] = error "avg of zero elements"
avg' ds = dsSum / dsCount
    where
        (dsSum, dsCount) = foldr f (0,0) ds
        f :: Double -> (Double, Double) -> (Double, Double)
        f d (runningSum, runningCount) = (runningSum + d, runningCount + 1)


        

--recursive, using pattern matching
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs


--non-recursive, in terms of foldr
foldl'' :: (b -> a -> b) -> b -> [a] -> b

