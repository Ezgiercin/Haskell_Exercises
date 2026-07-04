module Week5 where

import Test.QuickCheck
import Data.Char
import Data.List

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' x = x ++ cycle' x


foldl :: (b-> a-> b)-> b-> [a]-> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (z `f` x) xs


foldr :: (a-> b-> b)-> b-> [a]-> b
foldr _ z [] = z
foldr f z (x:xs) = x `f` foldr f z xs
