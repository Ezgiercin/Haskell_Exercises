module SampleExam where

import Test.QuickCheck
import Data.Char

--implement without explicit recursion

--counts how many elements in the list have an absolute value strictly greater than 10.
f :: (Ord a, Num a) => [a] -> Int
f xs = length (filter(\x -> abs x > 10) xs)

--counts how many times the given element occurs in the list
g :: (Eq a) => [a] -> a -> Int
g xs y = length (filter(\x -> x == y) xs)


--returns the sum of all elements whose index is multiple of 3(index starting at 0)
h :: Num a => [a] -> a
h xs = sum [x | (i,x) <- zip [0..] xs, i `mod` 3 == 0]


--lazy evalution

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ q [] = [q]
scanl f q (x:xs) = q : scanl f (f q x) xs

--using scanl generate an infinite list of powers of 2, where powersOf2 !! n = 2^n
powersOf2 :: [Integer]
powersOf2 = scanl (*) 1 (repeat 2)


--given a number x returns an infinite list [1, x, x^2, x^3, ...]
powers :: Num a => a -> [a]
powers x = scanl (*) 1 (repeat x)


--given a number n and a list(possibly infinite) returns the list(possibly infinite) where every n-th element is removed
everyNth :: Int -> [a] -> [a]
everyNth n xs = [x | (i,x) <- zip [1..] xs, i `mod` n /= 0]


--IO Monad

numberOfChar :: IO ()
numberOfChar = do
    line <- getLine
    print (length line)


untilStop :: IO ()
untilStop = do
    input <- getContents
    let ls = lines input
        beforeStop = takeWhile (/="stop") ls
    print (length beforeStop)


nonEmpty :: IO ()
nonEmpty = do
    content <- readFile "input.txt"
    let allLines = lines content
        nonEmptyLines = filter (not . null) allLines
        count = length nonEmptyLines
    print count

