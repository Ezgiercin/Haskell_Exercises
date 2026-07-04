module Week2 where

import Test.QuickCheck
import Data.Char

push :: Integer -> [Integer] -> [Integer]
push n stack = n : stack

pop :: [Integer] -> [Integer]
pop [] = []
pop (x:xs) = xs

-- Duplicate the top element
dup :: [Integer] -> [Integer]
dup [] = []
dup (x:xs) = x : x : xs

-- Take the two top elements, add them and put the result back on the stack
add :: [Integer] -> [Integer]
add [] = []
add [x] = [x]
add (x:y:xs) = (x + y) : xs

-- Take the two top elements, subtract the second from the first and put the result back on the stack
sub :: [Integer] -> [Integer]
sub [] = []
sub [x] = [x]
sub (x:y:xs) = (x-y) : xs

-- Take the two top elements, multiply them and put the result back on the stack
mul :: [Integer] -> [Integer]
mul [] = []
mul [x] = [x]
mul (x:y:xs) = (x*y) :xs

-- Replace the top element with its negative
neg :: [Integer] -> [Integer]
neg [] = []
neg (x:xs) = (-x) : xs


--PROPERTIES
prop_push :: Integer -> [Integer] -> Bool
prop_push n stack = push n stack == n : stack

prop_pop :: [Integer] -> Bool
prop_pop [] = pop [] == []
prop_pop (x:xs) = pop (x:xs) == xs

prop_dup :: [Integer] -> Bool
prop_dup [] = dup [] == []
prop_dup (x:xs) = dup (x:xs) == (x:x:xs)

prop_add :: [Integer] -> Bool
prop_add [] = add [] == []
prop_add [x] = add [x] == [x]
prop_add (x:y:xs) = add (x:y:xs) == (x+y):xs

prop_sub :: [Integer] -> Bool
prop_sub [] = sub [] == []
prop_sub [x] = sub [x] == [x]
prop_sub (x:y:xs) = sub (x:y:xs) == (x-y):xs

prop_mul :: [Integer] -> Bool
prop_mul [] = mul [] == []
prop_mul [x] = mul [x] == [x]
prop_mul (x:y:xs) = mul (x:y:xs) == (x*y):xs

prop_neg :: [Integer] -> Bool
prop_neg [] = neg [] == []
prop_neg (x:xs) = neg (x:xs) == (-x):xs



head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

--PROPERTIES
prop_head :: Integer -> [Integer] -> Bool
prop_head x xs = head' (x:xs) == head (x:xs)
--prop_head x xs = head' (x:xs) == x

prop_tail :: Integer -> [Integer] -> Bool
prop_tail x xs = tail' (x:xs) == tail (x:xs)
--prop_tail x xs = tail' (x:xs) == xs

prop_last :: Integer -> [Integer] -> Bool
prop_last x xs = last' (x:xs) == last (x:xs)
--prop_last x xs = last' (xs ++ [x]) == x

--Returns the number of elements
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

--Returns True only if all Boolean values are True
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--Returns all elements except the last one
init' :: [a] -> [a]
init' [] = error "empty list"
init [x] = []
init' (x:xs) = x : init' xs

--Concatenates two lists
(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = x : (xs +++ ys)

--Pairs elements of two lists into tuples
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

--Reverses a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]