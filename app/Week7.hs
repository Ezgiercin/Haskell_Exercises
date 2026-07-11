{-# LANGUAGE BlockArguments #-}
module Week7 where

import Test.QuickCheck
import Data.Char
import Control.Monad (when)
import System.IO


go :: Integer-> Integer-> IO Integer
go lo hi = do
    let mid = (lo + hi) `div` 2
    putStrLn ("Is it " ++ show mid ++ "?")
    answer <- getLine

    case answer of
        "yes"-> do
            return 1
        "smaller"-> do
            steps <- go lo (mid- 1)
            return (steps + 1)
        "greater"-> do
            steps <- go (mid + 1) hi
            return (steps + 1)
        _-> do
            putStrLn ("Please answer with: yes, smaller or greater!")
            go lo hi

main :: IO ()
main = do
    putStrLn "Choose a number between 1 and 100!"
    steps <- go 1 100
    putStrLn ("I won in " ++ show steps ++ " attempts!")
    



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




type Stack = [Integer]
type StackOp = Stack -> Stack

readCommand :: String -> StackOp
readCommand input = 
    case words input of
        ["push", n] -> push (read n)
        ["add"] -> add
        ["subtract"] -> sub
        ["multiply"] -> mul
        ["negate"] -> neg
        ["dup"] -> dup
        ["pop"] -> pop
        _ -> id



main2 :: IO ()
main2 = go []
    where
        go :: Stack -> IO ()
        go stack = do
            putStr "> "
            hFlush stdout
            cmd <- getLine
            when (cmd /= "exit") do
                let stack' = readCommand cmd stack
                print stack'
                go stack'