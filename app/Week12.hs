module Week12 where

import Test.QuickCheck
import Data.Char

--reads two integers from standard input and returns their sum
sumTwo :: IO Int
sumTwo = liftA2 (+) (read <$> getLine) (read <$> getLine)


data Person = Person String Int

--reads a person’s name and age from standard input
readPerson :: IO Person
readPerson = liftA2 Person (getLine) (read <$> getLine)


--reads three integers and returns them as a triple
readTriple :: IO (Int, Int, Int)
readTriple = (,,) <$> (read <$> getLine) <*> (read <$> getLine) <*> (read <$> getLine)