module Week12 where

import Test.QuickCheck
import Data.Char

--reads two integers from standard input and returns their sum
sumTwo :: IO Int
--sumTwo = liftA2 (+) (read <$> getLine) (read <$> getLine)
sumTwo = (+) <$> readLn <*> readLn


data Person = Person String Int

--reads a person’s name and age from standard input
readPerson :: IO Person
readPerson = pure (Person) <*> readLn <*> readLn


--reads three integers and returns them as a triple
readTriple :: IO (Int, Int, Int)
readTriple = pure (,,) <*> (readLn) <*> (readLn) <*> (readLn)