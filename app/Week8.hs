module Week8 where
import Foreign (new)

import Test.QuickCheck
import Data.Char

-- a lowercase ASCII character generator
genLowerChar :: Gen Char
genLowerChar = choose ('a', 'z')

-- a 5-letter lowercase string generator
genLowerString :: Gen String
genLowerString = vectorOf 5 genLowerChar

prop_LowerString :: Property
prop_LowerString = forAll genLowerString $ \s ->
    all isLower s && length s == 5


data Color = Red | Green | Blue deriving (Show, Eq)

-- a generator for Color values
genColor :: Gen Color
genColor = elements [Red, Green, Blue]

prop_ValidColor :: Property
prop_ValidColor = forAll genColor $ \c ->
    c == Red || c == Green || c == Blue


-- a generator for a list of Ints with length between 0 and 10
genIntList :: Gen [Int]
genIntList = do
    len <- choose (0, 10)
    vectorOf len arbitrary

-- a generator for a list of 5-15 integers, each between 1 and 100
genBoundedIntList :: Gen [Int]
genBoundedIntList = do
    len <- choose (5, 15)
    vectorOf len (choose (1, 100))

-- a generator for a list of even numbers between 0 and 100
genEvenList :: Gen [Int]
genEvenList = listOf (elements [0, 2 .. 100])