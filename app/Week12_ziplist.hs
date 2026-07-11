module Week12_ziplist where

import Test.QuickCheck
import Data.Char
import Prelude hiding (fmap, (<*>), pure)
import qualified Prelude as P

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
fmap g (Z xs) = Z (P.fmap g xs)  -- fmap :: (a -> b) -> ZipList a -> ZipList b

instance Applicative ZipList where
pure x = Z [x]  -- pure :: a -> ZipList a

(Z gs) <*> (Z xs) = Z (P.zipWith ($) gs xs)  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
