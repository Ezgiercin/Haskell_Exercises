ones :: [Integer]
ones = 1 : ones -- recursion is only unfolded as much as you need it

repeat' :: a -> [a]
repeat' x = x : repeat' x

repeat'' :: a -> [a]
repeat'' x = xs
  where xs = x : xs


fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
  -- fib (n+2) = fib (n+1) + fib n

nfib :: Integer -> Integer
nfib n | n == 0 = 0
       | n == 1 = 1
       | otherwise = nfib (n-1) + nfib (n-2)

nfib30 = nfib 30

primes :: [Integer]
primes = sieve [2..]
  
sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs) 

data BTree = Leaf Integer | Branch BTree BTree
  deriving (Show)

mintree :: BTree -> BTree
mintree b = mb
  where
    (ib, mb) = helper ib b
    helper :: Integer -> BTree -> (Integer, BTree)
    helper m (Leaf i) = (i, Leaf m)
    helper m (Branch l r) = 
      let (il, ml) = helper m l
          (ir, mr) = helper m r
      in (min il ir, Branch ml mr)


t =
  Branch
    (Leaf 5)
    (Branch (Leaf 2) (Leaf 8))