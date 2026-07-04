pick :: Integer -> (a, a) -> a
pick 1 = fst
pick 2 = snd

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' g a b = g (a, b)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

or' :: [Bool] -> Bool
or' xs = foldr' (||) False xs

and' :: [Bool] -> Bool
and' xs = foldr' (&&) True xs

-- flattens a list of lists
concat' :: [[a]] -> [a]
concat' xs = foldr' (++) [] xs

maximum' :: Ord a => [a] -> a
maximum' (x:xs)  = foldr' (max) x xs

takeLine :: String -> String
takeLine s = takeWhile (/= '\n') s

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a] 
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs



lines' :: String -> [String]
lines' [] = []
lines' xs = foldr' f [] xs
  where 
    f '\n' acc = [] : acc -- Start a new line at each \n
    f x [] = [[x]] -- Base case: accumulator is empty, start first line
    f x (line:acc) = (x:line) : acc -- add character to current line

segments' :: (a -> Bool) -> [a] -> [[a]]
segments' _ [] = []
segments' p xs = foldr' f [] xs
  where 
    f x [] = [[x]]
    f x (y:ys) | p x = [] : (y:ys) -- start new segment when predicate matches
            | otherwise = (x : y) : ys -- otherwise, add x to current segment

words' :: String -> [String]
words' = segments' (== ' ')

lines'' :: String -> [String]
lines'' = segments' (== '\n')
