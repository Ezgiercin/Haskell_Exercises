ones :: [Int]
ones = [1,1,1]

a = 3:2:1:[]
b = [3,2,1]

c :: [[Bool]]
c = (True : []) :[]

d = 5.1

summarize :: [String] -> String
summarize [] = "None"
summarize [x] = "Only " ++ x
summarize [x,y] = "Two Things: " ++ x ++ " and " ++ y
summarize [_,_,_] = "Three words, C don't care which"
summarize (x:xs) = "First " ++ x ++ " and then " ++ concat xs


-- doubles [3,6,12] = [6,12,24]
doubles :: [Integer] -> [Integer]
doubles [] = []
doubles (x:xs) = (2*x) : (doubles xs)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x          = x : filter' p xs
                 | otherwise    = filter' p xs

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show)