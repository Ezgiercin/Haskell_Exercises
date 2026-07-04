elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

(===) :: Eq a => (a,a) -> (a,a) -> Bool
(===) (x1,x2) (y1,y2) = x1 == y1 && x2 == y2


(====) :: Eq a => [a] -> [a] -> Bool
(====) [] [] = True
(====) _ [] = False
(====) [] _ = False
(====) (x:xs) (y:ys) = x == y && xs ==== ys

data Size 
    = Small 
    | Medium 
    | Large 
    deriving (Eq,Show)
    

instance Enum Size where
    toEnum 0 = Small
    toEnum 1 = Medium
    toEnum 2 = Large

    fromEnum Small = 0
    fromEnum Medium = 1
    fromEnum Large = 2

--instance Ord Size where
--    compare Small _ = LT
--    compare Large _ = GT
--    compare Medium Small = GT
--    compare Medium Large = LT

instance Ord Size where 

    (<=) a b = fromEnum a <= fromEnum b


f1 :: Show a => a -> String
f1 x = show x

f2 :: Show a => a -> String
f2 = \ x -> show x
