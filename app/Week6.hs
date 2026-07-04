module Week6 where

import Test.QuickCheck
import Data.Char

data Vec = Vec Double Double

instance Eq Vec where
    Vec x1 y1 == Vec x2 y2 = x1 = x2 && y1 = y2

instance Show Vec where
    show (Vec x y) = "Vec " ++ show x ++ " " ++ show y

instance Num Vec where
    Vec x1 y1 + Vec x2 y2 = Vec (x1 + x2) (y1 + y2)
    Vec x1 y1 * Vec x2 y2 = Vec (x1 * x2) (y1 * y2)




data Bool2 = No | Yes 

instance Eq Bool2 where
    No == No = True
    Yes == Yes = True
    _ == _ = False

instance Show Bool2 where
    show No = "No"
    show Yes = "Yes"

instance Ord Bool2 where
    No <= No = True
    No <= Yes = True
    Yes <= No = False
    Yes <= Yes = True

instance Enum Bool2 where
    toEnum 0 = No  --toENnum :: Int -> type
    toEnum 1 = Yes
    toEnum _ = error "Invalid Bool2"

    fromEnum No = 0
    fromEnum Yes = 1  --fromEnum :: type -> Int

instance Bounded Bool2 where
    minBound = No
    maxBound = Yes



data Color = Color Int Int Int

instance Eq Color where
    Color r1 g1 b1 == Color r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
    show (Color r g b) = "Color " ++ show r ++ " " ++ show g ++ " " ++ show b

helper :: Int -> Int
helper x = max 0 (min 255 x)

instance Num Color where
    Color r1 g1 b1 + Color r2 g2 b2 = Color (helper (r1 + r2)) (helper (g1 + g2)) (helper (b1 + b2))
    Color r1 g1 b1 * Color r2 g2 b2 = Color (helper (r1 * r2)) (helper (g1 * g2)) (helper (b1 * b2))