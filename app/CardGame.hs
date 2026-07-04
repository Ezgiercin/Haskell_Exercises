module CardGame where
import Foreign (new)

main :: IO ()
main = putStrLn "Hello Haskell"




data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Eq)

instance Show Suit where
    show Spades   = "&spades;"
    show Hearts   = "&hearts;"
    show Diamonds = "&diams;"
    show Clubs    = "&clubs;"


data Color = Black | Red
    deriving (Show, Eq)

color :: Suit -> Color
color Spades   = Black
color Hearts   = Red
color Diamonds = Red
color Clubs    = Black


data Rank = Numeric Integer | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace                 = False
rankBeats Ace _                 = True
rankBeats _ King                = False
rankBeats King _                = True
rankBeats _ Queen               = False
rankBeats Queen _               = True
rankBeats _ Jack                = False
rankBeats Jack _                = True
rankBeats (Numeric n1) (Numeric n2) = n1 > n2

rankBeats' :: Rank -> Rank -> Bool
rankBeats' r1 r2 = r1 > r2


data Card = Card { rank :: Rank, suit :: Suit}
    deriving (Show, Eq)

-- rank :: Card -> Rank
-- rank (Card r s) = r

-- suit :: Card -> Suit
-- suit (Card r s) = s

cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = (suit givenCard == suit c) && rankBeats (rank givenCard) (rank c)

cardBeats' :: Card -> Card -> Bool
cardBeats' (Card gr gs) (Card r s) = (gs == s) && rankBeats gr r


data Hand = Last Card | Next Card Hand
    deriving (Show, Eq)

topCard :: Hand -> Card
topCard (Last c)     = c
topCard (Next c _)   = c

-- choose a beating card, if possible
chooseCard :: Card -> Hand -> Card
chooseCard gc (Last c) = c
chooseCard gc (Next c h) | cardBeats gc c = c
                         | otherwise      = chooseCard gc h

