data Rank = Numeric Int | Jack | Queen | King | Ace 
  deriving Show

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving Show 

data Card = Card Suit Rank
  deriving Show

--not part of the exercise
makeCard :: Suit -> Rank -> Maybe Card
makeCard suit (Numeric v) = if 2 <= v && v <= 10
                              then Just (Card suit (Numeric v))
                              else Nothing

getValue :: Rank -> Int
getValue Ace          = 11
getValue Jack         = 10
getValue Queen        = 10
getValue King         = 10
getValue (Numeric v)  = v

getCardValue :: Card -> Int
getCardValue (Card _ rank) = getValue rank

getCardValue' :: Card -> Int
getCardValue' (Card _ (Numeric x))  = x
getCardValue' (Card _ Ace)          = 11
getCardValue' (Card _ _)            = 10

getCardValue'' :: Card -> Int
getCardValue'' (Card _ rank) = getValue rank
  where
    --getValue :: Rank -> Int
    getValue Ace         = 11
    getValue (Numeric v) = v
    getValue _           = 10


getCardValue''' :: Card -> Int
getCardValue''' (Card _ rank) = 
  let
    --getValue :: Rank -> Int
    getValue Ace         = 11
    getValue (Numeric v) = v
    getValue _           = 10
  in getValue rank


--data Hand = Nil | Cons Card Hand
--  deriving Show

--(<+>) :: Hand -> Hand -> Hand
--(<+>) Nil Nil    = Nil
--(<+>) Nil h      = h
--(<+>) h Nil      = h
--(<+>) (Cons c1 h1) h2  = Cons c1 ((<+>) h1 h2)

type Hand = [Card]

(<+>) :: Hand -> Hand -> Hand
(<+>) = (++)
