--Praesenzteil Black Jack 1
data Rank = Numeric Int | Jack | Queen | King | Ace 
  deriving Show

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving Show 

data Card = Card Suit Rank
  deriving Show

type Hand = [Card]

(<+>) :: Hand -> Hand -> Hand
(<+>) = (++)

--Hausaufgabe 1) Black Jack Erweiterung
getCardValue :: Card -> Int
getCardValue (Card _ (Numeric n)) = n
getCardValue (Card _ Ace) = 11
getCardValue (Card _ _) = 10

--List of all Suits
suits :: [Suit]
suits = [Clubs, Spades, Hearts, Diamonds]

--List of all Ranks
ranks :: [Rank]
ranks = [Numeric v | v <- [2..10]] ++ [Jack, Queen, King, Ace]

--returns a full deck of all 52 cards
fullDeck :: Hand
fullDeck = [Card suit rank | suit <- suits, rank <- ranks]

--Number of Aces in a hand
numOfAces :: Hand -> Int
numOfAces [] = 0
numOfAces ((Card _ Ace):xs) = 1 + numOfAces xs
numOfAces (_:xs) = numOfAces xs

--total Value of a hand
getValue :: Hand -> Int
getValue hand = if totalValue <= 21 then totalValue 
                                    else totalValue - 10 * numOfAces hand
  where totalValue = sum (map getCardValue hand)