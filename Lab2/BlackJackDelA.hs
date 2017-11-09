module BlackJackDelA where
import Cards
import RunGame

{-
size hand2
  = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size(Empty)
  = 1 + 1 + 0
  = 2
-}

--Hand w/ val 13
exh = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades)
        (Add (Card Ace Spades) Empty) ))
exh2 = (Add (Card Ace Hearts) (Add (Card Ace Spades)
        (Add (Card Ace Spades) Empty) ))

--Returns the empty hand
empty :: Hand
empty = Empty

--Returns the value of the Hand
value :: Hand -> Integer
value Empty = 0
value hand
        | val <= 21 = val
        | otherwise = val - (10 * numberOfAces(hand))
        where val = valueHand(hand)

--Returns value of Hand where aces = 11
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add c h) = valueCard c + valueHand h

--Returns value of Rank
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank Ace = 11
valueRank r = 10

--Returns value of Card
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

--Returns number of aces in Hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add c hand) = numberOfAces hand


--Returns true if Hand value is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--Returns the winner of two Hands, in order Guest, Bank
winner :: Hand -> Hand -> Player
winner h1 h2 | (value h1 > value h2) && not(gameOver h1) = Guest
winner h1 h2 | otherwise = Bank


