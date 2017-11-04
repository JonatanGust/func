module BlackJack where
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
exh = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) (Add (Card Ace Spades) Empty) ))

--Returns the empty hand
empty :: Hand
empty = Empty

--Returns the value of a hand
value :: Hand -> Integer
value Empty = 0
value hand | val <= 21 = val
    where val = valueHand(hand)
value hand | otherwise = valueHand(hand) - (10 * numberOfAces(hand))



--Returns value of hand where aces = 11
valueHand :: Hand -> Integer
valueHand (Add c h) = valueCard c + value h

--Returns value of Rank
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank r | r==Jack || r==Queen || r==King = 10
valueRank (Numeric r) = r

--Returns value of Card
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

--Returns number of aces in Hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r s) hand) | r == Ace = 1 + numberOfAces hand
numberOfAces (Add (Card r s) hand) = numberOfAces hand


--Returns true if Hand is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--Returns the winner of two Hands, (Guest, Bank)
winner :: Hand -> Hand -> Player
winner h1 h2 | gameOver h1 = Bank
winner h1 h2 | value h1 > value h2 = Guest
winner h1 h2 | otherwise = Bank


