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

--Hand w/ val 13 3 cards
exh1 = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) (Add
        (Card Ace Spades) Empty) ))
--Hand w/ val 15 4 cards
exh2 = (Add (Card (Numeric 3) Hearts) (Add (Card Jack Spades) (Add
        (Card Ace Spades) (Add (Card Ace Hearts) Empty)) ))
--Hand w/ val 23 3 cards
exh3 = (Add (Card (Numeric 3) Clubs) (Add (Card Jack Spades) (Add
        (Card King Diamonds) Empty) ))

--Returns the empty hand
empty :: Hand
empty = Empty

--Returns the value of the Hand
value :: Hand -> Integer
value Empty = 0
value hand | val <= 21 = val
    where val = valueHand(hand)
value hand | otherwise = valueHand(hand) - (10 * numberOfAces(hand))

--Returns value of Hand where aces = 11
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
numberOfAces (Add card hand) = numberOfAces hand


--Returns true if Hand value is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--Returns the winner of two Hands, in order Guest, Bank
winner :: Hand -> Hand -> Player
winner h1 h2 | gameOver h1 = Bank
winner h1 h2 | value h1 > value h2 = Guest
winner h1 h2 | otherwise = Bank

(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2 = hand2
(<+) (Add c1 h1) h2 = Add c1 (h1 <+ h2)

addHand :: Hand -> Hand -> Hand
addHand Empty hand2 = hand2
addHand (Add c1 h1) h2 = Add c1 (addHand h1 h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size(h1 <+ h2)

fullDeck :: Hand
fullDeck = (fullSuit Spades 2) <+ (fullSuit Hearts 2)
           <+ (fullSuit Diamonds 2) <+ (fullSuit Clubs 2)

--Call w/ desired Suit and 2 to create hand of full suit
fullSuit :: Suit -> Integer -> Hand
fullSuit suit 1 = Empty
--fullSuit suit i | i>14 || i<1 = Error "Call to fullSuit done w/ bad number, use 2"
fullSuit suit i | i<11 = Add (Card (Numeric i) suit) (fullSuit suit (i+1))
fullSuit suit 11 = Add (Card Jack suit) (fullSuit suit (12))
fullSuit suit 12 = Add (Card Queen suit) (fullSuit suit (13))
fullSuit suit 13 = Add (Card King suit) (fullSuit suit (14))
fullSuit suit 14 = Add (Card Ace suit) (fullSuit suit (1))