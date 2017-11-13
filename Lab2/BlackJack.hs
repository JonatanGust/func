module BlackJack where
import Cards
import RunGame
import System.Random
import Test.QuickCheck hiding (shuffle)

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
winner h1 h2  | not(gameOver h1)
                && ((gameOver h2) || (value h1 > value h2)) = Guest
              | otherwise = Bank


------------------- Part A Above, Part B Below --------------


--Returns the first Hand ontop of the second Hand
(<+) :: Hand -> Hand -> Hand
(<+) hand Empty = hand
(<+) Empty hand = hand
(<+) (Add c1 h1) h2 = Add c1 (h1 <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size(h1 <+ h2)

--Returns the Hand containing all cards of all suits (not shuffled)
fullDeck :: Hand
fullDeck = (fullSuit Spades) <+ (fullSuit Hearts)
           <+ (fullSuit Diamonds) <+ (fullSuit Clubs)

--Call w/ desired Suit to create hand of full suit
fullSuit :: Suit -> Hand
fullSuit suit = fillSuitUp suit 2

--fullSuit helper function (hides initial value i from user)
fillSuitUp :: Suit -> Integer -> Hand
fillSuitUp suit i | i<11 = card (Numeric i) suit <+ (fillSuitUp suit (i+1))
fillSuitUp suit 11 = card Jack suit <+ card Queen suit <+ card King suit <+
                     card Ace suit

--Makes notation of fillSuitUp much shorter
card :: Rank -> Suit -> Hand
card r s = Add (Card r s) Empty

--Takes the first card from the Deck (first Hand) and ads it to the second
--  Hand, returns (Deck, Hand)
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add cD hD) hP = (hD, (Add cD Empty) <+ hP)

--Makes the bank play the game with the given deck (Hand)
playBank :: Hand -> Hand
playBank deck = playBank' deck empty

--Helper function that does all the recursions (playBank initiates it)
playBank' :: Hand -> Hand -> Hand
playBank' _ hand | value hand >= 16 = hand
playBank' deck hand = playBank' deck' hand'
  where (deck', hand') = draw deck hand


--Shuffles the order of the cards in Hand (first card given by StdGen)
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g h = pickCard r h <+ shuffle g1 (removeCard r h)
    where (r, g1) = randomR(1, size h) g

--Returns the n'th (Integer) card in the Hand
pickCard :: Integer -> Hand -> Hand
pickCard 1 (Add card _) = (Add card Empty)
pickCard n (Add _ hand) = pickCard (n-1) hand

--Returns the Hand without the n'th (Integer) card
removeCard :: Integer -> Hand -> Hand
removeCard 1 (Add _ hand) = hand
removeCard n (Add card hand) = (Add card Empty) <+ removeCard (n-1) hand


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation