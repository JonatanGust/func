module Othello where
import Data.Maybe
--import Test.QuickCheck
data Brick = Black | White
    deriving (Eq)

instance Show Brick where
    show Black = "Black"
    show White = "White"

data Othello = Othello {rows :: [[Maybe Brick]]}
    deriving (Show, Eq)
{--- | an instance for generating Arbitrary Othellos
rBrick :: Gen (Maybe Brick)
rBrick = do rb <- (frequency [(5,elements [Just White, Just Black]),
                                (5, return Nothing)])
            return rb
instance Arbitrary Othello where
  arbitrary =
    do rows <- vectorOf 8 (vectorOf 8 rBrick)
       if isOOK (Othello rows)
       then return (Othello rows)
       else arbitrary
instance Arbitrary Brick where
          arbitrary =
            do rb <- elements [White, Black]
               return rb-}

type Pos = (Int,Int)

--A list of all possible Pos in an Othello
posList :: [Pos]
posList = [(x,y) | x <- [0..7], y <- [0..7]]

--emptyOthello is an enmpty othello (in the sense that no moves has been made)
emptyO :: Othello
emptyO =
    Othello
      [ [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,w  ,b  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,b  ,w  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n   ]
      ]
  where
    n = Nothing
    b = Just Black
    w = Just White
palmO :: Othello
palmO =
    Othello
      [ [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,w  ,w  ,w  ,w  ,b  ,w   ]
      , [w  ,w  ,b  ,w  ,w  ,b  ,w  ,w   ]
      , [w  ,b  ,n  ,b  ,w  ,n  ,n  ,w   ]
      ]
  where
    n = Nothing
    b = Just Black
    w = Just White

-- printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printO :: Othello -> IO ()
printO o = do putStr (getOStr o)
-- Gets a string representing the whole othello
getOStr :: Othello -> String
getOStr (Othello []) = ""
getOStr o = (getRowStr x) ++"\n"++ getOStr(Othello xs)
            where (x:xs) = (rows o)

--Gets a string from a row in an othello
getRowStr :: [Maybe Brick] -> String
getRowStr [] = ""
getRowStr ((Just b):xs) = text ++ getRowStr xs
        where text = if b == Black then "B"
                                    else "W"
getRowStr (x:xs) = "." ++ getRowStr xs

--isOthelloOK checks that the othello is actually an othello (rows, columns)
isOOK :: Othello -> Bool
isOOK o = length(rows o) == 8 && isOROK o
--isOthelloRowsOK checks that all rows in the given othello are ok
--(can be less/more than 8)
isOROK :: Othello -> Bool
isOROK o = and $ map isROK oRows
    where oRows = rows o
--isRowOK checks that the length of the othello row is ok
isROK :: [Maybe Brick] -> Bool
isROK r = length r == 8

--Insert function, inserts the given element at given position
--(index 0) in list
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _                                = []
(!!=) xs (n,_) | n < 0 || length(xs) <= n = xs
(!!=) (x:xs) (0,a)                        = a:xs
(!!=) (x:xs) (n,a)                        = x:((!!=) xs ((n-1),a))
--Property check of insert
prop_insert :: [Maybe Brick] -> (Int,Maybe Brick) -> Bool
prop_insert [] _                                  = True
prop_insert xs (n,_) | n < 0 || length(xs) <= n   = True
prop_insert a (n,mb)                              = (length a2 == length a)
                                                    && ((a2 !! n) == mb)
                                                    && firstA == firstA2
                                                    && lastA == lastA2
                      where a2                  = (a !!= (n,mb))
                            firstA              = take (n) a
                            firstA2             = take (n) a2
                            lastA               = drop (n+1) a
                            lastA2              = drop (n+1) a2

--Updates the othello with the given brick at the given pos,
--returns updated othello
update :: Othello -> Pos -> Maybe Brick -> Othello
update (Othello o) p mb = Othello (o !!=
                        (fst p, ((o !! (fst p)) !!= ((snd p),mb))))
--Property check of update
prop_update :: Othello -> Pos -> Maybe Brick -> Bool
prop_update (Othello o) p mb          = (o2 !! (fst p')) !! (snd p') == mb
                                        && rowsBeforeO == rowsBeforeO2
                                        && rowsAfterO == rowsAfterO2
                                        && elemsBeforeO == elemsBeforeO2
                                        && elemsAfterO == elemsAfterO2
                where (Othello o2)   = (update (Othello o) p' mb)
                      p'            = (mod (abs(fst p)) 8, mod (abs(snd p)) 8)
                      rowsBeforeO   = take (fst p') o
                      rowsBeforeO2  = take (fst p') o2
                      rowsAfterO    = drop ((fst p')+1) o
                      rowsAfterO2   = drop ((fst p')+1) o2
                      elemsBeforeO  = take (snd p') $o !! (fst p')
                      elemsBeforeO2 = take (snd p') $o2 !! (fst p')
                      elemsAfterO   = drop ((snd p')+1) $o !! (fst p')
                      elemsAfterO2  = drop ((snd p')+1) $o2 !! (fst p')

--flipBricks flips all bricks going outward from the given point according to
--rules
flipB :: Othello -> Pos -> Maybe Brick -> Othello
flipB o p mb = flipB' o p mb (findBF o p mb)
--Recursive helper function of flipBricks
flipB' :: Othello -> Pos -> Maybe Brick -> [Pos] -> Othello
flipB' o _ _ []    = o
flipB' o p mb (x:xs) = flipB' o2 p mb xs
                    where o2 = flipBB o p mb x
--flipBricksBetween
--Flips bricks between the first and the second point
--pA = point At, pG = point Goal, pC = point Cardinality, pN = point Next
flipBB :: Othello -> Pos -> Maybe Brick -> Pos -> Othello
flipBB o pA mb pG = if pA == pG
                        then o
                        else flipBB (update o pA mb) pN mb pG
                    where
                        pCr = if (fst pA) > (fst pG)
                                then -1
                                else if (fst pA) < (fst pG)
                                        then 1
                                        else 0
                        pCc = if (snd pA) > (snd pG)
                                then -1
                                else if (snd pA) < (snd pG)
                                        then 1
                                        else 0
                        pN = ((fst pA)+pCr,(snd pA)+pCc)
--List of directions is a list of all directions as vectors (legal directions)
--        (+,+),(+,0),(+,-) ,(0,-) ,(-,-)  ,(-,0) ,(-,+) ,(0,+)
pCList = [(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1),(0,1)]

--findBricks(to)Flip finds all positions that bounds a flipping line
findBF :: Othello -> Pos -> Maybe Brick -> [Pos]
findBF o p mb = catMaybes(map (findBF' 1 o p mb) pCList)
--findBricks(to)Flip' is a helper function that finds one positions that
--bounds a flipping line given a cardinality (second pos)
findBF' :: Int -> Othello -> Pos -> Maybe Brick -> Pos -> Maybe Pos
findBF' n o pA mb pC | (fst pN) < 0
                     || (fst pN) > 7
                     || (snd pN) < 0
                     || (snd pN) > 7
                     || (((rows o) !! (fst pN)) !! (snd pN)) == Nothing
                        = Nothing
                   | (((rows o) !! (fst pN)) !! (snd pN)) == mb =
                        if n > 1 then Just pN
                                 else Nothing
                   | otherwise = findBF' (n+1) o pN mb pC
                   where pN = ((fst pA)+(fst pC),(snd pA)+(snd pC))

--isLegalMove(for)Brick returns true if the given brick can be placed here
isLMB :: Maybe Brick -> (Othello, Pos) -> Bool
isLMB mb (o, p) = length (findBF o p mb) > 0

--allLegalMoves(for)Brick returns a list of all possible moves (pos) of the
--given brick type
allLMB :: Othello -> Maybe Brick -> [Pos]
allLMB o mb = map snd $filter (isLMB mb) $zip (64 `replicate` o)
                $filter (isPE o) posList

--isPosEmpty checks if the position is empty (Nothing)
isPE :: Othello -> Pos -> Bool
isPE o (r,c) = Nothing == ((rows o) !! r !! c)

--allLegalMoves is a list of all positions where any brick can be placed (can
--contain duplicates)
allLM :: Othello -> [Pos]
allLM o = (allLMB o (Just Black))++(allLMB o (Just White))

--placeBrickAt(pos) places the given brick at the position and updates the
--rest of the board acordingly
placeBA :: Othello -> Pos -> Brick -> Othello
placeBA o p b = flipB (update o p (Just b)) p (Just b)

--canPlayer(Brick)Move returns true if there is atleast one move that the
--player (brick) can do
canPM :: Othello -> Brick -> Bool
canPM o b = length (allLMB o (Just b)) > 0

--try(to)PlaceBrick places the brick at the given pos if it is a legal move,
--returns a tuple of (success of op, result), wich is to say (true, updated
--othello) or (false, given othello)
tryPB :: Othello -> Pos -> Brick -> (Bool, Othello)
tryPB o p b = if p `elem` (allLMB o (Just b))
                    then (True, placeBA o p b)
                    else (False, o)

--getPlayerScore returns score of that player
getPS :: Othello -> Brick -> Int
getPS o b = length $ filter ((Just b) == ) $ concat $ rows o

--returns the winner of the current boardstate, nothing if draw
getWinner :: Othello -> Maybe Brick
getWinner o | (getPS o White) > (getPS o Black) = Just White
            | (getPS o White) < (getPS o Black) = Just Black
            | otherwise = Nothing

--Given that a player has just made a move, this func decides who the next
--player should be
decidePlayer :: Othello -> Brick -> Brick
decidePlayer o Black = if canPM o White
                        then White
                        else Black
decidePlayer o White = if canPM o Black
                        then Black
                        else White