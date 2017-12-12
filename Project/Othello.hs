module Othello where
import Data.Maybe

data Brick = Black | White
    deriving (Eq)

instance Show Brick where
    show Black = "Black"
    show White = "White"

data Othello = Othello {rows :: [[Maybe Brick]]}

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
      , [w  ,w  ,w  ,w  ,w  ,w  ,w  ,w   ]
      , [w  ,w  ,b  ,w  ,w  ,b  ,w  ,w   ]
      , [w  ,b  ,n  ,b  ,w  ,w  ,b  ,w   ]
      ]
  where
    n = Nothing
    b = Just Black
    w = Just White

-- printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printO :: Othello -> IO ()
printO o = do putStr (getOStr o)
-- Gets a string from a sudoku
getOStr :: Othello -> String
getOStr (Othello []) = ""
getOStr o = (getRowStr x) ++"\n"++ getOStr(Othello xs)
            where (x:xs) = (rows o)

--Gets a string from a row in a sudoku
getRowStr :: [Maybe Brick] -> String
getRowStr [] = ""
getRowStr ((Just b):xs) = show b ++ getRowStr xs
getRowStr (x:xs) = "." ++ getRowStr xs

--Maybe not needed...
--removeNothing, removes all Nothing elements from a list of Maybe _wildcard_
--removeN :: [Maybe a] -> [Maybe a]
--removeN []     = []
--removeN (x:xs) = if x == Nothing
--                    then removeN xs
--                    else x:(removeN xs)

--isOthelloOK
isOOK :: Othello -> Bool
isOOK o = length(rows o) == 8 && isOROK o
--isOthelloRowsOK
isOROK :: Othello -> Bool
isOROK o = and $ map isROK oRows
    where oRows = rows o
--isRowOK
isROK :: [Maybe Brick] -> Bool
isROK r = length r == 8

--Insert function
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

--Updates the sudoku with the given int at the given pos, returns updated sud
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

flipB' :: Othello -> Pos -> Maybe Brick -> [Pos] -> Othello
flipB' o _ _ []    = o
flipB' o p mb (x:xs) = flipB' o2 p mb xs
                    where o2 = flipBB o p mb x

--flipBricksBetween
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
--List of directions
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

--allLegalMoves(for)Brick returns a list of all possible moves (pos) of the given
--brick type
allLMB :: Othello -> Maybe Brick -> [Pos]
allLMB o mb = map snd $filter (isLMB mb) $zip (64 `replicate` o)
                $filter (isPE o) posList
--isPosEmpty
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

--canPlayer*Brick*Move
canPM :: Othello -> Brick -> Bool
canPM o b = length (allLMB o (Just b)) > 0

--try(to)PlaceBrick
tryPB :: Othello -> Pos -> Brick -> (Bool, Othello)
tryPB o p b = if p `elem` (allLMB o (Just b))
                    then (True, placeBA o p b)
                    else (False, o)

--getPlayerScore returns score of that player
getPS :: Othello -> Brick -> Int
getPS o b = length $ filter ((Just b) == ) $ concat $ rows o

getWinner :: Othello -> Maybe Brick
getWinner o | (getPS o White) > (getPS o Black) = Just White
            | (getPS o White) < (getPS o Black) = Just Black
            | otherwise = Nothing

helpPlayers :: Othello -> Brick -> (Othello, Brick)
helpPlayers o p | 1 == length (allLMB o (Just p)) = helpPlayers no np
                | otherwise                  = (o, p)
            where no = snd $tryPB o (head (allLMB o (Just p))) p
                  np = decidePlayer no p

decidePlayer :: Othello -> Brick -> Brick
decidePlayer o Black = if canPM o White
                        then White
                        else Black
decidePlayer o White = if canPM o Black
                        then Black
                        else White