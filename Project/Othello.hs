module Othello where

data Brick = Black | White
 deriving (Show, Eq)

--data List8 = [Maybe Brick
--             ,
--             Maybe Brick
--             ,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,]
--data BS = Maybe Brick

data Othello = Othello {rows :: [[Maybe Brick]]}
type Pos = (Int,Int)

exampleO :: Othello
exampleO =
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




--(!!=) :: [a] -> (Int,a) -> [a]
--(!!=) [] _                                = []
--(!!=) xs (n,_) | n < 0 || length(xs) <= n = xs
--(!!=) (x:xs) (0,a)                        = a:xs
--(!!=) (x:xs) (n,a)                        = x:((!!=) xs ((n-1),a))
--
--
----Updates the sudoku with the given int at the given pos, returns updated sud
--update :: Othello -> Pos -> Maybe Brick -> Othello
--update (Othello o) p mb = Othello (o !!=
--                        (fst p, ((o !! (fst p)) !!= ((snd p),mb))))
--
--



legalMoves :: Othello -> [(Int,Int)]
legalMoves = undefined



--makeMove :: Othello -> Brick -> ()
