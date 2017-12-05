module Othello where
import Data.FixedList

data Brick = Black | White
 deriving (Show, Eq)

--data List8 = [Maybe Brick
--             ,
--             Maybe Brick
--             ,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,]
data BS = Maybe Brick

data Othello = Othello {rows :: [[BS]]}


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


legalMoves :: Othello -> [(Int,Int)]
legalMoves = undefined


--makeMove :: Othello -> Brick -> ()
