
data Brick = Black | White
 deriving (Show, Eq)

--data List8 = [Maybe Brick
--             ,
--             Maybe Brick
--             ,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,Maybe Brick,]


--type List8 a = [a,a,a,a,a,a,a,a]


data Othello = Othello {rows :: [[Maybe Brick]] }
 deriving (Show, Eq)

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


