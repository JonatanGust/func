
import Test.QuickCheck

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

exampleFail :: Sudoku
exampleFail =
    Sudoku
      [ [j 9,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n,j 0  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (9 `replicate` (9 `replicate` n))
    where n = Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle 9x9 and 1-9
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && isRowsOk s

isRowsOk :: Sudoku -> Bool
isRowsOk (Sudoku []) = True
isRowsOk s = (length x == 9) && isElementsOk x (Nothing:(map Just [1..9]))
                             && (isRowsOk (Sudoku xs))
    where (x:xs) = (rows s)

isElementsOk :: [Maybe Int] -> [Maybe Int] -> Bool
isElementsOk [] _ = True
isElementsOk (x:xs) a = x `elem`  a
                       && isElementsOk xs a



-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
-- can check if no nothing instead
isFilled :: Sudoku -> Bool
isFilled s = isElementsOk x (map Just [1..9]) && isFilled(Sudoku xs)
        where (x:xs) = (rows s)

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = do putStr (getSudokuStr s)


getSudokuStr :: Sudoku -> String
getSudokuStr s = (getRowStr x) ++"\n"++ getSudokuStr(Sudoku xs)
            where (x:xs) = (rows s)


getRowStr :: [Maybe Int] -> String
getRowStr [] = ""
getRowStr ((Just n):xs) = show n ++ getRowStr xs
getRowStr (x:xs) = "." ++ getRowStr xs

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do text <- readFile path

createSudoku :: String -> [[Maybe Int]]
createSudoku str = (take 9 str): createSudokuList [(drop 11)]

createSudokuList :: String -> [String]
createSudokuList "" = [""]
createSudokuList [str] = (take 9 str): (take 1 createSudokuList (drop 11 str))

--createIntList :: [String] -> [[Maybe Int]]
--createIntList str =

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-------------------------------------------------------------------------