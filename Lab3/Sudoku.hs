import Data.Char
import Data.List
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
getSudokuStr (Sudoku []) = ""
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
readSudoku path = do sudtext <- readFile path
                     let sud = createSudoku sudtext
                     if not (isSudoku sud)
                     then error "Not a sudoku"
                     else return sud



createSudoku :: String -> Sudoku--[[Maybe Int]]
createSudoku str = Sudoku (map createIntList (createSudokuList str))

createSudokuList :: String -> [String]
createSudokuList "" = []
createSudokuList str = (take 9 str): (createSudokuList(drop 10 str))

createIntList :: String -> [Maybe Int]
createIntList [] = []
createIntList (x:xs) | x == '.' = (Nothing):(createIntList xs)
                     | otherwise = (Just (digitToInt x)):(createIntList xs)

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(2,rMaybeInt),(8, elements [Nothing])]

rMaybeInt :: Gen (Maybe Int)
rMaybeInt = do n <- rNum
               return (Just n)

rNum :: Gen Int
rNum = elements [ n|n<-[1..9]]

--genNothing :: Gen (Maybe Int)
--genNothing = elements [Nothing]



-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-------------------------------------------------------------------------

-- * D1

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock [] = False
isOkayBlock b = isElementsOk b (Nothing:(map Just [1..9]))
                && noDuplicates b []

noDuplicates :: Block -> Block -> Bool
noDuplicates [] _ = True
noDuplicates (x:xs) y = if  (x /= Nothing) && (x `elem` y)
                        then False
                        else noDuplicates xs (x:y)

blocks :: Sudoku -> [Block]
blocks (Sudoku s) = concat [s,colsBlocks s,squareBlocks s]

colsBlocks :: [Block] -> [Block]
colsBlocks s = transpose s

squareBlocks :: [Block] -> [Block]
squareBlocks [] = []
squareBlocks s = concat (makeSquare (take 3 s)
                        : [squareBlocks (drop 3 s)])

makeSquare :: [Block] -> [Block]
makeSquare [[],[],[]] = []
makeSquare [a,b,c] = concat(map (take 3) [a,b,c])
                     : makeSquare(map (drop 3) [a,b,c])
makeSquare _ = error "Bad block!"

prop_Enough_Blocks :: Sudoku -> Bool
prop_Enough_Blocks (Sudoku s) = (length (blocks (Sudoku s))) == 27
                                && recLength (blocks (Sudoku s))

recLength :: [Block] -> Bool
recLength [] = True
recLength (x:xs) = (length x) == 9
                   && recLength xs










exB :: [Block]
exB = [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
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

exB1 :: [Block]
exB1 = [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
       , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
       , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
       ]
       where
            n = Nothing
            j = Just