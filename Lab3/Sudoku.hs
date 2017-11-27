import Data.Char
import Data.List
import Data.Int
import Data.Maybe hiding (isNothing)
import Test.QuickCheck

-------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 4,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 8,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]--
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just
-----------------------------------------------------------------------------

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

--Checks if all elements in a row are sudoku symbols
isRowsOk :: Sudoku -> Bool
isRowsOk (Sudoku []) = True
isRowsOk s = (length x == 9) && isElementsOk x (Nothing:(map Just [1..9]))
                             && (isRowsOk (Sudoku xs))
    where (x:xs) = (rows s)

--Checks if elemnts in the fist argument existis in the second
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

-----------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = do putStr (getSudokuStr s)

-- Gets a string from a sudoku
getSudokuStr :: Sudoku -> String
getSudokuStr (Sudoku []) = ""
getSudokuStr s = (getRowStr x) ++"\n"++ getSudokuStr(Sudoku xs)
            where (x:xs) = (rows s)

--Gets a string from a row in a sudoku
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


-- Creates a sudoku from a string
createSudoku :: String -> Sudoku
createSudoku str = Sudoku (map createIntList (createSudokuList str))

-- Splits a sudoku string (with \n as new line character) into a list of
-- strings with rows of 9 characters
createSudokuList :: String -> [String]
createSudokuList "" = []
createSudokuList str = (take 9 str): (createSudokuList(drop 10 str))

--Creates a list of Maybe Ints from a sudoku string row
createIntList :: String -> [Maybe Int]
createIntList [] = []
createIntList (x:xs) | x == '.' = (Nothing):(createIntList xs)
                     | otherwise = (Just (digitToInt x)):(createIntList xs)

-----------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = do n <- rNum
          x <- (frequency [(2,elements [Just n]),(8, return Nothing)])--elements [Nothing])])
          return x

--Generates a random integer between 1-9
rNum :: Gen Int
rNum = elements [ n|n<-[1..9]]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       if isOkay (Sudoku rows)
       then return (Sudoku rows)
       else arbitrary

-- * C3

-- Checks if sudoku contains the correct characters and has the correct size
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-----------------------------------------------------------------------------

-- * D1

--A block is either a row, column or a box in a Sudoku
type Block = [Maybe Int]

--Checks if the block is legal
isOkayBlock :: Block -> Bool
isOkayBlock [] = False
isOkayBlock b = isElementsOk b (Nothing:(map Just [1..9]))
                && noDuplicates b

--Check if there are no duplicates in the given block
noDuplicates :: Block -> Bool
noDuplicates b = noDuplicatesRecursion b []

--Checks if there are any common elements between arguments and also checks
--for duplicates in first list
noDuplicatesRecursion :: Block -> Block -> Bool
noDuplicatesRecursion [] _ = True
noDuplicatesRecursion (x:xs) y = if  (x /= Nothing) && (x `elem` y)
                        then False
                        else noDuplicatesRecursion xs (x:y)

--Generates a list of all Blocks from a Sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku s) = concat [s,transpose s, boxBlocks s]

--Creates a list of all boxes from a list of blocks (a sudoku)
boxBlocks :: [Block] -> [Block]
boxBlocks [] = []
boxBlocks s = concat (makeBox (take 3 s)
                        : [boxBlocks (drop 3 s)])

--Given 3 rows returns 3 boxes
makeBox :: [Block] -> [Block]
makeBox [[],[],[]] = []
makeBox [a,b,c] = concat(map (take 3) [a,b,c])
                     : makeBox(map (drop 3) [a,b,c])
makeBox _ = error "Bad block!"

--Property: a sudoku should have 9 rows, 9 columns and 9 boxes all containing
--9 cells
prop_Enough_Blocks :: Sudoku -> Bool
prop_Enough_Blocks (Sudoku s) = (length (blocks (Sudoku s))) == 27
                                && recLength (blocks (Sudoku s))

--Checks if size of all Blocks are legal
recLength :: [Block] -> Bool
recLength [] = True
recLength (x:xs) = (length x) == 9
                   && recLength xs

--Checks if a sudoku follows all rules
isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (blocks s))

------------------------------------ E --------------------------------------


type Pos = (Int,Int)

posList :: [Pos]
posList = [(x,y) | x <- [0..8], y <- [0..8]]
--
--isOneOne :: Pos -> Bool
--isOneOne (1,1) = True
--isOneOne _ = False

blanks :: Sudoku -> [Pos]
blanks (Sudoku s) = map snd (filter (isNothing) (zip (concat s) posList))

isNothing :: (Maybe Int, Pos) -> Bool
isNothing (Nothing, _) = True
isNothing _ = False

prop_isblanks :: Sudoku -> Bool
prop_isblanks (Sudoku ss)=
                 and[((ss !! (fst p)) !! (snd p) ) == Nothing | p <- ps ]
                 where ps = blanks (Sudoku ss)

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = error "List is empty"
(!!=) xs (n,_) | n < 0 || length(xs) <= n = error "Illegal index"
(!!=) (x:xs) (0,a) = a:xs
(!!=) (x:xs) (n,a) = x:((!!=) xs ((n-1),a))

prop_insert :: [Maybe Int] -> (Int,Maybe Int) -> Bool
prop_insert [] _ = True
prop_insert xs (n,_) | n < 0 || length(xs) <= n = True
prop_insert a (n,b) = (length a2 == length a)
                      && ((a2 !! n) == b)
                    where a2 = (a !!= (n,b))

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku s) p m =
                    Sudoku (s !!= (fst p, ((s !! (fst p)) !!= ((snd p),m))))

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update (Sudoku s) p m = isOkay (Sudoku s2)
                            && (s2 !! (fst p)) !! (snd p) == m
                where (Sudoku s2) = (update (Sudoku s) p m)


candidates :: Sudoku -> Pos -> [Int]
candidates s (r,c) = ([1..9] \\ (map fromJust (delete Nothing (nub taken))))
              where blockList = blocks s
                    boxCol = c `quot` 3
                    boxRow = r `quot` 3
                    boxIndex = 18 + (3 * boxRow) + boxCol
                    taken = (blockList !! (boxIndex))
                          `union` (blockList !! (c + 9))
                          `union` (blockList !! r)

prop_candi :: Sudoku -> Pos -> Bool
prop_candi s p = helpPropCandi s p1 (candidates s p1)
        where p1 = (abs((fst p) `mod` 8),abs ((snd p) `mod` 8))

helpPropCandi :: Sudoku -> Pos -> [Int] -> Bool
helpPropCandi _ _ [] = True
helpPropCandi s p (x:xs) = isSudoku s2 && isOkay s2 && (helpPropCandi s p xs)
                where s2 = update s p (Just x)


