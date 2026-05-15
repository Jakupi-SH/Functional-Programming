import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (ord, chr, isDigit, isLetter)
import Data.Bifunctor     

data CellValue = Number Double
                | Formula (Spreadsheet -> Double)
                | Reference String

instance Show CellValue where
  show (Number x)  = show x
  show (Formula _) = "Formula"
  show (Reference r) = "Ref(" ++ r ++ ")"

type Position = (Int, Int)  -- (Row, Column)
type Spreadsheet = [(Position, CellValue)]


evalCell :: Position -> Spreadsheet -> Double
evalCell _ [] = 0
evalCell pos sheet =
  case lookup pos sheet of
    Just (Number n)  -> n
    Just (Formula f) -> f sheet
    Just (Reference r) -> evalCell (referenceToPosition r) sheet
    Nothing          -> 0

updateCell :: Position -> CellValue -> Spreadsheet -> Spreadsheet
updateCell pos value [] = [(pos, value)]
updateCell pos value ((p,v):xs)
    | pos == p  = (pos, value) : xs
    | otherwise = (p,v) : updateCell pos value xs


--the map keyword ensures that a function is applied to every value which is given to it
-- the .second function ensures that function "f" affects only the second values
-- in the spreadsheet pair.
mapSpreadsheet :: (CellValue -> CellValue) -> Spreadsheet -> Spreadsheet
mapSpreadsheet f =
    map (Data.Bifunctor.second f)

filterCellsByValue :: (Double -> Bool) -> Spreadsheet -> Spreadsheet
filterCellsByValue predicate sheet =
    filter (\(_,val) -> predicate (cellToDouble val sheet)) sheet


countCellsBy :: (Double -> Bool) -> Spreadsheet -> Int
countCellsBy predicate sheet =
    length (filterCellsByValue predicate sheet)



sumRange :: Position -> Position -> Spreadsheet -> Double
sumRange (r1,c1) (r2,c2) sheet =
    sum
        [ evalCell (r,c) sheet
        | r <- [r1..r2]
        , c <- [c1..c2]
        ]


mapRange :: (Double -> Double)
         -> Position
         -> Position
         -> Spreadsheet
         -> Spreadsheet

mapRange f (r1,c1) (r2,c2) sheet =
    map update sheet
  where
    update (pos@(r,c), val)
        | inRange r c =
            (pos, Number (f (cellToDouble val sheet)))
        | otherwise =
            (pos, val)

    inRange r c =
        r >= r1 && r <= r2 &&
        c >= c1 && c <= c2


sortCellsByValue :: Spreadsheet -> Spreadsheet
sortCellsByValue sheet =
    sortBy
        (comparing (\(_,val) -> cellToDouble val sheet))
        sheet


-- Helper functions =======================

cellToDouble :: CellValue -> Spreadsheet -> Double
cellToDouble (Number n) _     = n
cellToDouble (Formula f) s    = f s
cellToDouble (Reference r) s  = evalCell (referenceToPosition r) s

--takewhile takes the values from the list as long as they are letters/ digits, 
--depending on the part of the function
--uses dropwhile which drops the letters in the list which is meant for digits
referenceToPosition :: String -> Position
referenceToPosition str =
    (row, column)
  where
    letters = takeWhile isLetter str
    digits  = takeWhile isDigit (dropWhile isLetter str)

    row = read digits
    column = lettersToNumber letters


positionToReference :: Position -> String
positionToReference (row,col) =
    numberToLetters col ++ show row



--this functions was made with the help of the foldl in order to accumulate the letters into a number
--it also utilizes ord to turn characters into ASCII numbers
--proper calculation is made afterwards
-- "AA1" -> (1,27)
lettersToNumber :: String -> Int
lettersToNumber =
    foldl (\acc ch -> acc * 26 + (ord ch - ord 'A' + 1)) 0

--utilizes recursion in case the integer is bigger than 26,
--making the reference a string with two characters like "AA = 27"
numberToLetters :: Int -> String
numberToLetters n
    | n <= 0    = ""
    | otherwise =
        numberToLetters ((n - 1) `div` 26)
        ++ [chr (ord 'A' + ((n - 1) `mod` 26))]



-- spreadsheet data for testing the functions

sheet :: Spreadsheet
sheet =
    [ ((1,1), Number 10)
    , ((1,2), Number 20)

    , ((2,1),
        Formula (\s ->
            evalCell (1,1) s +
            evalCell (1,2) s))

    , ((2,2), Reference "A1")
    ]


-- This is a list of the ways the functions were tested in the terminal

-- evalCell (1,1) sheet
-- 10.0

-- evalCell (2,1) sheet
-- 30.0

-- evalCell (2,2) sheet
-- 10.0

-- updateCell (1,1) (Number 100) sheet

-- sumRange (1,1) (2,2) sheet
-- 70.0

-- positionToReference (3,2)
-- "B3"

-- referenceToPosition "AA1"
-- (1,27)

-- countCellsBy (>15) sheet
-- 2

-- sortCellsByValue sheet
-- [((1,1),10.0),((2,2),Ref(A1)),((1,2),20.0),((2,1),Formula)]


--ghci> mapSpreadsheet (\c -> Number 1) sheet
--[((1,1),1.0),((1,2),1.0),((2,1),1.0),((2,2),1.0)]

-- ghci> filterCellsByValue (>15) sheet
-- [((1,2),20.0),((2,1),Formula)]

-- ghci> countCellsBy (>15) sheet
-- 2

-- ghci> mapRange (*2) (1,1) (2,2) sheet
-- [((1,1),20.0),((1,2),40.0),((2,1),60.0),((2,2),20.0)]
