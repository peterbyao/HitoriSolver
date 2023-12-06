module Lib
    ( someFunc
    ) where


data Cell = Cell {xCoord :: Int, yCoord :: Int, value :: Int, isShaded :: Bool}

instance Show Cell where 
    show (Cell _ _ val isBlack)
        | isBlack = "■"
        | otherwise = show val

newtype Board = Board [Cell]

instance Show Board where
    show board =
        unlines [concatMap (showCellInRow r) (getRow board r) | r <- [minY .. maxY]]
        where 
            Board cells = board
            minY = minimum $ map yCoord cells
            maxY = maximum $ map yCoord cells

showCellInRow :: Int -> Cell -> String
showCellInRow r cell
    | yCoord cell == r = show cell ++ " "
    | otherwise = " "

getRow :: Board -> Int -> [Cell]
getRow board r = filter (\cell -> yCoord cell == r) cells 
    where
        Board cells = board

getCol :: Board -> Int -> [Cell]
getCol board c = filter (\cell -> xCoord cell == c) cells 
    where
        Board cells = board 

someFunc :: IO ()
someFunc = putStrLn "someFunc"
