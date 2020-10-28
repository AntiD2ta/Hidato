module Structures
( Board(..),
  Coord,
  Cell,
  buildBoard,
  getFixedCoord,
  getStartCoord,
  lookupMax,
  getMatrixToStr,
  getStringToTable,
  ladder,
  easy,
  normal,
  hard,
  insane
) where

--import Data.Map((!))
import Data.Char
import Text.Read
import qualified Data.Map as Map
import qualified Data.Set as Set  

type Coord = (Integer, Integer)
type Cell = (Integer, Bool)

data Board = Board { fixedCoords :: Map.Map Integer Coord
                   , matrix :: Map.Map Coord Cell
                   } deriving (Show, Eq)


buildBoard :: [[Integer]] -> Board
buildBoard l = let matrix      = buildMatrix l
                   fixedSet    = buildFixedSet l
                   fixedCoords = buildFixedCoords fixedSet matrix
                in Board {fixedCoords=fixedCoords, matrix=matrix}


buildMatrix :: [[Integer]] -> Map.Map Coord Cell
buildMatrix l = let matrix = zipWith (\x r -> buildRow x r 0) l [0..] 
               in Map.fromList $ concat matrix


buildRow :: [Integer] -> Integer -> Integer -> [(Coord, Cell)]
buildRow l r c
    | l == [] = []
    | head l /= 0 = ((r, c), (head l, True)) : buildRow (tail l) r (c+1)
    | otherwise =  ((r, c), (0, False)) : buildRow (tail l) r (c+1)


buildFixedSet :: [[Integer]] -> Set.Set Integer
buildFixedSet l = Set.fromList $ concat $ map (\l' -> nonZero l') l
    where nonZero = filter (\x -> x /= 0)


buildFixedCoords :: Set.Set Integer -> Map.Map Coord Cell -> Map.Map Integer Coord
buildFixedCoords s b = Map.fromList $ reverse listMapped
    where getPairs   = Map.filterWithKey (\k v -> Set.member (fst v) s) b
          mapToList  = Map.toList getPairs
          listMapped = map (\(p, (n, _)) -> (n, p)) mapToList


getFixedCoord :: Board -> Integer -> Maybe Coord
getFixedCoord b x = Map.lookup x (fixedCoords b)


getStartCoord :: Board -> Coord
getStartCoord b = case Map.lookup 1 dict of Nothing -> (-1, -1)
                                            Just c -> c
    where dict = fixedCoords b


lookupMax :: (Ord k) => Map.Map k a -> k
lookupMax m = last $ Map.keys m

lookupMin :: (Ord k) => Map.Map k a -> k
lookupMin m = head $ Map.keys m


getSpace :: (Num a, Ord a) => a -> String
getSpace n
    | n < 10    = "  "
    | otherwise = " "


getMatrixToStr :: Board -> String
getMatrixToStr b = unlines (map concat getRows) ++ "\n"
    where table    = matrix b 
          pairs    = Map.assocs table
          getRow x = map (\((_,_), (n,_)) -> show n ++ getSpace n ) $ filter (\((r,_), (_,_)) -> r == x) pairs
          top      = fst $ lookupMax table 
          getRows  = foldr (\x acc -> getRow x:acc) [] [0..top]


-- //TODO: Print a message error in main instead or raising error. Return ([[Integer]], Bool)
getStringToTable :: String -> [[Integer]]
getStringToTable input
    | check     = table
    | otherwise = error("Invalid input")
    where rawRows       = lines input
          readInteger x = getMaybeInt $ readMaybe x :: Integer
          table         = map (map readInteger) $ map words rawRows
          check         = checkTable table
          

checkTable :: [[Integer]] -> Bool
checkTable t = getMax == numCells
    where getMax   = fromInteger $ foldl (\acc x -> if maximum x > acc then maximum x else acc) 0 t
          numCells = foldl (\acc x -> acc + length x) 0 t


getMaybeInt :: Maybe Integer -> Integer
getMaybeInt Nothing = 0
getMaybeInt (Just n) = n


easy = [[12,0,0,24,25],
        [0,10,16,0,0],
        [0,7,1,0,0],
        [6,0,18,0,0],
        [0,0,0,0,0]]

ladder = [[0],
          [0, 8],
          [0, 0, 11],
          [29, 0, 10, 0],
          [30, 0, 0, 0, 0],
          [0, 31, 1, 38, 0, 0],
          [0, 32, 0, 0, 39, 41, 0],
          [0, 0, 0, 22, 0, 0, 42, 0],
          [0, 0, 0, 0, 0, 0, 0, 44, 45]]

insane = [[0,0,0,0,81,0,55,0,0],
        [17,0,0,0,0,3,0,0,0],
        [16,0,10,0,0,0,1,0,0],
        [12,11,0,0,0,0,58,49,0],
        [0,0,0,0,23,0,59,48,0],
        [0,0,0,0,30,0,60,0,47],
        [73,0,0,34,0,0,0,62,44],
        [0,71,0,66,0,0,0,0,43],
        [69,0,0,0,37,64,0,42,0] ]

hard = [[0, 0, 49, 0, 0, 0, 34],
          [0, 0, 0, 0, 31, 0, 0],
          [0, 14, 0, 5, 29, 38, 0],
          [0, 0, 4, 0, 0, 0, 0],
          [0, 0, 16, 0, 0, 0, 22],
          [0, 0, 0, 0, 0, 0, 0],
          [0, 0, 1, 19, 0, 0, 25]]

normal = [[0, 12, 0, 0, 0, 36],
        [0, 0, 0, 10, 0, 0],
        [17, 0, 0, 0, 0, 0],
        [0, 16, 23, 6, 0, 31],
        [1, 0, 0, 0, 0, 29],
        [0, 0, 4, 26, 0, 0]]
